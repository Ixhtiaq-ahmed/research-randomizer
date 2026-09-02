# =============================================================================
#  randomization_core.R
#  Randomness sources, allocation-ratio arithmetic, integer targets, design
#  objects, sequence IDs, and generate_sequence() for complete lists.
#
#  Principle: every allocation is ONE direct draw from the specified design.
#  Nothing here generates several candidate sequences or scores them.
#  Diagnostics (diagnostics.R) are descriptive and are never consulted here.
# =============================================================================

RANDOMIZER_APP_VERSION <- "1.3.0"

`%||%` <- function(a, b) if (is.null(a)) b else a

# -----------------------------------------------------------------------------
# 1. Randomness sources
# -----------------------------------------------------------------------------
# One interface for every source:
#   rng$ints(n, max)          n uniform integers in 1..max
#   rng$int(max)              one uniform integer in 1..max
#   rng$permutations(lengths) list of uniform random permutations of 1..k
#   rng$permute(x), rng$pick(x), rng$hex(n)
#   rng$describe()            audit record of exactly what was used
# Sources (see RNG_SOURCES): "openssl", "pcg64", "mt", "xorshift128plus",
# "randomorg" are production sources; "test" is Mersenne-Twister with a typed
# seed for developer regression testing only.  No source ever substitutes for
# another: if the selected source is unavailable, rng_new() stops.

RNG_SOURCES <- data.frame(
  id    = c("openssl", "pcg64", "mt", "xorshift128plus", "randomorg", "test"),
  label = c("Operating-system cryptographically secure generator (OpenSSL)",
            "PCG64 statistical generator (dqrng), seeded from the operating system",
            "Mersenne-Twister (R's default generator), seed drawn from the operating system",
            "xorshift128+ (browser Math.random algorithm; Research Randomizer-style), seeded locally",
            "RANDOM.ORG true random service (internet, API key required)",
            "TEST MODE - NOT FOR PRODUCTION: Mersenne-Twister with a typed seed"),
  class = c("recommended", "recommended", "additional", "additional", "additional", "test"),
  stringsAsFactors = FALSE)

pkg_version <- function(p) if (requireNamespace(p, quietly = TRUE)) as.character(utils::packageVersion(p)) else NA_character_
csprng_available <- function() requireNamespace("openssl", quietly = TRUE)

source_available <- function(source, api_key = NULL) {
  need <- function(pkgs) { miss <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
    if (length(miss)) list(ok = FALSE, reason = sprintf("requires package(s) %s: install.packages(c(%s))", paste(miss, collapse = ", "),
                                                        paste(sprintf("'%s'", miss), collapse = ", "))) else list(ok = TRUE, reason = "") }
  switch(source,
         openssl = need("openssl"),
         pcg64 = need(c("openssl", "dqrng")),
         mt = need("openssl"),
         xorshift128plus = need("openssl"),
         randomorg = { r <- need(c("curl", "jsonlite")); if (r$ok && (is.null(api_key) || !nzchar(trimws(api_key)))) list(ok = FALSE, reason = "requires a RANDOM.ORG API key") else r },
         test = list(ok = TRUE, reason = ""),
         list(ok = FALSE, reason = sprintf("unknown source '%s'", source)))
}

os_entropy_bytes <- function(n) {
  if (!csprng_available()) stop("Operating-system entropy requires the 'openssl' package: install.packages('openssl').")
  as.numeric(openssl::rand_bytes(as.integer(n)))
}
u32_from_bytes <- function(b) { m <- matrix(b, nrow = 4); m[1, ] * 16777216 + m[2, ] * 65536 + m[3, ] * 256 + m[4, ] }

# Uniform integer in 1..n from a uniform 32-bit word supplier, by rejection.
rejection_int <- function(n, next_u32) {
  n <- as.integer(n); if (is.na(n) || n < 1) stop("max must be >= 1")
  if (n == 1L) return(1L)
  limit <- floor(4294967296 / n) * n
  repeat { u <- next_u32(); if (u < limit) return(as.integer(u %% n) + 1L) }
}
fisher_yates <- function(k, int_fun) {
  p <- seq_len(k)
  if (k > 1) for (i in k:2) { j <- int_fun(i); tmp <- p[i]; p[i] <- p[j]; p[j] <- tmp }
  p
}

# ---- global-state guards ----------------------------------------------------
# PCG64 and Mersenne-Twister swap their private state through a process-wide
# register (dqrng's state / .Random.seed) for the duration of one draw.  That
# is atomic in the single-threaded process that created the generator, and
# only there: a generator must never be used from a forked or background
# process (its state would be a copy and the register another process's).
same_process_guard <- function(env) {
  if (!identical(env$pid, Sys.getpid()))
    stop("This generator was created in another R process (pid ", env$pid, ", now ", Sys.getpid(),
         "). PCG64 and Mersenne-Twister generators must be created and used in the same process; ",
         "do not pass them to background or async workers.")
}
get_global_seed <- function() if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) get(".Random.seed", envir = globalenv()) else NULL
set_global_seed <- function(s) { if (is.null(s)) { if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) rm(".Random.seed", envir = globalenv()) } else assign(".Random.seed", s, envir = globalenv()) }

# ---- 64-bit helpers for xorshift128+ (each word = c(hi, lo), 32-bit doubles) --
xs_h16   <- function(x) c(floor(x / 65536), x %% 65536)
xs_xor32 <- function(a, b) { ha <- xs_h16(a); hb <- xs_h16(b); bitwXor(as.integer(ha[1]), as.integer(hb[1])) * 65536 + bitwXor(as.integer(ha[2]), as.integer(hb[2])) }
xs_xor64 <- function(a, b) c(xs_xor32(a[1], b[1]), xs_xor32(a[2], b[2]))
xs_shl64 <- function(a, k) c(((a[1] * 2^k) %% 4294967296) + floor(a[2] / 2^(32 - k)), (a[2] * 2^k) %% 4294967296)
xs_shr64 <- function(a, k) c(floor(a[1] / 2^k), floor(a[2] / 2^k) + (a[1] %% 2^k) * 2^(32 - k))
xs_add64 <- function(a, b) { lo <- a[2] + b[2]; c((a[1] + b[1] + floor(lo / 4294967296)) %% 4294967296, lo %% 4294967296) }
xs_hex64 <- function(w) paste0(sprintf("%04X%04X", as.integer(floor(w[1] / 65536)), as.integer(w[1] %% 65536)),
                               sprintf("%04X%04X", as.integer(floor(w[2] / 65536)), as.integer(w[2] %% 65536)))
xs_parse64 <- function(h) { h <- toupper(gsub("[^0-9A-Fa-f]", "", h)); if (nchar(h) != 16) stop("xorshift128+ state words must be 16 hex digits.")
  c(strtoi(substr(h, 1, 4), 16L) * 65536 + strtoi(substr(h, 5, 8), 16L), strtoi(substr(h, 9, 12), 16L) * 65536 + strtoi(substr(h, 13, 16), 16L)) }
# One xorshift128+ step (Vigna 2016, shifts 23/17/26; the same core as V8/SpiderMonkey/JavaScriptCore Math.random)
xs_next <- function(env) {
  s1 <- env$s0; s0 <- env$s1; env$s0 <- s0
  s1 <- xs_xor64(s1, xs_shl64(s1, 23))
  env$s1 <- xs_xor64(xs_xor64(xs_xor64(s1, s0), xs_shr64(s1, 17)), xs_shr64(s0, 26))
  xs_add64(env$s1, s0)
}

rng_new <- function(source = "openssl", seed = NULL, api_key = NULL, signed = TRUE, transport = NULL, verify = NULL, ...) {
  if (!source %in% RNG_SOURCES$id) stop(sprintf("Unknown randomness source '%s'.", source))
  av <- source_available(source, api_key)
  if (!av$ok) stop(sprintf("Randomness source '%s' is not available: %s. Generation stopped; no other source was substituted.", source, av$reason))
  env <- new.env(parent = emptyenv())
  env$source <- source; env$label <- RNG_SOURCES$label[RNG_SOURCES$id == source]; env$class <- RNG_SOURCES$class[RNG_SOURCES$id == source]
  env$created <- Sys.time()

  if (source == "openssl") {
    env$buffer <- numeric(0); env$pos <- 0L
    next_u32 <- function() {
      if (env$pos >= length(env$buffer)) { env$buffer <- u32_from_bytes(os_entropy_bytes(4096L)); env$pos <- 0L }
      env$pos <- env$pos + 1L; env$buffer[env$pos]
    }
    env$int <- function(n) rejection_int(n, next_u32)
    env$permutations <- function(lengths) lapply(as.integer(lengths), fisher_yates, int_fun = env$int)
    env$describe <- function() c(algorithm = "operating-system CSPRNG (OpenSSL RAND_bytes)",
                                 implementation = "openssl::rand_bytes(); 32-bit words; rejection sampling; Fisher-Yates permutations",
                                 seed_or_state = "none (no seed exists)", seed_origin = "operating-system entropy", reproducible = "no",
                                 reproduction_statement = "cannot be regenerated: archive the exported list with this audit record",
                                 package_versions = sprintf("openssl %s", pkg_version("openssl")), library_version = openssl::openssl_config()$version)

  } else if (source == "xorshift128plus") {
    if (is.null(seed)) { b <- os_entropy_bytes(16L); w <- u32_from_bytes(b); env$s0 <- w[1:2]; env$s1 <- w[3:4]; env$seed_origin <- "operating-system entropy (openssl)" }
    else { if (length(seed) != 2) stop("xorshift128+ state must be two 16-hex-digit words."); env$s0 <- xs_parse64(seed[1]); env$s1 <- xs_parse64(seed[2]); env$seed_origin <- "user-supplied recorded state" }
    if (all(c(env$s0, env$s1) == 0)) stop("xorshift128+ state must not be all zero.")
    env$state0_hex <- xs_hex64(env$s0); env$state1_hex <- xs_hex64(env$s1)
    env$int <- function(n) rejection_int(n, function() xs_next(env)[1])           # upper 32 bits only
    env$permutations <- function(lengths) lapply(as.integer(lengths), fisher_yates, int_fun = env$int)
    env$describe <- function() c(algorithm = "xorshift128+ (Vigna 2016; the Math.random algorithm of current browsers)",
                                 implementation = "pure R 64-bit emulation; upper 32 bits; rejection sampling; Fisher-Yates permutations. Does NOT reproduce Research Randomizer website output.",
                                 seed_or_state = sprintf("state0 %s, state1 %s (128-bit initial state, hex)", env$state0_hex, env$state1_hex),
                                 seed_origin = env$seed_origin, reproducible = "yes",
                                 reproduction_statement = sprintf("regenerate with source 'xorshift128plus', seed = c('%s','%s'), application %s", env$state0_hex, env$state1_hex, RANDOMIZER_APP_VERSION),
                                 package_versions = sprintf("openssl %s (entropy only)", pkg_version("openssl")), library_version = "")

  } else if (source %in% c("mt", "test")) {
    if (source == "mt") {
      if (is.null(seed)) { env$seed <- as.integer(u32_from_bytes(os_entropy_bytes(4L)) %% 2147483647); env$seed_origin <- "operating-system entropy (openssl), 31 bits" }
      else { env$seed <- as.integer(seed); env$seed_origin <- "user-supplied recorded seed" }
    } else {
      if (is.null(seed) || is.na(seed) || length(seed) != 1 || seed != round(seed)) stop("TEST MODE needs a single explicit integer seed.")
      env$seed <- as.integer(seed); env$seed_origin <- "typed by the user (TEST MODE)"
    }
    saved <- get_global_seed()
    set.seed(env$seed, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
    env$state <- get_global_seed(); set_global_seed(saved)
    env$pid <- Sys.getpid()
    with_private <- function(f) {
      same_process_guard(env)
      saved <- get_global_seed(); set_global_seed(env$state)
      on.exit({ env$state <- get_global_seed(); set_global_seed(saved) })
      f()
    }
    env$int <- function(n) { n <- as.integer(n); if (n < 1) stop("max must be >= 1"); if (n == 1L) return(1L); with_private(function() sample.int(n, 1L)) }
    env$permutations <- function(lengths) lapply(as.integer(lengths), function(k) if (k > 1) with_private(function() sample.int(k)) else seq_len(k))
    env$describe <- function() c(algorithm = "Mersenne-Twister MT19937 (R default), Inversion, Rejection sampling",
                                 implementation = "base R set.seed()/sample.int() with a private generator state; R's global RNG state is preserved",
                                 seed_or_state = sprintf("seed %d (32-bit set.seed interface)", env$seed), seed_origin = env$seed_origin,
                                 reproducible = "yes", reproduction_statement = sprintf("regenerate with source '%s', seed = %d, R %s", source, env$seed, R.version.string),
                                 package_versions = if (source == "mt") sprintf("openssl %s (entropy only)", pkg_version("openssl")) else "", library_version = "")

  } else if (source == "pcg64") {
    if (is.null(seed)) { w <- u32_from_bytes(os_entropy_bytes(8L)); env$seed <- as.integer(w %% 2147483647); env$seed_origin <- "operating-system entropy (openssl), 2 x 31 bits" }
    else { if (length(seed) != 2) stop("PCG64 seed must be two integers."); env$seed <- as.integer(seed); env$seed_origin <- "user-supplied recorded seed" }
    saved <- dqrng::dqrng_get_state()
    dqrng::dqRNGkind("pcg64"); dqrng::dqset.seed(env$seed)
    env$state <- dqrng::dqrng_get_state(); dqrng::dqrng_set_state(saved)
    env$pid <- Sys.getpid()
    with_private <- function(f) {
      same_process_guard(env)
      saved <- dqrng::dqrng_get_state(); dqrng::dqrng_set_state(env$state)
      on.exit({ env$state <- dqrng::dqrng_get_state(); dqrng::dqrng_set_state(saved) })
      f()
    }
    env$int <- function(n) { n <- as.integer(n); if (n < 1) stop("max must be >= 1"); if (n == 1L) return(1L); with_private(function() dqrng::dqsample.int(n, 1L)) }
    env$permutations <- function(lengths) lapply(as.integer(lengths), function(k) if (k > 1) with_private(function() dqrng::dqsample.int(k)) else seq_len(k))
    env$describe <- function() c(algorithm = "PCG64 (O'Neill 2014)",
                                 implementation = "dqrng::dqset.seed()/dqsample.int() (unbiased sampling) with private state via dqrng_get_state()/dqrng_set_state()",
                                 seed_or_state = sprintf("seed c(%d, %d), stream default", env$seed[1], env$seed[2]), seed_origin = env$seed_origin,
                                 reproducible = "yes", reproduction_statement = sprintf("regenerate with source 'pcg64', seed = c(%d, %d), dqrng %s", env$seed[1], env$seed[2], pkg_version("dqrng")),
                                 package_versions = sprintf("dqrng %s; openssl %s (entropy only)", pkg_version("dqrng"), pkg_version("openssl")), library_version = "")

  } else if (source == "randomorg") {
    randomorg_backend(env, api_key = api_key, signed = signed, transport = transport, verify = verify %||% signed)
  }

  if (is.null(env$ints)) env$ints <- function(n, max) vapply(seq_len(n), function(i) env$int(max), integer(1))
  if (is.null(env$int))  env$int  <- function(n) env$ints(1L, n)[1]
  env$permute <- function(x) if (length(x) > 1) x[env$permutations(length(x))[[1]]] else x
  env$pick    <- function(x) x[env$int(length(x))]
  env$hex     <- function(n_chars) paste(sprintf("%X", env$ints(n_chars, 16L) - 1L), collapse = "")
  env$mode    <- if (source == "test") "TEST MODE - NOT FOR PRODUCTION" else "production"
  class(env) <- "rand_rng"
  env
}

print.rand_rng <- function(x, ...) { cat(sprintf("Randomness source: %s [%s]\n", x$label, x$mode)); d <- x$describe(); cat(sprintf("  %-22s %s\n", names(d), d), sep = ""); invisible(x) }

# Full audit description of a source (named character vector).
rng_audit <- function(rng) {
  c(source = rng$source, source_label = rng$label, source_class = rng$class, mode = rng$mode, rng$describe(),
    r_version = R.version.string, platform = R.version$platform, application_version = RANDOMIZER_APP_VERSION)
}

# Metadata identifier.  Local sources: a throw-away instance of the SAME
# source (fresh entropy) so the identifier never influences the list's draws.
# RANDOM.ORG: derived from the response serial number (no extra request).
new_sequence_id <- function(rng = NULL, prefix = "RAND") {
  hex <- if (is.null(rng)) {
    r <- if (csprng_available()) rng_new("openssl") else NULL
    if (!is.null(r)) r$hex(6) else { saved <- get_global_seed(); set.seed(NULL); h <- paste(sprintf("%X", sample.int(16, 6, TRUE) - 1L), collapse = ""); set_global_seed(saved); h }
  } else if (rng$source == "randomorg") {
    sprintf("RO%s", rng$last_serial())
  } else if (rng$source == "test") {
    if (csprng_available()) rng_new("openssl")$hex(6) else { saved <- get_global_seed(); set.seed(NULL); h <- paste(sprintf("%X", sample.int(16, 6, TRUE) - 1L), collapse = ""); set_global_seed(saved); h }
  } else rng_new(rng$source)$hex(6)
  sprintf("%s-%s-%s", prefix, format(Sys.time(), "%Y%m%d"), hex)
}

# -----------------------------------------------------------------------------
# 2. Allocation ratio arithmetic
# -----------------------------------------------------------------------------
gcd2 <- function(a, b) { while (b != 0) { t <- b; b <- a %% b; a <- t }; a }
gcd_vec <- function(x) Reduce(gcd2, x)
reduce_ratio <- function(ratio) { ratio <- as.integer(ratio); ratio %/% gcd_vec(ratio) }
ratio_unit   <- function(ratio) sum(reduce_ratio(ratio))
ratio_string <- function(ratio) paste(ratio, collapse = ":")
parse_ratio <- function(text) {
  parts <- trimws(strsplit(as.character(text), "[:,/ ]+")[[1]]); parts <- parts[nzchar(parts)]
  if (!length(parts) || !all(grepl("^[0-9]+$", parts))) stop("Allocation ratio must be positive integers such as 2:1:1.")
  r <- as.integer(parts); if (any(r < 1)) stop("Allocation ratio entries must be >= 1."); r
}
is_compatible_block_size <- function(size, ratio) { size <- as.integer(size); u <- ratio_unit(ratio); !is.na(size) & size >= u & size %% u == 0 }
compatible_block_sizes <- function(ratio, max_size = NULL, max_multiple = 6) { u <- ratio_unit(ratio); s <- u * seq_len(max_multiple); if (!is.null(max_size)) s <- s[s <= max_size]; s }
suggested_block_sizes <- function(ratio) ratio_unit(ratio) * c(2L, 3L, 4L)

# -----------------------------------------------------------------------------
# 3. Integer targets for a list of KNOWN length (largest remainder / Hamilton).
#    Never used to divide a sample among strata.
# -----------------------------------------------------------------------------
target_counts <- function(n, ratio, rng = NULL) {
  n <- as.integer(n); ratio <- as.integer(ratio)
  quota <- n * ratio / sum(ratio); base <- floor(quota); frac <- quota - base; left <- n - sum(base)
  tie_broken <- FALSE
  if (left > 0) {
    ord <- order(-frac, seq_along(frac)); cutoff <- frac[ord[left]]
    above <- which(frac > cutoff + 1e-9); tied <- which(abs(frac - cutoff) < 1e-9); need <- left - length(above)
    chosen <- above
    if (need > 0) {
      if (length(tied) > need) {
        tie_broken <- TRUE; pool <- tied; picked <- integer(0)
        for (i in seq_len(need)) { p <- if (is.null(rng)) pool[1] else pool[rng$int(length(pool))]; picked <- c(picked, p); pool <- setdiff(pool, p) }
        chosen <- c(chosen, picked)
      } else chosen <- c(chosen, tied[seq_len(need)])
    }
    base[chosen] <- base[chosen] + 1
  }
  structure(as.integer(base), quota = quota, tie_broken_randomly = tie_broken)
}

# -----------------------------------------------------------------------------
# 4. Design: planned N (optional), groups, codes, ratio, method, blocks, and
#    stratification VARIABLES with LEVELS.  Never stratum sizes.
# -----------------------------------------------------------------------------
rand_design <- function(name = "Untitled trial", n = NULL, groups = c("1", "2"), codes = NULL, ratio = NULL,
                        method = c("simple", "block"), block_type = c("fixed", "variable"), block_sizes = NULL,
                        final_block = c("truncate", "complete"), strata = NULL, description = "") {
  method <- match.arg(method); block_type <- match.arg(block_type); final_block <- match.arg(final_block)
  groups <- as.character(groups); k <- length(groups)
  codes  <- as.character(codes %||% seq_len(k)); ratio <- as.integer(ratio %||% rep(1L, k))
  if (method == "block" && is.null(block_sizes)) block_sizes <- if (block_type == "fixed") suggested_block_sizes(ratio)[1] else suggested_block_sizes(ratio)
  block_sizes <- if (is.null(block_sizes)) NULL else sort(unique(as.integer(block_sizes)))
  n_store <- if (is.null(n)) NULL else if (is.numeric(n) && length(n) == 1 && !is.na(n) && n == round(n)) as.integer(n) else n
  strata_table <- if (!is.null(strata) && length(strata)) build_strata(strata) else NULL
  structure(list(name = name, description = description, n = n_store, groups = groups, codes = codes, ratio = ratio,
                 method = method, block_type = block_type, block_sizes = block_sizes, final_block = final_block,
                 strata = strata, strata_table = strata_table), class = "rand_design")
}

design_summary_lines <- function(d) {
  c(sprintf("Trial:            %s", d$name),
    if (nzchar(d$description %||% "")) sprintf("Description:      %s", d$description),
    sprintf("Sample size N:    %s", if (is.null(d$n)) "not specified" else as.character(d$n)),
    sprintf("Groups:           %s", paste(sprintf("%s = %s", d$codes, d$groups), collapse = ", ")),
    sprintf("Allocation ratio: %s", ratio_string(d$ratio)),
    sprintf("Method:           %s", switch(d$method, simple = "simple randomization",
                                          block = sprintf("permuted blocks (%s sizes: %s; final block of a list: %s)", d$block_type, paste(d$block_sizes, collapse = ", "), d$final_block))),
    if (!is.null(d$strata_table))
      sprintf("Stratification:   %s -> %d strata, each with its own independent stream (stratum sizes are never fixed or inferred)",
              paste(sprintf("%s (%s)", names(d$strata), vapply(d$strata, paste, character(1), collapse = "/")), collapse = " x "), nrow(d$strata_table))
    else "Stratification:   none")
}
print.rand_design <- function(x, ...) { cat(design_summary_lines(x), sep = "\n"); invisible(x) }

# -----------------------------------------------------------------------------
# 5. Generate ONE complete list.  Unstratified: exactly N rows.  Stratified:
#    `stratum_length` rows in every stratum, each stratum an independent stream.
# -----------------------------------------------------------------------------
generate_sequence <- function(design, source = "openssl", seed = NULL, length = NULL, stratum_length = NULL,
                              api_key = NULL, signed = TRUE, transport = NULL, rng = NULL) {
  v <- validate_design(design)
  if (!v$ok) stop(paste(c("Design is invalid:", paste(" -", v$errors)), collapse = "\n"))
  rng <- rng %||% rng_new(source, seed = seed, api_key = api_key, signed = signed, transport = transport)
  started <- Sys.time()

  if (is.null(design$strata_table)) {
    n <- length %||% design$n
    if (is.null(n) || !is.numeric(n) || length(n) != 1 || is.na(n) || n != round(n) || n < 1)
      stop("Generating a list needs its length: enter the sample size N (or pass length = <rows>).")
    n <- as.integer(n)
    tab <- generate_stratum(design, n, rng)
    tab$stratum_id <- 1L; tab$stratum <- NA_character_; tab$stratum_position <- tab$position
    lengths <- data.frame(stratum = "(all)", stratum_id = 1L, length = n, stringsAsFactors = FALSE)
    targets <- data.frame(stratum = "(all)", length = n, t(as.integer(attr(tab, "target"))), tie_broken = attr(tab, "tie_broken"), stringsAsFactors = FALSE)
    final_blocks <- attr(tab, "final_block"); if (!is.null(final_blocks)) final_blocks <- cbind(stratum = "(all)", final_blocks)
  } else {
    st <- design$strata_table; L <- resolve_stratum_lengths(design, stratum_length)
    parts <- vector("list", nrow(st)); tg <- vector("list", nrow(st)); fb <- list()
    for (i in seq_len(nrow(st))) {
      p <- generate_stratum(design, L[i], rng)
      p$stratum_id <- st$stratum_id[i]; p$stratum <- st$stratum_label[i]; p$stratum_position <- p$position
      for (v_ in names(design$strata)) p[[v_]] <- st[[v_]][i]
      tg[[i]] <- data.frame(stratum = st$stratum_label[i], length = L[i], t(as.integer(attr(p, "target"))), tie_broken = attr(p, "tie_broken"), stringsAsFactors = FALSE)
      if (!is.null(attr(p, "final_block"))) fb[[length(fb) + 1]] <- cbind(stratum = st$stratum_label[i], attr(p, "final_block"))
      parts[[i]] <- p
    }
    tab <- do.call(rbind, parts); tab$position <- seq_len(nrow(tab))
    lengths <- data.frame(stratum = st$stratum_label, stratum_id = st$stratum_id, length = L, stringsAsFactors = FALSE)
    targets <- do.call(rbind, tg); final_blocks <- if (length(fb)) do.call(rbind, fb) else NULL
  }
  names(targets)[3:(2 + length(design$groups))] <- paste0("target_", design$codes)
  tab$group_label <- design$groups[match(tab$group_code, design$codes)]
  front <- c("position", "stratum_id", "stratum", "stratum_position", "block", "block_size", "position_in_block", "group_code", "group_label", "beyond_n", names(design$strata))
  tab <- tab[, intersect(front, names(tab))]; rownames(tab) <- NULL

  result <- structure(list(sequence_id = new_sequence_id(rng), generated_at = started, design = design,
                           source = rng$source, mode = rng$mode, rng_info = rng_audit(rng),
                           rng_responses = if (rng$source == "randomorg") rng$responses() else NULL,
                           table = tab, lengths = lengths, targets = targets, final_blocks = final_blocks,
                           app_version = RANDOMIZER_APP_VERSION, r_version = R.version.string), class = "rand_sequence")
  chk <- validate_sequence(result); result$validation <- chk
  if (!chk$ok) stop(paste(c("Generated list failed validation (generation aborted):", paste(" -", chk$errors)), collapse = "\n"))
  result
}

generate_stratum <- function(design, n, rng) {
  target <- target_counts(n, design$ratio, rng)
  tab <- switch(design$method,
                simple = simple_sequence(n, design$codes, design$ratio, rng),
                block  = block_sequence(n, design$codes, design$ratio, design$block_sizes, design$block_type, design$final_block, rng))
  attr(tab, "target") <- as.integer(target); attr(tab, "tie_broken") <- isTRUE(attr(target, "tie_broken_randomly"))
  tab
}

print.rand_sequence <- function(x, ...) {
  cat(sprintf("Randomization list %s  (generated %s)\n", x$sequence_id, format(x$generated_at, "%Y-%m-%d %H:%M:%S")))
  cat(sprintf("Randomness source: %s [%s]\n", x$rng_info[["source_label"]], x$mode))
  cat(design_summary_lines(x$design), sep = "\n")
  cat("Allocation counts:\n"); print(allocation_counts(x), row.names = FALSE)
  if (!is.null(x$final_blocks)) { cat("Final block(s):\n"); print(x$final_blocks, row.names = FALSE) }
  cat(sprintf("Validation: %s\n", if (x$validation$ok) "passed" else "FAILED")); invisible(x)
}

# Realised counts versus integer targets for the requested length.  For lists
# kept "complete", within_n = TRUE restricts to the first N rows of each stratum.
allocation_counts <- function(result, within_n = FALSE) {
  d <- result$design; t <- result$table
  if (within_n && "beyond_n" %in% names(t)) t <- t[!t$beyond_n, ]
  f <- factor(t$group_code, levels = d$codes)
  by_stratum <- if (is.null(d$strata_table)) list("(all)" = f) else split(f, factor(t$stratum, levels = d$strata_table$stratum_label))
  rows <- lapply(names(by_stratum), function(s) {
    cnt <- as.integer(table(by_stratum[[s]])); n <- sum(cnt)
    tg  <- result$targets[result$targets$stratum == s, paste0("target_", d$codes)]
    data.frame(stratum = s, n = n, setNames(as.list(cnt), paste0("n_", d$codes)),
               setNames(as.list(round(100 * cnt / max(n, 1), 1)), paste0("pct_", d$codes)),
               setNames(as.list(as.integer(cnt - unlist(tg))), paste0("dev_", d$codes)), stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  if (nrow(out) > 1) {
    tot <- as.integer(table(f)); tgt <- colSums(result$targets[, paste0("target_", d$codes), drop = FALSE])
    out <- rbind(out, data.frame(stratum = "TOTAL", n = sum(tot), setNames(as.list(tot), paste0("n_", d$codes)),
                                 setNames(as.list(round(100 * tot / max(1, sum(tot)), 1)), paste0("pct_", d$codes)),
                                 setNames(as.list(as.integer(tot - tgt)), paste0("dev_", d$codes)), stringsAsFactors = FALSE))
  }
  out
}
