# =============================================================================
#  randomorg.R  -  RANDOM.ORG JSON-RPC API v4 as a randomness source.
#  Integers come from generateIntegers / generateSignedIntegers; permutations
#  come directly from generateIntegerSequences / generateSignedIntegerSequences
#  with replacement = false (a uniformly random permutation, no local
#  Fisher-Yates needed).  Signed responses are verified with verifySignature
#  using the random object exactly as received.  Every failure stops the
#  generation; nothing is ever substituted for the service.
# =============================================================================

RANDOMORG_ENDPOINT <- "https://api.random.org/json-rpc/4/invoke"
RANDOMORG_LICENCE_NOTE <- paste("RANDOM.ORG API tiers: the free Developer tier is licensed 'strictly for development and testing only';",
                                "the Commercial (Non-Gambling) tier does not include the Signed API. Licensing is the user's responsibility.")

randomorg_default_transport <- function(url, body, timeout = 30) {
  h <- curl::new_handle()
  curl::handle_setopt(h, customrequest = "POST", postfields = body, timeout = as.integer(timeout))
  curl::handle_setheaders(h, "Content-Type" = "application/json", "Accept" = "application/json", "User-Agent" = sprintf("randomization_app/%s (R)", RANDOMIZER_APP_VERSION))
  r <- curl::curl_fetch_memory(url, handle = h)
  list(status = r$status_code, text = rawToChar(r$content))
}

# Extract the raw JSON text of the "random" object from a response, verbatim.
randomorg_raw_random <- function(text) {
  start <- regexpr('"random"\\s*:\\s*\\{', text)
  if (start < 0) return(NULL)
  i <- start + attr(start, "match.length") - 1L; depth <- 0L; in_str <- FALSE; esc <- FALSE
  chars <- strsplit(substr(text, i, nchar(text)), "")[[1]]
  for (k in seq_along(chars)) {
    ch <- chars[k]
    if (in_str) { if (esc) esc <- FALSE else if (ch == "\\") esc <- TRUE else if (ch == '"') in_str <- FALSE; next }
    if (ch == '"') in_str <- TRUE else if (ch == "{") depth <- depth + 1L else if (ch == "}") { depth <- depth - 1L; if (depth == 0L) return(paste(chars[1:k], collapse = "")) }
  }
  NULL
}

randomorg_backend <- function(env, api_key, signed = TRUE, transport = NULL, verify = signed, endpoint = RANDOMORG_ENDPOINT) {
  if (is.null(api_key) || !nzchar(trimws(api_key))) stop("RANDOM.ORG needs an API key.")
  env$api_key <- trimws(api_key); env$signed <- isTRUE(signed); env$verify <- isTRUE(verify) && isTRUE(signed)
  env$transport <- transport %||% randomorg_default_transport; env$endpoint <- endpoint
  env$log <- list(); env$req_id <- 0L; env$not_before <- Sys.time(); env$bits_used <- 0
  fail <- function(...) stop(paste0("RANDOM.ORG: ", sprintf(...), " Generation stopped; no other randomness source was used."), call. = FALSE)

  call <- function(method, params, body_text = NULL) {
    wait <- as.numeric(difftime(env$not_before, Sys.time(), units = "secs"))
    if (wait > 0) Sys.sleep(min(wait, 30))
    env$req_id <- env$req_id + 1L; id <- env$req_id
    body <- body_text %||% as.character(jsonlite::toJSON(list(jsonrpc = "2.0", method = method, params = params, id = id), auto_unbox = TRUE, digits = NA))
    resp <- tryCatch(env$transport(env$endpoint, body), error = function(e) fail("request failed (network/transport): %s.", conditionMessage(e)))
    if (!is.list(resp) || is.null(resp$status) || is.null(resp$text)) fail("transport returned an unusable response.")
    if (!identical(as.integer(resp$status), 200L)) fail("HTTP status %s.", resp$status)
    parsed <- tryCatch(jsonlite::fromJSON(resp$text, simplifyVector = FALSE), error = function(e) fail("response is not valid JSON (%s).", conditionMessage(e)))
    if (!is.null(parsed$error)) fail("error %s: %s.", parsed$error$code %||% "?", parsed$error$message %||% "(no message)")
    if (is.null(parsed$result)) fail("response contains no result.")
    if (!identical(as.integer(parsed$id %||% -1L), id)) fail("response id does not match the request.")
    ad <- parsed$result$advisoryDelay
    if (!is.null(ad) && is.numeric(ad) && ad > 0) env$not_before <- Sys.time() + ad / 1000
    list(parsed = parsed, raw = resp$text, method = method)
  }

  verify_signature <- function(r) {
    raw_random <- randomorg_raw_random(r$raw); sig <- r$parsed$result$signature
    if (is.null(raw_random) || is.null(sig) || !nzchar(sig)) fail("signed response lacks a random object or signature.")
    env$req_id <- env$req_id + 1L; id <- env$req_id
    body <- sprintf('{"jsonrpc":"2.0","method":"verifySignature","params":{"random":%s,"signature":"%s"},"id":%d}', raw_random, sig, id)
    env$req_id <- env$req_id - 1L
    v <- call("verifySignature", NULL, body_text = body)
    if (!isTRUE(v$parsed$result$authenticity)) fail("signature verification FAILED for serial number %s.", r$parsed$result$random$serialNumber %||% "?")
    TRUE
  }

  record <- function(r, kind) {
    res <- r$parsed$result; rnd <- res$random
    verified <- if (env$signed && env$verify) verify_signature(r) else NA
    env$bits_used <- env$bits_used + (res$bitsUsed %||% 0)
    env$log[[length(env$log) + 1]] <- list(kind = kind, method = r$method, signed = env$signed, verified = verified,
                                           serialNumber = rnd$serialNumber %||% NA, completionTime = rnd$completionTime %||% NA,
                                           hashedApiKey = rnd$hashedApiKey %||% NA, bitsUsed = res$bitsUsed %||% NA, bitsLeft = res$bitsLeft %||% NA,
                                           requestsLeft = res$requestsLeft %||% NA, signature = res$signature %||% NA,
                                           raw = if (env$signed) r$raw else NA_character_)
    rnd
  }

  env$ints <- function(n, max) {
    n <- as.integer(n); max <- as.integer(max)
    if (n < 1) return(integer(0)); if (max < 1) stop("max must be >= 1")
    if (max == 1L) return(rep(1L, n))
    out <- integer(0); remaining <- n
    while (remaining > 0) {
      take <- min(remaining, 10000L)
      r <- call(if (env$signed) "generateSignedIntegers" else "generateIntegers",
                list(apiKey = env$api_key, n = take, min = 1L, max = max, replacement = TRUE, base = 10L))
      rnd <- record(r, "integers")
      data <- rnd$data
      if (is.null(data) || length(data) != take) fail("expected %d integers, received %d.", take, length(data %||% list()))
      vals <- suppressWarnings(as.integer(unlist(data)))
      if (length(vals) != take || any(is.na(vals)) || any(vals < 1L | vals > max)) fail("integers outside the requested range 1..%d.", max)
      out <- c(out, vals); remaining <- remaining - take
    }
    out
  }
  env$permutations <- function(lengths) {
    lengths <- as.integer(lengths); if (!length(lengths)) return(list())
    out <- vector("list", length(lengths)); idx <- seq_along(lengths)
    single <- lengths == 1L; for (i in which(single)) out[[i]] <- 1L
    todo <- idx[!single]
    while (length(todo)) {
      batch <- todo[seq_len(min(length(todo), 1000L))]; L <- lengths[batch]
      r <- call(if (env$signed) "generateSignedIntegerSequences" else "generateIntegerSequences",
                list(apiKey = env$api_key, n = length(batch), length = if (length(L) == 1) L else I(L), min = 1L,
                     max = if (length(L) == 1) L else I(L), replacement = FALSE, base = 10L))
      rnd <- record(r, "sequences")
      data <- rnd$data
      if (is.null(data) || length(data) != length(batch)) fail("expected %d sequences, received %d.", length(batch), length(data %||% list()))
      for (j in seq_along(batch)) {
        p <- suppressWarnings(as.integer(unlist(data[[j]])))
        if (length(p) != L[j] || any(is.na(p)) || !identical(sort(p), seq_len(L[j]))) fail("sequence %d is not a permutation of 1..%d.", j, L[j])
        out[[batch[j]]] <- p
      }
      todo <- todo[-seq_along(batch)]
    }
    out
  }
  env$int <- function(n) env$ints(1L, n)[1]
  env$responses <- function() env$log
  env$last_serial <- function() { s <- vapply(env$log, function(l) as.character(l$serialNumber %||% "NA"), character(1)); if (length(s)) tail(s, 1) else "NONE" }
  env$describe <- function() {
    ser <- vapply(env$log, function(l) as.character(l$serialNumber %||% "NA"), character(1))
    ver <- vapply(env$log, function(l) as.character(l$verified), character(1))
    c(algorithm = "physical true random number generator (atmospheric noise), RANDOM.ORG",
      implementation = sprintf("JSON-RPC API v4 via curl/jsonlite; %s methods; integers via generateIntegers, permutations via generateIntegerSequences (replacement=false)", if (env$signed) "signed" else "basic (unsigned)"),
      seed_or_state = "none (external physical source)", seed_origin = "RANDOM.ORG",
      reproducible = "no (only through RANDOM.ORG pregeneratedRandomization/getResult features, not used here)",
      reproduction_statement = "cannot be regenerated locally: archive the exported list with this audit record and the signed responses",
      package_versions = sprintf("curl %s; jsonlite %s", pkg_version("curl"), pkg_version("jsonlite")), library_version = "",
      randomorg_endpoint = env$endpoint, randomorg_signed = as.character(env$signed), randomorg_signature_verified = paste(ver, collapse = ","),
      randomorg_serial_numbers = paste(ser, collapse = ","), randomorg_requests = as.character(length(env$log)),
      randomorg_bits_used = as.character(env$bits_used),
      randomorg_hashed_api_key = if (length(env$log)) as.character(env$log[[1]]$hashedApiKey) else "",
      randomorg_licence_note = RANDOMORG_LICENCE_NOTE)
  }
  invisible(env)
}

# -----------------------------------------------------------------------------
# TEST TRANSPORT (used by the test suite and the GUI self-test).  Produces
# responses with the exact shape of the service, with data drawn from the
# local OpenSSL source, and can simulate every failure mode.  It is never
# used unless explicitly passed as `transport`.
# -----------------------------------------------------------------------------
randomorg_mock_transport <- function(fail = NULL, delay_ms = 0) {
  local_rng <- rng_new("openssl"); serial <- 0L; calls <- 0L
  function(url, body, timeout = 30) {
    calls <<- calls + 1L
    if (identical(fail, "network")) stop("Could not resolve host: api.random.org")
    if (identical(fail, "http500")) return(list(status = 500L, text = "Internal Server Error"))
    if (identical(fail, "malformed")) return(list(status = 200L, text = '{"jsonrpc":"2.0","result":{"random":{"data":[1,2'))
    req <- jsonlite::fromJSON(body, simplifyVector = FALSE); m <- req$method; p <- req$params
    err <- function(code, msg) list(status = 200L, text = as.character(jsonlite::toJSON(list(jsonrpc = "2.0", error = list(code = code, message = msg, data = NULL), id = req$id), auto_unbox = TRUE, null = "null")))
    if (identical(fail, "auth")) return(err(400L, "Parameter 'apiKey' is invalid"))
    if (identical(fail, "quota")) return(err(402L, "The API key has exhausted its daily request allowance"))
    if (m == "verifySignature") return(list(status = 200L, text = as.character(jsonlite::toJSON(list(jsonrpc = "2.0", result = list(authenticity = !identical(fail, "badsig")), id = req$id), auto_unbox = TRUE))))
    signed <- grepl("^generateSigned", m)
    if (m %in% c("generateIntegers", "generateSignedIntegers")) {
      data <- local_rng$ints(p$n, p$max) + (p$min - 1L)
      if (identical(fail, "wrongn")) data <- data[-1]
      if (identical(fail, "range")) data[1] <- p$max + 1L
      data <- as.list(data)
    } else if (m %in% c("generateIntegerSequences", "generateSignedIntegerSequences")) {
      L <- rep_len(as.integer(unlist(p$length)), p$n)
      data <- local_rng$permutations(L)
      if (identical(fail, "dup")) data[[1]][2] <- data[[1]][1]
      if (identical(fail, "wrongn")) data <- data[-1]
      data <- lapply(data, as.list)
    } else return(err(-32601L, "Method not found"))
    serial <<- serial + 1L
    random <- list(method = m, hashedApiKey = "MOCKHASHEDKEY=", n = p$n, data = data, license = list(type = "developer"),
                   completionTime = format(Sys.time(), "%Y-%m-%d %H:%M:%SZ"), serialNumber = serial)
    result <- list(random = random, bitsUsed = 64L, bitsLeft = 249000L, requestsLeft = 990L, advisoryDelay = as.integer(delay_ms))
    if (signed) result$signature <- "MOCKSIGNATURE=="
    text <- as.character(jsonlite::toJSON(list(jsonrpc = "2.0", result = result, id = req$id), auto_unbox = TRUE))
    if (identical(fail, "wrongid")) text <- sub(sprintf('"id":%d', req$id), '"id":999', text, fixed = TRUE)
    list(status = 200L, text = text)
  }
}
