# =============================================================================
#  stratified_randomization.R
#  Strata = cross-classification of the stratification variables the user
#  defines.  Each stratum owns an independent stream (its own draws, its own
#  blocks and block sizes).  The observed stratum of a participant decides
#  which stream is used; how many participants each stratum will contain is
#  never required, assumed or inferred.
#
#  Primary workflow: generate_sequence(design, stratum_length = rows)  -  one
#      independent list per stratum of the explicit length the user chooses.
#  Secondary workflow: rand_allocator() + allocate_next()  -  allocate
#      participants one at a time; per-stratum state kept for the session.
# =============================================================================

build_strata <- function(strata) {
  if (!is.list(strata) || is.null(names(strata)) || any(!nzchar(names(strata))))
    stop("strata must be a named list, e.g. list(Sex = c('Male','Female'), Site = c('Site 1','Site 2')).")
  if (length(strata) > 5) stop(sprintf("Stratification supports at most 5 variables (got %d).", length(strata)))
  levels <- lapply(strata, function(l) as.character(l))
  grid <- expand.grid(rev(levels), stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
  grid <- grid[, rev(seq_along(levels)), drop = FALSE]; names(grid) <- names(strata)
  label <- apply(grid, 1, function(r) paste(sprintf("%s=%s", names(strata), r), collapse = "; "))
  out <- cbind(data.frame(stratum_id = seq_len(nrow(grid)), stratum_label = unname(label), stringsAsFactors = FALSE), grid)
  rownames(out) <- NULL; out
}

# Rows per stratum for a pre-generated list: one number for every stratum, a
# vector in strata_table order, or a vector named by stratum label.
resolve_stratum_lengths <- function(design, stratum_length) {
  st <- design$strata_table
  if (is.null(stratum_length))
    stop("Stratified designs have no fixed stratum sizes and the application never infers them. ",
         "Enter 'Rows to generate per stratum' (stratum_length = <rows>) to generate an independent list for every stratum.")
  sl <- stratum_length
  if (!is.numeric(sl) || any(is.na(sl)) || any(sl != round(sl)) || any(sl < 1)) stop("Rows per stratum must be positive integers.")
  if (length(sl) == 1 && is.null(names(sl))) return(rep(as.integer(sl), nrow(st)))
  if (!is.null(names(sl))) { miss <- setdiff(st$stratum_label, names(sl)); if (length(miss)) stop("stratum_length has no entry for: ", paste(miss, collapse = "; ")); return(as.integer(sl[st$stratum_label])) }
  if (length(sl) != nrow(st)) stop(sprintf("stratum_length has %d entries but there are %d strata.", length(sl), nrow(st)))
  as.integer(sl)
}

# -----------------------------------------------------------------------------
# Secondary workflow: sequential participant-level allocation
# -----------------------------------------------------------------------------
rand_allocator <- function(design, source = "openssl", seed = NULL, api_key = NULL, signed = TRUE, transport = NULL, rng = NULL) {
  v <- validate_design(design)
  if (!v$ok) stop(paste(c("Design is invalid:", paste(" -", v$errors)), collapse = "\n"))
  a <- new.env(parent = emptyenv())
  a$design <- design; a$rng <- rng %||% rng_new(source, seed = seed, api_key = api_key, signed = signed, transport = transport)
  a$source <- a$rng$source; a$mode <- a$rng$mode
  a$id <- if (a$source == "randomorg") sprintf("ALLOC-%s-RO", format(Sys.time(), "%Y%m%d")) else new_sequence_id(a$rng, "ALLOC")
  a$created <- Sys.time(); a$state <- list(); a$log <- empty_allocation_log(design)
  class(a) <- "rand_allocator"; a
}

empty_allocation_log <- function(design) {
  base <- data.frame(n = integer(0), participant = character(0), stratum = character(0), stringsAsFactors = FALSE)
  for (v in names(design$strata)) base[[v]] <- character(0)
  cbind(base, data.frame(stratum_position = integer(0), block = integer(0), block_size = integer(0), position_in_block = integer(0),
                         group_code = character(0), group_label = character(0), allocated_at = character(0), stringsAsFactors = FALSE))
}

stratum_label_for <- function(design, levels) {
  if (is.null(design$strata_table)) return("(all)")
  if (is.null(levels)) stop("This design is stratified: supply the participant's levels, e.g. list(Sex = 'Male').")
  st <- design$strata_table; hit <- rep(TRUE, nrow(st))
  for (v in names(design$strata)) {
    val <- levels[[v]]
    if (is.null(val) || is.na(val) || !nzchar(trimws(val))) stop(sprintf("Missing stratification variable '%s'.", v))
    val <- trimws(as.character(val))
    if (!val %in% design$strata[[v]]) stop(sprintf("'%s' is not a level of '%s' (allowed: %s).", val, v, paste(design$strata[[v]], collapse = ", ")))
    hit <- hit & st[[v]] == val
  }
  st$stratum_label[hit]
}

allocate_next <- function(allocator, levels = NULL, participant = NA_character_) {
  stopifnot(inherits(allocator, "rand_allocator"))
  d <- allocator$design; rng <- allocator$rng
  participant <- as.character(participant)
  if (!is.na(participant) && nzchar(trimws(participant)) && trimws(participant) %in% trimws(allocator$log$participant)) {
    prev <- allocator$log[trimws(allocator$log$participant) == trimws(participant), ][1, ]
    stop(sprintf("Participant '%s' has already been randomized (%s = %s, at %s). Each participant is randomized once.", participant, prev$group_code, prev$group_label, prev$allocated_at))
  }
  lab <- stratum_label_for(d, levels)
  s <- allocator$state[[lab]] %||% list(remaining = character(0), block = 0L, block_size = NA_integer_, pos = 0L, count = 0L)
  if (d$method == "simple") {
    code <- simple_draw(d$codes, d$ratio, rng); blk <- NA_integer_; bsz <- NA_integer_; pos <- NA_integer_
  } else {
    if (!length(s$remaining)) {                                    # this stream's block is exhausted: start a new one
      size <- if (d$block_type == "fixed") d$block_sizes[1] else d$block_sizes[rng$ints(1L, length(d$block_sizes))]
      s$remaining <- make_block(size, d$codes, d$ratio, rng)
      s$block <- s$block + 1L; s$block_size <- as.integer(size); s$pos <- 0L
    }
    code <- s$remaining[1]; s$remaining <- s$remaining[-1]; s$pos <- s$pos + 1L
    blk <- s$block; bsz <- s$block_size; pos <- s$pos
  }
  s$count <- s$count + 1L; allocator$state[[lab]] <- s
  row <- data.frame(n = nrow(allocator$log) + 1L, participant = participant, stratum = lab, stringsAsFactors = FALSE)
  for (v in names(d$strata)) row[[v]] <- trimws(as.character(levels[[v]]))
  row <- cbind(row, data.frame(stratum_position = s$count, block = blk, block_size = bsz, position_in_block = pos, group_code = code,
                               group_label = d$groups[match(code, d$codes)], allocated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), stringsAsFactors = FALSE))
  allocator$log <- rbind(allocator$log, row)
  attr(row, "note") <- if (!is.null(d$n) && nrow(allocator$log) > d$n) sprintf("Note: %d participants randomized, planned N was %d.", nrow(allocator$log), d$n) else NULL
  row
}

allocator_log <- function(allocator) allocator$log

allocator_counts <- function(allocator) {
  d <- allocator$design; lg <- allocator$log
  labels <- if (is.null(d$strata_table)) "(all)" else d$strata_table$stratum_label
  rows <- lapply(labels, function(s) {
    codes <- lg$group_code[lg$stratum == s]; cnt <- as.integer(table(factor(codes, levels = d$codes))); n <- length(codes)
    data.frame(stratum = s, n_randomized = n, setNames(as.list(cnt), paste0("n_", d$codes)),
               setNames(as.list(if (n) round(100 * cnt / n, 1) else rep(NA_real_, length(cnt))), paste0("pct_", d$codes)), stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  if (nrow(out) > 1) {
    tot <- as.integer(table(factor(lg$group_code, levels = d$codes))); n <- nrow(lg)
    out <- rbind(out, data.frame(stratum = "TOTAL", n_randomized = n, setNames(as.list(tot), paste0("n_", d$codes)),
                                 setNames(as.list(if (n) round(100 * tot / n, 1) else rep(NA_real_, length(tot))), paste0("pct_", d$codes)), stringsAsFactors = FALSE))
  }
  out
}

allocator_summary_lines <- function(a) {
  c(sprintf("Sequential allocation session %s  (design saved %s)  [secondary workflow]", a$id, format(a$created, "%Y-%m-%d %H:%M:%S")),
    sprintf("Randomness source: %s [%s]", a$rng$label, a$mode),
    design_summary_lines(a$design),
    sprintf("Participants randomized so far: %d", nrow(a$log)),
    "Realised allocation to date (the objective is the allocation ratio within each stratum):",
    capture.output(print(allocator_counts(a), row.names = FALSE)))
}
print.rand_allocator <- function(x, ...) { cat(allocator_summary_lines(x), sep = "\n"); cat("Unused assignments of each stream's current block are held in memory and not printed.\n"); invisible(x) }
