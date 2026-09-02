# =============================================================================
#  validation.R
#  (1) validate_design():      before any random draw; errors stop generation.
#  (2) validate_sequence():    structural checks of a pre-generated list.
#  (3) validate_allocations(): structural checks of a participant-level
#      randomization session (per-stratum positions, block composition,
#      unique participants).
#  These are correctness checks of the implementation, never judgements about
#  how random a sequence looks.
# =============================================================================

validate_design <- function(d) {
  errors <- character(0); warnings <- character(0)
  err  <- function(m) errors <<- c(errors, m)
  warn <- function(m) warnings <<- c(warnings, m)
  k <- length(d$groups)

  if (!nzchar(trimws(d$name %||% ""))) warn("Trial name is empty.")
  if (k < 2 || k > 4) err(sprintf("Number of groups must be 2-4 (got %d).", k))
  if (any(!nzchar(trimws(d$groups)))) err("Group labels must be non-empty.")
  if (anyDuplicated(trimws(d$groups))) err("Group labels must be unique.")
  if (length(d$codes) != k) err("Number of group codes must equal number of groups.")
  if (any(!nzchar(trimws(d$codes)))) err("Group codes must be non-empty.")
  if (anyDuplicated(trimws(d$codes))) err("Group codes must be unique.")
  if (length(d$ratio) != k) err(sprintf("Allocation ratio has %d entries but there are %d groups.", length(d$ratio), k))
  if (any(is.na(d$ratio)) || any(d$ratio != round(d$ratio)) || any(d$ratio < 1)) err("Allocation ratio entries must be positive integers.")

  if (!is.null(d$n)) {
    if (!is.numeric(d$n) || length(d$n) != 1 || is.na(d$n) || d$n != round(d$n) || d$n < 1) err("Planned sample size N must be a positive integer (or left empty).")
    else if (!length(errors) && d$n < sum(d$ratio)) warn(sprintf("Planned N (%d) is smaller than one balanced block (%d).", d$n, sum(d$ratio)))
  }

  if (d$method == "block" && !length(errors)) {
    bs <- d$block_sizes
    if (is.null(bs) || !length(bs)) err("Block randomization needs at least one block size.")
    else if (any(is.na(bs)) || any(bs != round(bs)) || any(bs < 1)) err("Block sizes must be positive integers.")
    else {
      bad <- bs[!is_compatible_block_size(bs, d$ratio)]
      if (length(bad)) err(sprintf("Block size(s) %s are not compatible with ratio %s (must be multiples of %d).",
                                   paste(bad, collapse = ", "), ratio_string(d$ratio), ratio_unit(d$ratio)))
      if (d$block_type == "fixed" && length(bs) != 1) err("Fixed block randomization needs exactly one block size.")
      if (d$block_type == "variable" && length(bs) < 2) err("Variable block randomization needs at least two block sizes.")
      if (any(bs == ratio_unit(d$ratio))) warn(sprintf("Block size %d equals the smallest balanced block; the last assignment of every such block is predictable.", ratio_unit(d$ratio)))
      if (!is.null(d$n) && is.numeric(d$n) && is.null(d$strata_table)) {
        if (d$block_type == "fixed" && d$n %% bs[1] != 0)
          warn(sprintf("N = %d is not a multiple of block size %d: a pre-generated list's final block will be %s.", d$n, bs[1],
                       if (d$final_block == "truncate") "incomplete (truncated at N)" else "kept complete (list longer than N)"))
        if (d$block_type == "variable") warn(sprintf("With variable blocks a pre-generated list's final block may be incomplete; handling = %s.", d$final_block))
      }
    }
  }

  if (!is.null(d$strata)) {
    s <- d$strata
    if (length(s) < 1 || length(s) > 5) err(sprintf("Stratification supports 1-5 variables (got %d).", length(s)))
    if (is.null(names(s)) || any(!nzchar(trimws(names(s))))) err("Every stratification variable needs a name.")
    if (anyDuplicated(trimws(names(s)))) err("Stratification variable names must be unique.")
    for (v in names(s)) {
      lv <- trimws(as.character(s[[v]]))
      if (length(lv) < 2) err(sprintf("Variable '%s' needs at least two levels.", v))
      if (any(!nzchar(lv))) err(sprintf("Variable '%s' has an empty level.", v))
      if (anyDuplicated(lv)) err(sprintf("Variable '%s' has duplicated levels.", v))
    }
    if (!length(errors) && !is.null(d$strata_table)) {
      if (anyDuplicated(d$strata_table$stratum_label)) err("Strata are ambiguous (duplicated stratum labels).")
      if (nrow(d$strata_table) > 20) warn(sprintf("%d strata: many strata with few participants each weaken the balance stratification is meant to provide.", nrow(d$strata_table)))
    }
  }
  list(ok = !length(errors), errors = errors, warnings = warnings)
}

# Shared block checks for one stream (a stratum): sizes permitted, positions
# sequential, every complete block follows the reduced ratio, only the last
# block may be incomplete.
check_stream_blocks <- function(g, d, err, where) {
  rr <- reduce_ratio(d$ratio); u <- sum(rr)
  for (b in split(g, g$block)) {
    size <- b$block_size[1]
    if (!size %in% d$block_sizes) err(sprintf("%s block %d uses size %d, not in the permitted set.", where, b$block[1], size))
    if (!identical(as.integer(b$position_in_block), seq_len(nrow(b)))) err(sprintf("%s block %d positions are not sequential.", where, b$block[1]))
    if (nrow(b) == size) {
      cnt <- as.integer(table(factor(b$group_code, levels = d$codes)))
      if (!all(cnt == rr * (size %/% u))) err(sprintf("%s block %d composition %s does not follow ratio %s.", where, b$block[1], paste(cnt, collapse = "/"), ratio_string(rr)))
    } else if (b$block[1] != max(g$block)) err(sprintf("%s block %d is incomplete but is not the last block.", where, b$block[1]))
  }
}

validate_sequence <- function(result) {
  d <- result$design; t <- result$table
  errors <- character(0); err <- function(m) errors <<- c(errors, m)
  n_expected <- if (d$method == "block" && d$final_block == "complete") NA else sum(result$lengths$length)
  if (!is.na(n_expected) && nrow(t) != n_expected) err(sprintf("List has %d rows, expected %d.", nrow(t), n_expected))
  if (!identical(t$position, seq_len(nrow(t)))) err("Positions are not 1..N in order.")
  if (!all(t$group_code %in% d$codes)) err("List contains unknown group codes.")
  if (!isTRUE(all(t$group_label == d$groups[match(t$group_code, d$codes)]))) err("Group labels do not match codes.")
  for (g in split(t, t$stratum_id)) {
    if (!identical(g$stratum_position, seq_len(nrow(g)))) err("Within-stratum positions are not sequential.")
    L <- result$lengths$length[result$lengths$stratum_id == g$stratum_id[1]]
    if (!is.null(d$strata_table)) {
      row <- d$strata_table[d$strata_table$stratum_id == g$stratum_id[1], ]
      if (!all(g$stratum == row$stratum_label)) err("Stratum label mismatch.")
      for (v in names(d$strata)) if (!all(g[[v]] == row[[v]])) err(sprintf("Stratum variable '%s' mismatch.", v))
    }
    if (d$final_block != "complete" && nrow(g) != L) err(sprintf("Stratum '%s' has %d rows, expected %d.", g$stratum[1], nrow(g), L))
    if (d$method == "block") check_stream_blocks(g, d, err, sprintf("Stratum '%s'", g$stratum[1] %||% "(all)"))
    else if (!all(is.na(g$block))) err("Simple randomization must not carry block numbers.")
  }
  list(ok = !length(errors), errors = errors,
       checks = c("length", "positions", "codes", "labels", "stratum assignment", if (d$method == "block") c("block sizes", "block composition", "block positions")))
}

validate_allocations <- function(allocator) {
  d <- allocator$design; lg <- allocator$log
  errors <- character(0); err <- function(m) errors <<- c(errors, m)
  if (!nrow(lg)) return(list(ok = TRUE, errors = errors, checks = "nothing randomized yet"))
  if (!identical(as.integer(lg$n), seq_len(nrow(lg)))) err("Allocation numbers are not sequential.")
  ids <- trimws(lg$participant[!is.na(lg$participant) & nzchar(trimws(lg$participant))])
  if (anyDuplicated(ids)) err(sprintf("Participant(s) randomized more than once: %s", paste(unique(ids[duplicated(ids)]), collapse = ", ")))
  if (!all(lg$group_code %in% d$codes)) err("Log contains unknown group codes.")
  if (!isTRUE(all(lg$group_label == d$groups[match(lg$group_code, d$codes)]))) err("Group labels do not match codes.")
  labels <- if (is.null(d$strata_table)) "(all)" else d$strata_table$stratum_label
  if (!all(lg$stratum %in% labels)) err("Log contains unknown strata.")
  for (g in split(lg, lg$stratum)) {
    if (!identical(as.integer(g$stratum_position), seq_len(nrow(g)))) err(sprintf("Stratum '%s': within-stratum positions are not sequential.", g$stratum[1]))
    if (d$method == "block") check_stream_blocks(g, d, err, sprintf("Stratum '%s'", g$stratum[1]))
    else if (!all(is.na(g$block))) err("Simple randomization must not carry block numbers.")
  }
  list(ok = !length(errors), errors = errors,
       checks = c("sequential numbering", "unique participants", "codes", "labels", "strata", if (d$method == "block") c("block sizes", "block composition per stratum", "block positions")))
}

format_validation <- function(v) {
  c(if (length(v$errors)) paste("ERROR:", v$errors),
    if (length(v$warnings)) paste("Warning:", v$warnings),
    if (!length(v$errors) && !length(v$warnings)) "No problems found.")
}
