# =============================================================================
#  diagnostics.R
#  DESCRIPTIVE statistics of a pre-generated list or of the allocations made
#  so far in a randomization session.  Computed after the fact, for
#  information only.  They never feed back into generation, are never used as
#  acceptance criteria and are never used to compare or choose sequences.
#  Long runs, alternation and imbalance occur in random sequences; they are
#  expected, not defects.
# =============================================================================

run_statistics <- function(codes) {
  if (!length(codes)) return(list(max_run = NA_integer_, n_runs = NA_integer_, max_run_group = NA_character_))
  r <- rle(codes); list(max_run = max(r$lengths), n_runs = length(r$lengths), max_run_group = r$values[which.max(r$lengths)])
}
transition_matrix <- function(codes, levels) {
  m <- matrix(0L, length(levels), length(levels), dimnames = list(from = levels, to = levels))
  if (length(codes) > 1) for (i in 2:length(codes)) m[codes[i - 1], codes[i]] <- m[codes[i - 1], codes[i]] + 1L
  m
}
switch_rate <- function(codes) if (length(codes) < 2) NA_real_ else mean(codes[-1] != codes[-length(codes)])
indicator_autocorrelation <- function(codes, levels, lags = 1:3) {
  out <- matrix(NA_real_, length(levels), length(lags), dimnames = list(group = levels, lag = paste0("lag", lags)))
  n <- length(codes)
  for (g in levels) {
    x <- as.numeric(codes == g); xc <- x - mean(x); den <- sum(xc^2)
    for (j in seq_along(lags)) { k <- lags[j]; if (n > k + 1 && den > 0) out[g, j] <- sum(xc[1:(n - k)] * xc[(k + 1):n]) / den }
  }
  round(out, 3)
}
# Shannon entropy (bits) of realised group proportions.  Maximum log2(k) for
# equal proportions; unequal ratios have a lower maximum by design.  It says
# nothing about the order of assignments.
proportion_entropy <- function(codes, levels) {
  p <- as.numeric(table(factor(codes, levels = levels))) / max(1, length(codes)); p <- p[p > 0]
  list(entropy_bits = round(-sum(p * log2(p)), 3), max_bits = round(log2(length(levels)), 3))
}

# Split either object into per-stratum code vectors (+ block sizes, targets).
diag_parts <- function(x) {
  if (inherits(x, "rand_sequence")) {
    d <- x$design; t <- x$table
    parts <- if (is.null(d$strata_table)) list("(all)" = t) else split(t, factor(t$stratum, levels = d$strata_table$stratum_label))
    list(id = x$sequence_id, kind = "pre-generated list", design = d, parts = parts, targets = x$targets)
  } else if (inherits(x, "rand_allocator")) {
    d <- x$design; lg <- x$log
    parts <- split(lg, factor(lg$stratum, levels = unique(lg$stratum)))
    list(id = x$id, kind = "allocations to date", design = d, parts = parts, targets = NULL)
  } else stop("sequence_diagnostics() expects a rand_sequence or a rand_allocator.")
}

sequence_diagnostics <- function(x, lags = 1:3) {
  inp <- diag_parts(x); d <- inp$design
  per <- lapply(names(inp$parts), function(s) {
    p <- inp$parts[[s]]; codes <- p$group_code
    cnt <- as.integer(table(factor(codes, levels = d$codes)))
    tg <- if (!is.null(inp$targets)) as.integer(unlist(inp$targets[inp$targets$stratum == s, paste0("target_", d$codes)])) else NULL
    list(stratum = s, n = length(codes),
         counts = setNames(cnt, d$codes), percent = setNames(round(100 * cnt / max(1, length(codes)), 1), d$codes),
         ideal = setNames(round(length(codes) * d$ratio / sum(d$ratio), 2), d$codes),
         target = if (is.null(tg)) NULL else setNames(tg, d$codes),
         deviation = if (is.null(tg)) NULL else setNames(as.integer(cnt - tg), d$codes),
         runs = run_statistics(codes), switch_rate = round(switch_rate(codes), 3),
         transitions = transition_matrix(codes, d$codes), autocorrelation = indicator_autocorrelation(codes, d$codes, lags),
         entropy = proportion_entropy(codes, d$codes),
         block_sizes = if (d$method == "block" && length(codes)) table(p$block_size[!duplicated(p$block)]) else NULL)
  })
  names(per) <- names(inp$parts)
  structure(list(id = inp$id, kind = inp$kind, per_stratum = per, descriptive_only = TRUE), class = "rand_diagnostics")
}

format_diagnostics <- function(dg) {
  out <- c(sprintf("Descriptive diagnostics for %s (%s)", dg$id, dg$kind),
           "These statistics describe what was generated. They are not acceptance criteria and",
           "were not used to select, modify or reject any allocation.", "")
  if (!length(dg$per_stratum)) return(c(out, "Nothing randomized yet."))
  for (p in dg$per_stratum) {
    out <- c(out, sprintf("== %s (n = %d) ==", p$stratum, p$n), "Allocation:")
    out <- c(out, if (is.null(p$target))
      sprintf("  %-14s count %4d  (%5.1f%%)  proportional ideal for this n: %7.2f", names(p$counts), p$counts, p$percent, p$ideal)
      else sprintf("  %-14s count %4d  (%5.1f%%)  target %4d  ideal %7.2f  deviation %+d", names(p$counts), p$counts, p$percent, p$target, p$ideal, p$deviation))
    out <- c(out, sprintf("Runs: maximum run length %s (group %s), number of runs %s, switch rate %s",
                          p$runs$max_run, p$runs$max_run_group, p$runs$n_runs, p$switch_rate),
             "Transition counts (row = from, column = to):", capture.output(print(p$transitions)),
             "Autocorrelation of group indicators:", capture.output(print(p$autocorrelation)),
             sprintf("Entropy of realised proportions: %.3f bits (maximum %.3f for equal proportions; unequal ratios have a lower maximum by design)",
                     p$entropy$entropy_bits, p$entropy$max_bits))
    if (!is.null(p$block_sizes)) out <- c(out, sprintf("Block sizes used in this stream: %s", paste(sprintf("%s x%d", names(p$block_sizes), as.integer(p$block_sizes)), collapse = ", ")))
    out <- c(out, "")
  }
  out
}
print.rand_diagnostics <- function(x, ...) { cat(format_diagnostics(x), sep = "\n"); invisible(x) }
