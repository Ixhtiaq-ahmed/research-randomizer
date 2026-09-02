# =============================================================================
#  simulation.R  -  planning / methodological testing only.
#  Many independent streams of a stated length are generated with the chosen
#  LOCAL source and summarised.  RANDOM.ORG is refused here (it would consume
#  the account's quota for a planning exercise); nothing is substituted.
#  Only summaries are returned, never sequences: the trial list is always a
#  separate draw from generate_sequence().
# =============================================================================

simulate_design <- function(design, n_sim = 1000, length = NULL, stratum_length = NULL, source = "openssl", seed = NULL,
                            positions = 10, progress = NULL, rng = NULL) {
  v <- validate_design(design)
  if (!v$ok) stop(paste(c("Design is invalid:", paste(" -", v$errors)), collapse = "\n"))
  if (source == "randomorg") stop("Simulation uses local randomness sources only; RANDOM.ORG is not used for planning simulations (no other source was substituted).")
  stratified <- !is.null(design$strata_table)
  sizes <- if (stratified) resolve_stratum_lengths(design, stratum_length %||% length) else {
    n <- length %||% design$n
    if (is.null(n) || !is.numeric(n) || n < 1) stop("Simulation needs a stream length: pass length = <rows> or set N.")
    as.integer(n)
  }
  rng <- rng %||% rng_new(source, seed = seed)
  k <- base::length(design$codes); n_pos <- min(positions, min(sizes))
  counts <- matrix(0L, n_sim, k, dimnames = list(NULL, design$codes))
  max_run <- integer(n_sim); max_dev <- numeric(n_sim)
  pos_hits <- matrix(0, n_pos, k, dimnames = list(paste0("pos", seq_len(n_pos)), design$codes))
  block_tab <- integer(0)
  for (i in seq_len(n_sim)) {
    codes_all <- character(0); dev_i <- 0
    for (s in seq_along(sizes)) {
      tab <- generate_stratum(design, sizes[s], rng)
      if (design$method == "block" && design$final_block == "complete") tab <- tab[!tab$beyond_n, ]
      cnt <- as.integer(table(factor(tab$group_code, levels = design$codes)))
      dev_i <- max(dev_i, max(abs(cnt - attr(tab, "target"))))
      codes_all <- c(codes_all, tab$group_code)
      if (s == 1) for (p in seq_len(n_pos)) pos_hits[p, tab$group_code[p]] <- pos_hits[p, tab$group_code[p]] + 1
      if (design$method == "block") block_tab <- c(block_tab, tab$block_size[!duplicated(tab$block)])
    }
    counts[i, ] <- as.integer(table(factor(codes_all, levels = design$codes)))
    max_run[i] <- max(rle(codes_all)$lengths); max_dev[i] <- dev_i
    if (!is.null(progress) && i %% 100 == 0) progress(i, n_sim)
  }
  ideal <- sum(sizes) * design$ratio / sum(design$ratio)
  structure(list(design = design, n_sim = n_sim, source = rng$source, source_label = rng$label, mode = rng$mode, sizes = sizes,
    count_summary = data.frame(group = design$codes, label = design$groups, ideal = round(ideal, 2), mean = round(colMeans(counts), 2),
                               sd = round(apply(counts, 2, sd), 2), min = apply(counts, 2, min), max = apply(counts, 2, max),
                               empirical_prob = round(colMeans(counts) / sum(sizes), 4), target_prob = round(design$ratio / sum(design$ratio), 4), stringsAsFactors = FALSE),
    max_deviation = table(factor(max_dev, levels = sort(unique(max_dev)))), max_run = summary(max_run),
    position_probability = round(pos_hits / n_sim, 3),
    block_size_frequency = if (base::length(block_tab)) round(prop.table(table(block_tab)), 3) else NULL,
    note = "Summaries of independent simulated streams for planning only; trial allocations are never chosen from these."), class = "rand_simulation")
}

format_simulation <- function(s) {
  out <- c(sprintf("Simulation of %d independent runs (%s), stream length%s %s", s$n_sim, s$source_label, if (length(s$sizes) > 1) "s per stratum" else "", paste(s$sizes, collapse = "/")),
           s$note, "", "Group counts across runs:", capture.output(print(s$count_summary, row.names = FALSE)), "",
           "Largest absolute deviation from the integer target within a stream (frequency):", capture.output(print(s$max_deviation)), "",
           "Maximum run length:", capture.output(print(s$max_run)), "",
           sprintf("Empirical probability of each group at the first %d positions (first stream):", nrow(s$position_probability)), capture.output(print(s$position_probability)))
  if (!is.null(s$block_size_frequency)) out <- c(out, "", "Relative frequency of block sizes:", capture.output(print(s$block_size_frequency)))
  out
}
print.rand_simulation <- function(x, ...) { cat(format_simulation(x), sep = "\n"); invisible(x) }
