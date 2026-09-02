# =============================================================================
#  simple_randomization.R
#  Simple (complete) randomization: every position is an independent uniform
#  draw in 1..sum(ratio) mapped to a group, so group i has probability
#  r_i / sum(r).  Counts are never forced or corrected.
# =============================================================================

codes_from_draws <- function(draws, codes, ratio) {
  idx <- findInterval(draws - 1L, c(0, cumsum(ratio)), rightmost.closed = TRUE)
  codes[idx]
}

simple_draw <- function(codes, ratio, rng) codes_from_draws(rng$ints(1L, sum(ratio)), codes, ratio)

simple_sequence <- function(n, codes, ratio, rng) {
  n <- as.integer(n)
  draws <- rng$ints(n, sum(ratio))
  data.frame(position = seq_len(n), block = NA_integer_, block_size = NA_integer_, position_in_block = NA_integer_,
             group_code = codes_from_draws(draws, codes, ratio), beyond_n = FALSE, stringsAsFactors = FALSE)
}
