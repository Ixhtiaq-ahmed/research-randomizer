# =============================================================================
#  block_randomization.R
#  Permuted-block randomization, fixed or randomly varying block sizes.
#  Block sizes: fixed, or each drawn uniformly from the permitted set,
#  independently of everything else.  Each block holds (size/unit) * r_i
#  assignments of group i and is permuted uniformly at random.
#  Final block of a list: "truncate" cuts at the requested length (the last
#  block is incomplete; its full composition is reported), "complete" keeps
#  the whole block and flags rows beyond the length.
#  No block size or permutation is ever chosen because of how it looks.
# =============================================================================

block_multiset <- function(size, codes, ratio_r) {
  u <- sum(ratio_r)
  if (size %% u != 0) stop(sprintf("Block size %d is not a multiple of the ratio total %d.", size, u))
  rep(codes, times = ratio_r * (size %/% u))
}

# One block: multiset permuted with one uniform random permutation.
make_block <- function(size, codes, ratio, rng) {
  m <- block_multiset(size, codes, reduce_ratio(ratio))
  m[rng$permutations(size)[[1]]]
}

block_sequence <- function(n, codes, ratio, block_sizes, block_type = c("fixed", "variable"), final_block = c("truncate", "complete"), rng) {
  block_type <- match.arg(block_type); final_block <- match.arg(final_block)
  n <- as.integer(n); ratio_r <- reduce_ratio(ratio)
  bad <- block_sizes[!is_compatible_block_size(block_sizes, ratio)]
  if (length(bad)) stop("Incompatible block size(s): ", paste(bad, collapse = ", "))
  if (block_type == "fixed" && length(block_sizes) != 1) stop("Fixed blocks need exactly one block size.")

  # block sizes: enough independent draws to reach n, then truncated to the blocks actually needed
  max_blocks <- as.integer(ceiling(n / min(block_sizes)))
  idx <- if (block_type == "fixed") rep(1L, max_blocks) else rng$ints(max_blocks, length(block_sizes))
  sizes_all <- block_sizes[idx]
  k <- which(cumsum(sizes_all) >= n)[1]
  sizes <- sizes_all[seq_len(k)]
  perms <- rng$permutations(sizes)                       # one uniform permutation per block

  rows <- lapply(seq_along(sizes), function(b) {
    m <- block_multiset(sizes[b], codes, ratio_r)
    data.frame(block = b, block_size = as.integer(sizes[b]), position_in_block = seq_len(sizes[b]),
               group_code = m[perms[[b]]], stringsAsFactors = FALSE)
  })
  tab <- do.call(rbind, rows)
  tab$position <- seq_len(nrow(tab)); tab$beyond_n <- tab$position > n
  final_info <- NULL
  if (nrow(tab) > n) {
    last <- tab[tab$block == k, ]
    used <- last[!last$beyond_n, ]
    full_cnt <- as.integer(table(factor(last$group_code, levels = codes)))
    used_cnt <- as.integer(table(factor(used$group_code, levels = codes)))
    final_info <- data.frame(block = k, block_size = sizes[k], rows_within_n = nrow(used), rows_beyond_n = nrow(last) - nrow(used),
                             full_block_counts = paste(full_cnt, collapse = "/"), counts_within_n = paste(used_cnt, collapse = "/"),
                             handling = final_block, stringsAsFactors = FALSE)
    if (final_block == "truncate") tab <- tab[seq_len(n), ]
  }
  tab <- tab[, c("position", "block", "block_size", "position_in_block", "group_code", "beyond_n")]
  rownames(tab) <- NULL
  attr(tab, "final_block") <- final_info
  tab
}
