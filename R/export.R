# =============================================================================
#  export.R
#  Works for a complete list (rand_sequence) and for a sequential-allocation
#  session (rand_allocator).  CSV / XLSX, configurable REDCap CSV,
#  audit/metadata, session history.  The allocations and the audit record are
#  separate files; seeds, states and signed responses appear only in the
#  audit record, never in the list or REDCap files.
# =============================================================================

is_session <- function(x) inherits(x, "rand_allocator")
object_id  <- function(x) if (is_session(x)) x$id else x$sequence_id

sequence_table <- function(x) {
  d <- x$design
  if (is_session(x)) {
    lg <- x$log
    out <- data.frame(session_id = rep(x$id, nrow(lg)), n = lg$n, participant = lg$participant, stringsAsFactors = FALSE)
    if (!is.null(d$strata_table)) { out$stratum <- lg$stratum; for (v in names(d$strata)) out[[v]] <- lg[[v]]; out$stratum_position <- lg$stratum_position }
    if (d$method == "block") { out$block <- lg$block; out$block_size <- lg$block_size; out$position_in_block <- lg$position_in_block }
    out$group_code <- lg$group_code; out$group_label <- lg$group_label; out$allocated_at <- lg$allocated_at
    return(out)
  }
  t <- x$table
  out <- data.frame(list_id = x$sequence_id, position = t$position, stringsAsFactors = FALSE)
  if (!is.null(d$strata_table)) { out$stratum <- t$stratum; for (v in names(d$strata)) out[[v]] <- t[[v]]; out$stratum_position <- t$stratum_position }
  if (d$method == "block") { out$block <- t$block; out$block_size <- t$block_size; out$position_in_block <- t$position_in_block; if (d$final_block == "complete") out$beyond_n <- t$beyond_n }
  out$group_code <- t$group_code; out$group_label <- t$group_label
  out
}

write_csv_plain <- function(df, path) { df[] <- lapply(df, function(v) if (is.factor(v)) as.character(v) else v); utils::write.csv(df, path, row.names = FALSE, na = "", fileEncoding = "UTF-8"); invisible(path) }
export_csv <- function(x, path) write_csv_plain(sequence_table(x), path)

export_xlsx <- function(x, path) {
  if (!requireNamespace("writexl", quietly = TRUE)) stop("XLSX export needs the 'writexl' package: install.packages('writexl').")
  sheets <- list(Allocations = sequence_table(x), Counts = if (is_session(x)) allocator_counts(x) else allocation_counts(x))
  if (!is_session(x)) { sheets$Targets <- x$targets; if (!is.null(x$final_blocks)) sheets$FinalBlocks <- x$final_blocks }
  sheets$Metadata <- audit_table(x)
  writexl::write_xlsx(sheets, path, format_headers = FALSE); invisible(path)
}

redcap_table <- function(x, record_id_var = "record_id", allocation_var = "randomization", coding = c("code", "label"), record_ids = NULL,
                         id_prefix = "", id_width = 0, include_strata = FALSE, strata_vars = NULL, event_name = NULL) {
  coding <- match.arg(coding); st <- sequence_table(x); n <- nrow(st)
  ids <- if (!is.null(record_ids)) { if (length(record_ids) != n) stop(sprintf("record_ids has %d entries but there are %d allocations.", length(record_ids), n)); as.character(record_ids) }
  else if (is_session(x)) { p <- st$participant; num <- if (id_width > 0) formatC(st$n, width = id_width, flag = "0") else as.character(st$n); ifelse(is.na(p) | !nzchar(trimws(p)), paste0(id_prefix, num), p) }
  else { num <- if (id_width > 0) formatC(st$position, width = id_width, flag = "0") else as.character(st$position); paste0(id_prefix, num) }
  out <- data.frame(ids, stringsAsFactors = FALSE); names(out) <- record_id_var
  if (!is.null(event_name) && nzchar(event_name)) out$redcap_event_name <- event_name
  out[[allocation_var]] <- if (coding == "code") st$group_code else st$group_label
  if (include_strata && !is.null(x$design$strata_table)) for (v in names(x$design$strata)) {
    var <- if (!is.null(strata_vars) && !is.null(strata_vars[[v]])) strata_vars[[v]] else make_redcap_name(v); out[[var]] <- st[[v]] }
  out
}
make_redcap_name <- function(x) { x <- tolower(gsub("[^A-Za-z0-9]+", "_", trimws(x))); x <- gsub("^_+|_+$", "", x); if (!grepl("^[a-z]", x)) x <- paste0("v_", x); x }
export_redcap_csv <- function(x, path, ...) write_csv_plain(redcap_table(x, ...), path)

# ---- audit / metadata --------------------------------------------------------
audit_table <- function(x) {
  d <- x$design; session <- is_session(x)
  info  <- if (session) rng_audit(x$rng) else x$rng_info
  valid <- if (session) validate_allocations(x)$ok else isTRUE(x$validation$ok)
  kv <- c(
    record_type          = if (session) "sequential allocation session (secondary workflow): allocations made so far" else "complete randomization list",
    identifier           = object_id(x),
    created_at           = format(if (session) x$created else x$generated_at, "%Y-%m-%d %H:%M:%S %Z"),
    exported_at          = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    application_version  = RANDOMIZER_APP_VERSION,
    trial_name           = d$name,
    description          = d$description %||% "",
    sample_size_n        = if (is.null(d$n)) "not specified" else as.character(d$n),
    allocations          = as.character(if (session) nrow(x$log) else nrow(x$table)),
    number_of_groups     = as.character(length(d$groups)),
    group_codes          = paste(d$codes, collapse = " | "),
    group_labels         = paste(d$groups, collapse = " | "),
    allocation_ratio     = ratio_string(d$ratio),
    randomization_method = d$method,
    block_type           = if (d$method == "block") d$block_type else "",
    block_sizes          = if (d$method == "block") paste(d$block_sizes, collapse = ", ") else "",
    final_block_handling = if (d$method == "block" && !session) d$final_block else "",
    stratification       = if (is.null(d$strata)) "none" else paste(sprintf("%s: %s", names(d$strata), vapply(d$strata, paste, character(1), collapse = "/")), collapse = " ; "),
    strata               = if (is.null(d$strata_table)) "" else sprintf("%d cross-classified strata, one independent stream each; stratum sizes never fixed or inferred", nrow(d$strata_table)),
    rows_per_stratum     = if (session || is.null(x$lengths) || is.null(d$strata_table)) "" else paste(sprintf("%s: %d", x$lengths$stratum, x$lengths$length), collapse = " ; "),
    target_count_method  = if (session) "not applicable (realised counts only)" else "largest remainder (Hamilton) for the requested list length; exact ties broken by a random draw",
    structural_validation = if (valid) "passed" else "FAILED")
  kv <- c(kv, setNames(info, paste0("rng_", names(info))))
  kv <- c(kv, allocation_selection = "every allocation is a single direct draw from the specified design; no candidate sequences were generated, scored, ranked or selected")
  data.frame(item = names(kv), value = unname(kv), stringsAsFactors = FALSE)
}

format_randomorg_responses <- function(resps) {
  if (is.null(resps) || !length(resps)) return(character(0))
  c("RANDOM.ORG responses (serial number, method, verified, completion time, bits used):",
    vapply(resps, function(r) sprintf("  #%s %s verified=%s %s bits=%s", r$serialNumber, r$method, r$verified, r$completionTime, r$bitsUsed), character(1)),
    "Raw signed responses (verbatim, for independent verification with RANDOM.ORG's public key):",
    unlist(lapply(resps, function(r) if (!is.na(r$raw)) c(sprintf("  --- serial %s ---", r$serialNumber), paste0("  ", r$raw)) else character(0))))
}

export_audit <- function(x, path) {
  a <- audit_table(x)
  if (tolower(tools::file_ext(path)) == "csv") return(write_csv_plain(a, path))
  counts <- if (is_session(x)) allocator_counts(x) else allocation_counts(x)
  resps <- if (is_session(x)) { if (x$source == "randomorg") x$rng$responses() else NULL } else x$rng_responses
  lines <- c("RANDOMIZATION AUDIT / METADATA RECORD", "(the allocations themselves are exported separately; keep this record as confidential as the list)", "",
             sprintf("%-32s %s", paste0(a$item, ":"), a$value), "",
             if (!is_session(x)) c("Integer targets per stratum (for the requested list length):", capture.output(print(x$targets, row.names = FALSE)), ""),
             if (!is_session(x) && !is.null(x$final_blocks)) c("Final block(s): full block composition versus rows within the requested length:", capture.output(print(x$final_blocks, row.names = FALSE)), ""),
             "Realised counts:", capture.output(print(counts, row.names = FALSE)),
             if (!is_session(x) && !is.null(x$table$beyond_n) && any(x$table$beyond_n)) c("", "Realised counts within the requested length only:", capture.output(print(allocation_counts(x, within_n = TRUE), row.names = FALSE))),
             if (!is.null(resps)) c("", format_randomorg_responses(resps)))
  writeLines(lines, path); invisible(path)
}

history_row <- function(x, generation_number) {
  d <- x$design
  data.frame(generation = generation_number, identifier = object_id(x), type = if (is_session(x)) "sequential session (secondary)" else "randomization list",
             created_at = format(if (is_session(x)) x$created else x$generated_at, "%Y-%m-%d %H:%M:%S"), trial = d$name,
             method = if (d$method == "block") sprintf("block (%s)", d$block_type) else "simple", n = if (is.null(d$n)) NA_integer_ else d$n,
             allocations = if (is_session(x)) nrow(x$log) else nrow(x$table), groups = length(d$groups), ratio = ratio_string(d$ratio),
             stratified = !is.null(d$strata_table), source = if (is_session(x)) x$source else x$source, mode = x$mode, stringsAsFactors = FALSE)
}
export_history <- function(history, path) write_csv_plain(history, path)
