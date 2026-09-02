# =============================================================================
#  shiny/app.R  -  browser interface on the UNCHANGED randomization engine.
#
#  Run from the application folder:   Rscript shiny/run.R      (or shiny::runApp("shiny"))
#
#  Architecture (per the approved design):
#    Shiny UI -> per-session reactiveValues -> existing engine (R/) -> session-local
#    generator objects -> generated list -> downloads / audit.
#  * Nothing is stored globally or on disk; downloads are written to a
#    temporary file per request that Shiny serves once.  Bookmarking is off.
#  * The RANDOM.ORG API key is read server-side from RANDOMORG_API_KEY and is
#    never sent to the browser.  RANDOM.ORG generation runs in a background R
#    worker (future/promises) so its network waits cannot block other users;
#    the worker creates and consumes its own generator.  All local sources run
#    synchronously in the session's process (PCG64 / Mersenne-Twister require it).
#  * There is no fallback: an unavailable source is not offered, and failures
#    stop the generation with the engine's message.
# =============================================================================
suppressPackageStartupMessages(library(shiny))

APP_ROOT <- local({
  cands <- c(Sys.getenv("RANDOMIZER_ROOT", ""), getwd(), dirname(getwd()))
  cands <- cands[nzchar(cands)]
  hit <- cands[file.exists(file.path(cands, "R", "load_engine.R"))]
  if (!length(hit)) stop("Cannot find the randomization engine (R/load_engine.R). Run from the application folder or set RANDOMIZER_ROOT.")
  normalizePath(hit[1])
})
source(file.path(APP_ROOT, "R", "load_engine.R"), encoding = "UTF-8")

CFG <- list(
  api_key   = Sys.getenv("RANDOMORG_API_KEY", ""),
  transport = getOption("randomizer.randomorg_transport", NULL),      # tests inject the mock transport; NULL = the real service
  max_rows  = 500L,
  # A public deployment (see DEPLOYMENT.md) carries this marker file, or the
  # environment variable, and then says plainly that it is a demonstration.
  demo      = file.exists(file.path(APP_ROOT, "PUBLIC_DEMO")) || nzchar(Sys.getenv("RANDOMIZER_PUBLIC_DEMO", "")))
# Background workers exist only so RANDOM.ORG's network waits cannot block other
# users; they are started only when RANDOM.ORG is actually usable on this server.
CFG$async <- isTRUE(getOption("randomizer.async",
  nzchar(CFG$api_key) && requireNamespace("future", quietly = TRUE) && requireNamespace("promises", quietly = TRUE)))
if (CFG$async && is.null(CFG$transport)) future::plan(future::multisession, workers = 2)

# Source availability as seen by this server.  RANDOM.ORG is offered only when
# the key is configured server-side AND it can run in a background worker
# (or a test transport was injected, which runs synchronously: test setups only).
SOURCE_STATUS <- lapply(setNames(RNG_SOURCES$id, RNG_SOURCES$id), function(id) {
  av <- source_available(id, api_key = CFG$api_key)
  if (id == "randomorg" && av$ok && !CFG$async && is.null(CFG$transport))
    av <- list(ok = FALSE, reason = "this server lacks the 'future' and 'promises' packages needed to run RANDOM.ORG requests without blocking other users")
  if (id == "randomorg" && !av$ok && grepl("API key", av$reason)) av$reason <- "no RANDOMORG_API_KEY configured on the server"
  av
})
SOURCE_HELP <- c(
  openssl = "Generated locally on the server from operating-system entropy. No seed exists, so the list cannot be regenerated: download the list together with its audit record.",
  pcg64 = "Provides high-quality statistical pseudorandomness appropriate for randomization when correctly implemented. Reproducible: the seed (2 x 31 bits from OS entropy) is recorded in the audit file.",
  mt = "The conventional generator of statistical software. Reproducible from the recorded 32-bit seed. Smaller seed space than PCG64: keep the audit file confidential.",
  xorshift128plus = "The algorithm behind Math.random in current browsers. Does NOT reproduce numbers from the Research Randomizer website, which has no seed or interface. Reproducible from the recorded 128-bit state. No advantage over the recommended options; provided for familiarity.",
  randomorg = "Atmospheric-noise randomness fetched by the server over HTTPS with the server's API key (never shown to you). The free tier is licensed for development and testing only and the non-gambling paid tier has no signed responses. Not reproducible except through RANDOM.ORG's own replay features. RANDOM.ORG sees request sizes, never trial data. Generation stops if the service or the signature check fails.")
SOURCE_TITLE <- c(openssl = "Operating-system cryptographically secure generator (OpenSSL)  -  Recommended",
                  pcg64 = "PCG64 statistical generator (dqrng), seeded from the operating system  -  Recommended",
                  mt = "Mersenne-Twister (R's default generator), seed drawn from the operating system",
                  xorshift128plus = "xorshift128+ (browser Math.random algorithm; Research Randomizer-style), seeded locally",
                  randomorg = "RANDOM.ORG true random service (server-side API key)")

source_choices <- function(ids) {
  ok <- ids[vapply(ids, function(i) SOURCE_STATUS[[i]]$ok, logical(1))]
  if (!length(ok)) return(NULL)
  list(names = lapply(ok, function(i) tags$span(tags$b(SOURCE_TITLE[[i]]), tags$br(), tags$small(SOURCE_HELP[[i]], class = "text-muted"))), values = ok)
}
unavailable_text <- function() {
  bad <- names(SOURCE_STATUS)[!vapply(SOURCE_STATUS, function(s) s$ok, logical(1))]; bad <- setdiff(bad, "test")
  if (!length(bad)) return(NULL)
  tags$p(class = "text-danger", "Unavailable on this server: ", paste(sprintf("%s (%s)", SOURCE_TITLE[bad], vapply(bad, function(b) SOURCE_STATUS[[b]]$reason, character(1))), collapse = "; "))
}

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------
ui <- fluidPage(
  tags$head(tags$style(HTML(".mono{font-family:monospace;white-space:pre;font-size:12px}.note{color:#555;font-size:90%}.warn{color:#8b0000}"))),
  titlePanel(sprintf("Clinical Trial Randomization List Generator  v%s", RANDOMIZER_APP_VERSION)),
  if (isTRUE(CFG$demo)) tags$div(style = "border:2px solid #8b0000;background:#fff4f4;color:#8b0000;padding:10px;margin-bottom:12px",
    tags$b("Public demonstration."), " This copy is published on a public address with no login, so anyone with the link can use it. ",
    "Try the interface and check the outputs here, but do not use a list generated on this server to enrol participants: ",
    "for a real trial the list must be produced on infrastructure your institution controls, and kept with an independent person. ",
    "Nothing you type is stored on this server, and the list disappears when you close the tab."),
  fluidRow(column(3, actionButton("generate", "Generate Randomization List", class = "btn-primary btn-lg", width = "100%")),
           column(9, tags$div(style = "padding-top:12px;color:navy", textOutput("status")))),
  tags$hr(),
  tabsetPanel(id = "tabs",
    tabPanel("1. Study design", value = "t1", br(),
      textInput("name", "Trial / project name", "Untitled trial", width = "60%"),
      textInput("description", "Description (optional)", "", width = "60%"),
      numericInput("n", "Sample size N", 100, min = 1, step = 1),
      selectInput("n_groups", "Number of treatment groups", c("2", "3", "4"), "2"),
      tags$b("Groups (label and code):"),
      fluidRow(column(4, textInput("glabel1", "Group 1 label", "1")), column(2, textInput("gcode1", "Code", "1"))),
      fluidRow(column(4, textInput("glabel2", "Group 2 label", "2")), column(2, textInput("gcode2", "Code", "2"))),
      conditionalPanel("input.n_groups >= 3", fluidRow(column(4, textInput("glabel3", "Group 3 label", "3")), column(2, textInput("gcode3", "Code", "3")))),
      conditionalPanel("input.n_groups >= 4", fluidRow(column(4, textInput("glabel4", "Group 4 label", "4")), column(2, textInput("gcode4", "Code", "4")))),
      tags$p(class = "note", "For an unstratified design the list has exactly N rows. For a stratified design each stratum gets its own list of the length you enter on tab 4; N is recorded for information and is never divided among strata.")),
    tabPanel("2. Allocation & blocks", value = "t2", br(),
      textInput("ratio", "Allocation ratio (one integer per group, e.g. 1:1, 2:1, 2:1:1)", "1:1"),
      radioButtons("method", "Randomization method", choiceNames = list(
        "Simple randomization: every position is an independent draw with probability proportional to the ratio; counts are not forced.",
        "Permuted block randomization: each block holds the exact ratio and is randomly permuted; counts stay close to the ratio throughout."),
        choiceValues = c("simple", "block"), selected = "block"),
      conditionalPanel("input.method == 'block'",
        radioButtons("block_type", "Blocks", choiceNames = list("Fixed block size (one size)", "Variable block sizes: each block's size is drawn at random from the permitted sizes (independently in every stratum)"),
                     choiceValues = c("fixed", "variable"), selected = "variable"),
        fluidRow(column(3, textInput("block_sizes", "Permitted block sizes", "4, 6, 8")), column(3, br(), actionButton("suggest", "Suggest compatible sizes")), column(3, br(), actionButton("compat", "Show compatible sizes"))),
        textOutput("compat_text"),
        radioButtons("final_block", "Final block rule", choiceNames = list(
          "If the last block overshoots the list length, truncate at the length (last block incomplete; its full composition is reported)",
          "Keep the complete last block (list longer than the length; extra rows flagged)"), choiceValues = c("truncate", "complete"), selected = "truncate"))),
    tabPanel("3. Stratification", value = "t3", br(),
      checkboxInput("strat_on", "Enable stratification", FALSE),
      conditionalPanel("input.strat_on", lapply(1:5, function(i) fluidRow(column(3, textInput(paste0("sname", i), sprintf("Variable %d name", i), "")), column(7, textInput(paste0("slevels", i), "Levels, separated by commas (e.g. Male, Female)", ""))))),
      tags$p(class = "note", "Define the stratification variables and their levels only. The strata are their cross-classification. Each stratum receives its own independent randomization stream with its own blocks and block sizes; blocks never span strata. How many participants will fall into each stratum is an outcome of recruitment: it is not asked for, not assumed equal, not derived from N and never inferred. Balance is maintained within each stratum. Many strata with few participants each weaken that balance.")),
    tabPanel("4. Generate randomization list", value = "t4", br(),
      tags$h4("Randomness source"),
      tags$b("Recommended"),
      uiOutput("source_ui"),
      unavailable_text(),
      conditionalPanel("input.source == 'randomorg'", checkboxInput("signed", "signed responses (verified)", TRUE)),
      tags$b("Developer testing only"),
      fluidRow(column(6, checkboxInput("test_mode", "TEST MODE - NOT FOR PRODUCTION: Mersenne-Twister with a typed seed", FALSE)), column(2, numericInput("test_seed", "seed", NA, step = 1))),
      tags$p(class = "note", "All production options produce unbiased randomization when correctly implemented; the randomization method, not the generator, determines the properties of the list. The two recommended options are preferred. Seeds are never derived from trial, site or participant information. Whatever you choose is recorded in the audit file, which must be kept as confidential as the list. Nothing is ever substituted for the source you select."),
      conditionalPanel("input.strat_on", numericInput("rows_per_stratum", "Rows to generate per stratum (stratified designs)", NA, min = 1, step = 1),
        tags$p(class = "note", "Length of the randomization list generated for each stratum. This is not an estimate of recruitment: the application never infers stratum sizes. It is the list capacity you choose for each stratum. Total rows generated = number of strata x rows per stratum.")),
      tags$p(class = "warn", "Generating a list does not by itself provide allocation concealment: keep the list and the audit file with an independent person, as your protocol requires."),
      actionButton("generate4", "GENERATE RANDOMIZATION LIST", class = "btn-primary"),
      br(), br(), tags$div(class = "mono", textOutput("summary")), br(),
      tableOutput("list_table"), textOutput("list_note")),
    tabPanel("5. Diagnostics", value = "t5", br(),
      actionButton("diag_list", "Diagnostics of the randomization list"), actionButton("diag_session", "Diagnostics of the sequential session"),
      tags$p(class = "note", "Descriptive only. Never used to select, modify, rank or reject a list."), tags$div(class = "mono", textOutput("diag"))),
    tabPanel("6. Export", value = "t6", br(),
      tags$h4("REDCap export settings"),
      fluidRow(column(3, textInput("rc_id", "Record ID column name", "record_id")), column(3, textInput("rc_alloc", "Allocation column name", "randomization")),
               column(2, selectInput("rc_coding", "Allocation values", c("code", "label"))), column(2, textInput("rc_prefix", "Record ID prefix", "")), column(2, numericInput("rc_width", "Zero-pad width", 0, min = 0))),
      fluidRow(column(4, textInput("rc_event", "redcap_event_name (optional)", "")), column(4, br(), checkboxInput("rc_strata", "Include stratification variables as columns", FALSE))),
      tags$h4("Randomization list"),
      downloadButton("dl_list_csv", "CSV"), downloadButton("dl_list_xlsx", "XLSX"), downloadButton("dl_list_redcap", "REDCap CSV"), downloadButton("dl_list_audit", "Audit / metadata"),
      tags$p(class = "note", "The list files contain the assignments. The audit file contains the design, identifiers, versions, the exact randomness source and its seed/state or signed responses; keep it as confidential as the list. Seeds and states are never written to the list or REDCap files. Files are produced on request and are not kept on the server.")),
    tabPanel("7. Simulation (planning)", value = "t7", br(),
      fluidRow(column(2, numericInput("n_sim", "Independent runs", 1000, min = 1)), column(2, numericInput("sim_length", "Stream length (per stratum)", 50, min = 1)), column(4, br(), actionButton("run_sim", "Run simulation (planning / methodological check only)"))),
      tags$p(class = "warn", "Uses the selected local randomness source (RANDOM.ORG is refused for simulations). Simulated streams are summarised and discarded; they are never used to choose or replace a list."),
      tags$div(class = "mono", textOutput("sim"))),
    tabPanel("8. Sequential allocation (secondary)", value = "t8", br(),
      tags$h4("Secondary workflow: allocate participants one at a time from the saved design. The primary purpose of this application is the complete list on tab 4."),
      actionButton("start_session", "Save design & start session (secondary workflow)"), actionButton("end_session", "End session"),
      br(), br(), tags$div(class = "mono", textOutput("sess_text")), br(),
      fluidRow(column(3, textInput("participant", "Participant ID", "")), column(6, uiOutput("level_ui"))),
      actionButton("allocate", "Allocate participant"), tags$h4(textOutput("last_alloc")),
      tableOutput("sess_table"), tags$div(class = "mono", textOutput("sess_counts")), br(),
      downloadButton("dl_sess_csv", "CSV"), downloadButton("dl_sess_xlsx", "XLSX"), downloadButton("dl_sess_redcap", "REDCap CSV"), downloadButton("dl_sess_audit", "Audit / metadata")),
    tabPanel("9. Session history", value = "t9", br(),
      tableOutput("history"), downloadButton("dl_history", "Export history CSV"),
      tags$p(class = "note", "Kept in memory for this browser session only; nothing is stored on the server."))
  )
)

# -----------------------------------------------------------------------------
# Server: everything below runs once per browser session (per-session closure)
# -----------------------------------------------------------------------------
server <- function(input, output, session) {
  rv <- reactiveValues(result = NULL, sess = NULL, history = NULL, gen_count = 0L, last_error = NULL,
                       status = "Define the trial on tabs 1-3, then generate the complete randomization list on tab 4.",
                       diag = "Generate a list first.", sim = "", compat = "", last_alloc = "", ro_confirmed = FALSE, busy = FALSE)
  session$onSessionEnded(function() { isolate({ rv$result <- NULL; rv$sess <- NULL; rv$history <- NULL }); invisible(gc()) })

  show_error <- function(msg) { rv$last_error <- msg; rv$busy <- FALSE; showModal(modalDialog(title = "Cannot continue", tags$pre(msg), easyClose = TRUE), session = session) }
  output$status <- renderText(rv$status)
  output$source_ui <- renderUI({
    ch <- source_choices(c("openssl", "pcg64", "mt", "xorshift128plus", "randomorg"))
    if (is.null(ch)) return(tags$p(class = "warn", "No production randomness source is available on this server."))
    tagList(radioButtons("source", NULL, choiceNames = ch$names, choiceValues = ch$values, selected = ch$values[1], width = "100%"),
            tags$p(class = "note", "Recommended: the first two. Additional established options: Mersenne-Twister, xorshift128+, RANDOM.ORG."))
  })

  # ---- design from inputs -------------------------------------------------
  read_design <- function() {
    k <- as.integer(input$n_groups)
    labels <- trimws(vapply(1:k, function(i) input[[paste0("glabel", i)]] %||% "", character(1))); codes <- trimws(vapply(1:k, function(i) input[[paste0("gcode", i)]] %||% "", character(1)))
    ratio <- parse_ratio(input$ratio); method <- input$method; strata <- NULL
    if (isTRUE(input$strat_on)) {
      for (i in 1:5) { nm <- trimws(input[[paste0("sname", i)]] %||% ""); lv <- trimws(strsplit(input[[paste0("slevels", i)]] %||% "", ",")[[1]]); lv <- lv[nzchar(lv)]
        if (nzchar(nm) || length(lv)) { if (!nzchar(nm)) stop(sprintf("Stratification row %d has levels but no variable name.", i)); strata[[nm]] <- lv } }
      if (!length(strata)) stop("Stratification is enabled but no variable is defined.")
    }
    n <- input$n; n <- if (is.null(n) || is.na(n)) NULL else n
    bs <- if (method == "block") { p <- trimws(strsplit(input$block_sizes, "[,; ]+")[[1]]); p <- p[nzchar(p)]; if (!all(grepl("^[0-9]+$", p))) stop("Block sizes must be positive integers separated by commas."); as.integer(p) } else NULL
    rand_design(name = trimws(input$name), description = trimws(input$description), n = n, groups = labels, codes = codes, ratio = ratio, method = method,
                block_type = input$block_type, block_sizes = bs, final_block = input$final_block, strata = strata)
  }
  checked_design <- function() {
    d <- tryCatch(read_design(), error = function(e) { show_error(conditionMessage(e)); NULL }); if (is.null(d)) return(NULL)
    v <- validate_design(d); if (!v$ok) { show_error(paste(format_validation(v), collapse = "\n")); return(NULL) }
    attr(d, "warnings") <- v$warnings; d
  }
  current_source <- function() {
    if (isTRUE(input$test_mode)) { seed <- suppressWarnings(as.integer(input$test_seed)); if (is.na(seed)) stop("TEST MODE needs an integer seed."); return(list(source = "test", seed = seed)) }
    src <- input$source; if (is.null(src) || !isTRUE(SOURCE_STATUS[[src]]$ok)) stop("The selected randomness source is not available on this server; nothing else was substituted.")
    list(source = src, seed = NULL)
  }

  observeEvent(input$suggest, { r <- tryCatch(parse_ratio(input$ratio), error = function(e) NULL); if (is.null(r)) return(show_error("Enter a valid ratio first."))
    s <- if (input$block_type == "fixed") suggested_block_sizes(r)[1] else suggested_block_sizes(r); updateTextInput(session, "block_sizes", value = paste(s, collapse = ", ")) })
  observeEvent(input$compat, { r <- tryCatch(parse_ratio(input$ratio), error = function(e) NULL); if (is.null(r)) return(show_error("Enter a valid ratio first."))
    rv$compat <- sprintf("Ratio %s reduces to %s; compatible block sizes are multiples of %d: %s ...", ratio_string(r), ratio_string(reduce_ratio(r)), ratio_unit(r), paste(compatible_block_sizes(r, max_multiple = 8), collapse = ", ")) })
  output$compat_text <- renderText(rv$compat)

  # ---- primary workflow: generate ONE complete list ---------------------------
  finish_generate <- function(r, d) {
    rv$result <- r; rv$gen_count <- rv$gen_count + 1L; rv$history <- rbind(rv$history, history_row(r, rv$gen_count)); rv$busy <- FALSE
    rv$status <- sprintf("Generated list %s (%d rows) with %s. Every click generates a new independent list.", r$sequence_id, nrow(r$table), r$rng_info[["source_label"]])
    updateTabsetPanel(session, "tabs", "t4")
  }
  do_generate <- function() {
    if (isTRUE(rv$busy)) return(show_error("A generation is already running in this session."))
    d <- checked_design(); if (is.null(d)) return(invisible(NULL))
    sl <- NULL
    if (!is.null(d$strata)) { sl <- input$rows_per_stratum; if (is.null(sl) || is.na(sl) || sl < 1 || sl != round(sl)) return(show_error("Enter 'Rows to generate per stratum' (a positive integer). Stratified designs need an explicit number of rows per stratum; the application never infers stratum sizes.")) ; sl <- as.integer(sl) }
    else if (is.null(d$n)) return(show_error("Enter the sample size N on tab 1: an unstratified list has exactly N rows."))
    cs <- tryCatch(current_source(), error = function(e) { show_error(conditionMessage(e)); NULL }); if (is.null(cs)) return(invisible(NULL))
    if (cs$source == "randomorg" && !isTRUE(rv$ro_confirmed)) {
      showModal(modalDialog(title = "RANDOM.ORG", tags$p("This server will request randomness from RANDOM.ORG over the internet using its configured API key."),
        tags$p("Licence: the free Developer tier is licensed strictly for development and testing; the Commercial (Non-Gambling) tier has no signed responses. Licensing is your institution's responsibility."),
        tags$p("Privacy: RANDOM.ORG will see the request sizes and the numbers it serves, never trial data. If the service or the signature check fails, generation stops; no other source is substituted."),
        footer = tagList(modalButton("Cancel"), actionButton("ro_confirm", "Continue"))), session = session)
      return(invisible(NULL))
    }
    rv$busy <- TRUE; rv$status <- sprintf("Generating list with %s ...", RNG_SOURCES$label[RNG_SOURCES$id == cs$source])
    if (cs$source == "randomorg" && CFG$async && is.null(CFG$transport)) {
      key <- CFG$api_key; signed <- isTRUE(input$signed); root <- APP_ROOT
      fut <- future::future({ source(file.path(root, "R", "load_engine.R"), encoding = "UTF-8")
                              generate_sequence(d, source = "randomorg", api_key = key, signed = signed, stratum_length = sl) },
                            seed = FALSE, globals = list(root = root, d = d, sl = sl, key = key, signed = signed), packages = NULL)
      promises::then(fut, onFulfilled = function(r) finish_generate(r, d), onRejected = function(e) show_error(conditionMessage(e)))
      return(invisible(NULL))
    }
    r <- tryCatch(generate_sequence(d, source = cs$source, seed = cs$seed, api_key = if (cs$source == "randomorg") CFG$api_key else NULL,
                                    signed = isTRUE(input$signed), transport = if (cs$source == "randomorg") CFG$transport else NULL, stratum_length = sl),
                  error = function(e) { show_error(conditionMessage(e)); NULL })
    if (is.null(r)) { rv$status <- "Generation stopped."; return(invisible(NULL)) }
    finish_generate(r, d)
  }
  observeEvent(input$generate, do_generate()); observeEvent(input$generate4, do_generate())
  observeEvent(input$ro_confirm, { rv$ro_confirmed <- TRUE; removeModal(session = session); do_generate() })

  output$summary <- renderText({
    r <- rv$result; if (is.null(r)) return("No list generated yet.")
    paste(c(sprintf("List ID: %s   generated: %s", r$sequence_id, format(r$generated_at, "%Y-%m-%d %H:%M:%S")),
            sprintf("Randomness source: %s [%s]", r$rng_info[["source_label"]], r$mode), design_summary_lines(r$design), "",
            "Integer targets for the requested length (largest remainder):", capture.output(print(r$targets, row.names = FALSE)), "",
            "Realised allocation:", capture.output(print(allocation_counts(r), row.names = FALSE)),
            if (!is.null(r$final_blocks)) c("", "Final block (full composition vs rows within the requested length):", capture.output(print(r$final_blocks, row.names = FALSE))),
            if (!is.null(r$table$beyond_n) && any(r$table$beyond_n)) c("", "Counts within the requested length only:", capture.output(print(allocation_counts(r, within_n = TRUE), row.names = FALSE))),
            "", sprintf("Structural validation: %s (%s)", if (r$validation$ok) "passed" else "FAILED", paste(r$validation$checks, collapse = ", "))), collapse = "\n")
  })
  output$list_table <- renderTable({ r <- rv$result; if (is.null(r)) return(NULL); head(sequence_table(r)[, -1], CFG$max_rows) }, striped = TRUE)
  output$list_note <- renderText({ r <- rv$result; if (!is.null(r) && nrow(r$table) > CFG$max_rows) sprintf("Showing the first %d of %d rows; download the file for the complete list.", CFG$max_rows, nrow(r$table)) else "" })

  # ---- diagnostics ----------------------------------------------------------
  observeEvent(input$diag_list, { if (is.null(rv$result)) return(show_error("No randomization list yet.")); rv$diag <- paste(format_diagnostics(sequence_diagnostics(rv$result)), collapse = "\n") })
  observeEvent(input$diag_session, { if (is.null(rv$sess)) return(show_error("No sequential-allocation session.")); rv$diag <- paste(format_diagnostics(sequence_diagnostics(rv$sess)), collapse = "\n") })
  output$diag <- renderText(rv$diag)

  # ---- downloads: one temporary file per request, served once by Shiny ---------
  write_export <- function(x, kind, file) {
    switch(kind, csv = export_csv(x, file), xlsx = export_xlsx(x, file),
           redcap = export_redcap_csv(x, file, record_id_var = trimws(input$rc_id), allocation_var = trimws(input$rc_alloc), coding = input$rc_coding, id_prefix = input$rc_prefix,
                                      id_width = if (is.na(input$rc_width)) 0 else as.integer(input$rc_width), include_strata = isTRUE(input$rc_strata), event_name = trimws(input$rc_event)),
           audit = export_audit(x, file), history = export_history(x, file))
  }
  dl <- function(what, kind, ext) downloadHandler(
    filename = function() { x <- if (what == "list") rv$result else if (what == "session") rv$sess else rv$history; req(x)
      base <- if (what == "history") "randomization_history" else object_id(x); paste0(base, "_", kind, ".", ext) },
    content = function(file) { x <- if (what == "list") rv$result else if (what == "session") rv$sess else rv$history; req(x); write_export(x, kind, file) })
  output$dl_list_csv <- dl("list", "csv", "csv"); output$dl_list_xlsx <- dl("list", "xlsx", "xlsx"); output$dl_list_redcap <- dl("list", "redcap", "csv"); output$dl_list_audit <- dl("list", "audit", "txt")
  output$dl_sess_csv <- dl("session", "csv", "csv"); output$dl_sess_xlsx <- dl("session", "xlsx", "xlsx"); output$dl_sess_redcap <- dl("session", "redcap", "csv"); output$dl_sess_audit <- dl("session", "audit", "txt")
  output$dl_history <- dl("history", "history", "csv")

  # ---- simulation (planning) --------------------------------------------------
  observeEvent(input$run_sim, {
    d <- checked_design(); if (is.null(d)) return(invisible(NULL))
    cs <- tryCatch(current_source(), error = function(e) { show_error(conditionMessage(e)); NULL }); if (is.null(cs)) return(invisible(NULL))
    s <- tryCatch(simulate_design(d, as.integer(input$n_sim), length = as.integer(input$sim_length), stratum_length = as.integer(input$sim_length), source = cs$source, seed = cs$seed),
                  error = function(e) { show_error(conditionMessage(e)); NULL }); if (is.null(s)) return(invisible(NULL))
    rv$sim <- paste(format_simulation(s), collapse = "\n"); rv$status <- sprintf("Simulation of %d runs finished.", as.integer(input$n_sim))
  })
  output$sim <- renderText(rv$sim)

  # ---- secondary workflow: sequential allocation --------------------------------
  observeEvent(input$start_session, {
    d <- checked_design(); if (is.null(d)) return(invisible(NULL))
    cs <- tryCatch(current_source(), error = function(e) { show_error(conditionMessage(e)); NULL }); if (is.null(cs)) return(invisible(NULL))
    if (cs$source == "randomorg" && CFG$async && is.null(CFG$transport)) return(show_error("Sequential allocation with RANDOM.ORG is not offered in the browser version: each allocation would be a live request that blocks the server. Use a local source for sessions, or the desktop application."))
    a <- tryCatch(rand_allocator(d, source = cs$source, seed = cs$seed, api_key = if (cs$source == "randomorg") CFG$api_key else NULL, signed = isTRUE(input$signed), transport = if (cs$source == "randomorg") CFG$transport else NULL),
                  error = function(e) { show_error(conditionMessage(e)); NULL }); if (is.null(a)) return(invisible(NULL))
    rv$sess <- a; rv$gen_count <- rv$gen_count + 1L; rv$history <- rbind(rv$history, history_row(a, rv$gen_count)); rv$last_alloc <- ""
    rv$status <- sprintf("Sequential session %s started (secondary workflow) with %s.", a$id, a$rng$label); updateTabsetPanel(session, "tabs", "t8")
  })
  observeEvent(input$end_session, { rv$sess <- NULL; rv$last_alloc <- ""; rv$status <- "Session ended." })
  output$level_ui <- renderUI({ a <- rv$sess; if (is.null(a) || is.null(a$design$strata)) return(tags$span(class = "note", if (is.null(a)) "" else "(no stratification)"))
    tagList(lapply(names(a$design$strata), function(v) selectInput(paste0("level_", make.names(v)), v, c("", a$design$strata[[v]])))) })
  observeEvent(input$allocate, {
    a <- rv$sess; if (is.null(a)) return(show_error("Start a session first."))
    pid <- trimws(input$participant); if (!nzchar(pid)) return(show_error("Enter a participant ID."))
    levels <- if (!is.null(a$design$strata)) setNames(lapply(names(a$design$strata), function(v) input[[paste0("level_", make.names(v))]] %||% ""), names(a$design$strata)) else NULL
    row <- tryCatch(allocate_next(a, levels, pid), error = function(e) { show_error(conditionMessage(e)); NULL }); if (is.null(row)) return(invisible(NULL))
    rv$last_alloc <- sprintf("%s  ->  %s   (allocation: %s)%s", row$participant, row$group_label, row$group_code, if (!is.null(a$design$strata)) sprintf("   [stratum: %s]", row$stratum) else "")
    rv$history$allocations[rv$history$identifier == a$id] <- nrow(a$log); rv$sess <- a; updateTextInput(session, "participant", value = "")
    rv$status <- sprintf("%s allocated. %d participants in session %s.%s", row$participant, nrow(a$log), a$id, if (!is.null(attr(row, "note"))) paste("", attr(row, "note")) else "")
  })
  output$sess_text <- renderText({ a <- rv$sess; if (is.null(a)) "No active session. Complete tabs 1-3, choose the randomness source on tab 4, then start a session." else paste(c(allocator_summary_lines(a)[1:2], design_summary_lines(a$design)), collapse = "\n") })
  output$last_alloc <- renderText(rv$last_alloc)
  output$sess_table <- renderTable({ a <- rv$sess; if (is.null(a) || !nrow(a$log)) return(NULL); tail(sequence_table(a)[, -1], CFG$max_rows) }, striped = TRUE)
  output$sess_counts <- renderText({ a <- rv$sess; if (is.null(a)) "" else paste(c("Realised allocation to date (objective: the ratio within each stratum):", capture.output(print(allocator_counts(a), row.names = FALSE))), collapse = "\n") })
  output$history <- renderTable(rv$history, striped = TRUE)
}

shinyApp(ui, server)
