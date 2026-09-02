# Sources the randomization engine (everything except the GUI).
#   source("R/load_engine.R")
local({
  this_file <- NULL
  for (i in rev(seq_len(sys.nframe()))) { of <- sys.frame(i)$ofile; if (!is.null(of)) { this_file <- of; break } }
  here <- if (is.null(this_file)) "R" else dirname(normalizePath(this_file))
  if (!file.exists(file.path(here, "randomization_core.R"))) here <- "R"
  for (f in c("randomization_core.R", "randomorg.R", "simple_randomization.R", "block_randomization.R",
              "stratified_randomization.R", "validation.R", "diagnostics.R", "simulation.R", "export.R"))
    source(file.path(here, f), local = FALSE, encoding = "UTF-8")
})
