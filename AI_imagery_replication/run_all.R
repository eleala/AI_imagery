# =============================================================================
# run_all.R  —  Master replication script
# =============================================================================
#
# Reproduces every figure and table reported in the paper from the raw
# Qualtrics CSV exports stored in DATA/.
#
# Usage:
#   From a terminal, in the replication-package root folder:
#       Rscript run_all.R
#
#   Or, from an interactive R / RStudio session, with the working directory
#   set to the replication-package root:
#       source("run_all.R")
#
# Outputs are written to:
#   figures/        Experiment 1 plots
#   figures/wave2/  Experiment 2 plots
#   tables/         Experiment 1 LaTeX tables
#   tables/wave2/   Experiment 2 LaTeX tables
#   DATA/ai_labels_mechs_cleaned.{csv,rds}   cleaned wave-2 data
#
# Total runtime: roughly 5–10 minutes on a modern laptop.
# =============================================================================

# ---- 1. Locate the replication-package root --------------------------------
# Robust to being run via Rscript, sourced interactively, or sourced from
# RStudio.  Falls back to the current working directory.

get_script_dir <- function() {
  # (a) Rscript:  --file=/path/to/run_all.R among commandArgs
  args <- commandArgs(trailingOnly = FALSE)
  hit  <- grep("^--file=", args, value = TRUE)
  if (length(hit) == 1L) {
    return(dirname(normalizePath(sub("^--file=", "", hit))))
  }
  # (b) source()-ed: sys.frames may carry an ofile slot
  for (i in rev(seq_len(sys.nframe()))) {
    of <- sys.frame(i)$ofile
    if (!is.null(of)) return(dirname(normalizePath(of)))
  }
  # (c) RStudio interactive
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    p <- tryCatch(rstudioapi::getSourceEditorContext()$path,
                  error = function(e) "")
    if (nzchar(p)) return(dirname(normalizePath(p)))
  }
  # (d) fallback
  getwd()
}

# Use a hidden name (.ROOT) so that rm(list = ls()) calls inside the
# individual scripts (which default to all.names = FALSE) don't wipe it.
.ROOT <- get_script_dir()
setwd(.ROOT)
cat(sprintf("[run_all] Working directory set to: %s\n", .ROOT))

# ---- 2. Sanity checks -------------------------------------------------------
required_scripts <- c(
  "Experiment1.R",
  "Experiment2_cleaning.R",
  "Experiment2_graphs.R",
  "Experiment2_reg_analysis.R",
  "Journalists_analysis.R"
)
required_data <- c(
  "DATA/AI labels - Full Study_October 19, 2025_15.58.csv",
  "DATA/AI labels_October 15, 2025_21.02.csv",
  "DATA/AI labels - Mechs - full_April 8, 2026_07.22.csv",
  "DATA/AI labels - Journalists_April 27, 2026_04.25.csv"
)

missing <- c(
  required_scripts[!file.exists(required_scripts)],
  required_data[!file.exists(required_data)]
)
if (length(missing) > 0L) {
  stop("Missing required files:\n  ",
       paste(missing, collapse = "\n  "),
       "\nAre you running this from the replication-package root?")
}

# ---- 3. Make sure output folders exist --------------------------------------
dir.create("figures",              showWarnings = FALSE, recursive = TRUE)
dir.create("figures/wave2",        showWarnings = FALSE, recursive = TRUE)
dir.create("figures/journalists",  showWarnings = FALSE, recursive = TRUE)
dir.create("tables",               showWarnings = FALSE, recursive = TRUE)
dir.create("tables/wave2",         showWarnings = FALSE, recursive = TRUE)

# ---- 4. Run pipeline --------------------------------------------------------
# Order matters:
#   - Experiment2_cleaning.R must run before the wave-2 graphs/regs (it
#     produces DATA/ai_labels_mechs_cleaned.rds).
#   - The other scripts are independent of one another.

.run_step <- function(label, file) {
  cat(sprintf("\n========== %s : %s ==========\n", label, file))
  # Defensive: re-assert cwd before each script in case a previous step
  # (or one of its loaded packages, or rm(list = ls())) altered it.
  setwd(.ROOT)
  t0 <- Sys.time()
  source(file, echo = FALSE, max.deparse.length = Inf)
  cat(sprintf("[run_all] %s finished in %.1fs\n",
              label, as.numeric(difftime(Sys.time(), t0, units = "secs"))))
}

.run_step("Experiment 1 (full pipeline: clean + analysis + figs/tables)",
         "Experiment1.R")
.run_step("Experiment 2 — cleaning",                "Experiment2_cleaning.R")
.run_step("Experiment 2 — figures",                 "Experiment2_graphs.R")
.run_step("Experiment 2 — regression tables",       "Experiment2_reg_analysis.R")
.run_step("Journalists survey — figures + tables",  "Journalists_analysis.R")

# ---- 5. Provenance ----------------------------------------------------------
cat("\n========== sessionInfo ==========\n")
print(sessionInfo())
cat("\n[run_all] Done. All figures and tables have been regenerated.\n")

