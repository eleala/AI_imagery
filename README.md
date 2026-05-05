# Replication package 

This repository contains the code and data needed to reproduce every analysis,
figure, and table in the paper.

> **Citation.** *Add full citation here once available, e.g.:*
> Adena M. et al. (2026). *AI Images, Trust and News Demand*, CESifo Working Paper, No. 12277

> **Contact.** Eleonora Alabrese — `alabrese.eleonora@gmail.com`

---

## 1. Overview

The paper combines three studies:

| Study | Description | Sample |
|---|---|---|
| **Experiment 1** | Online experiment varying AI vs. human-made article images, with and without an AI label, measuring newsletter sign-up demand and perceived quality. | UK Prolific respondents |
| **Experiment 2** ("wave 2", "Mechs") | Follow-up experiment with six arms (Control, Easy AI, Hard AI, each with/without label) designed to isolate mechanisms. | UK Prolific respondents |
| **Journalists survey** | Belief-elicitation survey with 132 international journalists. | International Prolific journalists |

All three are reproduced from raw Qualtrics CSV exports stored in `DATA/`.

---

## 2. Repository structure

```
.
├── README.md                       <- this file
├── run_all.R                       <- master script: runs the full pipeline
├── .gitignore
│
├── Experiment1.R                   <- Exp 1: cleaning + analysis + figures + tables
├── Experiment2_cleaning.R          <- Exp 2: produces DATA/ai_labels_mechs_cleaned.{csv,rds}
├── Experiment2_graphs.R            <- Exp 2: figures (reads cleaned .rds)
├── Experiment2_reg_analysis.R      <- Exp 2: regression tables (reads cleaned .rds)
├── Journalists_analysis.R          <- Journalists survey: figures + tables
│
├── DATA/                           <- raw Qualtrics exports + Exp 2 cleaned data
│   ├── AI labels - Full Study_October 19, 2025_15.58.csv      (Exp 1, full study)
│   ├── AI labels_October 15, 2025_21.02.csv                   (Exp 1, pilot wave; combined with full study)
│   ├── AI labels - Mechs - full_April 8, 2026_07.22.csv       (Exp 2, raw)
│   ├── AI labels - Journalists_April 27, 2026_04.25.csv       (Journalists survey, raw)
│   ├── ai_labels_mechs_cleaned.csv                            (Exp 2, cleaned — produced by Experiment2_cleaning.R)
│   └── ai_labels_mechs_cleaned.rds                            (same, R binary — produced by Experiment2_cleaning.R)
│
├── figures/                        <- created on first run
│   ├── wave2/                      <- Exp 2 figures
│   └── journalists/                <- Journalists-survey figures
└── tables/                         <- created on first run
    └── wave2/                      <- Exp 2 LaTeX tables
```

The cleaned Exp 2 data (`ai_labels_mechs_cleaned.{csv,rds}`) is shipped for
convenience, but `run_all.R` regenerates it from the raw CSV.

---

## 3. Software requirements

- **R** ≥ 4.3 (tested on R 4.4)
- The following CRAN packages (a single `install.packages()` call below
  installs all of them):

```r
install.packages(c(
  # core
  "data.table", "dplyr", "tidyr", "tidyverse", "purrr", "tibble", "readr",
  "stringr", "lubridate", "janitor", "reshape2", "tidyfast",
  # I/O
  "haven", "foreign", "openxlsx", "readxl",
  # modelling
  "fixest", "AER", "sandwich", "lmtest", "car", "rstatix", "dreamerr",
  # tables
  "stargazer", "xtable", "knitr", "kableExtra",
  # plotting
  "ggplot2", "ggthemes", "ggpubr", "scales", "patchwork", "gridExtra",
  "coefplot", "plotly",
  # misc
  "RecordLinkage", "httr2", "jsonlite", "parallel", "plyr"
))
```

After running `run_all.R`, full version provenance is printed via
`sessionInfo()` and can be saved by redirecting stdout (e.g.
`Rscript run_all.R > run.log 2>&1`).

---

## 4. How to reproduce

From a terminal, in the folder that contains this README:

```bash
Rscript run_all.R
```

Or, from inside RStudio: open `run_all.R` and click **Source**.

`run_all.R` will:

1. Set the working directory to the repository root.
2. Verify that all required scripts and data files exist (and stop with a
   helpful error otherwise).
3. Create `figures/`, `figures/wave2/`, `tables/`, and `tables/wave2/`.
4. Run the five analysis scripts in dependency order.
5. Print `sessionInfo()` for provenance.

Total runtime: roughly 5–10 minutes on a modern laptop.

---

## 5. What each script does

| Script | Inputs | Outputs |
|---|---|---|
| `Experiment1.R` | `DATA/AI labels - Full Study_…csv`, `DATA/AI labels_October 15…csv` | `figures/Fig_*.pdf`, `tables/table_*.tex`, `tables/iv_table.tex` |
| `Experiment2_cleaning.R` | `DATA/AI labels - Mechs - full_…csv` | `DATA/ai_labels_mechs_cleaned.{csv,rds}` |
| `Experiment2_graphs.R` | `DATA/ai_labels_mechs_cleaned.rds` | `figures/wave2/Fig_*.pdf` |
| `Experiment2_reg_analysis.R` | `DATA/ai_labels_mechs_cleaned.rds` | `tables/wave2/table_*.tex` |
| `Journalists_analysis.R` | `DATA/AI labels - Journalists_…csv` | `figures/journalists/journalists_plot_*.png`, `tables/table_*_journalists.tex` |

The scripts are independent of one another with one exception:
`Experiment2_graphs.R` and `Experiment2_reg_analysis.R` both depend on the
cleaned `.rds` produced by `Experiment2_cleaning.R`. `run_all.R` enforces the
correct order.

---

## 6. Data provenance

All survey data were collected on **Prolific** using **Qualtrics** between
October 2025 and April 2026:

- **Experiment 1 — pilot:** October 15, 2025 (n ≈ small pilot); merged with the full study below.
- **Experiment 1 — full study:** October 19, 2025.
- **Experiment 2 ("Mechs"):** April 8, 2026.
- **Journalists survey:** April 27, 2026 (n = 132 after excluding survey-preview rows and attention-check failures).

Qualtrics exports include three header rows (machine names / question text /
import IDs); each script handles this format explicitly. The exact filenames
(with timestamps) are preserved as shipped.

The pre-registration, IRB approvals, and survey instruments are documented
separately in the manuscript's online appendix.

---

## 7. Known issues / notes for reviewers

A few things worth flagging:

- **Static figures.** Three figures in the manuscript are not produced by
  code (they are the experimental-setup screenshots
  `Figures/Experimental.png`, `Figures/Control.png`, `Figures/AI.png`) and
  are not included here.

- **Several Exp 1 plots are supplementary.** `Experiment1.R` saves a number
  of additional plots (`Fig_Reader.pdf`, `Fig_HiglyAIfamiliar.pdf`, …) that
  are produced as robustness diagnostics and not all of them appear in the
  paper.

- **Case sensitivity.** Output folders are lowercase (`tables/`, `figures/`).
  On macOS this is interchangeable with capitalised forms; on Linux it is
  not. The pipeline is internally consistent.

- **Reproducibility caveat.** Some figure layouts depend on the precise
  versions of `ggplot2` / `ggpubr` / `patchwork`; minor cosmetic differences
  may appear under different versions. All reported numerical results are
  version-independent.

---

## 8. License

*Add a license of your choice (e.g. MIT for code, CC-BY-4.0 for data)
before publishing.*
