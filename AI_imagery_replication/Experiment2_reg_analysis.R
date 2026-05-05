# -----------------------------------------------------------------------------
# 0. PARAMETERS
# -----------------------------------------------------------------------------####

library(data.table)
library(fixest)
library(xtable)

rm(list = ls())
# NOTE: working directory must be set to the replication-package root
# (the folder containing this script). run_all.R does this automatically.

# output folder for wave 2 tables
dir.create("tables/wave2", recursive = TRUE, showWarnings = FALSE)

# Load cleaned wave-2 data produced in 0_wave2_cleaning.R
dt <- readRDS("DATA/ai_labels_mechs_cleaned.rds")

cat(sprintf("Loaded cleaned wave-2 data: %d rows, %d cols\n", nrow(dt), ncol(dt)))


# -----------------------------------------------------------------------------
# 1. TREATMENT STRUCTURE
# -----------------------------------------------------------------------------####

# explicit ordering of treatment groups
dt[, treatment := factor(
  treatment,
  levels = c("C", "LC", "EAI", "LEAI", "HAI", "LHAI")
)]

# readable labels
dt[, treatment_label := fcase(
  treatment == "C",    "Control",
  treatment == "LC",   "Control + Label",
  treatment == "EAI",  "Easy AI",
  treatment == "LEAI", "Easy AI + Label",
  treatment == "HAI",  "Hard AI",
  treatment == "LHAI", "Hard AI + Label"
)]

dt[, treatment_label := factor(
  treatment_label,
  levels = c(
    "Control",
    "Control + Label",
    "Easy AI",
    "Easy AI + Label",
    "Hard AI",
    "Hard AI + Label"
  )
)]

# useful treatment dimensions
dt[, label := treatment %in% c("LC", "LEAI", "LHAI")]
dt[, ai := treatment %in% c("EAI", "HAI", "LEAI", "LHAI")]
dt[, easy_ai := treatment %in% c("EAI", "LEAI")]
dt[, hard_ai := treatment %in% c("HAI", "LHAI")]

table(dt$treatment, useNA = "ifany")
table(dt$treatment_label, useNA = "ifany")

# -----------------------------------------------------------------------------
# 2. IMAGE STRUCTURE
# -----------------------------------------------------------------------------#####

dt[, image_id := factor(
  image_id,
  levels = c("1", "3", "4", "5", "7", "8")
)]

table(dt$image_id, useNA = "ifany")
table(dt$treatment, dt$image_id, useNA = "ifany")


# -----------------------------------------------------------------------------
# 3. BALANCE VARIABLES
# -----------------------------------------------------------------------------####

bal_vars <- c(
  "sex_birth",
  "age",
  "ethnicity",
  "born_uk",
  "uk_region",
  "income",
  "employment_status",
  "education",
  "political_orientation",
  "party_id",
  "news_interest",
  "news_frequency",
  "newsletter_subscribed",
  "trust_news_sources",
  "ai_familiarity",
  "ai_exposure"
)

bal_vars


# -----------------------------------------------------------------------------
# 4. SIMPLE BALANCE TABLE / DESCRIPTIVES SUMMARY
# -----------------------------------------------------------------------------####

arm_levels <- levels(dt$treatment)

is_numericish <- function(x) {
  is.numeric(x) || is.logical(x) || inherits(x, "integer")
}

balance_list <- list()

for (v in bal_vars) {
  
  x <- dt[[v]]
  
  # numeric / logical variables
  if (is_numericish(x)) {
    
    tmp <- dt[!is.na(get(v)), .(
      value = mean(get(v), na.rm = TRUE)
    ), by = treatment]
    
    tmp_full <- data.table(treatment = arm_levels)
    tmp_full <- merge(tmp_full, tmp, by = "treatment", all.x = TRUE, sort = FALSE)
    
    pval <- tryCatch(
      summary(lm(get(v) ~ treatment, data = dt))$coefficients,
      error = function(e) NULL
    )
    
    global_p <- tryCatch(
      anova(lm(get(v) ~ treatment, data = dt))["treatment", "Pr(>F)"],
      error = function(e) NA_real_
    )
    
    row <- data.table(
      variable = v,
      category = "mean"
    )
    
    for (i in seq_along(arm_levels)) {
      row[[arm_levels[i]]] <- tmp_full$value[i]
    }
    
    row[, p_value := global_p]
    balance_list[[v]] <- row
    
  } else {
    
    # factor / character: use shares of each category
    levs <- sort(unique(as.character(x)))
    levs <- levs[!is.na(levs)]
    
    for (lev in levs) {
      
      tmp <- dt[!is.na(get(v)), .(
        value = mean(as.character(get(v)) == lev, na.rm = TRUE)
      ), by = treatment]
      
      tmp_full <- data.table(treatment = arm_levels)
      tmp_full <- merge(tmp_full, tmp, by = "treatment", all.x = TRUE, sort = FALSE)
      
      global_p <- tryCatch(
        chisq.test(table(dt[[v]], dt$treatment))$p.value,
        error = function(e) NA_real_
      )
      
      row <- data.table(
        variable = v,
        category = lev
      )
      
      for (i in seq_along(arm_levels)) {
        row[[arm_levels[i]]] <- tmp_full$value[i]
      }
      
      row[, p_value := global_p]
      balance_list[[paste(v, lev, sep = "_")]] <- row
    }
  }
}

balance_dt <- rbindlist(balance_list, fill = TRUE)

balance_dt

# round numeric columns
num_cols <- setdiff(names(balance_dt), c("variable", "category"))
for (cc in num_cols) {
  balance_dt[, (cc) := round(get(cc), 3)]
}

balance_dt

print(
  xtable(balance_dt),
  file = "tables/wave2/table_balance_wave2.tex",
  include.rownames = FALSE,
  append = FALSE
)



# -----------------------------------------------------------------------------
# 4b_alt. TABLE B.2 STYLE BALANCE TABLE (wave-1 format):
#   one row per variable, group means, global p, min pairwise p
# -----------------------------------------------------------------------------

# Variables in order: demographics first, then post-treatment (separated by a midrule)
bal_vars_b2_pre  <- c("sex_birth", "age", "ethnicity", "born_uk", "uk_region",
                      "income", "employment_status", "education",
                      "political_orientation", "party_id",
                      "news_interest", "news_frequency", "newsletter_subscribed")

bal_vars_b2_post <- c("timing_last_click", "belief_image_ai", "recognition_correct")

arm_levels_b2 <- levels(dt$treatment)   # C LC EAI LEAI HAI LHAI

# helper: truly numeric (not factor/character masquerading as numeric)
is_numericish_b2 <- function(x) {
  if (is.factor(x) || is.character(x)) return(FALSE)
  is.numeric(x) || is.integer(x) || is.logical(x)
}

make_balance_row <- function(v) {
  x   <- dt[[v]]
  sub <- dt[!is.na(treatment) & !is.na(x), .(treatment, x)]
  if (nrow(sub) == 0L) {
    means <- setNames(as.list(rep(NA_real_, length(arm_levels_b2))), arm_levels_b2)
    return(c(list(Variable = v), means, list(Global_p = NA_real_, Min_pairwise_p = NA_real_)))
  }
  
  if (is_numericish_b2(sub$x)) {
    sub[, y := suppressWarnings(as.numeric(x))]
    mns <- sub[, .(mean = mean(y, na.rm = TRUE)), by = treatment]
    mns <- merge(data.table(treatment = factor(arm_levels_b2, levels = arm_levels_b2)),
                 mns, by = "treatment", all.x = TRUE, sort = TRUE)
    arm_means <- as.list(round(mns$mean, 2)); names(arm_means) <- arm_levels_b2
    
    Global_p <- tryCatch({
      as.numeric(anova(lm(y ~ treatment, data = sub))["treatment", "Pr(>F)"])
    }, error = function(e) NA_real_)
    
    pw <- tryCatch({
      out <- pairwise.t.test(sub$y, sub$treatment, p.adjust.method = "holm")
      suppressWarnings(min(out$p.value, na.rm = TRUE))
    }, warning = function(w) NA_real_, error = function(e) NA_real_)
    
    c(list(Variable = v), arm_means, list(Global_p = round(Global_p, 2), Min_pairwise_p = round(pw, 2)))
    
  } else {
    fac <- factor(sub$x)
    if (nlevels(fac) <= 1L) {
      means <- setNames(as.list(rep(NA_real_, length(arm_levels_b2))), arm_levels_b2)
      return(c(list(Variable = v), means, list(Global_p = NA_real_, Min_pairwise_p = NA_real_)))
    }
    modal_lvl <- names(sort(table(fac), decreasing = TRUE))[1]
    sub[, flag := (fac == modal_lvl)]
    shr <- sub[, .(mean = mean(flag, na.rm = TRUE)), by = treatment]
    shr <- merge(data.table(treatment = factor(arm_levels_b2, levels = arm_levels_b2)),
                 shr, by = "treatment", all.x = TRUE, sort = TRUE)
    arm_means <- as.list(round(shr$mean, 2)); names(arm_means) <- arm_levels_b2
    
    Global_p <- tryCatch(
      suppressWarnings(chisq.test(table(fac, sub$treatment))$p.value),
      error = function(e) NA_real_)
    
    levs <- arm_levels_b2[arm_levels_b2 %in% sub$treatment]
    cm   <- combn(levs, 2, simplify = FALSE)
    pvec <- vapply(cm, function(pr) {
      a <- sub[treatment == pr[1], .(x = sum(flag), n = .N)]
      b <- sub[treatment == pr[2], .(x = sum(flag), n = .N)]
      if (nrow(a) == 0L || nrow(b) == 0L) return(NA_real_)
      suppressWarnings(prop.test(x = c(a$x, b$x), n = c(a$n, b$n), correct = FALSE)$p.value)
    }, numeric(1L))
    pw_holm <- tryCatch(min(p.adjust(pvec, method = "holm"), na.rm = TRUE),
                        error = function(e) NA_real_)
    
    c(list(Variable = sprintf("%s (share of '%s')", v, modal_lvl)),
      arm_means, list(Global_p = round(Global_p, 2), Min_pairwise_p = round(pw_holm, 2)))
  }
}

# build pre- and post-treatment panels
pre_rows  <- lapply(bal_vars_b2_pre,  make_balance_row)
post_rows_b2 <- lapply(bal_vars_b2_post, make_balance_row)

pre_dt  <- rbindlist(lapply(pre_rows,  as.list), fill = TRUE)
post_dt_b2 <- rbindlist(lapply(post_rows_b2, as.list), fill = TRUE)

# -- override auto-generated Variable labels with human-readable ones --
var_labels <- c(
  "sex_birth"             = "Gender (share of 'Female')",
  "age"                   = "Age",
  "ethnicity"             = "Ethnicity (share of 'White')",
  "born_uk"               = "UKborn (share of 'Yes')",
  "uk_region"             = "Region (share of 'South East')",
  "income"                = "Income (share of '\\pounds20,000 - \\pounds29,999')",
  "employment_status"     = "Employment (share of 'Employed full time')",
  "education"             = "Education (share of 'University or higher')",
  "political_orientation" = "Political (share of 'Liberal')",
  "party_id"              = "Party (share of 'Labour')",
  "news_interest"         = "InterestNews (share of 'Interested')",
  "news_frequency"        = "ConsumptionNews (share of 'Every day')",
  "newsletter_subscribed" = "Newsletter (share of 'Yes')",
  "timing_last_click"     = "ArticleTime (seconds reading article)",
  "belief_image_ai"       = "Manipulation (share believe AI)",
  "recognition_correct"   = "OriginCorrect (share identify AI/Not)"
)

relabel <- function(dt) {
  for (v in names(var_labels)) {
    # match rows where Variable starts with the raw var name
    idx <- grepl(paste0("^", v), dt$Variable)
    dt[idx, Variable := var_labels[[v]]]
  }
  dt
}

pre_dt     <- relabel(pre_dt)
post_dt_b2 <- relabel(post_dt_b2)

# readable column names for printing
col_labels <- c("Control", "Control + Label", "Easy AI", "Easy AI + Label", "Hard AI", "Hard AI + Label")

fmt_row <- function(r, arm_cols) {
  vals <- sapply(arm_cols, function(a) {
    v <- r[[a]]
    if (is.na(v)) "---" else as.character(v)
  })
  paste(c(r$Variable, vals, sprintf("%.2f", as.numeric(r$Global_p)),
          sprintf("%.2f", as.numeric(r$Min_pairwise_p))),
        collapse = " & ")
}

# -- sanitize Variable column: escape LaTeX special chars in labels --
sanitize_label <- function(x) {
  x <- gsub("&",  "\\\\&",  x, fixed = TRUE)
  x <- gsub("%",  "\\\\%",  x, fixed = TRUE)
  x <- gsub("#",  "\\\\#",  x, fixed = TRUE)
  x <- gsub("_",  "\\\\_",  x, fixed = TRUE)
  x  # apostrophes and £ via \pounds are already correct
}
pre_dt[,     Variable := sanitize_label(Variable)]
post_dt_b2[, Variable := sanitize_label(Variable)]

# -- export as booktabs LaTeX --
header_cols <- paste(c("Variable", col_labels, "Global\\_p", "Min\\_pairwise\\_p"),
                     collapse = " & ")

tex_lines <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Balance across Treatments: Group Means, Global and Pairwise Tests}",
  "\\label{tab:balance_w2_b2}",
  "{\\small",
  "\\begin{tabular}{lcccccccc}",
  "\\toprule",
  paste0(header_cols, " \\\\"),
  "\\midrule"
)

for (i in seq_len(nrow(pre_dt))) {
  tex_lines <- c(tex_lines, paste0(fmt_row(pre_dt[i], arm_levels_b2), " \\\\"))
}

tex_lines <- c(
  tex_lines,
  "\\midrule",
  "\\multicolumn{9}{c}{\\textit{Post-treatment variables}} \\\\",
  "\\midrule"
)

for (i in seq_len(nrow(post_dt_b2))) {
  tex_lines <- c(tex_lines, paste0(fmt_row(post_dt_b2[i], arm_levels_b2), " \\\\"))
}

tex_lines <- c(
  tex_lines,
  "\\bottomrule",
  "\\end{tabular}",
  "}",
  paste0(
    "\\begin{minipage}{\\linewidth}\\vspace{4pt}\\footnotesize",
    "\\textbf{Notes:} This table reports the mean of each demographic and attitudinal characteristic ",
    "by treatment arm. For continuous variables, the reported $p$-values come from an ANOVA test of ",
    "equality of means across treatment groups. For categorical variables, $p$-values correspond to ",
    "Pearson $\\chi^2$ tests of independence. Categorical rows show the share of the modal category. ",
    "The column \\textit{Min pairwise p} shows the smallest Holm-adjusted $p$-value from pairwise ",
    "$t$- or proportion tests between any two treatment arms.",
    "\\end{minipage}"
  ),
  "\\end{table}"
)

writeLines(tex_lines, con = "tables/wave2/table_balance_wave2_b2style.tex")
cat("Exported tables/wave2/table_balance_wave2_b2style.tex\n")




# -----------------------------------------------------------------------------
# 4b. TABLE B.1 STYLE SUMMARY: WAVE 2 vs UK POPULATION
# -----------------------------------------------------------------------------

wave2_b1 <- data.table(
  Variable = c(
    "Gender (share female)",
    "Age (years)",
    "Ethnicity (share White)",
    "UK-born (share born in the UK)",
    "Region (share living in the South East)",
    "Income (share earning > £40K)",
    "Employment (share employed)",
    "Education (share with university degree)",
    "Party (share previously voting Labour)"
  ),
  Experiment = c(
    mean(dt$sex_birth == "Female", na.rm = TRUE),
    mean(dt$age, na.rm = TRUE),
    mean(dt$ethnicity == "White", na.rm = TRUE),
    mean(dt$born_uk == TRUE, na.rm = TRUE),
    mean(dt$uk_region == "South East", na.rm = TRUE),
    mean(dt$income %in% c(
      "£40,000 - £49,999",
      "£50,000 - £59,999",
      "£60,000 - £69,999",
      "£70,000 - £79,999",
      "£80,000 - £89,999",
      "£90,000 - £99,999",
      "More than £100,000"
    ), na.rm = TRUE),
    mean(dt$employment_status %in% c(
      "Employed full time",
      "Employed part time"
    ), na.rm = TRUE),
    mean(dt$education == "University or higher", na.rm = TRUE),
    mean(dt$party_id == "Labour", na.rm = TRUE)
  ),
  `UK Population` = c(
    0.511,
    40.7,
    0.830,
    0.840,
    0.156,
    0.250,
    0.751,
    0.338,
    0.337
  )
)

wave2_b1_display <- copy(wave2_b1)

wave2_b1_display[, Experiment := as.character(Experiment)]
wave2_b1_display[, `UK Population` := as.character(`UK Population`)]

pct_rows <- c(1, 3, 4, 5, 6, 7, 8, 9)

wave2_b1_display[pct_rows, Experiment := sprintf("%.1f%%", 100 * wave2_b1$Experiment[pct_rows])]
wave2_b1_display[pct_rows, `UK Population` := sprintf("%.1f%%", 100 * wave2_b1$`UK Population`[pct_rows])]

wave2_b1_display[2, Experiment := sprintf("%.1f", wave2_b1$Experiment[2])]
wave2_b1_display[2, `UK Population` := "40.7 (median)"]

wave2_b1_display

print(
  xtable(
    wave2_b1_display,
    caption = "Summary Statistics: Experiment vs. UK population",
    label = "tab:sumstats_wave2"
  ),
  include.rownames = FALSE,
  sanitize.text.function = identity
)

# -----------------------------------------------------------------------------
# 5. SUMMARY STATS: MAIN OUTCOMES BY TREATMENT
# -----------------------------------------------------------------------------####

summary_main_treat <- dt[, .(
  n = .N,
  newsletter_takeup_mean = mean(newsletter_takeup),
  belief_image_ai_mean   = mean(belief_image_ai)
), by = treatment][order(treatment)]

summary_main_treat

summary_main_treat[, `:=`(
  newsletter_takeup_mean = round(newsletter_takeup_mean, 3),
  belief_image_ai_mean   = round(belief_image_ai_mean, 3)
)]

summary_main_treat

summary_main_treat_wide <- dcast(
  melt(
    summary_main_treat,
    id.vars = "treatment",
    measure.vars = c("newsletter_takeup_mean", "belief_image_ai_mean"),
    variable.name = "outcome",
    value.name = "value"
  ),
  outcome ~ treatment,
  value.var = "value"
)

summary_main_treat_wide

# -----------------------------------------------------------------------------
# 6. SUMMARY STATS: MAIN OUTCOMES BY TREATMENT X IMAGE
# -----------------------------------------------------------------------------####

summary_main_treat_image <- dt[, .(
  n = .N,
  newsletter_takeup_mean = mean(newsletter_takeup),
  belief_image_ai_mean   = mean(belief_image_ai)
), by = .(treatment, image_id)][order(treatment, image_id)]

summary_main_treat_image

summary_main_treat_image[, `:=`(
  newsletter_takeup_mean = round(newsletter_takeup_mean, 3),
  belief_image_ai_mean   = round(belief_image_ai_mean, 3)
)]

summary_main_treat_image

summary_newsletter_treat_image <- dcast(
  summary_main_treat_image,
  image_id ~ treatment,
  value.var = "newsletter_takeup_mean"
)

summary_belief_treat_image <- dcast(
  summary_main_treat_image,
  image_id ~ treatment,
  value.var = "belief_image_ai_mean"
)

summary_newsletter_treat_image
summary_belief_treat_image

# -----------------------------------------------------------------------------
# 7. EXPORT SUMMARY TABLES TO LATEX
# -----------------------------------------------------------------------------####

print(
  xtable(summary_main_treat_wide,
         caption = "Main outcomes by treatment",
         label = "tab:main_outcomes_treat"),
  file = "tables/wave2/table_summary_outcomes_by_treatment.tex",
  include.rownames = FALSE,
  sanitize.text.function = identity,
  append = FALSE
)

print(
  xtable(summary_newsletter_treat_image,
         caption = "Newsletter take-up by treatment and image",
         label = "tab:newsletter_treat_image"),
  file = "tables/wave2/table_summary_newsletter_by_treatment_image.tex",
  include.rownames = FALSE,
  sanitize.text.function = identity,
  append = FALSE
)

print(
  xtable(summary_belief_treat_image,
         caption = "AI recognition by treatment and image",
         label = "tab:belief_treat_image"),
  file = "tables/wave2/table_summary_belief_by_treatment_image.tex",
  include.rownames = FALSE,
  sanitize.text.function = identity,
  append = FALSE
)

# -----------------------------------------------------------------------------
# 8. MAIN REGRESSIONS (QUICK FIRST PASS, treatment)
# -----------------------------------------------------------------------------####

# make sure control is the reference category
dt[, treatment := relevel(treatment, ref = "C")]

# 1. Newsletter take-up
m_newsletter <- feols(
  newsletter_takeup ~ i(treatment, ref = "C"),
  data = dt,
  vcov = "hetero"
)

# 2. AI recognition
m_ai_recognition <- feols(
  belief_image_ai ~ i(treatment, ref = "C"),
  data = dt,
  vcov = "hetero"
)

# 3. AI *correct* recognition
m_ai_correct_recognition <- feols(
  recognition_correct ~ i(treatment, ref = "C"),
  data = dt,
  vcov = "hetero"
)


etable(
  m_newsletter,
  m_ai_recognition,
  m_ai_correct_recognition,
  digits = 3,
  fitstat = ~ n + r2
)

etable(
  m_newsletter,
  m_ai_recognition,
  m_ai_correct_recognition,
  digits = 3,
  fitstat = ~ n + r2,
  tex = TRUE,
  file = "tables/wave2/table_main_quick_wave2.tex",
  replace = TRUE
)


# PERCEPTIONS
summary(dt[, .(
  expect_accuracy_num,
  expect_trustworthiness_num,
  expect_quality_num,
  expect_political_bias_num,
  expect_complexity_num,
  trust_researchers_num,
  expect_entertainment_num,
  z_index,
  not_accurate,
  not_trustworthy,
  low_quality,
  biased,
  not_entertaining,
  very_complex,
  no_researcher_trust
)])


m_accuracy <- feols(expect_accuracy_num ~ i(treatment, ref = "C"), data = dt, vcov = "hetero")
m_trustworthiness <- feols(expect_trustworthiness_num ~ i(treatment, ref = "C"), data = dt, vcov = "hetero")
m_quality <- feols(expect_quality_num ~ i(treatment, ref = "C"), data = dt, vcov = "hetero")
m_bias <- feols(expect_political_bias_num ~ i(treatment, ref = "C"), data = dt, vcov = "hetero")
m_complexity <- feols(expect_complexity_num ~ i(treatment, ref = "C"), data = dt, vcov = "hetero")
m_researchers <- feols(trust_researchers_num ~ i(treatment, ref = "C"), data = dt, vcov = "hetero")
m_entertainment <- feols(expect_entertainment_num ~ i(treatment, ref = "C"), data = dt, vcov = "hetero")
m_z_index <- feols(z_index ~ i(treatment, ref = "C"), data = dt, vcov = "hetero")

m_not_accurate <- feols(not_accurate ~ i(treatment, ref = "C"), data = dt, vcov = "hetero")
m_not_trustworthy <- feols(not_trustworthy ~ i(treatment, ref = "C"), data = dt, vcov = "hetero")
m_low_quality <- feols(low_quality ~ i(treatment, ref = "C"), data = dt, vcov = "hetero")
m_biased <- feols(biased ~ i(treatment, ref = "C"), data = dt, vcov = "hetero")
m_not_entertaining <- feols(not_entertaining ~ i(treatment, ref = "C"), data = dt, vcov = "hetero")
m_very_complex <- feols(very_complex ~ i(treatment, ref = "C"), data = dt, vcov = "hetero")
m_no_researcher_trust <- feols(no_researcher_trust ~ i(treatment, ref = "C"), data = dt, vcov = "hetero")

etable(
  m_z_index,
  m_accuracy,
  m_trustworthiness,
  m_quality,
  m_bias,
  m_complexity,
  m_entertainment,
  m_researchers,
  digits = 3,
  fitstat = ~ n + r2
)

etable(
  m_z_index,
  m_not_accurate,
  m_not_trustworthy,
  m_low_quality,
  m_biased,
  m_very_complex,
  m_not_entertaining,
  m_no_researcher_trust,
  digits = 3,
  fitstat = ~ n + r2
)

etable(
  m_z_index,
  m_accuracy,
  m_trustworthiness,
  m_quality,
  m_bias,
  m_complexity,
  m_entertainment,
  m_researchers,
  digits = 3,
  fitstat = ~ n + r2,
  tex = TRUE,
  file = "tables/wave2/table_quality_quick_wave2.tex",
  replace = TRUE
)

etable(
  m_z_index,
  m_not_accurate,
  m_not_trustworthy,
  m_low_quality,
  m_biased,
  m_very_complex,
  m_not_entertaining,
  m_no_researcher_trust,
  digits = 3,
  fitstat = ~ n + r2,
  tex = TRUE,
  file = "tables/wave2/table_quality2_quick_wave2.tex",
  replace = TRUE
)


# -----------------------------------------------------------------------------
# 8a. MAIN REGRESSIONS (TREATMENT components: easy_ai + hard_ai + label + easy_ai:label + hard_ai:label)
# -----------------------------------------------------------------------------####

# 1. Newsletter take-up
m_newsletter_c <- feols(
  newsletter_takeup ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label,
  data = dt,
  vcov = "hetero"
)

# 2. AI recognition
m_ai_recognition_c <- feols(
  belief_image_ai ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label,
  data = dt,
  vcov = "hetero"
)

# 3. AI *correct* recognition
m_ai_correct_recognition_c <- feols(
  recognition_correct ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label,
  data = dt,
  vcov = "hetero"
)


etable(
  m_newsletter_c,
  m_ai_recognition_c,
  m_ai_correct_recognition_c,
  digits = 3,
  fitstat = ~ n + r2
)

etable(
  m_newsletter_c,
  m_ai_recognition_c,
  m_ai_correct_recognition_c,
  digits = 3,
  fitstat = ~ n + r2,
  tex = TRUE,
  file = "tables/wave2/table_main_components_wave2.tex",
  replace = TRUE
)


# PERCEPTIONS
summary(dt[, .(
  expect_accuracy_num,
  expect_trustworthiness_num,
  expect_quality_num,
  expect_political_bias_num,
  expect_complexity_num,
  trust_researchers_num,
  expect_entertainment_num,
  z_index,
  not_accurate,
  not_trustworthy,
  low_quality,
  biased,
  not_entertaining,
  very_complex,
  no_researcher_trust
)])


m_accuracy_c <- feols(expect_accuracy_num ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label, data = dt, vcov = "hetero")
m_trustworthiness_c <- feols(expect_trustworthiness_num ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label, data = dt, vcov = "hetero")
m_quality_c <- feols(expect_quality_num ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label, data = dt, vcov = "hetero")
m_bias_c <- feols(expect_political_bias_num ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label, data = dt, vcov = "hetero")
m_complexity_c <- feols(expect_complexity_num ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label, data = dt, vcov = "hetero")
m_researchers_c <- feols(trust_researchers_num ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label, data = dt, vcov = "hetero")
m_entertainment_c <- feols(expect_entertainment_num ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label, data = dt, vcov = "hetero")
m_z_index_c <- feols(z_index ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label, data = dt, vcov = "hetero")

m_not_accurate_c <- feols(not_accurate ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label, data = dt, vcov = "hetero")
m_not_trustworthy_c <- feols(not_trustworthy ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label, data = dt, vcov = "hetero")
m_low_quality_c <- feols(low_quality ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label, data = dt, vcov = "hetero")
m_biased_c <- feols(biased ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label, data = dt, vcov = "hetero")
m_not_entertaining_c <- feols(not_entertaining ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label, data = dt, vcov = "hetero")
m_very_complex_c <- feols(very_complex ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label, data = dt, vcov = "hetero")
m_no_researcher_trust_c <- feols(no_researcher_trust ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label, data = dt, vcov = "hetero")

etable(
  m_z_index_c,
  m_accuracy_c,
  m_trustworthiness_c,
  m_quality_c,
  m_bias_c,
  m_complexity_c,
  m_entertainment_c,
  m_researchers_c,
  digits = 3,
  fitstat = ~ n + r2
)

etable(
  m_z_index_c,
  m_not_accurate_c,
  m_not_trustworthy_c,
  m_low_quality_c,
  m_biased_c,
  m_very_complex_c,
  m_not_entertaining_c,
  m_no_researcher_trust_c,
  digits = 3,
  fitstat = ~ n + r2
)

etable(
  m_z_index_c,
  m_accuracy_c,
  m_trustworthiness_c,
  m_quality_c,
  m_bias_c,
  m_complexity_c,
  m_entertainment_c,
  m_researchers_c,
  digits = 3,
  fitstat = ~ n + r2,
  tex = TRUE,
  file = "tables/wave2/table_quality_components_wave2.tex",
  replace = TRUE
)

etable(
  m_z_index_c,
  m_not_accurate_c,
  m_not_trustworthy_c,
  m_low_quality_c,
  m_biased_c,
  m_very_complex_c,
  m_not_entertaining_c,
  m_no_researcher_trust_c,
  digits = 3,
  fitstat = ~ n + r2,
  tex = TRUE,
  file = "tables/wave2/table_quality2_components_wave2.tex",
  replace = TRUE
)


# -----------------------------------------------------------------------------
# 8b. MAIN REGRESSIONS WITH IMAGE FIXED EFFECTS (treatment)
# -----------------------------------------------------------------------------

m_newsletter_fe <- feols(
  newsletter_takeup ~ i(treatment, ref = "C") | image_id,
  data = dt,
  vcov = "hetero"
)

m_ai_recognition_fe <- feols(
  belief_image_ai ~ i(treatment, ref = "C") | image_id,
  data = dt,
  vcov = "hetero"
)

m_ai_correct_recognition_fe <- feols(
  recognition_correct ~ i(treatment, ref = "C") | image_id,
  data = dt,
  vcov = "hetero"
)

etable(
  m_newsletter_fe,
  m_ai_recognition_fe,
  m_ai_correct_recognition_fe,
  digits = 3,
  fitstat = ~ n + r2
)

etable(
  m_newsletter_fe,
  m_ai_recognition_fe,
  digits = 3,
  fitstat = ~ n + r2,
  tex = TRUE,
  file = "tables/wave2/table_main_fe_wave2.tex",
  replace = TRUE
)

# PERCEPTIONS
m_accuracy_fe <- feols(expect_accuracy_num ~ i(treatment, ref = "C") | image_id, data = dt, vcov = "hetero")
m_trustworthiness_fe <- feols(expect_trustworthiness_num ~ i(treatment, ref = "C") | image_id, data = dt, vcov = "hetero")
m_quality_fe <- feols(expect_quality_num ~ i(treatment, ref = "C") | image_id, data = dt, vcov = "hetero")
m_bias_fe <- feols(expect_political_bias_num ~ i(treatment, ref = "C") | image_id, data = dt, vcov = "hetero")
m_complexity_fe <- feols(expect_complexity_num ~ i(treatment, ref = "C") | image_id, data = dt, vcov = "hetero")
m_researchers_fe <- feols(trust_researchers_num ~ i(treatment, ref = "C") | image_id, data = dt, vcov = "hetero")
m_entertainment_fe <- feols(expect_entertainment_num ~ i(treatment, ref = "C") | image_id, data = dt, vcov = "hetero")
m_z_index_fe <- feols(z_index ~ i(treatment, ref = "C") | image_id, data = dt, vcov = "hetero")

m_not_accurate_fe <- feols(not_accurate ~ i(treatment, ref = "C") | image_id, data = dt, vcov = "hetero")
m_not_trustworthy_fe <- feols(not_trustworthy ~ i(treatment, ref = "C") | image_id, data = dt, vcov = "hetero")
m_low_quality_fe <- feols(low_quality ~ i(treatment, ref = "C") | image_id, data = dt, vcov = "hetero")
m_biased_fe <- feols(biased ~ i(treatment, ref = "C") | image_id, data = dt, vcov = "hetero")
m_not_entertaining_fe <- feols(not_entertaining ~ i(treatment, ref = "C") | image_id, data = dt, vcov = "hetero")
m_very_complex_fe <- feols(very_complex ~ i(treatment, ref = "C") | image_id, data = dt, vcov = "hetero")
m_no_researcher_trust_fe <- feols(no_researcher_trust ~ i(treatment, ref = "C") | image_id, data = dt, vcov = "hetero")

etable(
  m_z_index_fe,
  m_accuracy_fe,
  m_trustworthiness_fe,
  m_quality_fe,
  m_bias_fe,
  m_complexity_fe,
  m_entertainment_fe,
  m_researchers_fe,
  digits = 3,
  fitstat = ~ n + r2
)

etable(
  m_z_index_fe,
  m_not_accurate_fe,
  m_not_trustworthy_fe,
  m_low_quality_fe,
  m_biased_fe,
  m_very_complex_fe,
  m_not_entertaining_fe,
  m_no_researcher_trust_fe,
  digits = 3,
  fitstat = ~ n + r2
)

etable(
  m_z_index_fe,
  m_accuracy_fe,
  m_trustworthiness_fe,
  m_quality_fe,
  m_bias_fe,
  m_complexity_fe,
  m_entertainment_fe,
  m_researchers_fe,
  digits = 3,
  fitstat = ~ n + r2,
  tex = TRUE,
  file = "tables/wave2/table_quality_fe_wave2.tex",
  replace = TRUE
)

etable(
  m_z_index_fe,
  m_not_accurate_fe,
  m_not_trustworthy_fe,
  m_low_quality_fe,
  m_biased_fe,
  m_very_complex_fe,
  m_not_entertaining_fe,
  m_no_researcher_trust_fe,
  digits = 3,
  fitstat = ~ n + r2,
  tex = TRUE,
  file = "tables/wave2/table_quality2_fe_wave2.tex",
  replace = TRUE
)


# -----------------------------------------------------------------------------
# 8c. MAIN REGRESSIONS WITH IMAGE FIXED EFFECTS (treatment components)
# -----------------------------------------------------------------------------####

m_newsletter_fe_c <- feols(
  newsletter_takeup ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label | image_id,
  data = dt,
  vcov = "hetero"
)

m_ai_recognition_fe_c <- feols(
  belief_image_ai ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label | image_id,
  data = dt,
  vcov = "hetero"
)

m_ai_correct_recognition_fe_c <- feols(
  recognition_correct ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label | image_id,
  data = dt,
  vcov = "hetero"
)

etable(
  m_newsletter_fe_c,
  m_ai_recognition_fe_c,
  m_ai_correct_recognition_fe_c,
  digits = 3,
  fitstat = ~ n + r2
)

etable(
  m_newsletter_fe_c,
  m_ai_recognition_fe_c,
  digits = 3,
  fitstat = ~ n + r2,
  tex = TRUE,
  file = "tables/wave2/table_main_components_fe_wave2.tex",
  replace = TRUE
)

# PERCEPTIONS
m_accuracy_fe_c <- feols(expect_accuracy_num ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label | image_id, data = dt, vcov = "hetero")
m_trustworthiness_fe_c <- feols(expect_trustworthiness_num ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label | image_id, data = dt, vcov = "hetero")
m_quality_fe_c <- feols(expect_quality_num ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label | image_id, data = dt, vcov = "hetero")
m_bias_fe_c <- feols(expect_political_bias_num ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label | image_id, data = dt, vcov = "hetero")
m_complexity_fe_c <- feols(expect_complexity_num ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label | image_id, data = dt, vcov = "hetero")
m_researchers_fe_c <- feols(trust_researchers_num ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label | image_id, data = dt, vcov = "hetero")
m_entertainment_fe_c <- feols(expect_entertainment_num ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label | image_id, data = dt, vcov = "hetero")
m_z_index_fe_c <- feols(z_index ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label | image_id, data = dt, vcov = "hetero")

m_not_accurate_fe_c <- feols(not_accurate ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label | image_id, data = dt, vcov = "hetero")
m_not_trustworthy_fe_c <- feols(not_trustworthy ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label | image_id, data = dt, vcov = "hetero")
m_low_quality_fe_c <- feols(low_quality ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label | image_id, data = dt, vcov = "hetero")
m_biased_fe_c <- feols(biased ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label | image_id, data = dt, vcov = "hetero")
m_not_entertaining_fe_c <- feols(not_entertaining ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label | image_id, data = dt, vcov = "hetero")
m_very_complex_fe_c <- feols(very_complex ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label | image_id, data = dt, vcov = "hetero")
m_no_researcher_trust_fe_c <- feols(no_researcher_trust ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label | image_id, data = dt, vcov = "hetero")

etable(
  m_z_index_fe_c,
  m_accuracy_fe_c,
  m_trustworthiness_fe_c,
  m_quality_fe_c,
  m_bias_fe_c,
  m_complexity_fe_c,
  m_entertainment_fe_c,
  m_researchers_fe_c,
  digits = 3,
  fitstat = ~ n + r2
)

etable(
  m_z_index_fe_c,
  m_not_accurate_fe_c,
  m_not_trustworthy_fe_c,
  m_low_quality_fe_c,
  m_biased_fe_c,
  m_very_complex_fe_c,
  m_not_entertaining_fe_c,
  m_no_researcher_trust_fe_c,
  digits = 3,
  fitstat = ~ n + r2
)

etable(
  m_z_index_fe_c,
  m_accuracy_fe_c,
  m_trustworthiness_fe_c,
  m_quality_fe_c,
  m_bias_fe_c,
  m_complexity_fe_c,
  m_entertainment_fe_c,
  m_researchers_fe_c,
  digits = 3,
  fitstat = ~ n + r2,
  tex = TRUE,
  file = "tables/wave2/table_quality_components_fe_wave2.tex",
  replace = TRUE
)

etable(
  m_z_index_fe_c,
  m_not_accurate_fe_c,
  m_not_trustworthy_fe_c,
  m_low_quality_fe_c,
  m_biased_fe_c,
  m_very_complex_fe_c,
  m_not_entertaining_fe_c,
  m_no_researcher_trust_fe_c,
  digits = 3,
  fitstat = ~ n + r2,
  tex = TRUE,
  file = "tables/wave2/table_quality2_components_fe_wave2.tex",
  replace = TRUE
)

# -----------------------------------------------------------------------------
# 8d. EXPORT THE 4 MAIN REGRESSIONS TO ONE LATEX TABLE
# -----------------------------------------------------------------------------####

etable(
  m_newsletter,
  m_ai_recognition,
  m_ai_correct_recognition,
  m_newsletter_c,
  m_ai_recognition_c,
  m_ai_correct_recognition_c,
  m_newsletter_fe,
  m_ai_recognition_fe,
  m_ai_correct_recognition_fe,
  m_newsletter_fe_c,
  m_ai_recognition_fe_c,
  m_ai_correct_recognition_fe_c,
  digits = 3,
  fitstat = ~ n + r2,
  tex = TRUE,
  file = "tables/wave2/table_main_nocomp_comp_nofe_fe_wave2.tex",
  replace = TRUE
)

# -----------------------------------------------------------------------------
# 8e. TABLE B.12: Manipulation check (belief_image_ai) and OriginCorrect
#     Cols 1-4: DV = belief_image_ai   (Manipulation: AI = Yes)
#     Cols 5-8: DV = recognition_correct (OriginCorrect = Yes)
#     Within each block:
#       (odd)  treatment dummies, no controls, image FE
#       (even) treatment dummies, controls, image FE
#     First pair: i(treatment) saturated; second pair: easy_ai/hard_ai interactions
# -----------------------------------------------------------------------------

ctrl_vars <- "age + sex_birth + education + income + political_orientation + newsletter_subscribed"

# -- belief_image_ai (Manipulation) --
# (1) saturated treatment dummies, image FE, no controls
m_b12_manip_1 <- feols(
  belief_image_ai ~ i(treatment, ref = "C") | image_id,
  data = dt, vcov = "hetero"
)
# (2) saturated + controls + image FE
m_b12_manip_2 <- feols(
  belief_image_ai ~ i(treatment, ref = "C") +
    age + sex_birth + education + income + political_orientation + newsletter_subscribed + ai_familiarity_num | image_id,
  data = dt, vcov = "hetero"
)
# (3) interaction coding, image FE, no controls
m_b12_manip_3 <- feols(
  belief_image_ai ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label | image_id,
  data = dt, vcov = "hetero"
)
# (4) interaction coding + controls + image FE
m_b12_manip_4 <- feols(
  belief_image_ai ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label +
    age + sex_birth + education + income + political_orientation + newsletter_subscribed + ai_familiarity_num | image_id,
  data = dt, vcov = "hetero"
)

# -- recognition_correct (OriginCorrect) --
# (5) saturated treatment dummies, image FE, no controls
m_b12_origin_5 <- feols(
  recognition_correct ~ i(treatment, ref = "C") | image_id,
  data = dt, vcov = "hetero"
)
# (6) saturated + controls + image FE
m_b12_origin_6 <- feols(
  recognition_correct ~ i(treatment, ref = "C") +
    age + sex_birth + education + income + political_orientation + newsletter_subscribed + ai_familiarity_num | image_id,
  data = dt, vcov = "hetero"
)
# (7) interaction coding, image FE, no controls
m_b12_origin_7 <- feols(
  recognition_correct ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label | image_id,
  data = dt, vcov = "hetero"
)
# (8) interaction coding + controls + image FE
m_b12_origin_8 <- feols(
  recognition_correct ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label +
    age + sex_birth + education + income + political_orientation + newsletter_subscribed + ai_familiarity_num | image_id,
  data = dt, vcov = "hetero"
)

# -- readable coefficient labels (wave-1 style) --
b12_dict <- c(
  "treatment::LC"           = "Label Control",
  "treatment::EAI"          = "Easy AI",
  "treatment::LEAI"         = "Easy AI + Label",
  "treatment::HAI"          = "Hard AI",
  "treatment::LHAI"         = "Hard AI + Label",
  "easy_aiTRUE"             = "Easy AI",
  "hard_aiTRUE"             = "Hard AI",
  "labelTRUE"               = "Label",
  "easy_aiTRUE:labelTRUE"   = "Easy AI $\\times$ Label",
  "hard_aiTRUE:labelTRUE"   = "Hard AI $\\times$ Label"
)

# -- mean DV for extralines --
mean_manip  <- round(mean(as.numeric(dt$belief_image_ai),    na.rm = TRUE), 3)
mean_origin <- round(mean(as.numeric(dt$recognition_correct), na.rm = TRUE), 3)

b12_extralines <- list(
  "Image FE"   = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
  "Controls"   = c("No",  "Yes", "No",  "Yes", "No",  "Yes", "No",  "Yes"),
  "Mean DV"    = c(rep(mean_manip, 4), rep(mean_origin, 4))
)

# write without extralines first — we post-process to reorder sections
etable(
  m_b12_manip_1, m_b12_manip_2, m_b12_manip_3, m_b12_manip_4,
  m_b12_origin_5, m_b12_origin_6, m_b12_origin_7, m_b12_origin_8,
  digits = 3,
  fitstat = ~ pr2 + n,
  digits.stats = 3,
  dict = b12_dict,
  drop = c("age", "sex_birth", "education", "income",
           "political_orientation", "newsletter_subscribed", "ai_familiarity_num"),
  headers = list(
    "Manipulation ($AI = Yes$)" = 4,
    "OriginCorrect ($= Yes$)"   = 4
  ),
  tex = TRUE,
  file = "tables/wave2/table_main_nocomp_comp_nofe_fe_wave2_v2.tex",
  replace = TRUE
)

# -- post-process: remove Fixed-effects block, append Image FE / Controls / Mean DV after Fit stats --
b12_tex_path <- "tables/wave2/table_main_nocomp_comp_nofe_fe_wave2_v2.tex"
b12_lines <- readLines(b12_tex_path)

# 1. Drop the entire Fixed-effects block (the \emph{Fixed-effects}\\ line,
#    the image_id row, and the \midrule that follows it)
fe_start <- grep("\\\\emph\\{Fixed-effects\\}", b12_lines)
if (length(fe_start) > 0) {
  # find the next \midrule after the FE header
  fe_midrule <- fe_start + which(grepl("\\\\midrule", b12_lines[(fe_start+1):length(b12_lines)]))[1]
  b12_lines <- b12_lines[-c(fe_start:fe_midrule)]
}

# 2. Build the three footer rows using the actual column count (8 models)
nc <- 8
yes_no_controls <- c("No", "Yes", "No", "Yes", "No", "Yes", "No", "Yes")
footer <- c(
  "\\midrule",
  paste0("   Image FE & ",
         paste(rep("Yes", nc), collapse = " & "), "\\\\"),
  paste0("   Controls & ",
         paste(yes_no_controls, collapse = " & "), "\\\\"),
  paste0("   Mean DV & ",
         paste(c(rep(mean_manip, 4), rep(mean_origin, 4)), collapse = " & "), "\\\\")
)

# 3. Insert footer rows just before the final \midrule\midrule line
end_midrule <- tail(grep("\\\\midrule \\\\midrule", b12_lines), 1)
if (length(end_midrule) == 0) end_midrule <- tail(grep("\\\\midrule", b12_lines), 1)
b12_lines <- c(
  b12_lines[1:(end_midrule - 1)],
  footer,
  b12_lines[end_midrule:length(b12_lines)]
)

writeLines(b12_lines, b12_tex_path)
cat("Post-processed", b12_tex_path, "\n")


# -----------------------------------------------------------------------------
# 8f. TABLE B.12 LOGIT VERSION: same structure as 8e but feglm(family = binomial)
#     Note: feglm with logit + image FE uses the Mundlak/within-group correction
#     — incidental parameters are handled by fixest's bias correction
# -----------------------------------------------------------------------------

# -- belief_image_ai (Manipulation) --
m_b12l_manip_1 <- feglm(
  belief_image_ai ~ i(treatment, ref = "C") | image_id,
  data = dt, family = binomial, vcov = "hetero"
)
m_b12l_manip_2 <- feglm(
  belief_image_ai ~ i(treatment, ref = "C") +
    age + sex_birth + education + income + political_orientation + newsletter_subscribed + ai_familiarity_num | image_id,
  data = dt, family = binomial, vcov = "hetero"
)
m_b12l_manip_3 <- feglm(
  belief_image_ai ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label | image_id,
  data = dt, family = binomial, vcov = "hetero"
)
m_b12l_manip_4 <- feglm(
  belief_image_ai ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label +
    age + sex_birth + education + income + political_orientation + newsletter_subscribed + ai_familiarity_num | image_id,
  data = dt, family = binomial, vcov = "hetero"
)

# -- recognition_correct (OriginCorrect) --
m_b12l_origin_5 <- feglm(
  recognition_correct ~ i(treatment, ref = "C") | image_id,
  data = dt, family = binomial, vcov = "hetero"
)
m_b12l_origin_6 <- feglm(
  recognition_correct ~ i(treatment, ref = "C") +
    age + sex_birth + education + income + political_orientation + newsletter_subscribed + ai_familiarity_num | image_id,
  data = dt, family = binomial, vcov = "hetero"
)
m_b12l_origin_7 <- feglm(
  recognition_correct ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label | image_id,
  data = dt, family = binomial, vcov = "hetero"
)
m_b12l_origin_8 <- feglm(
  recognition_correct ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label +
    age + sex_birth + education + income + political_orientation + newsletter_subscribed + ai_familiarity_num | image_id,
  data = dt, family = binomial, vcov = "hetero"
)

b12l_tex_path <- "tables/wave2/table_main_nocomp_comp_nofe_fe_wave2_logit.tex"

etable(
  m_b12l_manip_1, m_b12l_manip_2, m_b12l_manip_3, m_b12l_manip_4,
  m_b12l_origin_5, m_b12l_origin_6, m_b12l_origin_7, m_b12l_origin_8,
  digits = 3,
  fitstat = ~ pr2 + n,
  digits.stats = 3,
  dict = b12_dict,   # same labels as 8e
  drop = c("age", "sex_birth", "education", "income",
           "political_orientation", "newsletter_subscribed", "ai_familiarity_num"),
  headers = list(
    "Manipulation ($AI = Yes$)" = 4,
    "OriginCorrect ($= Yes$)"   = 4
  ),
  tex = TRUE,
  file = b12l_tex_path,
  replace = TRUE
)

# -- same post-processing as 8e --
b12l_lines <- readLines(b12l_tex_path)

fe_start <- grep("\\\\emph\\{Fixed-effects\\}", b12l_lines)
if (length(fe_start) > 0) {
  fe_midrule <- fe_start + which(grepl("\\\\midrule", b12l_lines[(fe_start+1):length(b12l_lines)]))[1]
  b12l_lines <- b12l_lines[-c(fe_start:fe_midrule)]
}

footer <- c(
  "\\midrule",
  paste0("   Image FE & ", paste(rep("Yes", 8), collapse = " & "), "\\\\"),
  paste0("   Controls & ", paste(c("No","Yes","No","Yes","No","Yes","No","Yes"), collapse = " & "), "\\\\"),
  paste0("   Mean DV & ",  paste(c(rep(mean_manip, 4), rep(mean_origin, 4)), collapse = " & "), "\\\\")
)

end_midrule <- tail(grep("\\\\midrule \\\\midrule", b12l_lines), 1)
if (length(end_midrule) == 0) end_midrule <- tail(grep("\\\\midrule", b12l_lines), 1)
b12l_lines <- c(
  b12l_lines[1:(end_midrule - 1)],
  footer,
  b12l_lines[end_midrule:length(b12l_lines)]
)

writeLines(b12l_lines, b12l_tex_path)
cat("Post-processed", b12l_tex_path, "\n")


# -----------------------------------------------------------------------------
# 9. DEMAND & QUALITY ON TREATMENT × BELIEF IMAGE IS AI or RECOGNIZE IMAGE IS AI
# -----------------------------------------------------------------------------####

# Make sure Control is the omitted category
dt[, treatment := relevel(treatment, ref = "C")]

# Run regression
m_demand_belief_fe <- feols(
  newsletter_takeup ~ treatment * belief_image_ai | image_id,
  data = dt,
  vcov = "hetero"
)

m_demand_recognition_correct_fe <- feols(
  newsletter_takeup ~ treatment * recognition_correct | image_id,
  data = dt,
  vcov = "hetero"
)

# Show table in R first
etable(
  m_demand_belief_fe,
  m_demand_recognition_correct_fe,
  digits = 3,
  fitstat = ~ n + r2
)

etable(
 m_demand_belief_fe,
 m_demand_recognition_correct_fe,
 digits = 3,
 fitstat = ~ n + r2,
 tex = TRUE,
 file = "tables/wave2/table_demand_treatment_x_beliefAI_or_recognizeAI.tex",
 replace = TRUE
 )


# PERCEPTIONS (X belief)
m_accuracy_fe <- feols(expect_accuracy_num ~ treatment * belief_image_ai | image_id, data = dt, vcov = "hetero")
m_trustworthiness_fe <- feols(expect_trustworthiness_num ~ treatment * belief_image_ai | image_id, data = dt, vcov = "hetero")
m_quality_fe <- feols(expect_quality_num ~ treatment * belief_image_ai | image_id, data = dt, vcov = "hetero")
m_bias_fe <- feols(expect_political_bias_num ~ treatment * belief_image_ai | image_id, data = dt, vcov = "hetero")
m_complexity_fe <- feols(expect_complexity_num ~ treatment * belief_image_ai | image_id, data = dt, vcov = "hetero")
m_researchers_fe <- feols(trust_researchers_num ~ treatment * belief_image_ai | image_id, data = dt, vcov = "hetero")
m_entertainment_fe <- feols(expect_entertainment_num ~ treatment * belief_image_ai | image_id, data = dt, vcov = "hetero")
m_z_index_fe <- feols(z_index ~ treatment * belief_image_ai | image_id, data = dt, vcov = "hetero")

m_not_accurate_fe <- feols(not_accurate ~ treatment * belief_image_ai | image_id, data = dt, vcov = "hetero")
m_not_trustworthy_fe <- feols(not_trustworthy ~ treatment * belief_image_ai | image_id, data = dt, vcov = "hetero")
m_low_quality_fe <- feols(low_quality ~ treatment * belief_image_ai | image_id, data = dt, vcov = "hetero")
m_biased_fe <- feols(biased ~ treatment * belief_image_ai | image_id, data = dt, vcov = "hetero")
m_not_entertaining_fe <- feols(not_entertaining ~ treatment * belief_image_ai | image_id, data = dt, vcov = "hetero")
m_very_complex_fe <- feols(very_complex ~ treatment * belief_image_ai | image_id, data = dt, vcov = "hetero")
m_no_researcher_trust_fe <- feols(no_researcher_trust ~ treatment * belief_image_ai | image_id, data = dt, vcov = "hetero")

etable(
  m_z_index_fe,
  m_accuracy_fe,
  m_trustworthiness_fe,
  m_quality_fe,
  m_bias_fe,
  m_complexity_fe,
  m_entertainment_fe,
  m_researchers_fe,
  digits = 3,
  fitstat = ~ n + r2
)

etable(
  m_z_index_fe,
  m_not_accurate_fe,
  m_not_trustworthy_fe,
  m_low_quality_fe,
  m_biased_fe,
  m_very_complex_fe,
  m_not_entertaining_fe,
  m_no_researcher_trust_fe,
  digits = 3,
  fitstat = ~ n + r2
)

etable(
  m_z_index_fe,
  m_accuracy_fe,
  m_trustworthiness_fe,
  m_quality_fe,
  m_bias_fe,
  m_complexity_fe,
  m_entertainment_fe,
  m_researchers_fe,
  digits = 3,
  fitstat = ~ n + r2,
  tex = TRUE,
  file = "tables/wave2/table_quality_treatment_x_beliefAI.tex",
  replace = TRUE
)

etable(
  m_z_index_fe,
  m_not_accurate_fe,
  m_not_trustworthy_fe,
  m_low_quality_fe,
  m_biased_fe,
  m_very_complex_fe,
  m_not_entertaining_fe,
  m_no_researcher_trust_fe,
  digits = 3,
  fitstat = ~ n + r2,
  tex = TRUE,
  file = "tables/wave2/table_quality2_treatment_x_beliefAI.tex",
  replace = TRUE
)



# PERCEPTIONS (X recognize)
m_accuracy_fe <- feols(expect_accuracy_num ~ treatment * recognition_correct | image_id, data = dt, vcov = "hetero")
m_trustworthiness_fe <- feols(expect_trustworthiness_num ~ treatment * recognition_correct | image_id, data = dt, vcov = "hetero")
m_quality_fe <- feols(expect_quality_num ~ treatment * recognition_correct | image_id, data = dt, vcov = "hetero")
m_bias_fe <- feols(expect_political_bias_num ~ treatment * recognition_correct | image_id, data = dt, vcov = "hetero")
m_complexity_fe <- feols(expect_complexity_num ~ treatment * recognition_correct | image_id, data = dt, vcov = "hetero")
m_researchers_fe <- feols(trust_researchers_num ~ treatment * recognition_correct | image_id, data = dt, vcov = "hetero")
m_entertainment_fe <- feols(expect_entertainment_num ~ treatment * recognition_correct | image_id, data = dt, vcov = "hetero")
m_z_index_fe <- feols(z_index ~ treatment * recognition_correct | image_id, data = dt, vcov = "hetero")

m_not_accurate_fe <- feols(not_accurate ~ treatment * recognition_correct | image_id, data = dt, vcov = "hetero")
m_not_trustworthy_fe <- feols(not_trustworthy ~ treatment * recognition_correct | image_id, data = dt, vcov = "hetero")
m_low_quality_fe <- feols(low_quality ~ treatment * recognition_correct | image_id, data = dt, vcov = "hetero")
m_biased_fe <- feols(biased ~ treatment * recognition_correct | image_id, data = dt, vcov = "hetero")
m_not_entertaining_fe <- feols(not_entertaining ~ treatment * recognition_correct | image_id, data = dt, vcov = "hetero")
m_very_complex_fe <- feols(very_complex ~ treatment * recognition_correct | image_id, data = dt, vcov = "hetero")
m_no_researcher_trust_fe <- feols(no_researcher_trust ~ treatment * recognition_correct | image_id, data = dt, vcov = "hetero")

etable(
  m_z_index_fe,
  m_accuracy_fe,
  m_trustworthiness_fe,
  m_quality_fe,
  m_bias_fe,
  m_complexity_fe,
  m_entertainment_fe,
  m_researchers_fe,
  digits = 3,
  fitstat = ~ n + r2
)

etable(
  m_z_index_fe,
  m_not_accurate_fe,
  m_not_trustworthy_fe,
  m_low_quality_fe,
  m_biased_fe,
  m_very_complex_fe,
  m_not_entertaining_fe,
  m_no_researcher_trust_fe,
  digits = 3,
  fitstat = ~ n + r2
)

etable(
  m_z_index_fe,
  m_accuracy_fe,
  m_trustworthiness_fe,
  m_quality_fe,
  m_bias_fe,
  m_complexity_fe,
  m_entertainment_fe,
  m_researchers_fe,
  digits = 3,
  fitstat = ~ n + r2,
  tex = TRUE,
  file = "tables/wave2/table_quality_treatment_x_recognizeAI.tex",
  replace = TRUE
)

etable(
  m_z_index_fe,
  m_not_accurate_fe,
  m_not_trustworthy_fe,
  m_low_quality_fe,
  m_biased_fe,
  m_very_complex_fe,
  m_not_entertaining_fe,
  m_no_researcher_trust_fe,
  digits = 3,
  fitstat = ~ n + r2,
  tex = TRUE,
  file = "tables/wave2/table_quality2_treatment_x_recognizeAI.tex",
  replace = TRUE
)


# -----------------------------------------------------------------------------
# 9b. TABLE B.14: Demand (Newsletter = Yes)
#     Cols  1- 4: treatment dummies only (OLS / Logit, w/o and w/ controls)
#     Cols  5- 8: treatment × Manipulation (belief_image_ai)
#     Cols  9-12: treatment × OriginCorrect (recognition_correct)
#     All models include image FE
# -----------------------------------------------------------------------------

mean_demand <- round(mean(dt$newsletter_takeup, na.rm = TRUE), 3)

ctrl <- "age + sex_birth + education + income + political_orientation + newsletter_subscribed + ai_familiarity_num"

# -- cols 1-4: treatment dummies --
m_b13_1 <- feols(
  newsletter_takeup ~ treatment | image_id,
  data = dt, vcov = "hetero"
)
m_b13_2 <- feglm(
  newsletter_takeup ~ treatment | image_id,
  data = dt, family = binomial, vcov = "hetero"
)
m_b13_3 <- feols(
  newsletter_takeup ~ treatment +
    age + sex_birth + education + income + political_orientation + newsletter_subscribed + ai_familiarity_num | image_id,
  data = dt, vcov = "hetero"
)
m_b13_4 <- feglm(
  newsletter_takeup ~ treatment +
    age + sex_birth + education + income + political_orientation + newsletter_subscribed + ai_familiarity_num | image_id,
  data = dt, family = binomial, vcov = "hetero"
)

# -- cols 5-8: treatment × Manipulation (belief_image_ai) --
m_b13_5 <- feols(
  newsletter_takeup ~ treatment * belief_image_ai | image_id,
  data = dt, vcov = "hetero"
)
m_b13_6 <- feglm(
  newsletter_takeup ~ treatment * belief_image_ai | image_id,
  data = dt, family = binomial, vcov = "hetero"
)
m_b13_7 <- feols(
  newsletter_takeup ~ treatment * belief_image_ai +
    age + sex_birth + education + income + political_orientation + newsletter_subscribed + ai_familiarity_num | image_id,
  data = dt, vcov = "hetero"
)
m_b13_8 <- feglm(
  newsletter_takeup ~ treatment * belief_image_ai +
    age + sex_birth + education + income + political_orientation + newsletter_subscribed + ai_familiarity_num | image_id,
  data = dt, family = binomial, vcov = "hetero"
)

# -- cols 9-12: treatment × OriginCorrect (recognition_correct) --
m_b13_9 <- feols(
  newsletter_takeup ~ treatment * recognition_correct | image_id,
  data = dt, vcov = "hetero"
)
m_b13_10 <- feglm(
  newsletter_takeup ~ treatment * recognition_correct | image_id,
  data = dt, family = binomial, vcov = "hetero"
)
m_b13_11 <- feols(
  newsletter_takeup ~ treatment * recognition_correct +
    age + sex_birth + education + income + political_orientation + newsletter_subscribed + ai_familiarity_num | image_id,
  data = dt, vcov = "hetero"
)
m_b13_12 <- feglm(
  newsletter_takeup ~ treatment * recognition_correct +
    age + sex_birth + education + income + political_orientation + newsletter_subscribed + ai_familiarity_num | image_id,
  data = dt, family = binomial, vcov = "hetero"
)

# -- coefficient labels --
b13_dict <- c(
  "treatmentLC"                            = "Label Control",
  "treatmentEAI"                           = "Easy AI",
  "treatmentLEAI"                          = "Easy AI + Label",
  "treatmentHAI"                           = "Hard AI",
  "treatmentLHAI"                          = "Hard AI + Label",
  "belief_image_aiTRUE"                      = "Manipulation",
  "treatmentLC:belief_image_aiTRUE"          = "Label Control $\\times$ Manipulation",
  "treatmentEAI:belief_image_aiTRUE"         = "Easy AI $\\times$ Manipulation",
  "treatmentLEAI:belief_image_aiTRUE"        = "Easy AI + Label $\\times$ Manipulation",
  "treatmentHAI:belief_image_aiTRUE"         = "Hard AI $\\times$ Manipulation",
  "treatmentLHAI:belief_image_aiTRUE"        = "Hard AI + Label $\\times$ Manipulation",
  "recognition_correctTRUE"                 = "OriginCorrect",
  "treatmentLC:recognition_correctTRUE"      = "Label Control $\\times$ OriginCorrect",
  "treatmentEAI:recognition_correctTRUE"     = "Easy AI $\\times$ OriginCorrect",
  "treatmentLEAI:recognition_correctTRUE"    = "Easy AI + Label $\\times$ OriginCorrect",
  "treatmentHAI:recognition_correctTRUE"     = "Hard AI $\\times$ OriginCorrect",
  "treatmentLHAI:recognition_correctTRUE"    = "Hard AI + Label $\\times$ OriginCorrect"
)

b13_tex_path <- "tables/wave2/table_demand_treatment_x_beliefAI_or_recognizeAI_v2.tex"

etable(
  m_b13_1,  m_b13_2,  m_b13_3,  m_b13_4,
  m_b13_5,  m_b13_6,  m_b13_7,  m_b13_8,
  m_b13_9,  m_b13_10, m_b13_11, m_b13_12,
  digits = 3,
  fitstat = ~ pr2 + n,
  digits.stats = 3,
  dict = b13_dict,
  drop = c("age", "sex_birth", "education", "income",
           "political_orientation", "newsletter_subscribed", "ai_familiarity_num"),
  headers = list(
    "Demand ($Newsletter = Yes$)" = 12
  ),
  tex = TRUE,
  file = b13_tex_path,
  replace = TRUE
)

# -- post-process: remove FE block, add footer rows after Fit statistics --
b13_lines <- readLines(b13_tex_path)

fe_start <- grep("\\\\emph\\{Fixed-effects\\}", b13_lines)
if (length(fe_start) > 0) {
  fe_midrule <- fe_start + which(grepl("\\\\midrule", b13_lines[(fe_start+1):length(b13_lines)]))[1]
  b13_lines <- b13_lines[-c(fe_start:fe_midrule)]
}

controls_row <- c("No","Logit","Yes","Logit","No","Logit","Yes","Logit","No","Logit","Yes","Logit")
footer13 <- c(
  "\\midrule",
  paste0("   Image FE & ",  paste(rep("Yes", 12), collapse = " & "), "\\\\"),
  paste0("   Controls & ",  paste(c("No","No","Yes","Yes","No","No","Yes","Yes","No","No","Yes","Yes"), collapse = " & "), "\\\\"),
  paste0("   Estimator & ", paste(rep(c("OLS","Logit"), 6), collapse = " & "), "\\\\"),
  paste0("   Mean DV & ",   paste(rep(mean_demand, 12), collapse = " & "), "\\\\")
)

end_midrule <- tail(grep("\\\\midrule \\\\midrule", b13_lines), 1)
if (length(end_midrule) == 0) end_midrule <- tail(grep("\\\\midrule", b13_lines), 1)
b13_lines <- c(
  b13_lines[1:(end_midrule - 1)],
  footer13,
  b13_lines[end_midrule:length(b13_lines)]
)

writeLines(b13_lines, b13_tex_path)
cat("Post-processed", b13_tex_path, "\n")


# -----------------------------------------------------------------------------
# 9c. mechanism v2 versions: same models + controls, readable labels, pr2, mean DV footer
# -----------------------------------------------------------------------------

ctrl_q <- "age + sex_birth + education + income + political_orientation + newsletter_subscribed + ai_familiarity_num"

m_not_accurate_fe_c    <- feols(not_accurate      ~ treatment * recognition_correct + age + sex_birth + education + income + political_orientation + newsletter_subscribed + ai_familiarity_num | image_id, data = dt, vcov = "hetero")
m_not_trustworthy_fe_c <- feols(not_trustworthy   ~ treatment * recognition_correct + age + sex_birth + education + income + political_orientation + newsletter_subscribed + ai_familiarity_num | image_id, data = dt, vcov = "hetero")
m_low_quality_fe_c     <- feols(low_quality       ~ treatment * recognition_correct + age + sex_birth + education + income + political_orientation + newsletter_subscribed + ai_familiarity_num | image_id, data = dt, vcov = "hetero")
m_biased_fe_c          <- feols(biased            ~ treatment * recognition_correct + age + sex_birth + education + income + political_orientation + newsletter_subscribed + ai_familiarity_num | image_id, data = dt, vcov = "hetero")
m_very_complex_fe_c    <- feols(very_complex      ~ treatment * recognition_correct + age + sex_birth + education + income + political_orientation + newsletter_subscribed + ai_familiarity_num | image_id, data = dt, vcov = "hetero")
m_not_entertaining_fe_c<- feols(not_entertaining  ~ treatment * recognition_correct + age + sex_birth + education + income + political_orientation + newsletter_subscribed + ai_familiarity_num | image_id, data = dt, vcov = "hetero")
m_no_researcher_trust_fe_c <- feols(no_researcher_trust ~ treatment * recognition_correct + age + sex_birth + education + income + political_orientation + newsletter_subscribed + ai_familiarity_num | image_id, data = dt, vcov = "hetero")
m_z_index_fe_c         <- feols(z_index           ~ treatment * recognition_correct + age + sex_birth + education + income + political_orientation + newsletter_subscribed + ai_familiarity_num | image_id, data = dt, vcov = "hetero")

q2_dict <- c(
  # dependent variable labels
  "z_index"            = "z-index",
  "not_accurate"       = "NotAccurate",
  "not_trustworthy"    = "NotTrustworthy",
  "low_quality"        = "LowQuality",
  "biased"             = "Biased",
  "very_complex"       = "VeryComplex",
  "not_entertaining"   = "NotEntertaining",
  "no_researcher_trust"= "NoResearcherTrust",
  # coefficient labels
  "treatmentLC"                           = "Label Control",
  "treatmentEAI"                          = "Easy AI",
  "treatmentLEAI"                         = "Easy AI + Label",
  "treatmentHAI"                          = "Hard AI",
  "treatmentLHAI"                         = "Hard AI + Label",
  "recognition_correctTRUE"              = "OriginCorrect",
  "treatmentLC:recognition_correctTRUE"   = "Label Control $\\times$ OriginCorrect",
  "treatmentEAI:recognition_correctTRUE"  = "Easy AI $\\times$ OriginCorrect",
  "treatmentLEAI:recognition_correctTRUE" = "Easy AI + Label $\\times$ OriginCorrect",
  "treatmentHAI:recognition_correctTRUE"  = "Hard AI $\\times$ OriginCorrect",
  "treatmentLHAI:recognition_correctTRUE" = "Hard AI + Label $\\times$ OriginCorrect"
)

# mean DV for each outcome
q2_dvs <- list(
  z_index = m_z_index_fe_c,
  not_accurate = m_not_accurate_fe_c,
  not_trustworthy = m_not_trustworthy_fe_c,
  low_quality = m_low_quality_fe_c,
  biased = m_biased_fe_c,
  very_complex = m_very_complex_fe_c,
  not_entertaining = m_not_entertaining_fe_c,
  no_researcher_trust = m_no_researcher_trust_fe_c
)
q2_means <- sapply(names(q2_dvs), function(v) round(mean(dt[[v]], na.rm = TRUE), 3))

q2_tex_path <- "tables/wave2/table_quality2_treatment_x_recognizeAI_v2.tex"

etable(
  m_z_index_fe_c,
  m_not_accurate_fe_c,
  m_not_trustworthy_fe_c,
  m_low_quality_fe_c,
  m_biased_fe_c,
  m_very_complex_fe_c,
  m_not_entertaining_fe_c,
  m_no_researcher_trust_fe_c,
  digits = 3,
  digits.stats = 3,
  fitstat = ~ r2 + n,
  dict = q2_dict,
  drop = c("age", "sex_birth", "education", "income",
           "political_orientation", "newsletter_subscribed", "ai_familiarity_num"),
  tex = TRUE,
  file = q2_tex_path,
  replace = TRUE
)

# post-process: remove FE block, add footer rows
q2_lines <- readLines(q2_tex_path)

fe_start <- grep("\\\\emph\\{Fixed-effects\\}", q2_lines)
if (length(fe_start) > 0) {
  fe_midrule <- fe_start + which(grepl("\\\\midrule", q2_lines[(fe_start+1):length(q2_lines)]))[1]
  q2_lines <- q2_lines[-c(fe_start:fe_midrule)]
}

nc_q2 <- 8
footer_q2 <- c(
  "\\midrule",
  paste0("   Image FE & ",  paste(rep("Yes", nc_q2), collapse = " & "), "\\\\"),
  paste0("   Controls & ",  paste(rep("Yes", nc_q2), collapse = " & "), "\\\\"),
  paste0("   Mean DV & ",   paste(q2_means, collapse = " & "), "\\\\")
)

end_midrule <- tail(grep("\\\\midrule \\\\midrule", q2_lines), 1)
if (length(end_midrule) == 0) end_midrule <- tail(grep("\\\\midrule", q2_lines), 1)
q2_lines <- c(
  q2_lines[1:(end_midrule - 1)],
  footer_q2,
  q2_lines[end_midrule:length(q2_lines)]
)

writeLines(q2_lines, q2_tex_path)
cat("Post-processed", q2_tex_path, "\n")


# -----------------------------------------------------------------------------
# 10. 2SLS: DEMAND & QUALITY ON BELIEF IMAGE IS AI or RECOGNIZE IMAGE IS AI (IV: treatment)
# -----------------------------------------------------------------------------####

# Make sure treatment reference is Control
dt[, treatment := relevel(treatment, ref = "C")]

iv_belief_demand <- feols(
  newsletter_takeup ~ 1 | image_id | belief_image_ai ~ treatment,
  data = dt,
  vcov = "hetero"
)

iv_recognition_correct_demand <- feols(
  newsletter_takeup ~ 1 | image_id | recognition_correct ~ treatment,
  data = dt,
  vcov = "hetero"
)

etable(
  iv_belief_demand,
  iv_recognition_correct_demand,
  digits = 3,
  fitstat = ~ n + ivf + ivwald2
)

etable(
  iv_belief_demand,
  iv_recognition_correct_demand,
  digits = 3,
  fitstat = ~ n + ivf + ivwald2,
  tex = TRUE,
  file = "tables/wave2/table_iv_demand_beliefAI_or_recognizeAI.tex",
  replace = TRUE
)

summary(iv_belief_demand, stage = 1) # strong
summary(iv_belief_demand, stage = 2) # null effect

summary(iv_recognition_correct_demand, stage = 1) # strong
summary(iv_recognition_correct_demand, stage = 2) # null effect


# -----------------------------------------------------------------------------
# 10b. TABLE IV DEMAND v2: + controls + z_index columns
#   Col 1: IV demand ~ belief_image_ai (FE only)
#   Col 2: IV demand ~ belief_image_ai (FE + controls)
#   Col 3: IV demand ~ recognition_correct (FE only)
#   Col 4: IV demand ~ recognition_correct (FE + controls)
#   Col 5: IV z_index ~ belief_image_ai (FE only)
#   Col 6: IV z_index ~ belief_image_ai (FE + controls)
#   Col 7: IV z_index ~ recognition_correct (FE only)
#   Col 8: IV z_index ~ recognition_correct (FE + controls)
# -----------------------------------------------------------------------------

ctrl_iv <- "age + sex_birth + education + income + political_orientation + newsletter_subscribed + ai_familiarity_num"

# demand × belief_image_ai
iv_belief_demand_c <- feols(
  newsletter_takeup ~ age + sex_birth + education + income + political_orientation + newsletter_subscribed + ai_familiarity_num | image_id | belief_image_ai ~ treatment,
  data = dt, vcov = "hetero"
)

# demand × recognition_correct
iv_recognition_demand_c <- feols(
  newsletter_takeup ~ age + sex_birth + education + income + political_orientation + newsletter_subscribed + ai_familiarity_num | image_id | recognition_correct ~ treatment,
  data = dt, vcov = "hetero"
)

# z_index × belief_image_ai
iv_belief_zindex <- feols(
  z_index ~ 1 | image_id | belief_image_ai ~ treatment,
  data = dt, vcov = "hetero"
)
iv_belief_zindex_c <- feols(
  z_index ~ age + sex_birth + education + income + political_orientation + newsletter_subscribed + ai_familiarity_num | image_id | belief_image_ai ~ treatment,
  data = dt, vcov = "hetero"
)

# z_index × recognition_correct
iv_recognition_zindex <- feols(
  z_index ~ 1 | image_id | recognition_correct ~ treatment,
  data = dt, vcov = "hetero"
)
iv_recognition_zindex_c <- feols(
  z_index ~ age + sex_birth + education + income + political_orientation + newsletter_subscribed + ai_familiarity_num | image_id | recognition_correct ~ treatment,
  data = dt, vcov = "hetero"
)

iv_v2_dict <- c(
  # fixest 2SLS names the endogenous var with fit_ prefix
  "fit_belief_image_ai"      = "Manipulation",
  "fit_recognition_correct"  = "OriginCorrect",
  # fallback: plain names in case fixest drops the fit_ prefix
  "belief_image_aiTRUE"      = "Manipulation",
  "recognition_correctTRUE"  = "OriginCorrect",
  "belief_image_ai"          = "Manipulation",
  "recognition_correct"      = "OriginCorrect"
)

mean_zindex  <- round(mean(dt$z_index, na.rm = TRUE), 3)

iv_v2_tex_path <- "tables/wave2/table_iv_demand_beliefAI_or_recognizeAI_v2.tex"

etable(
  iv_belief_demand,       iv_belief_demand_c,
  iv_recognition_correct_demand, iv_recognition_demand_c,
  iv_belief_zindex,       iv_belief_zindex_c,
  iv_recognition_zindex,  iv_recognition_zindex_c,
  digits = 3,
  digits.stats = 3,
  fitstat = ~ n + ivf + ivwald2,
  dict = iv_v2_dict,
  drop = c("age", "sex_birth", "education", "income",
           "political_orientation", "newsletter_subscribed", "ai_familiarity_num"),
  headers = list(
    "Demand ($Newsletter = Yes$)" = 4,
    "z-index"                     = 4
  ),
  tex = TRUE,
  file = iv_v2_tex_path,
  replace = TRUE
)

# post-process: remove FE block, add footer
iv_v2_lines <- readLines(iv_v2_tex_path)

fe_start <- grep("\\\\emph\\{Fixed-effects\\}", iv_v2_lines)
if (length(fe_start) > 0) {
  fe_midrule <- fe_start + which(grepl("\\\\midrule", iv_v2_lines[(fe_start+1):length(iv_v2_lines)]))[1]
  iv_v2_lines <- iv_v2_lines[-c(fe_start:fe_midrule)]
}

footer_iv_v2 <- c(
  "\\midrule",
  paste0("   Image FE & ",  paste(rep("Yes", 8), collapse = " & "), "\\\\"),
  paste0("   Controls & ",  paste(rep(c("No", "Yes"), 4), collapse = " & "), "\\\\"),
  paste0("   Mean DV & ",   paste(c(rep(mean_demand, 4), rep(mean_zindex, 4)), collapse = " & "), "\\\\")
)

end_midrule <- tail(grep("\\\\midrule \\\\midrule", iv_v2_lines), 1)
if (length(end_midrule) == 0) end_midrule <- tail(grep("\\\\midrule", iv_v2_lines), 1)
iv_v2_lines <- c(
  iv_v2_lines[1:(end_midrule - 1)],
  footer_iv_v2,
  iv_v2_lines[end_midrule:length(iv_v2_lines)]
)

writeLines(iv_v2_lines, iv_v2_tex_path)
cat("Post-processed", iv_v2_tex_path, "\n")


# PERCEPTION (belief_image_ai instrumented with treatment)
iv_belief_accuracy <- feols(expect_accuracy_num ~ 1 | image_id | belief_image_ai ~ treatment, data = dt, vcov = "hetero")
iv_belief_trust <- feols(expect_trustworthiness_num ~ 1 | image_id | belief_image_ai ~ treatment, data = dt, vcov = "hetero")
iv_belief_quality <- feols(expect_quality_num ~ 1 | image_id | belief_image_ai ~ treatment, data = dt, vcov = "hetero")
iv_belief_bias <- feols(expect_political_bias_num ~ 1 | image_id | belief_image_ai ~ treatment, data = dt, vcov = "hetero")
iv_belief_complex <- feols(expect_complexity_num ~ 1 | image_id | belief_image_ai ~ treatment, data = dt, vcov = "hetero")
iv_belief_research <- feols(trust_researchers_num ~ 1 | image_id | belief_image_ai ~ treatment, data = dt, vcov = "hetero")
iv_belief_entertain <- feols(expect_entertainment_num ~ 1 | image_id | belief_image_ai ~ treatment, data = dt, vcov = "hetero")
iv_belief_Z <- feols(z_index ~ 1 | image_id | belief_image_ai ~ treatment, data = dt, vcov = "hetero")

iv_belief_noaccurate <- feols(not_accurate ~ 1 | image_id | belief_image_ai ~ treatment, data = dt, vcov = "hetero")
iv_belief_notrust <- feols(not_trustworthy ~ 1 | image_id | belief_image_ai ~ treatment, data = dt, vcov = "hetero")
iv_belief_lowquality <- feols(low_quality ~ 1 | image_id | belief_image_ai ~ treatment, data = dt, vcov = "hetero")
iv_belief_biased <- feols(biased ~ 1 | image_id | belief_image_ai ~ treatment, data = dt, vcov = "hetero")
iv_belief_noentertain <- feols(not_entertaining ~ 1 | image_id | belief_image_ai ~ treatment, data = dt, vcov = "hetero")
iv_belief_vcomplex <- feols(very_complex ~ 1 | image_id | belief_image_ai ~ treatment, data = dt, vcov = "hetero")
iv_belief_noresearch <- feols(no_researcher_trust ~ 1 | image_id | belief_image_ai ~ treatment, data = dt, vcov = "hetero")

etable(
  iv_belief_Z,
  iv_belief_accuracy,
  iv_belief_trust,
  iv_belief_quality,
  iv_belief_bias,
  iv_belief_complex,
  iv_belief_entertain,
  iv_belief_research,
  digits = 3,
  fitstat = ~ n + ivf + ivwald2
)

etable(
  iv_belief_Z,
  iv_belief_noaccurate,
  iv_belief_notrust,
  iv_belief_lowquality,
  iv_belief_biased,
  iv_belief_vcomplex,
  iv_belief_noentertain,
  iv_belief_noresearch,
  digits = 3,
  fitstat = ~ n + ivf + ivwald2
)


etable(
  iv_belief_Z,
  iv_belief_accuracy,
  iv_belief_trust,
  iv_belief_quality,
  iv_belief_bias,
  iv_belief_complex,
  iv_belief_entertain,
  iv_belief_research,
  digits = 3,
  fitstat = ~ n + ivf + ivwald2,
  tex = TRUE,
  file = "tables/wave2/table_iv_quality_beliefAI.tex",
  replace = TRUE
)

etable(
  iv_belief_Z,
  iv_belief_noaccurate,
  iv_belief_notrust,
  iv_belief_lowquality,
  iv_belief_biased,
  iv_belief_vcomplex,
  iv_belief_noentertain,
  iv_belief_noresearch,
  digits = 3,
  fitstat = ~ n + ivf + ivwald2,
  tex = TRUE,
  file = "tables/wave2/table_iv_quality2_beliefAI.tex",
  replace = TRUE
)



# PERCEPTION (recognition_correct instrumented with treatment)
iv_recognition_correct_accuracy <- feols(expect_accuracy_num ~ 1 | image_id | recognition_correct ~ treatment, data = dt, vcov = "hetero")
iv_recognition_correct_trust <- feols(expect_trustworthiness_num ~ 1 | image_id | recognition_correct ~ treatment, data = dt, vcov = "hetero")
iv_recognition_correct_quality <- feols(expect_quality_num ~ 1 | image_id | recognition_correct ~ treatment, data = dt, vcov = "hetero")
iv_recognition_correct_bias <- feols(expect_political_bias_num ~ 1 | image_id | recognition_correct ~ treatment, data = dt, vcov = "hetero")
iv_recognition_correct_complex <- feols(expect_complexity_num ~ 1 | image_id | recognition_correct ~ treatment, data = dt, vcov = "hetero")
iv_recognition_correct_research <- feols(trust_researchers_num ~ 1 | image_id | recognition_correct ~ treatment, data = dt, vcov = "hetero")
iv_recognition_correct_entertain <- feols(expect_entertainment_num ~ 1 | image_id | recognition_correct ~ treatment, data = dt, vcov = "hetero")
iv_recognition_correct_Z <- feols(z_index ~ 1 | image_id | recognition_correct ~ treatment, data = dt, vcov = "hetero")

iv_recognition_correct_noaccurate <- feols(not_accurate ~ 1 | image_id | recognition_correct ~ treatment, data = dt, vcov = "hetero")
iv_recognition_correct_notrust <- feols(not_trustworthy ~ 1 | image_id | recognition_correct ~ treatment, data = dt, vcov = "hetero")
iv_recognition_correct_lowquality <- feols(low_quality ~ 1 | image_id | recognition_correct ~ treatment, data = dt, vcov = "hetero")
iv_recognition_correct_biased <- feols(biased ~ 1 | image_id | recognition_correct ~ treatment, data = dt, vcov = "hetero")
iv_recognition_correct_noentertain <- feols(not_entertaining ~ 1 | image_id | recognition_correct ~ treatment, data = dt, vcov = "hetero")
iv_recognition_correct_vcomplex <- feols(very_complex ~ 1 | image_id | recognition_correct ~ treatment, data = dt, vcov = "hetero")
iv_recognition_correct_noresearch <- feols(no_researcher_trust ~ 1 | image_id | recognition_correct ~ treatment, data = dt, vcov = "hetero")


etable(
  iv_recognition_correct_Z,
  iv_recognition_correct_accuracy,
  iv_recognition_correct_trust,
  iv_recognition_correct_quality,
  iv_recognition_correct_bias,
  iv_recognition_correct_complex,
  iv_recognition_correct_entertain,
  iv_recognition_correct_research,
  digits = 3,
  fitstat = ~ n + ivf + ivwald2
)

etable(
  iv_recognition_correct_Z,
  iv_recognition_correct_noaccurate,
  iv_recognition_correct_notrust,
  iv_recognition_correct_lowquality,
  iv_recognition_correct_biased,
  iv_recognition_correct_vcomplex,
  iv_recognition_correct_noentertain,
  iv_recognition_correct_noresearch,
  digits = 3,
  fitstat = ~ n + ivf + ivwald2
)



etable(
  iv_recognition_correct_Z,
  iv_recognition_correct_accuracy,
  iv_recognition_correct_trust,
  iv_recognition_correct_quality,
  iv_recognition_correct_bias,
  iv_recognition_correct_complex,
  iv_recognition_correct_entertain,
  iv_recognition_correct_research,
  digits = 3,
  fitstat = ~ n + ivf + ivwald2,
  tex = TRUE,
  file = "tables/wave2/table_iv_quality_recognizeAI.tex",
  replace = TRUE
)

etable(
  iv_recognition_correct_Z,
  iv_recognition_correct_noaccurate,
  iv_recognition_correct_notrust,
  iv_recognition_correct_lowquality,
  iv_recognition_correct_biased,
  iv_recognition_correct_vcomplex,
  iv_recognition_correct_noentertain,
  iv_recognition_correct_noresearch,
  digits = 3,
  fitstat = ~ n + ivf + ivwald2,
  tex = TRUE,
  file = "tables/wave2/table_iv_quality2_recognizeAI.tex",
  replace = TRUE
)

# -----------------------------------------------------------------------------
# 10. IV SETUP: COMPONENT TREATMENT INSTRUMENTS
# -----------------------------------------------------------------------------####

# check mapping
table(dt$treatment, dt$label)
table(dt$treatment, dt$easy_ai)
table(dt$treatment, dt$hard_ai)

# small set of baseline controls
controls <- c("age", "sex_birth", "education", "income", "political_orientation", "newsletter_subscribed", "ai_familiarity_num")

# -----------------------------------------------------------------------------
# 11. FIRST STAGE: BELIEF (or RECOGNIZE) IMAGE IS AI ON TREATMENT COMPONENTS
# -----------------------------------------------------------------------------

# no controls
fs_belief_nocontrols <- feols(
  belief_image_ai ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label | image_id,
  data = dt,
  vcov = "hetero"
)

# with controls
fs_belief_controls <- feols(
  belief_image_ai ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label +
    age + sex_birth + education + income + political_orientation + newsletter_subscribed | image_id,
  data = dt,
  vcov = "hetero"
)

# no controls
fs_recogn_nocontrols <- feols(
  recognition_correct ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label | image_id,
  data = dt,
  vcov = "hetero"
)

# with controls
fs_recogn_controls <- feols(
  recognition_correct ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label +
    age + sex_birth + education + income + political_orientation + newsletter_subscribed | image_id,
  data = dt,
  vcov = "hetero"
)

# show first-stage table in R
etable(
  fs_belief_nocontrols,
  fs_belief_controls,
  fs_recogn_nocontrols,
  fs_recogn_controls,
  digits = 3,
  fitstat = ~ n + r2
)

# export first-stage table in R
etable(
  fs_belief_nocontrols,
  fs_belief_controls,
  fs_recogn_nocontrols,
  fs_recogn_controls,
  digits = 3,
  fitstat = ~ n + r2,
  tex = TRUE,
  file = "tables/wave2/table_fs_demand_beliefAI_or_recognizeAI_components_w_wo_controls.tex",
  replace = TRUE
)

# -----------------------------------------------------------------------------
# 12. 2SLS: DEMAND & QUALITY ON BELIEF (or RECOGNIZE) IMAGE IS AI instrumented with  ON TREATMENT COMPONENTS
# -----------------------------------------------------------------------------

# no controls
iv_demand_bel_nocontrols <- feols(
  newsletter_takeup ~ 1 | image_id | belief_image_ai ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label,
  data = dt,
  vcov = "hetero"
)

# with controls
iv_demand_bel_controls <- feols(
  newsletter_takeup ~ age + sex_birth + education + income + political_orientation + newsletter_subscribed |
    image_id | belief_image_ai ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label,
  data = dt,
  vcov = "hetero"
)

# no controls
iv_demand_rec_nocontrols <- feols(
  newsletter_takeup ~ 1 | image_id | recognition_correct ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label,
  data = dt,
  vcov = "hetero"
)

# with controls
iv_demand_rec_controls <- feols(
  newsletter_takeup ~ age + sex_birth + education + income + political_orientation + newsletter_subscribed |
    image_id | recognition_correct ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label,
  data = dt,
  vcov = "hetero"
)

# show second-stage table in R
etable(
  iv_demand_bel_nocontrols,
  iv_demand_bel_controls,
  iv_demand_rec_nocontrols,
  iv_demand_rec_controls,
  digits = 3,
  fitstat = ~ n + ivf + ivwald2
)

# PERCEPTION (belief_image_ai instrumented with treatment)
iv_bel_accuracy <- feols(expect_accuracy_num ~ 1 | image_id | belief_image_ai ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label, data = dt, vcov = "hetero")
iv_bel_trust <- feols(expect_trustworthiness_num ~ 1 | image_id | belief_image_ai ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label, data = dt, vcov = "hetero")
iv_bel_quality <- feols(expect_quality_num ~ 1 | image_id | belief_image_ai ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label, data = dt, vcov = "hetero")
iv_bel_bias <- feols(expect_political_bias_num ~ 1 | image_id | belief_image_ai ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label, data = dt, vcov = "hetero")
iv_bel_complex <- feols(expect_complexity_num ~ 1 | image_id | belief_image_ai ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label, data = dt, vcov = "hetero")
iv_bel_research <- feols(trust_researchers_num ~ 1 | image_id | belief_image_ai ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label, data = dt, vcov = "hetero")
iv_bel_entertain <- feols(expect_entertainment_num ~ 1 | image_id | belief_image_ai ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label, data = dt, vcov = "hetero")
iv_bel_Z <- feols(z_index ~ 1 | image_id | belief_image_ai ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label, data = dt, vcov = "hetero")

iv_bel_noaccurate <- feols(not_accurate ~ 1 | image_id | belief_image_ai ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label, data = dt, vcov = "hetero")
iv_bel_notrust <- feols(not_trustworthy ~ 1 | image_id | belief_image_ai ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label, data = dt, vcov = "hetero")
iv_bel_lowquality <- feols(low_quality ~ 1 | image_id | belief_image_ai ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label, data = dt, vcov = "hetero")
iv_bel_biased <- feols(biased ~ 1 | image_id | belief_image_ai ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label, data = dt, vcov = "hetero")
iv_bel_noentertain <- feols(not_entertaining ~ 1 | image_id | belief_image_ai ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label, data = dt, vcov = "hetero")
iv_bel_vcomplex <- feols(very_complex ~ 1 | image_id | belief_image_ai ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label, data = dt, vcov = "hetero")
iv_bel_noresearch <- feols(no_researcher_trust ~ 1 | image_id | belief_image_ai ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label, data = dt, vcov = "hetero")

etable(
  iv_bel_Z,
  iv_bel_accuracy,
  iv_bel_trust,
  iv_bel_quality,
  iv_bel_bias,
  iv_bel_complex,
  iv_bel_entertain,
  iv_bel_research,
  digits = 3,
  fitstat = ~ n + ivf + ivwald2
)

etable(
  iv_bel_Z,
  iv_bel_noaccurate,
  iv_bel_notrust,
  iv_bel_lowquality,
  iv_bel_biased,
  iv_bel_vcomplex,
  iv_bel_noentertain,
  iv_bel_noresearch,
  digits = 3,
  fitstat = ~ n + ivf + ivwald2
)


# PERCEPTION (recognition_correct instrumented with treatment)
iv_rec_correct_accuracy <- feols(expect_accuracy_num ~ 1 | image_id | recognition_correct ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label, data = dt, vcov = "hetero")
iv_rec_correct_trust <- feols(expect_trustworthiness_num ~ 1 | image_id | recognition_correct ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label, data = dt, vcov = "hetero")
iv_rec_correct_quality <- feols(expect_quality_num ~ 1 | image_id | recognition_correct ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label, data = dt, vcov = "hetero")
iv_rec_correct_bias <- feols(expect_political_bias_num ~ 1 | image_id | recognition_correct ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label, data = dt, vcov = "hetero")
iv_rec_correct_complex <- feols(expect_complexity_num ~ 1 | image_id | recognition_correct ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label, data = dt, vcov = "hetero")
iv_rec_correct_research <- feols(trust_researchers_num ~ 1 | image_id | recognition_correct ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label, data = dt, vcov = "hetero")
iv_rec_correct_entertain <- feols(expect_entertainment_num ~ 1 | image_id | recognition_correct ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label, data = dt, vcov = "hetero")
iv_rec_correct_Z <- feols(z_index ~ 1 | image_id | recognition_correct ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label, data = dt, vcov = "hetero")

iv_rec_correct_noaccurate <- feols(not_accurate ~ 1 | image_id | recognition_correct ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label, data = dt, vcov = "hetero")
iv_rec_correct_notrust <- feols(not_trustworthy ~ 1 | image_id | recognition_correct ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label, data = dt, vcov = "hetero")
iv_rec_correct_lowquality <- feols(low_quality ~ 1 | image_id | recognition_correct ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label, data = dt, vcov = "hetero")
iv_rec_correct_biased <- feols(biased ~ 1 | image_id | recognition_correct ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label, data = dt, vcov = "hetero")
iv_rec_correct_noentertain <- feols(not_entertaining ~ 1 | image_id | recognition_correct ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label, data = dt, vcov = "hetero")
iv_rec_correct_vcomplex <- feols(very_complex ~ 1 | image_id | recognition_correct ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label, data = dt, vcov = "hetero")
iv_rec_correct_noresearch <- feols(no_researcher_trust ~ 1 | image_id | recognition_correct ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label, data = dt, vcov = "hetero")


etable(
  iv_rec_correct_Z,
  iv_rec_correct_accuracy,
  iv_rec_correct_trust,
  iv_rec_correct_quality,
  iv_rec_correct_bias,
  iv_rec_correct_complex,
  iv_rec_correct_entertain,
  iv_rec_correct_research,
  digits = 3,
  fitstat = ~ n + ivf + ivwald2
)

etable(
  iv_rec_correct_Z,
  iv_rec_correct_noaccurate,
  iv_rec_correct_notrust,
  iv_rec_correct_lowquality,
  iv_rec_correct_biased,
  iv_rec_correct_vcomplex,
  iv_rec_correct_noentertain,
  iv_rec_correct_noresearch,
  digits = 3,
  fitstat = ~ n + ivf + ivwald2
)

# -----------------------------------------------------------------------------
# 13. INSPECT STAGES
# -----------------------------------------------------------------------------

# first stage summaries
summary(iv_demand_bel_nocontrols, stage = 1)
summary(iv_demand_bel_controls, stage = 1)
summary(iv_demand_rec_nocontrols, stage = 1)
summary(iv_demand_rec_controls, stage = 1)

# second stage summaries
summary(iv_demand_bel_nocontrols, stage = 2)
summary(iv_demand_bel_controls, stage = 2)
summary(iv_demand_rec_nocontrols, stage = 2)
summary(iv_demand_rec_controls, stage = 2)

# -----------------------------------------------------------------------------
# 14. EXPORT IV MODELS TO LATEX
# -----------------------------------------------------------------------------

etable(
  iv_demand_bel_nocontrols,
  iv_demand_bel_controls,
  iv_demand_rec_nocontrols,
  iv_demand_rec_controls,
  digits = 3,
  fitstat = ~ n + ivf + ivwald2,
  tex = TRUE,
  file = "tables/wave2/table_iv_demand_beliefAI_or_recognizeAI_components_w_wo_controls.tex",
  replace = TRUE
 )

etable(
  iv_bel_Z,
  iv_bel_accuracy,
  iv_bel_trust,
  iv_bel_quality,
  iv_bel_bias,
  iv_bel_complex,
  iv_bel_entertain,
  iv_bel_research,
  digits = 3,
  fitstat = ~ n + ivf + ivwald2,
  tex = TRUE,
  file = "tables/wave2/table_iv_quality_beliefAI_components.tex",
  replace = TRUE
)

etable(
  iv_bel_Z,
  iv_bel_noaccurate,
  iv_bel_notrust,
  iv_bel_lowquality,
  iv_bel_biased,
  iv_bel_vcomplex,
  iv_bel_noentertain,
  iv_bel_noresearch,
  digits = 3,
  fitstat = ~ n + ivf + ivwald2,
  tex = TRUE,
  file = "tables/wave2/table_iv_quality2_beliefAI_components.tex",
  replace = TRUE
)

etable(
  iv_rec_correct_Z,
  iv_rec_correct_accuracy,
  iv_rec_correct_trust,
  iv_rec_correct_quality,
  iv_rec_correct_bias,
  iv_rec_correct_complex,
  iv_rec_correct_entertain,
  iv_rec_correct_research,
  digits = 3,
  fitstat = ~ n + ivf + ivwald2,
  tex = TRUE,
  file = "tables/wave2/table_iv_quality_recognizeAI_components.tex",
  replace = TRUE
)

etable(
  iv_rec_correct_Z,
  iv_rec_correct_noaccurate,
  iv_rec_correct_notrust,
  iv_rec_correct_lowquality,
  iv_rec_correct_biased,
  iv_rec_correct_vcomplex,
  iv_rec_correct_noentertain,
  iv_rec_correct_noresearch,
  digits = 3,
  fitstat = ~ n + ivf + ivwald2,
  tex = TRUE,
  file = "tables/wave2/table_iv_quality2_recognizeAI_components.tex",
  replace = TRUE
)


# -----------------------------------------------------------------------------
# 14b v2: same table + controls, readable labels, matching stats/footer to 10b
# -----------------------------------------------------------------------------

iv_rec_correct_Z_c          <- feols(z_index           ~ age + sex_birth + education + income + political_orientation + newsletter_subscribed + ai_familiarity_num | image_id | recognition_correct ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label, data = dt, vcov = "hetero")
iv_rec_correct_noaccurate_c <- feols(not_accurate       ~ age + sex_birth + education + income + political_orientation + newsletter_subscribed + ai_familiarity_num | image_id | recognition_correct ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label, data = dt, vcov = "hetero")
iv_rec_correct_notrust_c    <- feols(not_trustworthy    ~ age + sex_birth + education + income + political_orientation + newsletter_subscribed + ai_familiarity_num | image_id | recognition_correct ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label, data = dt, vcov = "hetero")
iv_rec_correct_lowquality_c <- feols(low_quality        ~ age + sex_birth + education + income + political_orientation + newsletter_subscribed + ai_familiarity_num | image_id | recognition_correct ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label, data = dt, vcov = "hetero")
iv_rec_correct_biased_c     <- feols(biased             ~ age + sex_birth + education + income + political_orientation + newsletter_subscribed + ai_familiarity_num | image_id | recognition_correct ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label, data = dt, vcov = "hetero")
iv_rec_correct_vcomplex_c   <- feols(very_complex       ~ age + sex_birth + education + income + political_orientation + newsletter_subscribed + ai_familiarity_num | image_id | recognition_correct ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label, data = dt, vcov = "hetero")
iv_rec_correct_noentertain_c<- feols(not_entertaining   ~ age + sex_birth + education + income + political_orientation + newsletter_subscribed + ai_familiarity_num | image_id | recognition_correct ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label, data = dt, vcov = "hetero")
iv_rec_correct_noresearch_c <- feols(no_researcher_trust~ age + sex_birth + education + income + political_orientation + newsletter_subscribed + ai_familiarity_num | image_id | recognition_correct ~ easy_ai + hard_ai + label + easy_ai:label + hard_ai:label, data = dt, vcov = "hetero")

iv_q2_rec_dict <- c(
  # DV labels
  "z_index"             = "z-index",
  "not_accurate"        = "NotAccurate",
  "not_trustworthy"     = "NotTrustworthy",
  "low_quality"         = "LowQuality",
  "biased"              = "Biased",
  "very_complex"        = "VeryComplex",
  "not_entertaining"    = "NotEntertaining",
  "no_researcher_trust" = "NoResearcherTrust",
  # instrumented variable
  "fit_recognition_correct"  = "OriginCorrect",
  "recognition_correctTRUE"  = "OriginCorrect",
  "recognition_correct"      = "OriginCorrect"
)

# mean DVs in column order
iv_q2_rec_means <- c(
  round(mean(dt$z_index,            na.rm = TRUE), 3),
  round(mean(dt$not_accurate,       na.rm = TRUE), 3),
  round(mean(dt$not_trustworthy,    na.rm = TRUE), 3),
  round(mean(dt$low_quality,        na.rm = TRUE), 3),
  round(mean(dt$biased,             na.rm = TRUE), 3),
  round(mean(dt$very_complex,       na.rm = TRUE), 3),
  round(mean(dt$not_entertaining,   na.rm = TRUE), 3),
  round(mean(dt$no_researcher_trust,na.rm = TRUE), 3)
)

iv_q2_rec_v2_path <- "tables/wave2/table_iv_quality2_recognizeAI_components_v2.tex"

etable(
  iv_rec_correct_Z_c,
  iv_rec_correct_noaccurate_c,
  iv_rec_correct_notrust_c,
  iv_rec_correct_lowquality_c,
  iv_rec_correct_biased_c,
  iv_rec_correct_vcomplex_c,
  iv_rec_correct_noentertain_c,
  iv_rec_correct_noresearch_c,
  digits = 3,
  digits.stats = 3,
  fitstat = ~ n + ivf + ivwald2,
  dict = iv_q2_rec_dict,
  drop = c("age", "sex_birth", "education", "income",
           "political_orientation", "newsletter_subscribed", "ai_familiarity_num"),
  tex = TRUE,
  file = iv_q2_rec_v2_path,
  replace = TRUE
)

# post-process: remove FE block, add footer
iv_q2_rec_lines <- readLines(iv_q2_rec_v2_path)

fe_start <- grep("\\\\emph\\{Fixed-effects\\}", iv_q2_rec_lines)
if (length(fe_start) > 0) {
  fe_midrule <- fe_start + which(grepl("\\\\midrule", iv_q2_rec_lines[(fe_start+1):length(iv_q2_rec_lines)]))[1]
  iv_q2_rec_lines <- iv_q2_rec_lines[-c(fe_start:fe_midrule)]
}

footer_iv_q2_rec <- c(
  "\\midrule",
  paste0("   Image FE & ",  paste(rep("Yes", 8), collapse = " & "), "\\\\"),
  paste0("   Controls & ",  paste(rep("Yes", 8), collapse = " & "), "\\\\"),
  paste0("   Mean DV & ",   paste(iv_q2_rec_means, collapse = " & "), "\\\\")
)

end_midrule <- tail(grep("\\\\midrule \\\\midrule", iv_q2_rec_lines), 1)
if (length(end_midrule) == 0) end_midrule <- tail(grep("\\\\midrule", iv_q2_rec_lines), 1)
iv_q2_rec_lines <- c(
  iv_q2_rec_lines[1:(end_midrule - 1)],
  footer_iv_q2_rec,
  iv_q2_rec_lines[end_midrule:length(iv_q2_rec_lines)]
)

writeLines(iv_q2_rec_lines, iv_q2_rec_v2_path)
cat("Post-processed", iv_q2_rec_v2_path, "\n")