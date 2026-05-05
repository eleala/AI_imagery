# ============================================================
#  AI Labels – Journalists Survey
#  Analysis Script
#  Data: AI_labels_-_Journalists_April_15__2026_05_19.csv
# ============================================================
#
# SAMPLE DEFINITION
#   - Exclude Status == "Survey Preview"  (keeps IP Address responses only)
#   - Exclude Attention == 0              (failed attention check)
#   - Final clean sample: n = 96
#
# NOTE ON QUALTRICS FORMAT
#   Row 1 (index 0): human-readable question labels  → skipped
#   Row 2 (index 1): Qualtrics import IDs            → skipped
#   Data starts at row 3 (index 2)
# ============================================================

rm(list = ls())
# NOTE: working directory must be set to the replication-package root
# (the folder containing this script). run_all.R does this automatically.

# ── 0. Packages ─────────────────────────────────────────────
library(tidyverse)
library(scales)      # percent_format() for plots
library(ggplot2)
library(knitr)
library(kableExtra)
library(httr2)
library(jsonlite)

# ── 1. Load & clean ─────────────────────────────────────────

# Qualtrics exports 3 header rows:
#   row 1 – column names (machine-readable)
#   row 2 – full question text
#   row 3 – Qualtrics import IDs  {"ImportId": ...}
# Strategy: read row 1 for names, then read data skipping all 3 header rows.

# DATA/AI labels - Journalists_April 15, 2026_05.19.csv

col_names <- names(read_csv(
  "DATA/AI labels - Journalists_April 27, 2026_04.25.csv",
  n_max     = 0,
  col_types = cols(.default = col_character())
))

raw <- read_csv(
  "DATA/AI labels - Journalists_April 27, 2026_04.25.csv",
  skip      = 3,           # skip all 3 header rows
  col_names = col_names,   # apply the names from row 1
  col_types = cols(.default = col_character())
)

df <- raw %>%
  # Keep only real responses
  filter(Status != "Survey Preview") %>%
  # Keep only attention-check passers (Attention == NA in passing respondents)
  filter(is.na(Attention)) %>%
  # Rename variables to meaningful names
  rename(
    birth_year         = QID1_1,
    gender             = QID2,
    country            = QID295,
    employment         = QID8,
    is_journalist      = Q312,
    outlet_type        = QID293,
    tenure             = QID304,
    role               = QID296,
    # Belief elicitation: estimated newsletter sign-ups out of 100
    belief_baseline    = Q313_1,    # original (non-AI) pictures
    belief_ai_hard     = QID308_1,  # AI picture, hard to recognise (no label)
    belief_ai_easy     = QID309_1,  # AI picture, easy to recognise (no label)
    belief_ai_hard_lab = QID310_1,  # AI picture, hard to recognise + AI label
    belief_ai_easy_lab = QID311_1,  # AI picture, easy to recognise + AI label
    # Disclosure attitudes (Likert)
    att_disclose       = QID297,    # AI use should be clearly specified
    att_never_used_ai  = QID299,    # I have never used AI
    # Social norm beliefs
    norm_support_disc  = QID301_1,  # % journalists supporting disclosure
    norm_use_ai        = QID303_1,  # % journalists using AI
    # Open text
    open_why_ai        = QID305
  ) %>%
  # Convert numeric columns
  mutate(
    across(c(belief_baseline, belief_ai_hard, belief_ai_easy,
             belief_ai_hard_lab, belief_ai_easy_lab,
             norm_support_disc, norm_use_ai,
             birth_year),
           ~ as.numeric(.)),
    age = 2026 - birth_year
  )

cat("Clean sample size:", nrow(df), "\n")


# ── 2. Ordered factor levels ─────────────────────────────────

likert_levels <- c("Strongly disagree", "Somewhat disagree",
                   "Neither agree nor disagree",
                   "Somewhat agree", "Strongly agree")

tenure_levels <- c("Less than 1 year", "1-2 years", "3-5 years",
                   "More than 5 years")

df <- df %>%
  mutate(
    att_disclose    = factor(att_disclose,    levels = likert_levels),
    att_never_used_ai = factor(att_never_used_ai, levels = likert_levels),
    tenure          = factor(tenure, levels = tenure_levels)
  )


# ── 3. Demographics table ────────────────────────────────────

cat("\n=== DEMOGRAPHICS TABLE ===\n")

cat("\n-- Age (from birth year) --\n")
df %>%
  summarise(
    n      = sum(!is.na(age)),
    mean   = round(mean(age, na.rm = TRUE), 1),
    sd     = round(sd(age, na.rm = TRUE), 1),
    median = median(age, na.rm = TRUE),
    min    = min(age, na.rm = TRUE),
    max    = max(age, na.rm = TRUE)
  ) %>% print()

cat("\n-- Gender --\n")
df %>%
  count(gender) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  arrange(desc(n)) %>%
  print()

cat("\n-- Country (top 10) --\n")
df %>%
  count(country) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  arrange(desc(n)) %>%
  slice_head(n = 10) %>%
  print()

cat("\n-- Employment status --\n")
df %>%
  count(employment) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  arrange(desc(n)) %>%
  print()

cat("\n-- Currently a journalist --\n")
df %>%
  count(is_journalist) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  print()

cat("\n-- Outlet type --\n")
df %>%
  count(outlet_type) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  arrange(desc(n)) %>%
  print()

cat("\n-- Tenure in journalism --\n")
df %>%
  count(tenure) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  print()

cat("\n-- Role --\n")
df %>%
  count(role) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  arrange(desc(n)) %>%
  print()

# ── 3b. Demographics: combined table saved as .tex ──────────

age_tab <- df %>%
  summarise(
    Characteristic = "Age",
    Category = "",
    N = sum(!is.na(age)),
    Value = paste0(
      round(mean(age, na.rm = TRUE), 1), " (SD = ",
      round(sd(age, na.rm = TRUE), 1), "), median = ",
      median(age, na.rm = TRUE),
      ", [", min(age, na.rm = TRUE), ", ", max(age, na.rm = TRUE), "]"
    )
  )

gender_tab <- df %>%
  filter(!is.na(gender)) %>%
  count(gender) %>%
  mutate(
    Characteristic = "Gender",
    Category = gender,
    N = n,
    Value = paste0(round(100 * n / sum(n), 1), "\\%")
  ) %>%
  select(Characteristic, Category, N, Value)

country_tab <- df %>%
  filter(!is.na(country)) %>%
  count(country, sort = TRUE) %>%
  slice_head(n = 10) %>%
  mutate(
    Characteristic = "Country (top 10)",
    Category = country,
    N = n,
    Value = paste0(round(100 * n / sum(n), 1), "\\%")
  ) %>%
  select(Characteristic, Category, N, Value)

employment_tab <- df %>%
  filter(!is.na(employment)) %>%
  count(employment) %>%
  mutate(
    Characteristic = "Employment status",
    Category = employment,
    N = n,
    Value = paste0(round(100 * n / sum(n), 1), "\\%")
  ) %>%
  select(Characteristic, Category, N, Value)

journalist_tab <- df %>%
  filter(!is.na(is_journalist)) %>%
  count(is_journalist) %>%
  mutate(
    Characteristic = "Currently a journalist",
    Category = is_journalist,
    N = n,
    Value = paste0(round(100 * n / sum(n), 1), "\\%")
  ) %>%
  select(Characteristic, Category, N, Value)

outlet_tab <- df %>%
  filter(!is.na(outlet_type)) %>%
  count(outlet_type) %>%
  mutate(
    Characteristic = "Outlet type",
    Category = outlet_type,
    N = n,
    Value = paste0(round(100 * n / sum(n), 1), "\\%")
  ) %>%
  select(Characteristic, Category, N, Value)

tenure_tab <- df %>%
  filter(!is.na(tenure)) %>%
  count(tenure) %>%
  mutate(
    Characteristic = "Tenure in journalism",
    Category = as.character(tenure),
    N = n,
    Value = paste0(round(100 * n / sum(n), 1), "\\%")
  ) %>%
  select(Characteristic, Category, N, Value)

role_tab <- df %>%
  filter(!is.na(role)) %>%
  count(role) %>%
  mutate(
    Characteristic = "Role",
    Category = role,
    N = n,
    Value = paste0(round(100 * n / sum(n), 1), "\\%")
  ) %>%
  select(Characteristic, Category, N, Value)

demog_table <- bind_rows(
  age_tab,
  gender_tab,
  country_tab,
  employment_tab,
  journalist_tab,
  outlet_tab,
  tenure_tab,
  role_tab
)

demog_table %>%
  kbl(
    format = "latex",
    booktabs = TRUE,
    escape = FALSE,
    col.names = c("Characteristic", "Category", "N", "Value"),
    caption = "Sample characteristics"
  ) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  save_kable("tables/table_demographics_journalists.tex")

cat("Saved: tables/table_demographics_journalists.tex\n")

# ── 4. Belief elicitation: summary stats ────────────────────

cat("\n=== BELIEF ELICITATION: newsletter sign-ups out of 100 ===\n")

belief_vars <- c(
  "baseline (original pics)"        = "belief_baseline",
  "AI hard to recognise, no label"  = "belief_ai_hard",
  "AI easy to recognise, no label"  = "belief_ai_easy",
  "AI hard to recognise + label"    = "belief_ai_hard_lab",
  "AI easy to recognise + label"    = "belief_ai_easy_lab"
)

df %>%
  select(all_of(belief_vars)) %>%
  pivot_longer(everything(), names_to = "condition", values_to = "value") %>%
  group_by(condition) %>%
  summarise(
    n      = sum(!is.na(value)),
    mean   = round(mean(value, na.rm = TRUE), 1),
    sd     = round(sd(value, na.rm = TRUE), 1),
    median = median(value, na.rm = TRUE),
    p25    = quantile(value, 0.25, na.rm = TRUE),
    p75    = quantile(value, 0.75, na.rm = TRUE)
  ) %>%
  mutate(condition = names(belief_vars)[match(condition, belief_vars)]) %>%
  print()

belief_summary_table <- df %>%
  select(all_of(unname(belief_vars))) %>%
  pivot_longer(everything(), names_to = "condition", values_to = "value") %>%
  group_by(condition) %>%
  summarise(
    n      = sum(!is.na(value)),
    mean   = round(mean(value, na.rm = TRUE), 1),
    sd     = round(sd(value, na.rm = TRUE), 1),
    median = median(value, na.rm = TRUE),
    p25    = quantile(value, 0.25, na.rm = TRUE),
    p75    = quantile(value, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    condition = dplyr::recode(condition,
                       "belief_baseline"    = "Baseline (original pics)",
                       "belief_ai_hard"     = "AI hard to recognise, no label",
                       "belief_ai_easy"     = "AI easy to recognise, no label",
                       "belief_ai_hard_lab" = "AI hard to recognise + label",
                       "belief_ai_easy_lab" = "AI easy to recognise + label"
    )
  )

print(belief_summary_table)

belief_summary_table %>%
  kbl(
    format = "latex",
    booktabs = TRUE,
    escape = FALSE,
    col.names = c("Condition", "N", "Mean", "SD", "Median", "P25", "P75"),
    caption = "Beliefs about newsletter sign-ups across image conditions"
  ) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  save_kable("tables/table_belief_summary_jounalists.tex")

cat("Saved: tables/table_belief_summary_jounalists.tex\n")

# ── 5. Label effects: paired t-tests ────────────────────────
#  Hard AI picture: does labelling shift beliefs?
#  Easy AI picture: does labelling shift beliefs?

cat("\n=== LABEL EFFECTS: paired t-tests ===\n")

cat("\n-- Hard-to-recognise AI: no label vs. labelled --\n")
t.test(df$belief_ai_hard, df$belief_ai_hard_lab, paired = TRUE) %>% print()

cat("\n-- Easy-to-recognise AI: no label vs. labelled --\n")
t.test(df$belief_ai_easy, df$belief_ai_easy_lab, paired = TRUE) %>% print()

cat("\n-- Baseline vs. Hard AI (no label): effect of hard AI image --\n")
t.test(df$belief_baseline, df$belief_ai_hard, paired = TRUE) %>% print()

cat("\n-- Baseline vs. Easy AI (no label): effect of easy AI image --\n")
t.test(df$belief_baseline, df$belief_ai_easy, paired = TRUE) %>% print()


# ── 6. Social norm beliefs ───────────────────────────────────

cat("\n=== SOCIAL NORM BELIEFS ===\n")

df %>%
  summarise(
    across(c(norm_support_disc, norm_use_ai),
           list(n    = ~ sum(!is.na(.)),
                mean = ~ round(mean(., na.rm = TRUE), 1),
                sd   = ~ round(sd(., na.rm = TRUE), 1),
                p50  = ~ median(., na.rm = TRUE)))
  ) %>%
  pivot_longer(everything(),
               names_to  = c("variable", "stat"),
               names_sep = "_(?=[^_]+$)") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  print()


# ── 7. Disclosure attitudes ──────────────────────────────────

cat("\n=== DISCLOSURE ATTITUDES ===\n")

cat("\n-- AI use should be clearly specified --\n")
df %>%
  count(att_disclose) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  print()

cat("\n-- I have never used AI --\n")
df %>%
  count(att_never_used_ai) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  print()


# ── 8. Plots ─────────────────────────────────────────────────

theme_set(theme_minimal(base_size = 13))

# 8a. Belief distributions across conditions (boxplot)
belief_long <- df %>%
  select(all_of(unname(belief_vars))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  mutate(
    condition = dplyr::recode(variable,
      "belief_baseline"    = "Baseline\n(original)",
      "belief_ai_hard"     = "AI hard\n(no label)",
      "belief_ai_easy"     = "AI easy\n(no label)",
      "belief_ai_hard_lab" = "AI hard\n+ label",
      "belief_ai_easy_lab" = "AI easy\n+ label"
    ),
    condition = factor(condition, levels = c(
      "Baseline\n(original)",
      "AI hard\n(no label)", "AI hard\n+ label",
      "AI easy\n(no label)", "AI easy\n+ label"
    ))
  )

p_beliefs <- ggplot(belief_long, aes(x = condition, y = value, fill = condition)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21, outlier.size = 1.5) +
  geom_jitter(width = 0.15, alpha = 0.3, size = 1) +
  scale_fill_brewer(palette = "Set2", guide = "none") +
  labs(
    title    = "Journalists' beliefs about newsletter sign-ups (out of 100)",
    subtitle = "Clean sample, n = 96",
    x        = NULL,
    y        = "Estimated sign-ups out of 100"
  ) +
  theme_minimal(base_size = 18) +
  theme(plot.title   = element_text(size = 18, face = "bold"),
        axis.title   = element_text(size = 16),
        axis.text    = element_text(size = 14))

p_beliefs

ggsave("figures/journalists/journalists_plot_beliefs_boxplot.png", p_beliefs, width = 9, height = 5, dpi = 150)
cat("\nSaved: plot_beliefs_boxplot.png\n")


# 8b. Mean beliefs with 95% CI
belief_summary <- belief_long %>%
  group_by(condition) %>%
  summarise(
    mean  = mean(value, na.rm = TRUE),
    se    = sd(value, na.rm = TRUE) / sqrt(sum(!is.na(value))),
    lower = mean - 1.96 * se,
    upper = mean + 1.96 * se
  )
 

p_means <- ggplot(belief_summary, aes(x = condition, y = mean, colour = condition)) +
  geom_point(size = 3.5) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, linewidth = 0.8) +
  geom_hline(yintercept = 35, color = "red", lty=2) +
  annotate("text", x = 4, y = 35, color = "red",
           label = "Control sign-up from Exp.2", angle = 0, vjust = -0.4, size = 7) +
  scale_colour_brewer(palette = "Set2", guide = "none") +
  labs(
#    title    = "Mean beliefs about newsletter sign-ups (95% CI)",
#    subtitle = "Clean sample, n = 96",
    x        = NULL,
    y        = "Mean estimated sign-ups out of 100"
  ) +
  theme_minimal(base_size = 18) +
  theme(plot.title   = element_text(size = 18, face = "bold"),
        axis.title   = element_text(size = 16),
        axis.text    = element_text(size = 14))

p_means
ggsave("figures/journalists/journalists_plot_beliefs_means.png", p_means, width = 8, height = 5, dpi = 150)
cat("Saved: plot_beliefs_means.png\n")


# 8c. Gender bar chart
p_gender <- df %>%
  filter(!is.na(gender)) %>%
  count(gender) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(x = reorder(gender, -n), y = pct, fill = gender)) +
  geom_col(alpha = 0.85) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_brewer(palette = "Pastel1", guide = "none") +
  labs(title = "Gender distribution", x = NULL, y = "Share of respondents")

p_gender
ggsave("figures/journalists/journalists_plot_gender.png", p_gender, width = 6, height = 4, dpi = 150)
cat("Saved: plot_gender.png\n")


# 8d. Outlet type
p_outlet <- df %>%
  filter(!is.na(outlet_type)) %>%
  count(outlet_type) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(x = reorder(outlet_type, pct), y = pct, fill = outlet_type)) +
  geom_col(alpha = 0.85) +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_brewer(palette = "Set3", guide = "none") +
  labs(title = "Outlet type", x = NULL, y = "Share of respondents")

p_outlet
ggsave("figures/journalists/journalists_plot_outlet_type.png", p_outlet, width = 7, height = 4, dpi = 150)
cat("Saved: plot_outlet_type.png\n")


# 8e. Disclosure attitudes (stacked bar)
att_long <- df %>%
  select(att_disclose, att_never_used_ai) %>%
  pivot_longer(everything(), names_to = "question", values_to = "response") %>%
  filter(!is.na(response)) %>%
  mutate(
    question = dplyr::recode(question,
      "att_disclose"      = "AI use should be disclosed",
      "att_never_used_ai" = "I have never used AI"
    ),
    response = factor(response, levels = likert_levels)
  )


p_attitudes <- att_long %>%
  count(question, response) %>%
  group_by(question) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(x = pct, y = question, fill = response)) +
  geom_col(position = "stack", alpha = 0.9, width = 0.5) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_brewer(palette = "RdYlGn", direction = 1, name = NULL,
                    labels = c(
                      "Strongly disagree",
                      "Disagree",
                      "Neither",
                      "Agree",
                      "Strongly agree"
                    )) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
  labs(
    x = "Share of respondents",
    y = NULL
  ) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 9),
    legend.key.width = unit(1.2, "cm"),
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14)
  )

p_attitudes
ggsave("figures/journalists/journalists_plot_attitudes.png", p_attitudes, width = 9, height = 4, dpi = 150)
cat("Saved: plot_attitudes.png\n")


# 8f. Social norms: histograms
norms_long <- df %>%
  select(norm_support_disc, norm_use_ai) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  filter(!is.na(value)) %>%
  mutate(variable = dplyr::recode(variable,
    "norm_support_disc" = "Support disclosure",
    "norm_use_ai"       = "Use AI"
  ))

p_norms <- ggplot(norms_long, aes(x = value, fill = variable)) +
  geom_histogram(binwidth = 5, alpha = 0.8, colour = "white") +
  facet_wrap(~ variable, ncol = 2) +
  scale_fill_brewer(palette = "Set1", guide = "none") +
  labs(
    # title = "Beliefs about journalists' behaviour (social norms)",
    subtitle = "Out of 100 people in your profession, how many you think...",
    x     = "Estimated % out of 100 journalists",
    y     = "Count"
  ) +
theme_minimal(base_size = 18) +
  theme(plot.title   = element_text(size = 18, face = "bold"),
        axis.title   = element_text(size = 16),
        axis.text    = element_text(size = 14))
p_norms
ggsave("figures/journalists/journalists_plot_norms.png", p_norms, width = 9, height = 4, dpi = 150)
cat("Saved: plot_norms.png\n")

# 8f. Social norms: histograms v2
norms_long <- df %>%
  select(norm_support_disc, norm_use_ai) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  filter(!is.na(value)) %>%
  mutate(variable = dplyr::recode(variable,
                           "norm_support_disc" = "Support disclosure",
                           "norm_use_ai"       = "Use AI"
  )) %>%
  mutate(variable = factor(variable, levels = c("Use AI", "Support disclosure")))

p_norms <- ggplot(norms_long, aes(x = value, fill = variable)) +
  geom_histogram(binwidth = 5, alpha = 0.8, colour = "white") +
  facet_wrap(~ variable, ncol = 2) +
  scale_fill_brewer(palette = "Set1", guide = "none") +
  labs(
    # title = "Beliefs about journalists' behaviour (social norms)",
    subtitle = "Out of 100 people in your profession, how many you think...",
    x     = "Estimated % out of 100 journalists",
    y     = "Count"
  ) +
  theme_minimal(base_size = 18) +
  theme(plot.title   = element_text(size = 18, face = "bold"),
        axis.title   = element_text(size = 16),
        axis.text    = element_text(size = 14))
p_norms
ggsave("figures/journalists/journalists_plot_norms2.png", p_norms, width = 9, height = 4, dpi = 150)
cat("Saved: plot_norms.png\n")

cat("\n=== Done. All plots saved as PNG files in the working directory. ===\n")



# ── 9. Open-text analysis: inductive topic coding via Claude API ─────────────
#
# This section sends the open-text responses (open_why_ai) to the Anthropic API
# and asks Claude to inductively assign topic labels to each response.
# Each response can receive multiple labels (non-exclusive).
# The output is a frequency table of topics (% of respondents), saved as .tex.
#
# Requires:
#   - httr2 installed         : install.packages("httr2")
#   - API key in .Renviron    : ANTHROPIC_API_KEY=sk-ant-...
#     (run usethis::edit_r_environ(), add the line, restart R)
# ─────────────────────────────────────────────────────────────────────────────

# ── 9a. Extract clean responses ──────────────────────────────

open_df <- df %>%
  select(open_why_ai) %>%
  filter(!is.na(open_why_ai), str_trim(open_why_ai) != "") %>%
  mutate(resp_id = row_number()) %>%
  relocate(resp_id)

cat("\nOpen-text responses available for coding:", nrow(open_df), "\n")

# ── 9b. Build the prompt ─────────────────────────────────────
#
# Strategy: send all responses in a single API call.
# Claude returns a JSON array — one object per response — each with:
#   { "resp_id": <int>, "labels": ["label 1", "label 2", ...] }
# Labels are short noun phrases (3–5 words), inductive (not predefined).

responses_block <- open_df %>%
  mutate(formatted = paste0(resp_id, '. "', str_trim(open_why_ai), '"')) %>%
  pull(formatted) %>%
  paste(collapse = "\n")

prompt <- paste0(
  "Below are ", nrow(open_df), " open-text survey responses from journalists ",
  "explaining why they think journalists use AI in their work.\n\n",
  "Your task:\n",
  "1. Read each response carefully.\n",
  "2. Assign one or more short topic labels (3-5 words each) that capture the ",
  "main themes mentioned. Labels should be inductive — derived from the content ",
  "itself, not from a predefined list.\n",
  "3. If a response is too short, unintelligible, or contains no substantive ",
  "content, assign the single label 'Non-informative'.\n",
  "4. Use consistent label wording across responses: if the same theme appears ",
  "in multiple responses, use exactly the same label text each time.\n\n",
  "Return ONLY a JSON array with no preamble or explanation, in this format:\n",
  '[{"resp_id": 1, "labels": ["label a", "label b"]}, ',
  '{"resp_id": 2, "labels": ["label c"]}, ...]\n\n',
  "Responses:\n",
  responses_block
)

# ── 9c. Call the API ─────────────────────────────────────────

api_key <- Sys.getenv("ANTHROPIC_API_KEY")
if (api_key == "") stop("ANTHROPIC_API_KEY not found. Check your .Renviron file.")

cat("Sending", nrow(open_df), "responses to Claude for topic coding...\n")

resp <- request("https://api.anthropic.com/v1/messages") %>%
  req_headers(
    "x-api-key"         = api_key,
    "anthropic-version" = "2023-06-01",
    "content-type"      = "application/json"
  ) %>%
  req_body_json(list(
    model      = "claude-haiku-4-5-20251001",
    max_tokens = 10000,
    messages   = list(
      list(role = "user", content = prompt)
    )
  )) %>%
  req_error(is_error = \(r) FALSE) %>%  # handle errors manually below
  req_perform()

if (resp_status(resp) != 200) {
  stop("API request failed with status ", resp_status(resp), ":\n",
       resp_body_string(resp))
}

raw_content <- resp_body_json(resp)$content[[1]]$text
cat("API call successful.\n")

# ── 9d. Parse the JSON response ──────────────────────────────

# Strip any accidental markdown fences Claude might add
clean_json <- raw_content %>%
  str_remove_all("```json") %>%
  str_remove_all("```") %>%
  str_replace_all("，", ",") %>%   # replace fullwidth commas
  str_replace_all("　", " ") %>%   # replace fullwidth spaces if any
  str_trim()

# If truncated mid-array, close it gracefully
if (!endsWith(clean_json, "]")) {
  clean_json <- str_replace(clean_json, ",?\\s*\\{[^\\{]*$", "]")
}

labelled <- fromJSON(clean_json, simplifyDataFrame = FALSE)
cat("Parsed successfully:", length(labelled), "responses\n")

# Convert to a long data frame: one row per (resp_id, label)
labels_long <- map_dfr(labelled, function(x) {
  tibble(
    resp_id = x$resp_id,
    label   = unlist(x$labels)
  )
})

cat("Total (response × label) pairs:", nrow(labels_long), "\n")

# ── 9e. Compute topic frequencies ────────────────────────────

n_respondents <- nrow(open_df)

topic_freq <- labels_long %>%
  filter(label != "Non-informative") %>%
  # Count how many distinct respondents mention each label
  group_by(label) %>%
  summarise(n = n_distinct(resp_id), .groups = "drop") %>%
  mutate(pct = round(100 * n / n_respondents, 1)) %>%
  arrange(desc(pct))

cat("\n=== OPEN-TEXT TOPIC FREQUENCIES ===\n")
print(topic_freq)

# ── 9f. Save as LaTeX table ──────────────────────────────────

topic_freq %>%
  mutate(
    `Topic` = str_to_sentence(label),
    `\\% of Respondents` = paste0(pct, "\\%"),
    N = n
  ) %>%
  select(`Topic`, N, `\\% of Respondents`) %>%
  kbl(
    format    = "latex",
    booktabs  = TRUE,
    escape    = FALSE,
    caption   = "Journalists' Reported Reasons for Using AI in Their Work (inductive coding)",
    label     = "tab:open_why_ai"
  ) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  add_footnote(
    paste0("Note: Categories are not mutually exclusive; respondents could ",
           "mention multiple themes. Percentages reflect the share of the ",
           n_respondents, " respondents whose answer contained at least one ",
           "reference to the given theme."),
    notation = "none"
  ) %>%
  save_kable("tables/table_open_why_ai_journalists.tex")

cat("Saved: tables/table_open_why_ai_journalists.tex\n")

# ── 9g. Save raw labels to CSV (for inspection / replication) ─

labels_long %>%
  left_join(open_df, by = "resp_id") %>%
  write_csv("tables/open_why_ai_labels_raw.csv")

cat("Saved: tables/open_why_ai_labels_raw.csv\n")
cat("\n=== Section 9 complete. ===\n")



# ── 9h. Collapse granular labels into broad themes via Claude API ─────────────
#
# Sends the unique granular labels back to Claude, asking it to:
#   1. Collapse them into 6-8 broad thematic categories
#   2. Map every granular label to exactly one broad category
#   3. Select one illustrative verbatim quote per category from the raw responses
# ─────────────────────────────────────────────────────────────────────────────

# Build a list of unique non-informative labels with their counts
label_counts <- labels_long %>%
  filter(label != "Non-informative") %>%
  group_by(label) %>%
  summarise(n = n_distinct(resp_id), .groups = "drop") %>%
  arrange(desc(n))

labels_block <- label_counts %>%
  mutate(line = paste0('"', label, '" (n=', n, ')')) %>%
  pull(line) %>%
  paste(collapse = "\n")

# Also prepare the original responses for quote selection
responses_for_quotes <- open_df %>%
  mutate(formatted = paste0(resp_id, '. "', str_trim(open_why_ai), '"')) %>%
  pull(formatted) %>%
  paste(collapse = "\n")

collapse_prompt <- paste0(
  "I have coded ", nrow(open_df), " open-text survey responses from journalists ",
  "explaining why they think journalists use AI. The coding produced ", nrow(label_counts),
  " granular labels (with respondent counts). Your tasks:\n\n",
  
  "TASK 1: Collapse these granular labels into exactly 6-8 broad thematic categories. ",
  "Each granular label must map to exactly one broad category. ",
  "Categories should be mutually meaningful, non-redundant, and suitable for a ",
  "published academic paper table.\n\n",
  
  "TASK 2: For each broad category, select one short illustrative verbatim quote ",
  "from the original responses below. The quote should be punchy and representative ",
  "(1-2 sentences max).\n\n",
  
  "Return ONLY a JSON array, no preamble, in this exact format:\n",
  '[{"theme": "Theme name", "granular_labels": ["label 1", "label 2"], ',
  '"example_quote": "verbatim quote here"}, ...]\n\n',
  
  "GRANULAR LABELS (label: count of respondents):\n",
  labels_block, "\n\n",
  "ORIGINAL RESPONSES (for quote selection):\n",
  responses_for_quotes
)

cat("Sending labels to Claude for collapsing into broad themes...\n")

resp2 <- request("https://api.anthropic.com/v1/messages") %>%
  req_headers(
    "x-api-key"         = api_key,
    "anthropic-version" = "2023-06-01",
    "content-type"      = "application/json"
  ) %>%
  req_body_json(list(
    model      = "claude-haiku-4-5-20251001",
    max_tokens = 8000,
    messages   = list(
      list(role = "user", content = collapse_prompt)
    )
  )) %>%
  req_error(is_error = \(r) FALSE) %>%
  req_perform()

if (resp_status(resp2) != 200) {
  stop("API request failed with status ", resp_status(resp2), ":\n",
       resp_body_string(resp2))
}

raw2 <- resp_body_json(resp2)$content[[1]]$text
cat("API call successful.\n")

# ── 9i. Parse and compute frequencies for broad themes ───────────────────────

clean2 <- raw2 %>%
  str_remove_all("```json") %>%
  str_remove_all("```") %>%
  str_replace_all("，", ",") %>%
  str_replace_all("　", " ") %>%
  str_trim()

if (!endsWith(clean2, "]")) {
  clean2 <- str_replace(clean2, ",?\\s*\\{[^\\{]*$", "]")
}

themes <- fromJSON(clean2, simplifyDataFrame = FALSE)
cat("Parsed", length(themes), "broad themes.\n")

# Build mapping: granular label → broad theme
label_to_theme <- map_dfr(themes, function(t) {
  tibble(
    theme  = t$theme,
    label  = unlist(t$granular_labels),
    example_quote = t$example_quote
  )
})

# Join with labels_long to count respondents per broad theme
theme_freq <- labels_long %>%
  filter(label != "Non-informative") %>%
  left_join(label_to_theme, by = "label") %>%
  filter(!is.na(theme)) %>%
  group_by(theme, example_quote) %>%
  summarise(n = n_distinct(resp_id), .groups = "drop") %>%
  mutate(pct = round(100 * n / n_respondents, 1)) %>%
  arrange(desc(pct))

cat("\n=== BROAD THEME FREQUENCIES ===\n")
print(theme_freq %>% select(theme, n, pct))

# ── 9j. Save final table as LaTeX ────────────────────────────────────────────

theme_freq %>%
  mutate(
    `Topic Category`      = theme,
    `\\% of Respondents`  = paste0(pct, "\\%"),
    `Illustrative Example` = paste0('\\textit{"', example_quote, '"}')
  ) %>%
  select(`Topic Category`, `\\% of Respondents`, `Illustrative Example`) %>%
  kbl(
    format   = "latex",
    booktabs = TRUE,
    escape   = FALSE,
    caption  = "Journalists' Reported Reasons for Using AI in Their Work",
    label    = "tab:why_ai_themes"
  ) %>%
  column_spec(1, width = "4cm") %>%
  column_spec(2, width = "2cm") %>%
  column_spec(3, width = "8cm") %>%
  kable_styling(latex_options = c("hold_position")) %>%
  add_footnote(
    paste0("Note: Categories are not mutually exclusive; respondents could mention ",
           "multiple themes. Percentages reflect the share of the ",
           n_respondents, " respondents whose answer contained at least one ",
           "reference to the given theme."),
    notation = "none"
  ) %>%
  save_kable("tables/table_why_ai_themes_journalists.tex")

cat("Saved: tables/table_why_ai_themes_journalists.tex\n")

# Also save the theme-label mapping for transparency
label_to_theme %>%
  write_csv("tables/theme_label_mapping.csv")

cat("Saved: tables/theme_label_mapping.csv\n")
cat("\n=== Section 9h-j complete. ===\n")
