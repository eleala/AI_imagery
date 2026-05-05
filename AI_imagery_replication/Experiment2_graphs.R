# -----------------------------------------------------------------------------
# 0. PARAMETERS
# -----------------------------------------------------------------------------

library(data.table)
library(ggplot2)
library(dplyr)
library(tibble)
library(purrr)
library(ggpubr)
library(scales)

rm(list = ls())
# NOTE: working directory must be set to the replication-package root
# (the folder containing this script). run_all.R does this automatically.

# output folder for wave 2 figures
dir.create("figures/wave2", recursive = TRUE, showWarnings = FALSE)

# load cleaned data
dt <- readRDS("DATA/ai_labels_mechs_cleaned.rds")

cat(sprintf("Loaded cleaned wave-2 data: %d rows, %d cols\n", nrow(dt), ncol(dt)))

# -----------------------------------------------------------------------------
# 1. TREATMENT LABELS FOR PLOTS
# -----------------------------------------------------------------------------

dt[, treatment := factor(
  treatment,
  levels = c("C", "LC", "EAI", "LEAI", "HAI", "LHAI")
)]

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

table(dt$treatment_label, useNA = "ifany")


# -----------------------------------------------------------------------------
# 2. SUMMARY DATA FOR FIRST FIGURE
# -----------------------------------------------------------------------------

fig_belief <- dt[, .(
  n = .N,
  mean_belief = mean(belief_image_ai, na.rm = TRUE)
), by = .(treatment, treatment_label)]

fig_belief[, se_belief := sqrt(mean_belief * (1 - mean_belief) / n)]
fig_belief[, ci_belief := 1.96 * se_belief]

fig_belief


# -----------------------------------------------------------------------------
# 3. FIGURE: BELIEF IMAGE IS AI BY TREATMENT
# -----------------------------------------------------------------------------

p_belief <- ggplot(fig_belief, aes(x = treatment_label, y = mean_belief)) +
  geom_col(width = 0.7, fill = "#2c7fb8") +
  geom_errorbar(
    aes(
      ymin = pmax(0, mean_belief - ci_belief),
      ymax = pmin(1, mean_belief + ci_belief)
    ),
    width = 0.15,
    linewidth = 0.5
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    x = "",
    y = "Prop. believing image was AI-generated"
  ) +
  theme_minimal(base_size = 18) +     
  theme(plot.title   = element_text(size = 18, face = "bold"),           
        axis.title   = element_text(size = 16),           
        axis.text    = element_text(size = 14)) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.margin = margin(10, 30, 10, 10)
  ) +
  coord_cartesian(clip = "off")

p_belief
ggsave("figures/wave2/Fig_belief_AI_wave2.pdf", p_belief, width = 9, height = 4)


# -----------------------------------------------------------------------------
# 3. FIGURE: BELIEF IMAGE IS AI BY TREATMENT (with pariwise p-v)
# -----------------------------------------------------------------------------

# 3a. Summary data
fig_belief <- dt[, .(
  n = .N,
  mean_belief = mean(belief_image_ai, na.rm = TRUE)
), by = .(treatment, treatment_label)]

fig_belief[, se_belief := sqrt(mean_belief * (1 - mean_belief) / n)]
fig_belief[, ci_belief := 1.96 * se_belief]

# 3b. All pairwise proportion tests
levs <- levels(dt$treatment_label)

pair_df <- as.data.frame(t(combn(levs, 2)))
names(pair_df) <- c("group1", "group2")

get_counts <- function(g) {
  row <- fig_belief[treatment_label == g]
  list(
    x = round(row$mean_belief * row$n),
    n = row$n,
    ytop = row$mean_belief + row$ci_belief,
    idx = match(g, levs)
  )
}

tests <- purrr::map_dfr(seq_len(nrow(pair_df)), function(i) {
  g1 <- pair_df$group1[i]
  g2 <- pair_df$group2[i]
  
  a <- get_counts(g1)
  b <- get_counts(g2)
  
  tst <- prop.test(
    x = c(a$x, b$x),
    n = c(a$n, b$n),
    correct = FALSE
  )
  
  tibble(
    group1 = g1,
    group2 = g2,
    p = tst$p.value,
    y_base = max(a$ytop, b$ytop),
    d = abs(a$idx - b$idx),
    midx = (a$idx + b$idx) / 2
  )
})

# 3c. Holm adjustment + significance labels + bracket spacing
tests <- tests %>%
  mutate(
    p_adj = p.adjust(p, method = "holm"),
    p_label = case_when(
      p_adj < 0.01 ~ "***",
      p_adj < 0.05 ~ "**",
      p_adj < 0.10 ~ "*",
      TRUE ~ "ns"
    )
  ) %>%
  arrange(d, y_base, midx)

base_off <- c(`1` = 0.05, `2` = 0.10, `3` = 0.15, `4` = 0.20, `5` = 0.25)

tests <- tests %>%
  group_by(d) %>%
  mutate(
    y.position = y_base + base_off[as.character(d)] + 0.02 * (row_number() - 1)
  ) %>%
  ungroup()

# 3d. Plot
p_belief <- ggplot(fig_belief, aes(x = treatment_label, y = mean_belief)) +
  geom_col(width = 0.7, fill = "#2c7fb8") +
  geom_errorbar(
    aes(
      ymin = pmax(0, mean_belief - ci_belief),
      ymax = pmin(1, mean_belief + ci_belief)
    ),
    width = 0.15,
    linewidth = 0.5
  ) +
  ggpubr::stat_pvalue_manual(
    tests,
    label = "p_label",
    xmin = "group1",
    xmax = "group2",
    y.position = "y.position",
    tip.length = 0.01,
    size = 3
  ) +
  scale_y_continuous(
    limits = c(0, 1.45),
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    x = "",
    y = "Prop. believing image was AI-generated"
  ) +
  theme_minimal(base_size = 18) +     theme(plot.title   = element_text(size = 18, face = "bold"),           axis.title   = element_text(size = 16),           axis.text    = element_text(size = 14)) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.margin = margin(10, 30, 10, 10)
  ) +
  coord_cartesian(clip = "off")

p_belief

# 3e. Save
ggsave("figures/wave2/Fig_belief_AI_wave2_pv.pdf", p_belief, width = 9, height = 6)

# 3e. CLEANER VERSION: SIGNIFICANT BRACKETS ONLY, STACKED ABOVE ALL BARS

# Keep only significant pairs, sorted so shorter spans sit lower
tests_sig <- tests %>%
  filter(p_label != "ns") %>%
  arrange(d, midx)

# Stack all brackets above the tallest bar top, with a fixed step
bar_top    <- max(fig_belief$mean_belief + fig_belief$ci_belief, na.rm = TRUE)
bracket_start <- bar_top + 0.06          # clearance above bars
bracket_step  <- 0.07                    # vertical gap between brackets

tests_sig <- tests_sig %>%
  mutate(y.position = bracket_start + (row_number() - 1) * bracket_step)

# Dynamic y-axis ceiling
y_max <- max(tests_sig$y.position, bar_top) + 0.08

p_belief_clean <- ggplot(fig_belief, aes(x = treatment_label, y = mean_belief)) +
  geom_col(width = 0.7, fill = "#2c7fb8") +
  geom_errorbar(
    aes(
      ymin = pmax(0, mean_belief - ci_belief),
      ymax = pmin(1, mean_belief + ci_belief)
    ),
    width = 0.15,
    linewidth = 0.5
  ) +
  ggpubr::stat_pvalue_manual(
    tests_sig,
    label = "p_label",
    xmin  = "group1",
    xmax  = "group2",
    y.position = "y.position",
    tip.length = 0.005,
    size = 3
  ) +
  scale_y_continuous(
    limits = c(0, y_max),
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    x = "",
    y = "Prop. believing image was AI-generated"
  ) +
  theme_minimal(base_size = 18) +
  theme(plot.title   = element_text(size = 18, face = "bold"),
        axis.title   = element_text(size = 16),
        axis.text    = element_text(size = 14)) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.margin  = margin(10, 30, 10, 10)
  ) +
  coord_cartesian(clip = "off")

# 3f. SAVE
p_belief_clean
ggsave("figures/wave2/Fig_belief_AI_wave2_pv_clean.pdf",
       p_belief_clean, width = 9, height = 6)

# -----------------------------------------------------------------------------
# 4. SUMMARY DATA FOR CORRECT RECOGNITION FIGURE
# -----------------------------------------------------------------------------

fig_correct <- dt[, .(
  n = .N,
  mean_correct = mean(recognition_correct, na.rm = TRUE)
), by = .(treatment, treatment_label)]

fig_correct[, se_correct := sqrt(mean_correct * (1 - mean_correct) / n)]
fig_correct[, ci_correct := 1.96 * se_correct]

fig_correct

# -----------------------------------------------------------------------------
# 5. FIGURE: CORRECT RECOGNITION BY TREATMENT
# -----------------------------------------------------------------------------

p_correct <- ggplot(fig_correct, aes(x = treatment_label, y = mean_correct)) +
  geom_col(width = 0.7, fill = "#1b9e77") +
  geom_errorbar(
    aes(
      ymin = pmax(0, mean_correct - ci_correct),
      ymax = pmin(1, mean_correct + ci_correct)
    ),
    width = 0.15,
    linewidth = 0.5
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    x = "",
    y = "Prop. correctly recognizing image origin"
  ) +
  theme_minimal(base_size = 18) +     theme(plot.title   = element_text(size = 18, face = "bold"),           axis.title   = element_text(size = 16),           axis.text    = element_text(size = 14)) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.margin = margin(10, 30, 10, 10)
  ) +
  coord_cartesian(clip = "off")

p_correct
ggsave("figures/wave2/Fig_correct_recognition_wave2.pdf", p_correct, width = 9, height = 4)

# -----------------------------------------------------------------------------
# 5. FIGURE: CORRECT RECOGNITION BY TREATMENT (with pairwise p-v)
# -----------------------------------------------------------------------------

# 5a. All pairwise proportion tests
levs <- levels(dt$treatment_label)

pair_df <- as.data.frame(t(combn(levs, 2)))
names(pair_df) <- c("group1", "group2")

get_counts_correct <- function(g) {
  row <- fig_correct[treatment_label == g]
  list(
    x = round(row$mean_correct * row$n),
    n = row$n,
    ytop = row$mean_correct + row$ci_correct,
    idx = match(g, levs)
  )
}

tests_correct <- purrr::map_dfr(seq_len(nrow(pair_df)), function(i) {
  g1 <- pair_df$group1[i]
  g2 <- pair_df$group2[i]
  
  a <- get_counts_correct(g1)
  b <- get_counts_correct(g2)
  
  tst <- prop.test(
    x = c(a$x, b$x),
    n = c(a$n, b$n),
    correct = FALSE
  )
  
  tibble(
    group1 = g1,
    group2 = g2,
    p = tst$p.value,
    y_base = max(a$ytop, b$ytop),
    d = abs(a$idx - b$idx),
    midx = (a$idx + b$idx) / 2
  )
})

# 5b. Holm adjustment + significance labels + bracket spacing
tests_correct <- tests_correct %>%
  mutate(
    p_adj = p.adjust(p, method = "holm"),
    p_label = case_when(
      p_adj < 0.01 ~ "***",
      p_adj < 0.05 ~ "**",
      p_adj < 0.10 ~ "*",
      TRUE ~ "ns"
    )
  ) %>%
  arrange(d, y_base, midx)

base_off <- c(`1` = 0.05, `2` = 0.10, `3` = 0.15, `4` = 0.20, `5` = 0.25)

tests_correct <- tests_correct %>%
  group_by(d) %>%
  mutate(
    y.position = y_base + base_off[as.character(d)] + 0.02 * (row_number() - 1)
  ) %>%
  ungroup()

# 5c. Plot
p_correct_pv <- ggplot(fig_correct, aes(x = treatment_label, y = mean_correct)) +
  geom_col(width = 0.7, fill = "#1b9e77") +
  geom_errorbar(
    aes(
      ymin = pmax(0, mean_correct - ci_correct),
      ymax = pmin(1, mean_correct + ci_correct)
    ),
    width = 0.15,
    linewidth = 0.5
  ) +
  ggpubr::stat_pvalue_manual(
    tests_correct,
    label = "p_label",
    xmin = "group1",
    xmax = "group2",
    y.position = "y.position",
    tip.length = 0.01,
    size = 3
  ) +
  scale_y_continuous(
    limits = c(0, 1.45),
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    x = "",
    y = "Prop. correctly recognizing image origin"
  ) +
  theme_minimal(base_size = 18) +     theme(plot.title   = element_text(size = 18, face = "bold"),           axis.title   = element_text(size = 16),           axis.text    = element_text(size = 14)) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.margin = margin(10, 30, 10, 10)
  ) +
  coord_cartesian(clip = "off")

p_correct_pv
ggsave("figures/wave2/Fig_correct_recognition_wave2_pv.pdf", p_correct_pv, width = 9, height = 6)

# 5d. CLEANER VERSION: SIGNIFICANT BRACKETS ONLY, STACKED ABOVE ALL BARS

# Keep only significant pairs, sorted so shorter spans sit lower
tests_correct_sig <- tests_correct %>%
  filter(p_label != "ns") %>%
  arrange(d, midx)

# Stack all brackets above the tallest bar top, with a fixed step
bar_top_correct   <- max(fig_correct$mean_correct + fig_correct$ci_correct, na.rm = TRUE)
bracket_start_correct <- bar_top_correct + 0.06
bracket_step_correct  <- 0.07

tests_correct_sig <- tests_correct_sig %>%
  mutate(y.position = bracket_start_correct + (row_number() - 1) * bracket_step_correct)

# Dynamic y-axis ceiling
y_max_correct <- max(tests_correct_sig$y.position, bar_top_correct) + 0.08

p_correct_clean <- ggplot(fig_correct, aes(x = treatment_label, y = mean_correct)) +
  geom_col(width = 0.7, fill = "#1b9e77") +
  geom_errorbar(
    aes(
      ymin = pmax(0, mean_correct - ci_correct),
      ymax = pmin(1, mean_correct + ci_correct)
    ),
    width = 0.15,
    linewidth = 0.5
  ) +
  ggpubr::stat_pvalue_manual(
    tests_correct_sig,
    label = "p_label",
    xmin  = "group1",
    xmax  = "group2",
    y.position = "y.position",
    tip.length = 0.005,
    size = 3
  ) +
  scale_y_continuous(
    limits = c(0, y_max_correct),
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    x = "",
    y = "Prop. correctly recognizing image origin"
  ) +
  theme_minimal(base_size = 18) +
  theme(plot.title   = element_text(size = 18, face = "bold"),
        axis.title   = element_text(size = 16),
        axis.text    = element_text(size = 14)) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.margin  = margin(10, 30, 10, 10)
  ) +
  coord_cartesian(clip = "off")

# 6e. SAVE
p_correct_clean
ggsave("figures/wave2/Fig_correct_recognition_wave2_pv_clean.pdf",
       p_correct_clean, width = 9, height = 6)

# -----------------------------------------------------------------------------
# 6. SUMMARY DATA FOR NEWSLETTER DEMAND FIGURE
# -----------------------------------------------------------------------------

fig_demand <- dt[, .(
  n = .N,
  mean_demand = mean(newsletter_takeup, na.rm = TRUE)
), by = .(treatment, treatment_label)]

fig_demand[, se_demand := sqrt(mean_demand * (1 - mean_demand) / n)]
fig_demand[, ci_demand := 1.96 * se_demand]

fig_demand

# -----------------------------------------------------------------------------
# 7. FIGURE: NEWSLETTER DEMAND BY TREATMENT
# -----------------------------------------------------------------------------

p_demand <- ggplot(fig_demand, aes(x = treatment_label, y = mean_demand)) +
  geom_col(width = 0.7, fill = "#636363") +
  geom_errorbar(
    aes(
      ymin = pmax(0, mean_demand - ci_demand),
      ymax = pmin(1, mean_demand + ci_demand)
    ),
    width = 0.15,
    linewidth = 0.5
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    x = "",
    y = "Prop. willing to receive newsletter"
  ) +
  theme_minimal(base_size = 18) +     theme(plot.title   = element_text(size = 18, face = "bold"),           axis.title   = element_text(size = 16),           axis.text    = element_text(size = 14)) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.margin = margin(10, 30, 10, 10)
  ) +
  coord_cartesian(clip = "off")

p_demand
ggsave("figures/wave2/Fig_newsletter_demand_wave2.pdf", p_demand, width = 9, height = 4)

# -----------------------------------------------------------------------------
# 7. FIGURE: NEWSLETTER DEMAND BY TREATMENT (with pairwise p-v)
# -----------------------------------------------------------------------------

# 7a. All pairwise proportion tests
levs <- levels(dt$treatment_label)

pair_df <- as.data.frame(t(combn(levs, 2)))
names(pair_df) <- c("group1", "group2")

get_counts_demand <- function(g) {
  row <- fig_demand[treatment_label == g]
  list(
    x = round(row$mean_demand * row$n),
    n = row$n,
    ytop = row$mean_demand + row$ci_demand,
    idx = match(g, levs)
  )
}

tests_demand <- purrr::map_dfr(seq_len(nrow(pair_df)), function(i) {
  g1 <- pair_df$group1[i]
  g2 <- pair_df$group2[i]
  
  a <- get_counts_demand(g1)
  b <- get_counts_demand(g2)
  
  tst <- prop.test(
    x = c(a$x, b$x),
    n = c(a$n, b$n),
    correct = FALSE
  )
  
  tibble(
    group1 = g1,
    group2 = g2,
    p = tst$p.value,
    y_base = max(a$ytop, b$ytop),
    d = abs(a$idx - b$idx),
    midx = (a$idx + b$idx) / 2
  )
})

# 7b. Holm adjustment + significance labels + bracket spacing
tests_demand <- tests_demand %>%
  mutate(
    p_adj = p.adjust(p, method = "holm"),
    p_label = case_when(
      p_adj < 0.01 ~ "***",
      p_adj < 0.05 ~ "**",
      p_adj < 0.10 ~ "*",
      TRUE ~ "ns"
    )
  ) %>%
  arrange(d, y_base, midx)

base_off <- c(`1` = 0.05, `2` = 0.10, `3` = 0.15, `4` = 0.20, `5` = 0.25)

tests_demand <- tests_demand %>%
  group_by(d) %>%
  mutate(
    y.position = y_base + base_off[as.character(d)] + 0.02 * (row_number() - 1)
  ) %>%
  ungroup()

# 7c. Plot
p_demand_pv <- ggplot(fig_demand, aes(x = treatment_label, y = mean_demand)) +
  geom_col(width = 0.7, fill = "#636363") +
  geom_errorbar(
    aes(
      ymin = pmax(0, mean_demand - ci_demand),
      ymax = pmin(1, mean_demand + ci_demand)
    ),
    width = 0.15,
    linewidth = 0.5
  ) +
  ggpubr::stat_pvalue_manual(
    tests_demand,
    label = "p_label",
    xmin = "group1",
    xmax = "group2",
    y.position = "y.position",
    tip.length = 0.01,
    size = 3
  ) +
  scale_y_continuous(
    limits = c(0, 1.45),
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    x = "",
    y = "Prop. willing to receive newsletter"
  ) +
  theme_minimal(base_size = 18) +     theme(plot.title   = element_text(size = 18, face = "bold"),           axis.title   = element_text(size = 16),           axis.text    = element_text(size = 14)) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.margin = margin(10, 30, 10, 10)
  ) +
  coord_cartesian(clip = "off")

p_demand_pv
ggsave("figures/wave2/Fig_newsletter_demand_wave2_pv.pdf", p_demand_pv, width = 9, height = 6)


# 7d. CLEANER VERSION: SIGNIFICANT BRACKETS ONLY, STACKED ABOVE ALL BARS

tests_demand_sig <- tests_demand %>%
  filter(p_label != "ns") %>%
  arrange(d, midx)

if (nrow(tests_demand_sig) == 0) {
  # No significant pairs — plot without any brackets
  p_demand_clean <- ggplot(fig_demand, aes(x = treatment_label, y = mean_demand)) +
    geom_col(width = 0.7, fill = "#636363") +
    geom_errorbar(
      aes(
        ymin = pmax(0, mean_demand - ci_demand),
        ymax = pmin(1, mean_demand + ci_demand)
      ),
      width = 0.15,
      linewidth = 0.5
    ) +
    scale_y_continuous(
      limits = c(0, 1),
      labels = scales::percent_format(accuracy = 1)
    ) +
    labs(
      x = "",
      y = "Prop. willing to receive newsletter"
    ) +
    theme_minimal(base_size = 18) +     theme(plot.title   = element_text(size = 18, face = "bold"),           axis.title   = element_text(size = 16),           axis.text    = element_text(size = 14)) +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      plot.margin  = margin(10, 30, 10, 10)
    ) +
    coord_cartesian(clip = "off")
  
  message("7d: no significant pairs — saving plain bar chart.")
  
} else {
  
  bar_top_demand       <- max(fig_demand$mean_demand + fig_demand$ci_demand, na.rm = TRUE)
  bracket_start_demand <- bar_top_demand + 0.06
  bracket_step_demand  <- 0.07
  
  tests_demand_sig <- tests_demand_sig %>%
    mutate(y.position = bracket_start_demand + (row_number() - 1) * bracket_step_demand)
  
  y_max_demand <- max(tests_demand_sig$y.position, bar_top_demand) + 0.08
  
  p_demand_clean <- ggplot(fig_demand, aes(x = treatment_label, y = mean_demand)) +
    geom_col(width = 0.7, fill = "#636363") 
    geom_errorbar(
      aes(
        ymin = pmax(0, mean_demand - ci_demand),
        ymax = pmin(1, mean_demand + ci_demand)
      ),
      width = 0.15,
      linewidth = 0.5
    ) +
    ggpubr::stat_pvalue_manual(
      tests_demand_sig,
      label = "p_label",
      xmin  = "group1",
      xmax  = "group2",
      y.position = "y.position",
      tip.length = 0.005,
      size = 3
    ) +
    scale_y_continuous(
      limits = c(0, y_max_demand),
      labels = scales::percent_format(accuracy = 1)
    ) +
    labs(
      x = "",
      y = "Prop. willing to receive newsletter"
    ) +
    theme_minimal(base_size = 18) +
    theme(plot.title   = element_text(size = 18, face = "bold"),
          axis.title   = element_text(size = 16),
          axis.text    = element_text(size = 14)) +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      plot.margin  = margin(10, 30, 10, 10)
    ) +
    coord_cartesian(clip = "off")
}

# 7e. SAVE
p_demand_clean
ggsave("figures/wave2/Fig_newsletter_demand_wave2_pv_clean.pdf",
       p_demand_clean, width = 9, height = 6)

# -----------------------------------------------------------------------------
# 8. SUMMARY DATA: NEWSLETTER DEMAND BY TREATMENT X CORRECT RECOGNITION
# -----------------------------------------------------------------------------

fig_demand_correct <- dt[, .(
  n = .N,
  mean_demand = mean(newsletter_takeup, na.rm = TRUE)
), by = .(treatment, treatment_label, recognition_correct)]

fig_demand_correct[, se_demand := sqrt(mean_demand * (1 - mean_demand) / n)]
fig_demand_correct[, ci_demand := 1.96 * se_demand]

fig_demand_correct

# -----------------------------------------------------------------------------
# 8a. LABELS FOR CORRECT RECOGNITION SUBGROUPS
# -----------------------------------------------------------------------------

fig_demand_correct[, recognition_group := fifelse(
  recognition_correct == TRUE,  "Correct recognition",
  fifelse(recognition_correct == FALSE, "Incorrect recognition", NA_character_)
)]

fig_demand_correct[, recognition_group := factor(
  recognition_group,
  levels = c("Correct recognition", "Incorrect recognition")
)]

table(fig_demand_correct$recognition_group, useNA = "ifany")

# -----------------------------------------------------------------------------
# 9. FIGURE: NEWSLETTER DEMAND BY TREATMENT, BY CORRECT RECOGNITION
# -----------------------------------------------------------------------------

p_demand_correct <- ggplot(
  fig_demand_correct,
  aes(x = treatment_label, y = mean_demand, fill = recognition_group)
) +
  geom_col(width = 0.7) +
  geom_errorbar(
    aes(
      ymin = pmax(0, mean_demand - ci_demand),
      ymax = pmin(1, mean_demand + ci_demand)
    ),
    width = 0.15,
    linewidth = 0.5
  ) +
  facet_wrap(~ recognition_group) +
  scale_fill_manual(values = c(
    "Correct recognition" = "#1b9e77",
    "Incorrect recognition" = "#d95f02"
  )) +
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    x = "",
    y = "Prop. willing to receive newsletter"
  ) +
  theme_minimal(base_size = 18) +     theme(plot.title   = element_text(size = 18, face = "bold"),           axis.title   = element_text(size = 16),           axis.text    = element_text(size = 14)) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.margin = margin(10, 30, 10, 10),
    legend.position = "none"
  ) +
  coord_cartesian(clip = "off")

p_demand_correct
ggsave("figures/wave2/Fig_newsletter_demand_by_correct_recognition_wave2.pdf",
       p_demand_correct, width = 10, height = 4.5)

# -----------------------------------------------------------------------------
# 9. FIGURE: NEWSLETTER DEMAND BY TREATMENT, BY CORRECT RECOGNITION
#     (split into two separate plots, arranged side by side)
# -----------------------------------------------------------------------------

y_shared_demand_correct <- c(0, 1)

p_demand_correct_crt <- ggplot(
  fig_demand_correct[recognition_group == "Correct recognition"],
  aes(x = treatment_label, y = mean_demand, fill = recognition_group)
) +
  geom_col(width = 0.7) +
  geom_errorbar(
    aes(
      ymin = pmax(0, mean_demand - ci_demand),
      ymax = pmin(1, mean_demand + ci_demand)
    ),
    width = 0.15,
    linewidth = 0.5
  ) +
  scale_fill_manual(values = c("Correct recognition" = "#1b9e77")) +
  scale_y_continuous(
    limits = y_shared_demand_correct,
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(x = "", y = "Prop. willing to receive newsletter",
       title = "Correct recognition") +
  theme_minimal(base_size = 18) +     theme(plot.title   = element_text(size = 18, face = "bold"),           axis.title   = element_text(size = 16),           axis.text    = element_text(size = 14)) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.margin  = margin(10, 15, 10, 10),
    legend.position = "none"
  ) +
  coord_cartesian(clip = "off")

p_demand_correct_inc <- ggplot(
  fig_demand_correct[recognition_group == "Incorrect recognition"],
  aes(x = treatment_label, y = mean_demand, fill = recognition_group)
) +
  geom_col(width = 0.7) +
  geom_errorbar(
    aes(
      ymin = pmax(0, mean_demand - ci_demand),
      ymax = pmin(1, mean_demand + ci_demand)
    ),
    width = 0.15,
    linewidth = 0.5
  ) +
  scale_fill_manual(values = c("Incorrect recognition" = "#d95f02")) +
  scale_y_continuous(
    limits = y_shared_demand_correct,
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(x = "", y = "", title = "Incorrect recognition") +
  theme_minimal(base_size = 18) +
  theme(plot.title   = element_text(size = 18, face = "bold"),
        axis.title   = element_text(size = 16),
        axis.text    = element_text(size = 14)) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.margin  = margin(10, 15, 10, 10),
    legend.position = "none"
  ) +
  coord_cartesian(clip = "off")

p_demand_correct_crt
ggsave("figures/wave2/Fig_newsletter_demand_by_correct_recognition_wave2_split_correct.pdf",
       p_demand_correct_crt, width = 9, height = 4.5)

p_demand_correct_inc
ggsave("figures/wave2/Fig_newsletter_demand_by_correct_recognition_wave2_split_incorrect.pdf",
       p_demand_correct_inc, width = 9, height = 4.5)

# -----------------------------------------------------------------------------
# 9a. PAIRWISE TESTS WITHIN CORRECT-RECOGNITION SUBGROUPS
# -----------------------------------------------------------------------------

levs <- levels(fig_demand_correct$treatment_label)
subgroups <- levels(fig_demand_correct$recognition_group)

pair_df <- as.data.frame(t(combn(levs, 2)))
names(pair_df) <- c("group1", "group2")

get_counts_subgroup <- function(g, s) {
  row <- fig_demand_correct[treatment_label == g & recognition_group == s]
  list(
    x = round(row$mean_demand * row$n),
    n = row$n,
    ytop = row$mean_demand + row$ci_demand,
    idx = match(g, levs)
  )
}

tests_demand_correct <- purrr::map_dfr(subgroups, function(s) {
  purrr::map_dfr(seq_len(nrow(pair_df)), function(i) {
    g1 <- pair_df$group1[i]
    g2 <- pair_df$group2[i]
    
    a <- get_counts_subgroup(g1, s)
    b <- get_counts_subgroup(g2, s)
    
    tst <- prop.test(
      x = c(a$x, b$x),
      n = c(a$n, b$n),
      correct = FALSE
    )
    
    tibble(
      recognition_group = s,
      group1 = g1,
      group2 = g2,
      p = tst$p.value,
      y_base = max(a$ytop, b$ytop),
      d = abs(a$idx - b$idx),
      midx = (a$idx + b$idx) / 2
    )
  })
})

# -----------------------------------------------------------------------------
# 9b. HOLM-ADJUST WITHIN SUBGROUP + BRACKET POSITIONS
# -----------------------------------------------------------------------------

tests_demand_correct <- tests_demand_correct %>%
  group_by(recognition_group) %>%
  mutate(
    p_adj = p.adjust(p, method = "holm"),
    p_label = case_when(
      p_adj < 0.01 ~ "***",
      p_adj < 0.05 ~ "**",
      p_adj < 0.10 ~ "*",
      TRUE ~ "ns"
    )
  ) %>%
  arrange(recognition_group, d, y_base, midx) %>%
  ungroup()

base_off <- c(`1` = 0.05, `2` = 0.10, `3` = 0.15, `4` = 0.20, `5` = 0.25)

tests_demand_correct <- tests_demand_correct %>%
  group_by(recognition_group, d) %>%
  mutate(
    y.position = y_base + base_off[as.character(d)] + 0.02 * (row_number() - 1)
  ) %>%
  ungroup()

# -----------------------------------------------------------------------------
# 9c. FIGURE: NEWSLETTER DEMAND BY TREATMENT, BY CORRECT RECOGNITION
#      (with pairwise p-values within subgroup)
# -----------------------------------------------------------------------------

p_demand_correct_pv <- ggplot(
  fig_demand_correct,
  aes(x = treatment_label, y = mean_demand, fill = recognition_group)
) +
  geom_col(width = 0.7) +
  geom_errorbar(
    aes(
      ymin = pmax(0, mean_demand - ci_demand),
      ymax = pmin(1, mean_demand + ci_demand)
    ),
    width = 0.15,
    linewidth = 0.5
  ) +
  ggpubr::stat_pvalue_manual(
    tests_demand_correct,
    label = "p_label",
    xmin = "group1",
    xmax = "group2",
    y.position = "y.position",
    tip.length = 0.01,
    size = 3
  ) +
  facet_wrap(~ recognition_group) +
  scale_fill_manual(values = c(
    "Correct recognition" = "#1b9e77",
    "Incorrect recognition" = "#d95f02"
  )) +
  scale_y_continuous(
    limits = c(0, 1.45),
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    x = "",
    y = "Prop. willing to receive newsletter"
  ) +
  theme_minimal(base_size = 18) +
  theme(plot.title   = element_text(size = 18, face = "bold"),
        axis.title   = element_text(size = 16),
        axis.text    = element_text(size = 14)) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.margin = margin(10, 30, 10, 10),
    legend.position = "none"
  ) +
  coord_cartesian(clip = "off")

p_demand_correct_pv
ggsave(
  "figures/wave2/Fig_newsletter_demand_by_correct_recognition_wave2_pv.pdf",
  p_demand_correct_pv,
  width = 10,
  height = 6
)

# -----------------------------------------------------------------------------
# 9c. CLEANER VERSION: SIGNIFICANT BRACKETS ONLY, SPLIT, ONE FILE EACH
# -----------------------------------------------------------------------------

build_demand_correct_panel <- function(subgroup, fill_colour) {
  
  tests_sub <- tests_demand_correct %>%
    filter(recognition_group == subgroup, p_label != "ns") %>%
    arrange(d, midx)
  
  fig_sub <- fig_demand_correct[recognition_group == subgroup]
  bar_top  <- max(fig_sub$mean_demand + fig_sub$ci_demand, na.rm = TRUE)
  
  p <- ggplot(fig_sub, aes(x = treatment_label, y = mean_demand)) +
    geom_col(width = 0.7, fill = fill_colour) +
    geom_errorbar(
      aes(
        ymin = pmax(0, mean_demand - ci_demand),
        ymax = pmin(1, mean_demand + ci_demand)
      ),
      width = 0.15,
      linewidth = 0.5
    )
  
  if (nrow(tests_sub) == 0) {
    y_max <- 1
    message(sprintf("9c: no significant pairs for '%s'.", subgroup))
  } else {
    tests_sub <- tests_sub %>%
      mutate(y.position = (bar_top + 0.06) + (row_number() - 1) * 0.07)
    y_max <- max(tests_sub$y.position) + 0.08
    p <- p +
      ggpubr::stat_pvalue_manual(
        tests_sub,
        label = "p_label",
        xmin  = "group1",
        xmax  = "group2",
        y.position = "y.position",
        tip.length = 0.005,
        size = 3
      )
  }
  
  p +
    scale_y_continuous(
      limits = c(0, y_max),
      labels = scales::percent_format(accuracy = 1)
    ) +
    labs(x = "", y = "Prop. willing to receive newsletter") + #, title = subgroup) +
    theme_minimal(base_size = 18) +     
    theme(plot.title   = element_text(size = 18, face = "bold"),           
          axis.title   = element_text(size = 16),           
          axis.text    = element_text(size = 14)) +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      plot.margin  = margin(10, 15, 10, 10),
      legend.position = "none"
    ) +
    coord_cartesian(clip = "off")
}

p_demand_correct_crt_clean <- build_demand_correct_panel(
  "Correct recognition",   "#1b9e77"
)
p_demand_correct_inc_clean <- build_demand_correct_panel(
  "Incorrect recognition", "#d95f02"
)

p_demand_correct_crt_clean
ggsave("figures/wave2/Fig_newsletter_demand_by_correct_recognition_wave2_pv_clean_correct.pdf",
       p_demand_correct_crt_clean, width = 9, height = 6)

p_demand_correct_inc_clean
ggsave("figures/wave2/Fig_newsletter_demand_by_correct_recognition_wave2_pv_clean_incorrect.pdf",
       p_demand_correct_inc_clean, width = 9, height = 6)

# -----------------------------------------------------------------------------
# 10. SUMMARY DATA: NEWSLETTER DEMAND BY TREATMENT X BELIEF IMAGE IS AI
# -----------------------------------------------------------------------------

fig_demand_belief <- dt[, .(
  n = .N,
  mean_demand = mean(newsletter_takeup, na.rm = TRUE)
), by = .(treatment, treatment_label, belief_image_ai)]

fig_demand_belief[, se_demand := sqrt(mean_demand * (1 - mean_demand) / n)]
fig_demand_belief[, ci_demand := 1.96 * se_demand]

fig_demand_belief[, belief_group := fifelse(
  belief_image_ai == TRUE,  "Believes image is AI",
  fifelse(belief_image_ai == FALSE, "Believes image is not AI", NA_character_)
)]

fig_demand_belief[, belief_group := factor(
  belief_group,
  levels = c("Believes image is AI", "Believes image is not AI")
)]

fig_demand_belief

# -----------------------------------------------------------------------------
# 11. FIGURE: NEWSLETTER DEMAND BY TREATMENT, BY BELIEF IMAGE IS AI
# -----------------------------------------------------------------------------

p_demand_belief <- ggplot(
  fig_demand_belief,
  aes(x = treatment_label, y = mean_demand, fill = belief_group)
) +
  geom_col(width = 0.7) +
  geom_errorbar(
    aes(
      ymin = pmax(0, mean_demand - ci_demand),
      ymax = pmin(1, mean_demand + ci_demand)
    ),
    width = 0.15,
    linewidth = 0.5
  ) +
  facet_wrap(~ belief_group) +
  scale_fill_manual(values = c(
    "Believes image is AI" = "#7570b3",
    "Believes image is not AI" = "#e6ab02"
  )) +
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    x = "",
    y = "Prop. willing to receive newsletter"
  ) +
  theme_minimal(base_size = 18) +     theme(plot.title   = element_text(size = 18, face = "bold"),           axis.title   = element_text(size = 16),           axis.text    = element_text(size = 14)) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.margin = margin(10, 30, 10, 10),
    legend.position = "none"
  ) +
  coord_cartesian(clip = "off")

p_demand_belief
ggsave(
  "figures/wave2/Fig_newsletter_demand_by_beliefAI_wave2.pdf",
  p_demand_belief,
  width = 10,
  height = 4.5
)

# -----------------------------------------------------------------------------
# 11. FIGURE: NEWSLETTER DEMAND BY TREATMENT, BY BELIEF IMAGE IS AI
#     (split into two separate plots, one file each)
# -----------------------------------------------------------------------------

y_shared_demand_belief <- c(0, 1)

p_demand_belief_ai <- ggplot(
  fig_demand_belief[belief_group == "Believes image is AI"],
  aes(x = treatment_label, y = mean_demand, fill = belief_group)
) +
  geom_col(width = 0.7) +
  geom_errorbar(
    aes(
      ymin = pmax(0, mean_demand - ci_demand),
      ymax = pmin(1, mean_demand + ci_demand)
    ),
    width = 0.15,
    linewidth = 0.5
  ) +
  scale_fill_manual(values = c("Believes image is AI" = "#7570b3")) +
  scale_y_continuous(
    limits = y_shared_demand_belief,
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(x = "", y = "Prop. willing to receive newsletter",
       title = "Believes image is AI") +
  theme_minimal(base_size = 18) +     theme(plot.title   = element_text(size = 18, face = "bold"),           axis.title   = element_text(size = 16),           axis.text    = element_text(size = 14)) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.margin  = margin(10, 15, 10, 10),
    legend.position = "none"
  ) +
  coord_cartesian(clip = "off")

p_demand_belief_notai <- ggplot(
  fig_demand_belief[belief_group == "Believes image is not AI"],
  aes(x = treatment_label, y = mean_demand, fill = belief_group)
) +
  geom_col(width = 0.7) +
  geom_errorbar(
    aes(
      ymin = pmax(0, mean_demand - ci_demand),
      ymax = pmin(1, mean_demand + ci_demand)
    ),
    width = 0.15,
    linewidth = 0.5
  ) +
  scale_fill_manual(values = c("Believes image is not AI" = "#e6ab02")) +
  scale_y_continuous(
    limits = y_shared_demand_belief,
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(x = "", y = "Prop. willing to receive newsletter",
       title = "Believes image is not AI") +
  theme_minimal(base_size = 18) +     theme(plot.title   = element_text(size = 18, face = "bold"),           axis.title   = element_text(size = 16),           axis.text    = element_text(size = 14)) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.margin  = margin(10, 15, 10, 10),
    legend.position = "none"
  ) +
  coord_cartesian(clip = "off")

p_demand_belief_ai
ggsave("figures/wave2/Fig_newsletter_demand_by_beliefAI_wave2_split_AI.pdf",
       p_demand_belief_ai, width = 9, height = 4.5)

p_demand_belief_notai
ggsave("figures/wave2/Fig_newsletter_demand_by_beliefAI_wave2_split_notAI.pdf",
       p_demand_belief_notai, width = 9, height = 4.5)

# -----------------------------------------------------------------------------
# 11a. PAIRWISE TESTS WITHIN BELIEF-IMAGE-IS-AI SUBGROUPS
# -----------------------------------------------------------------------------

levs <- levels(fig_demand_belief$treatment_label)
subgroups <- levels(fig_demand_belief$belief_group)

pair_df <- as.data.frame(t(combn(levs, 2)))
names(pair_df) <- c("group1", "group2")

get_counts_belief <- function(g, s) {
  row <- fig_demand_belief[treatment_label == g & belief_group == s]
  list(
    x = round(row$mean_demand * row$n),
    n = row$n,
    ytop = row$mean_demand + row$ci_demand,
    idx = match(g, levs)
  )
}

tests_demand_belief <- purrr::map_dfr(subgroups, function(s) {
  purrr::map_dfr(seq_len(nrow(pair_df)), function(i) {
    g1 <- pair_df$group1[i]
    g2 <- pair_df$group2[i]
    
    a <- get_counts_belief(g1, s)
    b <- get_counts_belief(g2, s)
    
    tst <- prop.test(
      x = c(a$x, b$x),
      n = c(a$n, b$n),
      correct = FALSE
    )
    
    tibble(
      belief_group = s,
      group1 = g1,
      group2 = g2,
      p = tst$p.value,
      y_base = max(a$ytop, b$ytop),
      d = abs(a$idx - b$idx),
      midx = (a$idx + b$idx) / 2
    )
  })
})

# -----------------------------------------------------------------------------
# 11b. FIGURE: NEWSLETTER DEMAND BY TREATMENT, BY BELIEF IMAGE IS AI
#      (with pairwise p-values within subgroup)
# -----------------------------------------------------------------------------

tests_demand_belief <- tests_demand_belief %>%
  group_by(belief_group) %>%
  mutate(
    p_adj = p.adjust(p, method = "holm"),
    p_label = case_when(
      p_adj < 0.01 ~ "***",
      p_adj < 0.05 ~ "**",
      p_adj < 0.10 ~ "*",
      TRUE ~ "ns"
    )
  ) %>%
  arrange(belief_group, d, y_base, midx) %>%
  ungroup()

base_off <- c(`1` = 0.05, `2` = 0.10, `3` = 0.15, `4` = 0.20, `5` = 0.25)

tests_demand_belief <- tests_demand_belief %>%
  group_by(belief_group, d) %>%
  mutate(
    y.position = y_base + base_off[as.character(d)] + 0.02 * (row_number() - 1)
  ) %>%
  ungroup()

p_demand_belief_pv <- ggplot(
  fig_demand_belief,
  aes(x = treatment_label, y = mean_demand, fill = belief_group)
) +
  geom_col(width = 0.7) +
  geom_errorbar(
    aes(
      ymin = pmax(0, mean_demand - ci_demand),
      ymax = pmin(1, mean_demand + ci_demand)
    ),
    width = 0.15,
    linewidth = 0.5
  ) +
  ggpubr::stat_pvalue_manual(
    tests_demand_belief,
    label = "p_label",
    xmin = "group1",
    xmax = "group2",
    y.position = "y.position",
    tip.length = 0.01,
    size = 3
  ) +
  facet_wrap(~ belief_group) +
  scale_fill_manual(values = c(
    "Believes image is AI" = "#7570b3",
    "Believes image is not AI" = "#e6ab02"
  )) +
  scale_y_continuous(
    limits = c(0, 1.45),
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    x = "",
    y = "Prop. willing to receive newsletter"
  ) +
  theme_minimal(base_size = 18) +     theme(plot.title   = element_text(size = 18, face = "bold"),           axis.title   = element_text(size = 16),           axis.text    = element_text(size = 14)) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.margin = margin(10, 30, 10, 10),
    legend.position = "none"
  ) +
  coord_cartesian(clip = "off")

p_demand_belief_pv
ggsave(
  "figures/wave2/Fig_newsletter_demand_by_beliefAI_wave2_pv.pdf",
  p_demand_belief_pv,
  width = 10,
  height = 6
)

# -----------------------------------------------------------------------------
# 11c. CLEANER VERSION: SIGNIFICANT BRACKETS ONLY, SPLIT, ONE FILE EACH
# -----------------------------------------------------------------------------

build_demand_belief_panel <- function(subgroup, fill_colour) {
  
  tests_sub <- tests_demand_belief %>%
    filter(belief_group == subgroup, p_label != "ns") %>%
    arrange(d, midx)
  
  fig_sub <- fig_demand_belief[belief_group == subgroup]
  bar_top  <- max(fig_sub$mean_demand + fig_sub$ci_demand, na.rm = TRUE)
  
  p <- ggplot(fig_sub, aes(x = treatment_label, y = mean_demand)) +
    geom_col(width = 0.7, fill = fill_colour) +
    geom_errorbar(
      aes(
        ymin = pmax(0, mean_demand - ci_demand),
        ymax = pmin(1, mean_demand + ci_demand)
      ),
      width = 0.15,
      linewidth = 0.5
    )
  
  if (nrow(tests_sub) == 0) {
    y_max <- 1
    message(sprintf("11c: no significant pairs for '%s'.", subgroup))
  } else {
    tests_sub <- tests_sub %>%
      mutate(y.position = (bar_top + 0.06) + (row_number() - 1) * 0.07)
    y_max <- max(tests_sub$y.position) + 0.08
    p <- p +
      ggpubr::stat_pvalue_manual(
        tests_sub,
        label = "p_label",
        xmin  = "group1",
        xmax  = "group2",
        y.position = "y.position",
        tip.length = 0.005,
        size = 3
      )
  }
  
  p +
    scale_y_continuous(
      limits = c(0, y_max),
      labels = scales::percent_format(accuracy = 1)
    ) +
    labs(x = "", y = "Prop. willing to receive newsletter") + #, title = subgroup) +
    theme_minimal(base_size = 18) +     
    theme(plot.title   = element_text(size = 18, face = "bold"),           
          axis.title   = element_text(size = 16),           
          axis.text    = element_text(size = 14)) +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      plot.margin  = margin(10, 15, 10, 10),
      legend.position = "none"
    ) +
    coord_cartesian(clip = "off")
}

p_demand_belief_ai_clean <- build_demand_belief_panel(
  "Believes image is AI",     "#7570b3"
)
p_demand_belief_notai_clean <- build_demand_belief_panel(
  "Believes image is not AI", "#e6ab02"
)

p_demand_belief_ai_clean
ggsave("figures/wave2/Fig_newsletter_demand_by_beliefAI_wave2_pv_clean_AI.pdf",
       p_demand_belief_ai_clean, width = 9, height = 6)

p_demand_belief_notai_clean
ggsave("figures/wave2/Fig_newsletter_demand_by_beliefAI_wave2_pv_clean_notAI.pdf",
       p_demand_belief_notai_clean, width = 9, height = 6)

# -----------------------------------------------------------------------------
# 12. SUMMARY DATA FOR Z-INDEX FIGURE (ALL SAMPLE)
# -----------------------------------------------------------------------------

fig_zindex <- dt[, .(
  n = .N,
  mean_z = mean(z_index, na.rm = TRUE),
  sd_z   = sd(z_index, na.rm = TRUE)
), by = .(treatment, treatment_label)]

fig_zindex[, se_z := sd_z / sqrt(n)]
fig_zindex[, ci_z := 1.96 * se_z]

fig_zindex

# -----------------------------------------------------------------------------
# 13. FIGURE: Z-INDEX BY TREATMENT
# -----------------------------------------------------------------------------

p_zindex <- ggplot(fig_zindex, aes(x = treatment_label, y = mean_z)) +
  geom_col(width = 0.7, fill = "#636363") +
  geom_errorbar(
    aes(
      ymin = mean_z - ci_z,
      ymax = mean_z + ci_z
    ),
    width = 0.15,
    linewidth = 0.5
  ) +
  labs(
    x = "",
    y = "Mean z-index"
  ) +
  theme_minimal(base_size = 18) +     theme(plot.title   = element_text(size = 18, face = "bold"),           axis.title   = element_text(size = 16),           axis.text    = element_text(size = 14)) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.margin = margin(10, 30, 10, 10)
  ) +
  coord_cartesian(clip = "off")

p_zindex
ggsave("figures/wave2/Fig_zindex_wave2.pdf", p_zindex, width = 9, height = 4)


# -----------------------------------------------------------------------------
# 13. FIGURE: Z-INDEX BY TREATMENT (with pairwise p-v)
# -----------------------------------------------------------------------------

levs <- levels(fig_zindex$treatment_label)

pair_df <- as.data.frame(t(combn(levs, 2)))
names(pair_df) <- c("group1", "group2")

get_stats_z <- function(g) {
  row <- fig_zindex[treatment_label == g]
  list(
    y = row$mean_z,
    sd = row$sd_z,
    n = row$n,
    ytop = row$mean_z + row$ci_z,
    idx = match(g, levs)
  )
}

tests_z <- purrr::map_dfr(seq_len(nrow(pair_df)), function(i) {
  g1 <- pair_df$group1[i]
  g2 <- pair_df$group2[i]
  
  a <- get_stats_z(g1)
  b <- get_stats_z(g2)
  
  tst <- t.test(
    x = dt[treatment_label == g1, z_index],
    y = dt[treatment_label == g2, z_index]
  )
  
  tibble(
    group1 = g1,
    group2 = g2,
    p = tst$p.value,
    y_base = max(a$ytop, b$ytop),
    d = abs(a$idx - b$idx),
    midx = (a$idx + b$idx) / 2
  )
})

tests_z <- tests_z %>%
  mutate(
    p_adj = p.adjust(p, method = "holm"),
    p_label = case_when(
      p_adj < 0.01 ~ "***",
      p_adj < 0.05 ~ "**",
      p_adj < 0.10 ~ "*",
      TRUE ~ "ns"
    )
  ) %>%
  arrange(d, y_base, midx)

base_off <- c(`1` = 0.05, `2` = 0.10, `3` = 0.15, `4` = 0.20, `5` = 0.25)

tests_z <- tests_z %>%
  group_by(d) %>%
  mutate(
    y.position = y_base + base_off[as.character(d)] + 0.02 * (row_number() - 1)
  ) %>%
  ungroup()

p_zindex_pv <- ggplot(fig_zindex, aes(x = treatment_label, y = mean_z)) +
  geom_col(width = 0.7, fill = "#636363") +
  geom_errorbar(
    aes(
      ymin = mean_z - ci_z,
      ymax = mean_z + ci_z
    ),
    width = 0.15,
    linewidth = 0.5
  ) +
  ggpubr::stat_pvalue_manual(
    tests_z,
    label = "p_label",
    xmin = "group1",
    xmax = "group2",
    y.position = "y.position",
    tip.length = 0.01,
    size = 3
  ) +
  labs(
    x = "",
    y = "Mean z-index"
  ) +
  theme_minimal(base_size = 18) +     theme(plot.title   = element_text(size = 18, face = "bold"),           axis.title   = element_text(size = 16),           axis.text    = element_text(size = 14)) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.margin = margin(10, 30, 10, 10)
  ) +
  coord_cartesian(clip = "off")

p_zindex_pv
ggsave("figures/wave2/Fig_zindex_wave2_pv.pdf", p_zindex_pv, width = 9, height = 6)


# 13b. CLEANER VERSION: SIGNIFICANT BRACKETS ONLY, STACKED ABOVE ALL BARS

tests_z_sig <- tests_z %>%
  filter(p_label != "ns") %>%
  arrange(d, midx)

if (nrow(tests_z_sig) == 0) {
  p_zindex_clean <- ggplot(fig_zindex, aes(x = treatment_label, y = mean_z)) +
    geom_col(width = 0.7, fill = "#636363") +
    geom_errorbar(
      aes(
        ymin = mean_z - ci_z,
        ymax = mean_z + ci_z
      ),
      width = 0.15,
      linewidth = 0.5
    ) +
    labs(x = "", y = "Mean z-index") +
    theme_minimal(base_size = 18) +     theme(plot.title   = element_text(size = 18, face = "bold"),           axis.title   = element_text(size = 16),           axis.text    = element_text(size = 14)) +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      plot.margin  = margin(10, 30, 10, 10)
    ) +
    coord_cartesian(clip = "off")
  
  message("13b: no significant pairs — saving plain bar chart.")
  
} else {
  
  bar_top_z       <- max(fig_zindex$mean_z + fig_zindex$ci_z, na.rm = TRUE)
  bracket_start_z <- bar_top_z + 0.06
  bracket_step_z  <- 0.07
  
  tests_z_sig <- tests_z_sig %>%
    mutate(y.position = bracket_start_z + (row_number() - 1) * bracket_step_z)
  
  y_max_z <- max(tests_z_sig$y.position) + 0.08
  
  p_zindex_clean <- ggplot(fig_zindex, aes(x = treatment_label, y = mean_z)) +
    geom_col(width = 0.7, fill = "#636363") +
    geom_errorbar(
      aes(
        ymin = mean_z - ci_z,
        ymax = mean_z + ci_z
      ),
      width = 0.15,
      linewidth = 0.5
    ) +
    ggpubr::stat_pvalue_manual(
      tests_z_sig,
      label = "p_label",
      xmin  = "group1",
      xmax  = "group2",
      y.position = "y.position",
      tip.length = 0.005,
      size = 3
    ) +
    labs(x = "", y = "Mean z-index") +
    scale_y_continuous(limits = c(NA, y_max_z)) +
    theme_minimal(base_size = 18) +     theme(plot.title   = element_text(size = 18, face = "bold"),           axis.title   = element_text(size = 16),           axis.text    = element_text(size = 14)) +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      plot.margin  = margin(10, 30, 10, 10)
    ) +
    coord_cartesian(clip = "off")
}

# 13c. SAVE

p_zindex_clean
ggsave("figures/wave2/Fig_zindex_wave2_pv_clean.pdf",
       p_zindex_clean, width = 9, height = 6)

# -----------------------------------------------------------------------------
# 14. SUMMARY DATA: Z-INDEX BY TREATMENT X CORRECT RECOGNITION
# -----------------------------------------------------------------------------

fig_zindex_correct <- dt[, .(
  n = .N,
  mean_z = mean(z_index, na.rm = TRUE),
  sd_z   = sd(z_index, na.rm = TRUE)
), by = .(treatment, treatment_label, recognition_correct)]

fig_zindex_correct[, se_z := sd_z / sqrt(n)]
fig_zindex_correct[, ci_z := 1.96 * se_z]

fig_zindex_correct[, recognition_group := fifelse(
  recognition_correct == TRUE,  "Correct recognition",
  fifelse(recognition_correct == FALSE, "Incorrect recognition", NA_character_)
)]

fig_zindex_correct[, recognition_group := factor(
  recognition_group,
  levels = c("Correct recognition", "Incorrect recognition")
)]

fig_zindex_correct


# -----------------------------------------------------------------------------
# 15. FIGURE: Z-INDEX BY TREATMENT, BY CORRECT RECOGNITION
# -----------------------------------------------------------------------------

p_zindex_correct <- ggplot(
  fig_zindex_correct,
  aes(x = treatment_label, y = mean_z, fill = recognition_group)
) +
  geom_col(width = 0.7) +
  geom_errorbar(
    aes(
      ymin = mean_z - ci_z,
      ymax = mean_z + ci_z
    ),
    width = 0.15,
    linewidth = 0.5
  ) +
  facet_wrap(~ recognition_group) +
  scale_fill_manual(values = c(
    "Correct recognition" = "#1b9e77",
    "Incorrect recognition" = "#d95f02"
  )) +
  labs(
    x = "",
    y = "Mean z-index"
  ) +
  theme_minimal(base_size = 18) +     theme(plot.title   = element_text(size = 18, face = "bold"),           axis.title   = element_text(size = 16),           axis.text    = element_text(size = 14)) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.margin = margin(10, 30, 10, 10),
    legend.position = "none"
  ) +
  coord_cartesian(clip = "off")

p_zindex_correct
ggsave("figures/wave2/Fig_zindex_by_correct_recognition_wave2.pdf",
       p_zindex_correct, width = 10, height = 4.5)

# -----------------------------------------------------------------------------
# 15a. FIGURE: Z-INDEX BY TREATMENT, BY CORRECT RECOGNITION
#      (split into two separate plots, one file each)
# -----------------------------------------------------------------------------

p_zindex_correct_crt <- ggplot(
  fig_zindex_correct[recognition_group == "Correct recognition"],
  aes(x = treatment_label, y = mean_z, fill = recognition_group)
) +
  geom_col(width = 0.7) +
  geom_errorbar(
    aes(
      ymin = mean_z - ci_z,
      ymax = mean_z + ci_z
    ),
    width = 0.15,
    linewidth = 0.5
  ) +
  scale_fill_manual(values = c("Correct recognition" = "#1b9e77")) +
  labs(x = "", y = "Mean z-index", title = "Correct recognition") +
  theme_minimal(base_size = 18) +     theme(plot.title   = element_text(size = 18, face = "bold"),           axis.title   = element_text(size = 16),           axis.text    = element_text(size = 14)) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.margin  = margin(10, 15, 10, 10),
    legend.position = "none"
  ) +
  coord_cartesian(clip = "off")

p_zindex_correct_inc <- ggplot(
  fig_zindex_correct[recognition_group == "Incorrect recognition"],
  aes(x = treatment_label, y = mean_z, fill = recognition_group)
) +
  geom_col(width = 0.7) +
  geom_errorbar(
    aes(
      ymin = mean_z - ci_z,
      ymax = mean_z + ci_z
    ),
    width = 0.15,
    linewidth = 0.5
  ) +
  scale_fill_manual(values = c("Incorrect recognition" = "#d95f02")) +
  labs(x = "", y = "Mean z-index", title = "Incorrect recognition") +
  theme_minimal(base_size = 18) +     theme(plot.title   = element_text(size = 18, face = "bold"),           axis.title   = element_text(size = 16),           axis.text    = element_text(size = 14)) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.margin  = margin(10, 15, 10, 10),
    legend.position = "none"
  ) +
  coord_cartesian(clip = "off")

p_zindex_correct_crt
ggsave("figures/wave2/Fig_zindex_by_correct_recognition_wave2_split_correct.pdf",
       p_zindex_correct_crt, width = 9, height = 4.5)

p_zindex_correct_inc
ggsave("figures/wave2/Fig_zindex_by_correct_recognition_wave2_split_incorrect.pdf",
       p_zindex_correct_inc, width = 9, height = 4.5)

# -----------------------------------------------------------------------------
# 15b. FIGURE: Z-INDEX BY TREATMENT, BY CORRECT RECOGNITION
#       (with pairwise p-v within subgroup)
# -----------------------------------------------------------------------------

levs <- levels(fig_zindex_correct$treatment_label)
subgroups <- levels(fig_zindex_correct$recognition_group)

pair_df <- as.data.frame(t(combn(levs, 2)))
names(pair_df) <- c("group1", "group2")

tests_z_correct <- purrr::map_dfr(subgroups, function(s) {
  purrr::map_dfr(seq_len(nrow(pair_df)), function(i) {
    g1 <- pair_df$group1[i]
    g2 <- pair_df$group2[i]
    
    tst <- t.test(
      x = dt[treatment_label == g1 & recognition_correct == (s == "Correct recognition"), z_index],
      y = dt[treatment_label == g2 & recognition_correct == (s == "Correct recognition"), z_index]
    )
    
    a <- fig_zindex_correct[treatment_label == g1 & recognition_group == s]
    b <- fig_zindex_correct[treatment_label == g2 & recognition_group == s]
    
    tibble(
      recognition_group = s,
      group1 = g1,
      group2 = g2,
      p = tst$p.value,
      y_base = max(a$mean_z + a$ci_z, b$mean_z + b$ci_z),
      d = abs(match(g1, levs) - match(g2, levs)),
      midx = (match(g1, levs) + match(g2, levs)) / 2
    )
  })
})

tests_z_correct <- tests_z_correct %>%
  group_by(recognition_group) %>%
  mutate(
    p_adj = p.adjust(p, method = "holm"),
    p_label = case_when(
      p_adj < 0.01 ~ "***",
      p_adj < 0.05 ~ "**",
      p_adj < 0.10 ~ "*",
      TRUE ~ "ns"
    )
  ) %>%
  arrange(recognition_group, d, y_base, midx) %>%
  ungroup()

tests_z_correct <- tests_z_correct %>%
  group_by(recognition_group, d) %>%
  mutate(
    y.position = y_base + base_off[as.character(d)] + 0.02 * (row_number() - 1)
  ) %>%
  ungroup()

p_zindex_correct_pv <- ggplot(
  fig_zindex_correct,
  aes(x = treatment_label, y = mean_z, fill = recognition_group)
) +
  geom_col(width = 0.7) +
  geom_errorbar(
    aes(
      ymin = mean_z - ci_z,
      ymax = mean_z + ci_z
    ),
    width = 0.15,
    linewidth = 0.5
  ) +
  ggpubr::stat_pvalue_manual(
    tests_z_correct,
    label = "p_label",
    xmin = "group1",
    xmax = "group2",
    y.position = "y.position",
    tip.length = 0.01,
    size = 3
  ) +
  facet_wrap(~ recognition_group) +
  scale_fill_manual(values = c(
    "Correct recognition" = "#1b9e77",
    "Incorrect recognition" = "#d95f02"
  )) +
  labs(
    x = "",
    y = "Mean z-index"
  ) +
  theme_minimal(base_size = 18) +     theme(plot.title   = element_text(size = 18, face = "bold"),           axis.title   = element_text(size = 16),           axis.text    = element_text(size = 14)) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.margin = margin(10, 30, 10, 10),
    legend.position = "none"
  ) +
  coord_cartesian(clip = "off")

p_zindex_correct_pv
ggsave("figures/wave2/Fig_zindex_by_correct_recognition_wave2_pv.pdf",
       p_zindex_correct_pv, width = 10, height = 6)

# -----------------------------------------------------------------------------
# 15c. CLEANER VERSION: SIGNIFICANT BRACKETS ONLY, SPLIT, ONE FILE EACH
# -----------------------------------------------------------------------------

build_zindex_correct_panel <- function(subgroup, fill_colour) {
  
  tests_sub <- tests_z_correct %>%
    filter(recognition_group == subgroup, p_label != "ns") %>%
    arrange(d, midx)
  
  fig_sub <- fig_zindex_correct[recognition_group == subgroup]
  bar_top  <- max(fig_sub$mean_z + fig_sub$ci_z, na.rm = TRUE)
  
  p <- ggplot(fig_sub, aes(x = treatment_label, y = mean_z)) +
    geom_col(width = 0.7, fill = fill_colour) +
    geom_errorbar(
      aes(
        ymin = mean_z - ci_z,
        ymax = mean_z + ci_z
      ),
      width = 0.15,
      linewidth = 0.5
    )
  
  if (nrow(tests_sub) == 0) {
    y_max <- NA
    message(sprintf("15c: no significant pairs for '%s'.", subgroup))
  } else {
    tests_sub <- tests_sub %>%
      mutate(y.position = (bar_top + 0.06) + (row_number() - 1) * 0.07)
    y_max <- max(tests_sub$y.position) + 0.08
    p <- p +
      ggpubr::stat_pvalue_manual(
        tests_sub,
        label = "p_label",
        xmin  = "group1",
        xmax  = "group2",
        y.position = "y.position",
        tip.length = 0.005,
        size = 3
      )
  }
  
  p +
    scale_y_continuous(limits = c(NA, y_max)) +
    labs(x = "", y = "Mean z-index") + # , title = subgroup) +
    theme_minimal(base_size = 18) +     
    theme(plot.title   = element_text(size = 18, face = "bold"),           
          axis.title   = element_text(size = 16),           
          axis.text    = element_text(size = 14)) +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      plot.margin  = margin(10, 15, 10, 10),
      legend.position = "none"
    ) +
    coord_cartesian(clip = "off")
}

p_zindex_correct_crt_clean <- build_zindex_correct_panel(
  "Correct recognition",   "#1b9e77"
)
p_zindex_correct_inc_clean <- build_zindex_correct_panel(
  "Incorrect recognition", "#d95f02"
)

p_zindex_correct_crt_clean
ggsave("figures/wave2/Fig_zindex_by_correct_recognition_wave2_pv_clean_correct.pdf",
       p_zindex_correct_crt_clean, width = 9, height = 6)

p_zindex_correct_inc_clean
ggsave("figures/wave2/Fig_zindex_by_correct_recognition_wave2_pv_clean_incorrect.pdf",
       p_zindex_correct_inc_clean, width = 9, height = 6)

# -----------------------------------------------------------------------------
# 16. SUMMARY DATA: Z-INDEX BY TREATMENT X BELIEF IMAGE IS AI
# -----------------------------------------------------------------------------

fig_zindex_belief <- dt[, .(
  n = .N,
  mean_z = mean(z_index, na.rm = TRUE),
  sd_z   = sd(z_index, na.rm = TRUE)
), by = .(treatment, treatment_label, belief_image_ai)]

fig_zindex_belief[, se_z := sd_z / sqrt(n)]
fig_zindex_belief[, ci_z := 1.96 * se_z]

fig_zindex_belief[, belief_group := fifelse(
  belief_image_ai == TRUE,  "Believes image is AI",
  fifelse(belief_image_ai == FALSE, "Believes image is not AI", NA_character_)
)]

fig_zindex_belief[, belief_group := factor(
  belief_group,
  levels = c("Believes image is AI", "Believes image is not AI")
)]

fig_zindex_belief

# -----------------------------------------------------------------------------
# 17. FIGURE: Z-INDEX BY TREATMENT, BY BELIEF IMAGE IS AI
# -----------------------------------------------------------------------------

p_zindex_belief <- ggplot(
  fig_zindex_belief,
  aes(x = treatment_label, y = mean_z, fill = belief_group)
) +
  geom_col(width = 0.7) +
  geom_errorbar(
    aes(
      ymin = mean_z - ci_z,
      ymax = mean_z + ci_z
    ),
    width = 0.15,
    linewidth = 0.5
  ) +
  facet_wrap(~ belief_group) +
  scale_fill_manual(values = c(
    "Believes image is AI" = "#7570b3",
    "Believes image is not AI" = "#e6ab02"
  )) +
  labs(
    x = "",
    y = "Mean z-index"
  ) +
  theme_minimal(base_size = 18) +     theme(plot.title   = element_text(size = 18, face = "bold"),           axis.title   = element_text(size = 16),           axis.text    = element_text(size = 14)) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.margin = margin(10, 30, 10, 10),
    legend.position = "none"
  ) +
  coord_cartesian(clip = "off")

p_zindex_belief
ggsave("figures/wave2/Fig_zindex_by_beliefAI_wave2.pdf",
       p_zindex_belief, width = 10, height = 4.5)

# -----------------------------------------------------------------------------
# 17b. FIGURE: Z-INDEX BY TREATMENT, BY BELIEF IMAGE IS AI
#      (split into two separate plots, one file each)
# -----------------------------------------------------------------------------

p_zindex_belief_ai <- ggplot(
  fig_zindex_belief[belief_group == "Believes image is AI"],
  aes(x = treatment_label, y = mean_z, fill = belief_group)
) +
  geom_col(width = 0.7) +
  geom_errorbar(
    aes(
      ymin = mean_z - ci_z,
      ymax = mean_z + ci_z
    ),
    width = 0.15,
    linewidth = 0.5
  ) +
  scale_fill_manual(values = c("Believes image is AI" = "#7570b3")) +
  labs(x = "", y = "Mean z-index", title = "Believes image is AI") +
  theme_minimal(base_size = 18) +     theme(plot.title   = element_text(size = 18, face = "bold"),           axis.title   = element_text(size = 16),           axis.text    = element_text(size = 14)) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.margin  = margin(10, 15, 10, 10),
    legend.position = "none"
  ) +
  coord_cartesian(clip = "off")

p_zindex_belief_notai <- ggplot(
  fig_zindex_belief[belief_group == "Believes image is not AI"],
  aes(x = treatment_label, y = mean_z, fill = belief_group)
) +
  geom_col(width = 0.7) +
  geom_errorbar(
    aes(
      ymin = mean_z - ci_z,
      ymax = mean_z + ci_z
    ),
    width = 0.15,
    linewidth = 0.5
  ) +
  scale_fill_manual(values = c("Believes image is not AI" = "#e6ab02")) +
  labs(x = "", y = "Mean z-index", title = "Believes image is not AI") +
  theme_minimal(base_size = 18) +     theme(plot.title   = element_text(size = 18, face = "bold"),           axis.title   = element_text(size = 16),           axis.text    = element_text(size = 14)) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.margin  = margin(10, 15, 10, 10),
    legend.position = "none"
  ) +
  coord_cartesian(clip = "off")

p_zindex_belief_ai
ggsave("figures/wave2/Fig_zindex_by_beliefAI_wave2_split_AI.pdf",
       p_zindex_belief_ai, width = 9, height = 4.5)

p_zindex_belief_notai
ggsave("figures/wave2/Fig_zindex_by_beliefAI_wave2_split_notAI.pdf",
       p_zindex_belief_notai, width = 9, height = 4.5)

# -----------------------------------------------------------------------------
# 17a. FIGURE: Z-INDEX BY TREATMENT, BY BELIEF IMAGE IS AI
#       (with pairwise p-v within subgroup)
# -----------------------------------------------------------------------------

levs <- levels(fig_zindex_belief$treatment_label)
subgroups <- levels(fig_zindex_belief$belief_group)

pair_df <- as.data.frame(t(combn(levs, 2)))
names(pair_df) <- c("group1", "group2")

tests_z_belief <- purrr::map_dfr(subgroups, function(s) {
  purrr::map_dfr(seq_len(nrow(pair_df)), function(i) {
    g1 <- pair_df$group1[i]
    g2 <- pair_df$group2[i]
    
    tst <- t.test(
      x = dt[treatment_label == g1 & belief_image_ai == (s == "Believes image is AI"), z_index],
      y = dt[treatment_label == g2 & belief_image_ai == (s == "Believes image is AI"), z_index]
    )
    
    a <- fig_zindex_belief[treatment_label == g1 & belief_group == s]
    b <- fig_zindex_belief[treatment_label == g2 & belief_group == s]
    
    tibble(
      belief_group = s,
      group1 = g1,
      group2 = g2,
      p = tst$p.value,
      y_base = max(a$mean_z + a$ci_z, b$mean_z + b$ci_z),
      d = abs(match(g1, levs) - match(g2, levs)),
      midx = (match(g1, levs) + match(g2, levs)) / 2
    )
  })
})

tests_z_belief <- tests_z_belief %>%
  group_by(belief_group) %>%
  mutate(
    p_adj = p.adjust(p, method = "holm"),
    p_label = case_when(
      p_adj < 0.01 ~ "***",
      p_adj < 0.05 ~ "**",
      p_adj < 0.10 ~ "*",
      TRUE ~ "ns"
    )
  ) %>%
  arrange(belief_group, d, y_base, midx) %>%
  ungroup()

tests_z_belief <- tests_z_belief %>%
  group_by(belief_group, d) %>%
  mutate(
    y.position = y_base + base_off[as.character(d)] + 0.02 * (row_number() - 1)
  ) %>%
  ungroup()

p_zindex_belief_pv <- ggplot(
  fig_zindex_belief,
  aes(x = treatment_label, y = mean_z, fill = belief_group)
) +
  geom_col(width = 0.7) +
  geom_errorbar(
    aes(
      ymin = mean_z - ci_z,
      ymax = mean_z + ci_z
    ),
    width = 0.15,
    linewidth = 0.5
  ) +
  ggpubr::stat_pvalue_manual(
    tests_z_belief,
    label = "p_label",
    xmin = "group1",
    xmax = "group2",
    y.position = "y.position",
    tip.length = 0.01,
    size = 3
  ) +
  facet_wrap(~ belief_group) +
  scale_fill_manual(values = c(
    "Believes image is AI" = "#7570b3",
    "Believes image is not AI" = "#e6ab02"
  )) +
  labs(
    x = "",
    y = "Mean z-index"
  ) +
  theme_minimal(base_size = 18) +     theme(plot.title   = element_text(size = 18, face = "bold"),           axis.title   = element_text(size = 16),           axis.text    = element_text(size = 14)) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.margin = margin(10, 30, 10, 10),
    legend.position = "none"
  ) +
  coord_cartesian(clip = "off")

p_zindex_belief_pv
ggsave("figures/wave2/Fig_zindex_by_beliefAI_wave2_pv.pdf",
       p_zindex_belief_pv, width = 10, height = 6)

# -----------------------------------------------------------------------------
# 17c. CLEANER VERSION: SIGNIFICANT BRACKETS ONLY, SPLIT, ONE FILE EACH
# -----------------------------------------------------------------------------

build_zindex_belief_panel <- function(subgroup, fill_colour) {
  
  tests_sub <- tests_z_belief %>%
    filter(belief_group == subgroup, p_label != "ns") %>%
    arrange(d, midx)
  
  fig_sub <- fig_zindex_belief[belief_group == subgroup]
  bar_top  <- max(fig_sub$mean_z + fig_sub$ci_z, na.rm = TRUE)
  
  p <- ggplot(fig_sub, aes(x = treatment_label, y = mean_z)) +
    geom_col(width = 0.7, fill = fill_colour) +
    geom_errorbar(
      aes(
        ymin = mean_z - ci_z,
        ymax = mean_z + ci_z
      ),
      width = 0.15,
      linewidth = 0.5
    )
  
  if (nrow(tests_sub) == 0) {
    y_max <- NA
    message(sprintf("17c: no significant pairs for '%s'.", subgroup))
  } else {
    tests_sub <- tests_sub %>%
      mutate(y.position = (bar_top + 0.06) + (row_number() - 1) * 0.07)
    y_max <- max(tests_sub$y.position) + 0.08
    p <- p +
      ggpubr::stat_pvalue_manual(
        tests_sub,
        label = "p_label",
        xmin  = "group1",
        xmax  = "group2",
        y.position = "y.position",
        tip.length = 0.005,
        size = 3
      )
  }
  
  p +
    scale_y_continuous(limits = c(NA, y_max)) +
    labs(x = "", y = "Mean z-index") + #, title = subgroup) +
    theme_minimal(base_size = 18) +     
    theme(plot.title   = element_text(size = 18, face = "bold"),           
          axis.title   = element_text(size = 16),           
          axis.text    = element_text(size = 14)) +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      plot.margin  = margin(10, 15, 10, 10),
      legend.position = "none"
    ) +
    coord_cartesian(clip = "off")
}

p_zindex_belief_ai_clean <- build_zindex_belief_panel(
  "Believes image is AI",     "#7570b3"
)
p_zindex_belief_notai_clean <- build_zindex_belief_panel(
  "Believes image is not AI", "#e6ab02"
)

p_zindex_belief_ai_clean
ggsave("figures/wave2/Fig_zindex_by_beliefAI_wave2_pv_clean_AI.pdf",
       p_zindex_belief_ai_clean, width = 9, height = 6)

p_zindex_belief_notai_clean
ggsave("figures/wave2/Fig_zindex_by_beliefAI_wave2_pv_clean_notAI.pdf",
       p_zindex_belief_notai_clean, width = 9, height = 6)


# -----------------------------------------------------------------------------
# 18. BY-IMAGE VERSIONS OF THE 4 BELIEF-SUBGROUP FIGURES
#     No pairwise significance brackets (underpowered within image)
#     image_id: 1=Venezuela (P), 3=ICE (P), 4=basket,
#               5=kids-mindfulness, 7=homeless (P), 8=hurricane (P)
# -----------------------------------------------------------------------------

# -- image label lookup (neutral labels for display) --
image_labels <- c(
  "1" = "Article 1",
  "3" = "Article 3",
  "4" = "Article 4",
  "5" = "Article 5",
  "7" = "Article 7",
  "8" = "Article 8"
)

dt[, image_label := factor(image_labels[as.character(image_id)],
                           levels = image_labels)]

# -- 18a. Newsletter demand × belief_image_ai, by image --

fig_demand_belief_img <- dt[!is.na(belief_image_ai), .(
  n          = .N,
  mean_demand = mean(newsletter_takeup, na.rm = TRUE)
), by = .(treatment_label, belief_image_ai, image_label)]

fig_demand_belief_img[, se := sqrt(mean_demand * (1 - mean_demand) / n)]
fig_demand_belief_img[, ci := 1.96 * se]

fig_demand_belief_img[, belief_group := fifelse(
  belief_image_ai == TRUE, "Believes image is AI", "Believes image is not AI"
)]
fig_demand_belief_img[, belief_group := factor(
  belief_group,
  levels = c("Believes image is AI", "Believes image is not AI")
)]

make_demand_belief_byimg <- function(subgroup, fill_colour, filestem) {
  d <- fig_demand_belief_img[belief_group == subgroup]
  p <- ggplot(d, aes(x = treatment_label, y = mean_demand)) +
    geom_col(width = 0.7, fill = fill_colour) +
    geom_errorbar(
      aes(ymin = pmax(0, mean_demand - ci),
          ymax = pmin(1, mean_demand + ci)),
      width = 0.2, linewidth = 0.5
    ) +
    facet_wrap(~ image_label, nrow = 2) +
    scale_y_continuous(
      limits = c(0, 1),
      labels = scales::percent_format(accuracy = 1)
    ) +
    labs(x = "", y = "Prop. requesting newsletter") +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x  = element_text(angle = 45, hjust = 1, size = 10),
      axis.title   = element_text(size = 13),
      strip.text   = element_text(size = 12, face = "bold"),
      plot.margin  = margin(10, 15, 10, 10),
      legend.position = "none"
    )
  ggsave(
    sprintf("figures/wave2/%s.pdf", filestem),
    p, width = 14, height = 7
  )
  p
}

p_demand_belief_ai_img    <- make_demand_belief_byimg(
  "Believes image is AI",     "#7570b3",
  "Fig_newsletter_demand_by_beliefAI_wave2_byimage_AI"
)
p_demand_belief_notai_img <- make_demand_belief_byimg(
  "Believes image is not AI", "#e6ab02",
  "Fig_newsletter_demand_by_beliefAI_wave2_byimage_notAI"
)

# -- 18b. z-index × belief_image_ai, by image --

fig_zindex_belief_img <- dt[!is.na(belief_image_ai), .(
  n      = .N,
  mean_z = mean(z_index, na.rm = TRUE),
  sd_z   = sd(z_index,   na.rm = TRUE)
), by = .(treatment_label, belief_image_ai, image_label)]

fig_zindex_belief_img[, se := sd_z / sqrt(n)]
fig_zindex_belief_img[, ci := 1.96 * se]

fig_zindex_belief_img[, belief_group := fifelse(
  belief_image_ai == TRUE, "Believes image is AI", "Believes image is not AI"
)]
fig_zindex_belief_img[, belief_group := factor(
  belief_group,
  levels = c("Believes image is AI", "Believes image is not AI")
)]

make_zindex_belief_byimg <- function(subgroup, fill_colour, filestem) {
  d <- fig_zindex_belief_img[belief_group == subgroup]
  y_lo <- min(d$mean_z - d$ci, na.rm = TRUE)
  y_hi <- max(d$mean_z + d$ci, na.rm = TRUE)
  pad  <- (y_hi - y_lo) * 0.1
  p <- ggplot(d, aes(x = treatment_label, y = mean_z)) +
    geom_col(width = 0.7, fill = fill_colour) +
    geom_errorbar(
      aes(ymin = mean_z - ci, ymax = mean_z + ci),
      width = 0.2, linewidth = 0.5
    ) +
    facet_wrap(~ image_label, nrow = 2) +
    scale_y_continuous(limits = c(y_lo - pad, y_hi + pad)) +
    labs(x = "", y = "Mean z-index") +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x  = element_text(angle = 45, hjust = 1, size = 10),
      axis.title   = element_text(size = 13),
      strip.text   = element_text(size = 12, face = "bold"),
      plot.margin  = margin(10, 15, 10, 10),
      legend.position = "none"
    )
  ggsave(
    sprintf("figures/wave2/%s.pdf", filestem),
    p, width = 14, height = 7
  )
  p
}

p_zindex_belief_ai_img    <- make_zindex_belief_byimg(
  "Believes image is AI",     "#7570b3",
  "Fig_zindex_by_beliefAI_wave2_byimage_AI"
)
p_zindex_belief_notai_img <- make_zindex_belief_byimg(
  "Believes image is not AI", "#e6ab02",
  "Fig_zindex_by_beliefAI_wave2_byimage_notAI"
)


# -----------------------------------------------------------------------------
# 19. BY-IMAGE VERSIONS OF THE 4 RECOGNITION-SUBGROUP FIGURES
#     No pairwise significance brackets (underpowered within image)
#     image_id: 1=Venezuela (P), 3=ICE (P), 4=basket,
#               5=kids-mindfulness, 7=homeless (P), 8=hurricane (P)
# -----------------------------------------------------------------------------

# -- 19a. Newsletter demand × recognition_correct, by image --

fig_demand_correct_img <- dt[!is.na(recognition_correct), .(
  n           = .N,
  mean_demand = mean(newsletter_takeup, na.rm = TRUE)
), by = .(treatment_label, recognition_correct, image_label)]

fig_demand_correct_img[, se := sqrt(mean_demand * (1 - mean_demand) / n)]
fig_demand_correct_img[, ci := 1.96 * se]

fig_demand_correct_img[, recognition_group := fifelse(
  recognition_correct == TRUE, "Correct recognition", "Incorrect recognition"
)]
fig_demand_correct_img[, recognition_group := factor(
  recognition_group,
  levels = c("Correct recognition", "Incorrect recognition")
)]

make_demand_correct_byimg <- function(subgroup, fill_colour, filestem) {
  d <- fig_demand_correct_img[recognition_group == subgroup]
  p <- ggplot(d, aes(x = treatment_label, y = mean_demand)) +
    geom_col(width = 0.7, fill = fill_colour) +
    geom_errorbar(
      aes(ymin = pmax(0, mean_demand - ci),
          ymax = pmin(1, mean_demand + ci)),
      width = 0.2, linewidth = 0.5
    ) +
    facet_wrap(~ image_label, nrow = 2) +
    scale_y_continuous(
      limits = c(0, 1),
      labels = scales::percent_format(accuracy = 1)
    ) +
    labs(x = "", y = "Prop. requesting newsletter") +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x  = element_text(angle = 45, hjust = 1, size = 10),
      axis.title   = element_text(size = 13),
      strip.text   = element_text(size = 12, face = "bold"),
      plot.margin  = margin(10, 15, 10, 10),
      legend.position = "none"
    )
  ggsave(
    sprintf("figures/wave2/%s.pdf", filestem),
    p, width = 14, height = 7
  )
  p
}

p_demand_correct_img    <- make_demand_correct_byimg(
  "Correct recognition",   "#1b9e77",
  "Fig_newsletter_demand_by_recognitionAI_wave2_byimage_correct"
)
p_demand_incorrect_img  <- make_demand_correct_byimg(
  "Incorrect recognition", "#d95f02",
  "Fig_newsletter_demand_by_recognitionAI_wave2_byimage_incorrect"
)

# -- 19b. z-index × recognition_correct, by image --

fig_zindex_correct_img <- dt[!is.na(recognition_correct), .(
  n      = .N,
  mean_z = mean(z_index, na.rm = TRUE),
  sd_z   = sd(z_index,   na.rm = TRUE)
), by = .(treatment_label, recognition_correct, image_label)]

fig_zindex_correct_img[, se := sd_z / sqrt(n)]
fig_zindex_correct_img[, ci := 1.96 * se]

fig_zindex_correct_img[, recognition_group := fifelse(
  recognition_correct == TRUE, "Correct recognition", "Incorrect recognition"
)]
fig_zindex_correct_img[, recognition_group := factor(
  recognition_group,
  levels = c("Correct recognition", "Incorrect recognition")
)]

make_zindex_correct_byimg <- function(subgroup, fill_colour, filestem) {
  d <- fig_zindex_correct_img[recognition_group == subgroup]
  y_lo <- min(d$mean_z - d$ci, na.rm = TRUE)
  y_hi <- max(d$mean_z + d$ci, na.rm = TRUE)
  pad  <- (y_hi - y_lo) * 0.1
  p <- ggplot(d, aes(x = treatment_label, y = mean_z)) +
    geom_col(width = 0.7, fill = fill_colour) +
    geom_errorbar(
      aes(ymin = mean_z - ci, ymax = mean_z + ci),
      width = 0.2, linewidth = 0.5
    ) +
    facet_wrap(~ image_label, nrow = 2) +
    scale_y_continuous(limits = c(y_lo - pad, y_hi + pad)) +
    labs(x = "", y = "Mean z-index") +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x  = element_text(angle = 45, hjust = 1, size = 10),
      axis.title   = element_text(size = 13),
      strip.text   = element_text(size = 12, face = "bold"),
      plot.margin  = margin(10, 15, 10, 10),
      legend.position = "none"
    )
  ggsave(
    sprintf("figures/wave2/%s.pdf", filestem),
    p, width = 14, height = 7
  )
  p
}

p_zindex_correct_img   <- make_zindex_correct_byimg(
  "Correct recognition",   "#1b9e77",
  "Fig_zindex_by_recognitionAI_wave2_byimage_correct"
)
p_zindex_incorrect_img <- make_zindex_correct_byimg(
  "Incorrect recognition", "#d95f02",
  "Fig_zindex_by_recognitionAI_wave2_byimage_incorrect"
)

