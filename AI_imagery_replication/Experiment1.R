library(haven)
library(foreign)
library(plyr)
library(dplyr)
library(tidyr)
library(parallel)
library(lubridate)
library(openxlsx)
library(stringr)
# library(RecordLinkage)  # not used in this script (and pulls in a parallelly version with a known .onLoad bug). Re-enable if needed.
library(readxl)
library(dreamerr)
library(fixest)
library(tidyfast)
library(ggplot2)
library(janitor)
library(plotly)
library(data.table)
library(reshape2)
library(coefplot)
library("ggthemes")
library(gridExtra)
library(stargazer)
library(patchwork)
library(rstatix)
library(ggpubr)
library(scales)  
library(purrr)
library(readr)
library(xtable)
library(AER)        # ivreg
library(sandwich)   # robust vcov
library(lmtest)     # coeftest
library(car)        # linearHypothesis (robust joint F tests)
library(knitr)
library(kableExtra)
rm(list = ls())
# NOTE: working directory must be set to the replication-package root
# (the folder containing this script). run_all.R does this automatically.

##### #### #####
#### import ####
##### #### #####

data_new <- read.csv(file="DATA/AI labels - Full Study_October 19, 2025_15.58.csv")
data_final <- data_new[-(1:6),]

data_new2 <- read.csv(file="DATA/AI labels_October 15, 2025_21.02.csv")
data_final2 <- data_new2[-(1:17),]

data_final <- rbind(data_final, data_final2)

###### #### ######
#### cleaning ####
###### #### ######

data_final <- data_final %>% 
  rename(Gender = Q3.1, Age = Q3.2, Ethnicity = Q3.3, UKborn = Q3.4, Region = Q3.6, Income = Q24.2, Employment = Q3.7,
         Education = Q3.11, Political = Q6, Party = Q24.1, InterestNews = Q946, ConsumptionNews = Q947, Newsletter = Q948,
         ArticleTime1 = Q955_Page.Submit, ArticleTime2 = Q986_Page.Submit, ArticleTime3 = Q956_Page.Submit, ArticleTime4 = Q958_Page.Submit,
         TrustMedia = Q983, AIfamiliar = Q984, AIencounter = Q985, Demand = Q961, Manipulation = Q962, 
         Accurate = Q963, Trustworthy = Q964, Quality = Q965, Bias = Q966, Entertaining = Q967, 
         Complex = Q969, Researchertrust = Q970, Treatment = T)

data_final <- data_final %>% 
  select(Gender, Age, Ethnicity, UKborn, Region, Income, Employment, Education, Political, Party, InterestNews, ConsumptionNews, Newsletter, 
         ArticleTime1, ArticleTime2, ArticleTime3, ArticleTime4,
         TrustMedia, AIfamiliar, AIencounter, Demand, Manipulation, Accurate, Trustworthy, Quality, Bias, Entertaining, Complex, Researchertrust, Treatment)

## --- recodes to numeric scales (5 best -> 1 worst) ---

# Accurate / Trustworthy / Quality
map5 <- c("Very"=5, " "=NA) # not used; we’ll map explicitly below

data_final <- data_final %>%
  mutate(
    Accurate = case_when(
      Accurate == "Very accurate" ~ 5,
      Accurate == "Accurate" ~ 4,
      Accurate == "Somewhat accurate" ~ 3,
      Accurate == "Not accurate" ~ 2,
      Accurate == "Not accurate at all" ~ 1,
      TRUE ~ NA_real_
    ),
    Trustworthy = case_when(
      Trustworthy == "Very trustworthy" ~ 5,
      Trustworthy == "Trustworthy" ~ 4,
      Trustworthy == "Somewhat trustworthy" ~ 3,
      Trustworthy == "Not trustworthy" ~ 2,
      Trustworthy == "Not trustworthy at all" ~ 1,
      TRUE ~ NA_real_
    ),
    Quality = case_when(
      Quality == "Very high" ~ 5,
      Quality == "High" ~ 4,
      Quality == "Medium" ~ 3,
      Quality == "Low" ~ 2,
      Quality == "Very low" ~ 1,
      TRUE ~ NA_real_
    ),
    # Bias: left negative, neutral 0, right positive
    Bias = case_when(
      Bias == "Very left-wing bias"  ~ -2,
      Bias == "Left-wing bias"       ~ -1,
      Bias == "No bias/Neutral"      ~  0,
      Bias == "Right-wing bias"      ~  1,
      Bias == "Very right-wing bias" ~  2,
      TRUE ~ NA_real_
    ),
    Entertaining = case_when(
      Entertaining == "Very entertaining" ~ 5,
      Entertaining == "Entertaining" ~ 4,
      Entertaining == "Somewhat entertaining" ~ 3,
      Entertaining == "Not entertaining" ~ 2,
      Entertaining == "Not entertaining at all" ~ 1,
      TRUE ~ NA_real_
    ),
    Complex = case_when(
      Complex == "Very complex" ~ 5,
      Complex == "Complex" ~ 4,
      Complex == "Somewhat complex" ~ 3,
      Complex == "Not complex" ~ 2,
      Complex == "Not complex at all" ~ 1,
      TRUE ~ NA_real_
    ),
    Researchertrust = case_when(
      Researchertrust == "Very trustworthy" ~ 5,
      Researchertrust == "Trustworthy" ~ 4,
      Researchertrust == "Somewhat trustworthy" ~ 3,
      Researchertrust == "Not trustworthy" ~ 2,
      Researchertrust == "Not trustworthy at all" ~ 1,
      TRUE ~ NA_real_
    ),
    AIfamiliar = case_when(
      AIfamiliar == "Very familiar" ~ 5,
      AIfamiliar == "Fairly familiar" ~ 4,
      AIfamiliar == "Somewhat familiar" ~ 3,
      AIfamiliar == "Slightly familiar" ~ 2,
      AIfamiliar == "Not at all familiar" ~ 1,
      TRUE ~ NA_real_
    )
  )

data_final <- data_final %>%
  mutate(ArticleTime = coalesce(as.integer(ArticleTime1), as.integer(ArticleTime2), as.integer(ArticleTime3), as.integer(ArticleTime4)))
summary(data_final$ArticleTime)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  1.00   54.00   83.00   98.72  119.00 1662.00       1 

## --- robust binaries & treatment logic ---

data_final <- data_final %>%
  mutate(
    Demand = as.integer(Demand == "Yes"),
    Manipulation = as.integer(Manipulation == "Yes"),
    AI    = as.integer(Treatment %in% c("UAI", "LAI")),
    Label = as.integer(Treatment %in% c("C", "LAI")),
    OriginCorrect = as.integer( (Manipulation==1 & AI==1) | (Manipulation==0 & AI==0) ),
    UAI = as.integer(Treatment == "UAI"),
    LAI = as.integer(Treatment == "LAI"),
    ArticleTime = as.integer(ArticleTime),
    Reader = as.integer(ArticleTime > 83.00),
    HiglyAIfamiliar = as.integer(AIfamiliar %in% c(4,5)),
    AImediaencounterYes = as.integer(AIencounter == "Yes"),
  )

## --- binary mechanism dummies (consistent with scales above) ---

data_final <- data_final %>%
  mutate(
    NotAccurate      = as.integer(Accurate %in% c(1,2)),
    NotTrustworthy   = as.integer(Trustworthy %in% c(1,2)),
    LowQuality       = as.integer(Quality %in% c(1,2)),
    Biased           = as.integer(!is.na(Bias) & Bias != 0),
    NoEntertaining   = as.integer(Entertaining %in% c(1,2)),
    VComplex         = as.integer(Complex %in% c(4,5)),
    NoResearchertrust= as.integer(Researchertrust %in% c(1,2))
  )

## --- z-scores and index (Accurate, Trustworthy, Quality) ---

z_cols <- c("Accurate","Trustworthy","Quality","Bias","Complex","Researchertrust","Entertaining")

data_final <- data_final %>%
  mutate(
    across(all_of(z_cols), ~ as.numeric(.x), .names = "{.col}"),
    across(all_of(z_cols), ~ as.numeric(scale(.x)), .names = "z_{.col}")
  ) %>%
  mutate(
    z_index = rowMeans(pick(z_Accurate, z_Trustworthy, z_Quality), na.rm = TRUE)
  )


# ---------- BALANCE TABLE: group means + global p + min pairwise p ----------

# Make sure Treatment labels/order are set
data_final <- data_final %>%
  mutate(Treatment = dplyr::recode(Treatment,
                            "CP"  = "Unlabeled Control",   # CP = pure control
                            "C"   = "Labeled Control",     # C = control + label
                            "UAI" = "Unlabeled AI",
                            "LAI" = "Labeled AI")) %>%
  mutate(Treatment = factor(Treatment,
                            levels = c("Unlabeled Control",
                                       "Labeled Control",
                                       "Unlabeled AI",
                                       "Labeled AI")))

bal_vars <- c("Gender","Age","Ethnicity","UKborn","Region","Income",
              "Employment","Education","Political","Party",
              "InterestNews","ConsumptionNews", "ArticleTime", 
              "Manipulation", "OriginCorrect")

data.table(data_final)
setDT(data_final)

arm_levels <- levels(data_final$Treatment)  # c("Unlabeled Control","Labeled Control","Unlabeled AI","Labeled AI")

# helper: numeric-ish if at least half the non-NA entries coerce to numeric
is_numericish <- function(x) {
  x2 <- suppressWarnings(as.numeric(x))
  sum(!is.na(x2)) >= 0.5 * sum(!is.na(x))
}

balance_rows <- lapply(bal_vars, function(v) {
  x  <- data_final[[v]]
  dt <- data_final[!is.na(Treatment) & !is.na(x), .(Treatment, x)]
  if (nrow(dt) == 0L) {
    means <- setNames(as.list(rep(NA_real_, length(arm_levels))), arm_levels)
    return(c(list(Variable = v), means, list(Global_p = NA_real_, Min_pairwise_p = NA_real_)))
  }
  
  if (is_numericish(dt$x)) {
    # numeric means
    dt[, y := suppressWarnings(as.numeric(x))]
    # means by arm
    mns <- dt[, .(mean = mean(y, na.rm = TRUE)), by = Treatment]
    mns <- merge(data.table(Treatment = factor(arm_levels, levels = arm_levels)),
                 mns, by = "Treatment", all.x = TRUE, sort = TRUE)
    arm_means <- as.list(mns$mean); names(arm_means) <- arm_levels
    
    # global p (ANOVA / OLS F)
    Global_p <- tryCatch({
      fit <- lm(y ~ Treatment, data = dt)
      as.numeric(anova(fit)["Treatment", "Pr(>F)"])
    }, error = function(e) NA_real_)
    
    # pairwise t-tests (Holm)
    pw <- tryCatch({
      out <- pairwise.t.test(dt$y, dt$Treatment, p.adjust.method = "holm")
      suppressWarnings(min(out$p.value, na.rm = TRUE))
    }, warning = function(w) NA_real_, error = function(e) NA_real_)
    
    c(list(Variable = v), arm_means, list(Global_p = Global_p, Min_pairwise_p = pw))
    
  } else {
    # categorical: report share in overall modal category (quick, compact)
    fac <- factor(dt$x)
    if (nlevels(fac) <= 1L) {
      means <- setNames(as.list(rep(NA_real_, length(arm_levels))), arm_levels)
      return(c(list(Variable = v), means, list(Global_p = NA_real_, Min_pairwise_p = NA_real_)))
    }
    modal_lvl <- names(sort(table(fac), decreasing = TRUE))[1]
    dt[, flag := (fac == modal_lvl)]
    shr <- dt[, .(mean = mean(flag, na.rm = TRUE)), by = Treatment]
    shr <- merge(data.table(Treatment = factor(arm_levels, levels = arm_levels)),
                 shr, by = "Treatment", all.x = TRUE, sort = TRUE)
    arm_means <- as.list(shr$mean); names(arm_means) <- arm_levels
    
    # global p (chi-square across all levels×arms)
    Global_p <- tryCatch({
      suppressWarnings(chisq.test(table(fac, dt$Treatment))$p.value)
    }, warning = function(w) NA_real_, error = function(e) NA_real_)
    
    # pairwise tests (proportion test on modal share, Holm over pairs)
    levs <- arm_levels[arm_levels %in% dt$Treatment]
    cm   <- combn(levs, 2, simplify = FALSE)
    pvec <- vapply(cm, function(pr) {
      a <- dt[Treatment == pr[1], .(x = sum(flag), n = .N)]
      b <- dt[Treatment == pr[2], .(x = sum(flag), n = .N)]
      if (nrow(a) == 0L || nrow(b) == 0L) return(NA_real_)
      suppressWarnings(prop.test(x = c(a$x, b$x), n = c(a$n, b$n), correct = FALSE)$p.value)
    }, numeric(1L))
    pw_holm <- tryCatch(min(p.adjust(pvec, method = "holm"), na.rm = TRUE),
                        error = function(e) NA_real_)
    c(list(Variable = sprintf("%s (share of '%s')", v, modal_lvl)),
      arm_means, list(Global_p = Global_p, Min_pairwise_p = pw_holm))
  }
})

balance_dt <- rbindlist(lapply(balance_rows, as.list), fill = TRUE)

# order columns & round nicely
setcolorder(balance_dt, c("Variable", arm_levels, "Global_p", "Min_pairwise_p"))
for (nm in arm_levels) balance_dt[, (nm) := round(as.numeric(get(nm)), 3)]
balance_dt[, `Global_p` := round(as.numeric(Global_p), 3)] # global p-value: ANOVA for numeric-ish; chi-square for categorical
balance_dt[, `Min_pairwise_p` := round(as.numeric(Min_pairwise_p), 3)] # min pairwise t-test p-value (Holm-adjusted) across all arm pairs (useful for a quick sense of worst imbalance)

# preview
balance_dt[]

# export to LaTeX
dir.create("tables", showWarnings = FALSE)
xtab <- xtable(
  balance_dt,
  caption = "Balance Across Treatment Arms: Group Means, Global and Pairwise Tests",
  label   = "tab:balance_full",
  align   = c("l","l",rep("S[table-format=1.3]", length(arm_levels)), "S[table-format=1.3]","S[table-format=1.3]")
)
print(xtab,
      include.rownames = FALSE,
      caption.placement = "top",
      sanitize.text.function = identity,
      comment = FALSE,
      file = "tables/table_balance_full.tex")


# ---------- Census comparison ----------

summary_table <- data_final %>%
  summarize(
    Gender     = mean(str_to_lower(Gender) %in% c("female","f","woman","women"), na.rm = TRUE),
    Age        = mean(suppressWarnings(as.numeric(Age)), na.rm = TRUE),
    
    Ethnicity  = mean(str_detect(str_to_lower(Ethnicity), "\\bwhite\\b"), na.rm = TRUE),
    
    UKborn     = mean(
      # common yes/affirmatives OR birthplace within UK nations/labels
      str_to_lower(UKborn) %in% c("yes","y","1","uk","u.k.","british","great britain","gb",
                                  "england","scotland","wales","northern ireland") |
        str_detect(str_to_lower(UKborn), "united\\s*kingdom|\\buk\\b|britain|england|scotland|wales|northern\\s*ireland"),
      na.rm = TRUE
    ),
    
    Region     = mean(str_detect(str_to_lower(Region), "south\\s*east|southeast|se\\s*england"), na.rm = TRUE),
    
    Income     = {
      inc <- parse_number(Income)            # extracts digits like "£45,000" -> 45000
      mean(inc > 40000, na.rm = TRUE)
    },
    
    Employment = mean(str_detect(str_to_lower(Employment), "employ"), na.rm = TRUE),
    
    Education  = mean(str_detect(str_to_lower(Education),
                                 "degree|bachelor|ba\\b|bsc\\b|master|ma\\b|msc\\b|mphil|phd|doctor|university"),
                      na.rm = TRUE),
    
    Party      = mean(str_detect(str_to_lower(Party), "labou?r"), na.rm = TRUE) # matches "Labour" / "Labor"
  ) %>%
  # round shares to 3 decimals, age to 1
  mutate(
    across(c(Gender, Ethnicity, UKborn, Region, Income, Employment, Education, Party), ~round(.x, 3)),
    Age = round(Age, 1)
  )
summary_table

##### #### ######
#### results ####
##### #### ######

#### 1. manipulation worked #### 
# ---- Regression models ----

# --- baseline LPM (OLS) ---
m1_ols <- feols(Manipulation ~ AI*Label, data = data_final, vcov = "HC1")

# --- OLS with full controls ---
m2_ols <- feols(Manipulation ~ AI*Label +
                  Gender + Age + Ethnicity + UKborn + Region + Income + Employment +
                  Education + Political + Party + InterestNews + ConsumptionNews +
                  Newsletter + TrustMedia + AIfamiliar + AIencounter,
                data = data_final, vcov = "HC1")

# --- baseline Logit ---
m1_logit <- feglm(Manipulation ~ AI*Label, data = data_final, family = "logit", vcov = "HC1")

# --- Logit with full controls ---
m2_logit <- feglm(Manipulation ~ AI*Label +
                    Gender + Age + Ethnicity + UKborn + Region + Income + Employment +
                    Education + Political + Party + InterestNews + ConsumptionNews +
                    Newsletter + TrustMedia + AIfamiliar + AIencounter,
                  data = data_final, family = "logit", vcov = "HC1")

# --- Create directory for outputs  ---
dir.create("tables", showWarnings = FALSE)

# --- Display neatly ---
etable(m1_ols, m2_ols, m1_logit, m2_logit,
       headers = c("OLS", "OLS + Controls", "Logit", "Logit + Controls"),
       digits = 3,
       fitstat = c("pr2", "n"), 
       tex = TRUE,
       file = "tables/table_manipulation.tex")

# Manually create the Mean DV
mean_dv <- round(mean(data_final$Manipulation, na.rm = TRUE), 3)
mean_dv

meanMan_by_treatment <- data_final %>%
  group_by(Treatment) %>%                      # replace with the exact name of your treatment variable
  summarize(
    MeanDV = round(mean(Manipulation, na.rm = TRUE), 3),
    N = sum(!is.na(Manipulation))
  )
meanMan_by_treatment

meanAI_by_treatment <- data_final %>%
  group_by(Treatment) %>%                      # replace with the exact name of your treatment variable
  summarize(
    MeanDV = round(mean(OriginCorrect, na.rm = TRUE), 3),
    N = sum(!is.na(OriginCorrect))
  )
meanAI_by_treatment


# ---- Bar chart: Manipulation = 1 by Treatment (with stars & safe spacing) ----

# 0) Tiny helpers (self-contained)
space_brackets <- function(tests, base_off = c(`1`=0.08, `2`=0.16, `3`=0.24), within_step = 0.02) {
  tests %>%
    arrange(d, y_base, midx) %>%
    group_by(d) %>%
    mutate(y.position = y_base + base_off[as.character(d)] + within_step * (row_number() - 1)) %>%
    ungroup()
}
prepare_tests_tblstars <- function(tests) {
  tests %>%
    mutate(
      p_adj = p.adjust(p, method = "holm"),
      p_label = dplyr::case_when(
        p_adj < .01 ~ "***",
        p_adj < .05 ~ "**",
        p_adj < .10 ~ "*",
        TRUE        ~ "ns"
      )
    ) %>%
    dplyr::filter(p_label != "ns") %>%
    space_brackets()
}

# 1) Make sure Treatment labels/order are set
data_final <- data_final %>%
  mutate(Treatment = dplyr::recode(Treatment,
                            "CP"  = "Unlabeled Control",   # CP = pure control
                            "C"   = "Labeled Control",     # C = control + label
                            "UAI" = "Unlabeled AI",
                            "LAI" = "Labeled AI")) %>%
  mutate(Treatment = factor(Treatment,
                            levels = c("Unlabeled Control",
                                       "Labeled Control",
                                       "Unlabeled AI",
                                       "Labeled AI")))

# 2) Summary stats
sum_df <- data_final %>%
  filter(!is.na(Manipulation), !is.na(Treatment)) %>%
  group_by(Treatment) %>%
  summarise(
    n = n(),
    successes   = sum(Manipulation == 1),
    mean_manip  = successes / n,
    se          = sqrt(mean_manip * (1 - mean_manip) / n),
    ci          = 1.96 * se,
    .groups = "drop"
  )

# 3) All pairwise two-proportion z-tests
levs <- levels(data_final$Treatment)
pairs <- t(combn(levs, 2))
pair_df <- as.data.frame(pairs); names(pair_df) <- c("group1","group2")

get_counts <- function(g) {
  row <- sum_df %>% filter(Treatment == g)
  list(x=row$successes, n=row$n, mean=row$mean_manip,
       ytop=row$mean_manip + row$ci, idx=match(g, levs))
}

tests <- purrr::map_dfr(seq_len(nrow(pair_df)), function(i){
  g1 <- pair_df$group1[i]; g2 <- pair_df$group2[i]
  a <- get_counts(g1); b <- get_counts(g2)
  tst <- prop.test(x=c(a$x, b$x), n=c(a$n, b$n), correct=FALSE)
  tibble::tibble(
    group1=g1, group2=g2, p=tst$p.value,
    y_base = max(a$ytop, b$ytop),
    d = abs(a$idx - b$idx),
    midx = (a$idx + b$idx)/2
  )
})

# 4) Apply Holm stars + drop ns + stagger (safe if empty)
tests <- prepare_tests_tblstars(tests)

# 5) Plot (with fixed top and safe handling if no sig pairs)
y_max <- 0.70
bar_top <- max(sum_df$mean_manip + sum_df$ci, na.rm = TRUE) + 0.02
if (nrow(tests) > 0) {
  from <- range(tests$y.position, na.rm = TRUE)
  if (diff(from) > 0 && y_max > bar_top + 0.02) {
    tests$y.position <- scales::rescale(tests$y.position,
                                        from = from,
                                        to   = c(bar_top, y_max - 0.02))
  } else {
    tests$y.position <- pmin(pmax(tests$y.position, bar_top), y_max - 0.02)
  }
}

p_manip <- ggplot(sum_df, aes(x = Treatment, y = mean_manip)) +
  geom_col(width = 0.7, fill = "#2c7fb8") +
  geom_errorbar(aes(ymin = pmax(0, mean_manip - ci),
                    ymax = pmin(1, mean_manip + ci)),
                width = 0.15, linewidth = 0.5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, y_max)) +
  labs(x = "", y = "Prop. Believing Image was GAI") +
  theme_minimal(base_size = 12) +
  theme(plot.margin = margin(10, 30, 10, 10)) +
  coord_cartesian(clip = "off")

p_manip2 <- if (nrow(tests) == 0) {
  p_manip
} else {
  p_manip + ggpubr::stat_pvalue_manual(
    tests,
    label = "p_label",   # now holds stars
    xmin  = "group1",
    xmax  = "group2",
    y.position = "y.position",
    tip.length = 0.01,
    size = 2.9
  )
}
p_manip2
ggsave("figures/Fig_Manipulation.pdf", p_manip2, width = 7, height = 5)

# ---- Bar chart: OriginCorrect = 1 by Treatment (stars, overlap-safe) ----

# Helpers (define once; remove if already defined above)
space_brackets <- function(tests, base_off = c(`1`=0.08, `2`=0.16, `3`=0.24), within_step = 0.02) {
  tests %>%
    arrange(d, y_base, midx) %>%
    group_by(d) %>%
    mutate(y.position = y_base + base_off[as.character(d)] + within_step * (row_number() - 1)) %>%
    ungroup()
}
prepare_tests_tblstars <- function(tests) {
  tests %>%
    mutate(
      p_adj = p.adjust(p, method = "holm"),
      p_label = dplyr::case_when(
        p_adj < .01 ~ "***",
        p_adj < .05 ~ "**",
        p_adj < .10 ~ "*",
        TRUE        ~ "ns"
      )
    ) %>%
    dplyr::filter(p_label != "ns") %>%
    space_brackets()
}

# Summary stats per group
sum_df_corr <- data_final %>%
  filter(!is.na(OriginCorrect), !is.na(Treatment)) %>%
  group_by(Treatment) %>%
  summarise(
    n         = n(),
    successes = sum(OriginCorrect == 1),
    mean_corr = successes / n,
    se        = sqrt(mean_corr * (1 - mean_corr) / n),
    ci        = 1.96 * se,
    .groups   = "drop"
  )

# Pairwise proportion tests (self-contained)
levs <- levels(data_final$Treatment)
cm <- combn(levs, 2)
pair_df <- data.frame(group1 = cm[1,], group2 = cm[2,], stringsAsFactors = FALSE)

get_counts_corr <- function(g) {
  row <- sum_df_corr %>% filter(Treatment == g)
  list(x = row$successes, n = row$n, mean = row$mean_corr,
       ytop = row$mean_corr + row$ci, idx = match(g, levs))
}

tests_corr <- purrr::map_dfr(seq_len(nrow(pair_df)), function(i){
  g1 <- pair_df$group1[i]; g2 <- pair_df$group2[i]
  a <- get_counts_corr(g1); b <- get_counts_corr(g2)
  tst <- prop.test(x = c(a$x, b$x), n = c(a$n, b$n), correct = FALSE)
  tibble::tibble(
    group1 = g1, group2 = g2, p = tst$p.value,
    y_base = max(a$ytop, b$ytop),
    d      = abs(a$idx - b$idx),
    midx   = (a$idx + b$idx)/2
  )
})

# Holm stars + drop ns + stagger
tests_corr <- prepare_tests_tblstars(tests_corr)

# Plot (fixed top; safe when no significant pairs)
y_max_corr <- 0.70
bar_top_corr <- max(sum_df_corr$mean_corr + sum_df_corr$ci, na.rm = TRUE) + 0.02
if (nrow(tests_corr) > 0) {
  from_corr <- range(tests_corr$y.position, na.rm = TRUE)
  if (diff(from_corr) > 0 && y_max_corr > bar_top_corr + 0.02) {
    tests_corr$y.position <- scales::rescale(tests_corr$y.position,
                                             from = from_corr,
                                             to   = c(bar_top_corr, y_max_corr - 0.02))
  } else {
    tests_corr$y.position <- pmin(pmax(tests_corr$y.position, bar_top_corr), y_max_corr - 0.02)
  }
}

p_correct <- ggplot(sum_df_corr, aes(x = Treatment, y = mean_corr)) +
  geom_col(width = 0.7, fill = "#1b9e77") +
  geom_errorbar(aes(ymin = pmax(0, mean_corr - ci),
                    ymax = pmin(1, mean_corr + ci)),
                width = 0.15, linewidth = 0.5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, y_max_corr)) +
  labs(x = "", y = "Prop. Correctly Recognizing AI/Not") +
  theme_minimal(base_size = 12) +
  theme(plot.margin = margin(10, 30, 10, 10)) +
  coord_cartesian(clip = "off")

p_correct2 <- if (nrow(tests_corr) == 0) {
  p_correct
} else {
  p_correct + ggpubr::stat_pvalue_manual(
    tests_corr,
    label = "p_label",          # stars (***, **, *)
    xmin  = "group1",
    xmax  = "group2",
    y.position = "y.position",
    tip.length = 0.01,
    size = 2.9
  )
}
p_correct2
ggsave("figures/Fig_CorrectAI.pdf", p_correct2, width = 7, height = 5)

# ---- Bar chart: Reader = 1 by Treatment (stars, overlap-safe) ----

# Helpers (define once; remove if already defined above)
space_brackets <- function(tests, base_off = c(`1`=0.08, `2`=0.16, `3`=0.24), within_step = 0.02) {
  tests %>%
    arrange(d, y_base, midx) %>%
    group_by(d) %>%
    mutate(y.position = y_base + base_off[as.character(d)] + within_step * (row_number() - 1)) %>%
    ungroup()
}
prepare_tests_tblstars <- function(tests) {
  tests %>%
    mutate(
      p_adj = p.adjust(p, method = "holm"),
      p_label = dplyr::case_when(
        p_adj < .01 ~ "***",
        p_adj < .05 ~ "**",
        p_adj < .10 ~ "*",
        TRUE        ~ "ns"
      )
    ) %>%
    dplyr::filter(p_label != "ns") %>%
    space_brackets()
}

# Summary stats per group
sum_df_corr <- data_final %>%
  filter(!is.na(Reader), !is.na(Treatment)) %>%
  group_by(Treatment) %>%
  summarise(
    n         = n(),
    successes = sum(Reader == 1),
    mean_corr = successes / n,
    se        = sqrt(mean_corr * (1 - mean_corr) / n),
    ci        = 1.96 * se,
    .groups   = "drop"
  )

# Pairwise proportion tests (self-contained)
levs <- levels(data_final$Treatment)
cm <- combn(levs, 2)
pair_df <- data.frame(group1 = cm[1,], group2 = cm[2,], stringsAsFactors = FALSE)

get_counts_corr <- function(g) {
  row <- sum_df_corr %>% filter(Treatment == g)
  list(x = row$successes, n = row$n, mean = row$mean_corr,
       ytop = row$mean_corr + row$ci, idx = match(g, levs))
}

tests_corr <- purrr::map_dfr(seq_len(nrow(pair_df)), function(i){
  g1 <- pair_df$group1[i]; g2 <- pair_df$group2[i]
  a <- get_counts_corr(g1); b <- get_counts_corr(g2)
  tst <- prop.test(x = c(a$x, b$x), n = c(a$n, b$n), correct = FALSE)
  tibble::tibble(
    group1 = g1, group2 = g2, p = tst$p.value,
    y_base = max(a$ytop, b$ytop),
    d      = abs(a$idx - b$idx),
    midx   = (a$idx + b$idx)/2
  )
})

# Holm stars + drop ns + stagger
tests_corr <- prepare_tests_tblstars(tests_corr)

# Plot (fixed top; safe when no significant pairs)
y_max_corr <- 0.70
bar_top_corr <- max(sum_df_corr$mean_corr + sum_df_corr$ci, na.rm = TRUE) + 0.02
if (nrow(tests_corr) > 0) {
  from_corr <- range(tests_corr$y.position, na.rm = TRUE)
  if (diff(from_corr) > 0 && y_max_corr > bar_top_corr + 0.02) {
    tests_corr$y.position <- scales::rescale(tests_corr$y.position,
                                             from = from_corr,
                                             to   = c(bar_top_corr, y_max_corr - 0.02))
  } else {
    tests_corr$y.position <- pmin(pmax(tests_corr$y.position, bar_top_corr), y_max_corr - 0.02)
  }
}

p_read <- ggplot(sum_df_corr, aes(x = Treatment, y = mean_corr)) +
  geom_col(width = 0.7, fill = "#5c5e99") +
  geom_errorbar(aes(ymin = pmax(0, mean_corr - ci),
                    ymax = pmin(1, mean_corr + ci)),
                width = 0.15, linewidth = 0.5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, y_max_corr)) +
  labs(x = "", y = "Prop. above Median Reader") +
  theme_minimal(base_size = 12) +
  theme(plot.margin = margin(10, 30, 10, 10)) +
  coord_cartesian(clip = "off")

p_read2 <- if (nrow(tests_corr) == 0) {
  p_read
} else {
  p_read + ggpubr::stat_pvalue_manual(
    tests_corr,
    label = "p_label",          # stars (***, **, *)
    xmin  = "group1",
    xmax  = "group2",
    y.position = "y.position",
    tip.length = 0.01,
    size = 2.9
  )
}
p_read2
ggsave("figures/Fig_Reader.pdf", p_read2, width = 7, height = 5)

# ---- Bar chart: HighlyAIfamiliar = 1 by Treatment (stars, overlap-safe) ----

# Helpers (define once; remove if already defined above)
space_brackets <- function(tests, base_off = c(`1`=0.08, `2`=0.16, `3`=0.24), within_step = 0.02) {
  tests %>%
    arrange(d, y_base, midx) %>%
    group_by(d) %>%
    mutate(y.position = y_base + base_off[as.character(d)] + within_step * (row_number() - 1)) %>%
    ungroup()
}
prepare_tests_tblstars <- function(tests) {
  tests %>%
    mutate(
      p_adj = p.adjust(p, method = "holm"),
      p_label = dplyr::case_when(
        p_adj < .01 ~ "***",
        p_adj < .05 ~ "**",
        p_adj < .10 ~ "*",
        TRUE        ~ "ns"
      )
    ) %>%
    dplyr::filter(p_label != "ns") %>%
    space_brackets()
}

# Summary stats per group
sum_df_corr <- data_final %>%
  filter(!is.na(HiglyAIfamiliar), !is.na(Treatment)) %>%
  group_by(Treatment) %>%
  summarise(
    n         = n(),
    successes = sum(HiglyAIfamiliar == 1),
    mean_corr = successes / n,
    se        = sqrt(mean_corr * (1 - mean_corr) / n),
    ci        = 1.96 * se,
    .groups   = "drop"
  )

# Pairwise proportion tests (self-contained)
levs <- levels(data_final$Treatment)
cm <- combn(levs, 2)
pair_df <- data.frame(group1 = cm[1,], group2 = cm[2,], stringsAsFactors = FALSE)

get_counts_corr <- function(g) {
  row <- sum_df_corr %>% filter(Treatment == g)
  list(x = row$successes, n = row$n, mean = row$mean_corr,
       ytop = row$mean_corr + row$ci, idx = match(g, levs))
}

tests_corr <- purrr::map_dfr(seq_len(nrow(pair_df)), function(i){
  g1 <- pair_df$group1[i]; g2 <- pair_df$group2[i]
  a <- get_counts_corr(g1); b <- get_counts_corr(g2)
  tst <- prop.test(x = c(a$x, b$x), n = c(a$n, b$n), correct = FALSE)
  tibble::tibble(
    group1 = g1, group2 = g2, p = tst$p.value,
    y_base = max(a$ytop, b$ytop),
    d      = abs(a$idx - b$idx),
    midx   = (a$idx + b$idx)/2
  )
})

# Holm stars + drop ns + stagger
tests_corr <- prepare_tests_tblstars(tests_corr)

# Plot (fixed top; safe when no significant pairs)
y_max_corr <- 0.70
bar_top_corr <- max(sum_df_corr$mean_corr + sum_df_corr$ci, na.rm = TRUE) + 0.02
if (nrow(tests_corr) > 0) {
  from_corr <- range(tests_corr$y.position, na.rm = TRUE)
  if (diff(from_corr) > 0 && y_max_corr > bar_top_corr + 0.02) {
    tests_corr$y.position <- scales::rescale(tests_corr$y.position,
                                             from = from_corr,
                                             to   = c(bar_top_corr, y_max_corr - 0.02))
  } else {
    tests_corr$y.position <- pmin(pmax(tests_corr$y.position, bar_top_corr), y_max_corr - 0.02)
  }
}

p_aifamiliar <- ggplot(sum_df_corr, aes(x = Treatment, y = mean_corr)) +
  geom_col(width = 0.7, fill = "#9c4e99") +
  geom_errorbar(aes(ymin = pmax(0, mean_corr - ci),
                    ymax = pmin(1, mean_corr + ci)),
                width = 0.15, linewidth = 0.5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, y_max_corr)) +
  labs(x = "", y = "Prop. above Median HiglyAIfamiliar") +
  theme_minimal(base_size = 12) +
  theme(plot.margin = margin(10, 30, 10, 10)) +
  coord_cartesian(clip = "off")

p_aifamiliar2 <- if (nrow(tests_corr) == 0) {
  p_aifamiliar
} else {
  p_aifamiliar + ggpubr::stat_pvalue_manual(
    tests_corr,
    label = "p_label",          # stars (***, **, *)
    xmin  = "group1",
    xmax  = "group2",
    y.position = "y.position",
    tip.length = 0.01,
    size = 2.9
  )
}
p_aifamiliar2
ggsave("figures/Fig_HiglyAIfamiliar.pdf", p_aifamiliar2, width = 7, height = 5)


# ---- Bar chart: AImediaencounterYes = 1 by Treatment (stars, overlap-safe) ----

# Helpers (define once; remove if already defined above)
space_brackets <- function(tests, base_off = c(`1`=0.08, `2`=0.16, `3`=0.24), within_step = 0.02) {
  tests %>%
    arrange(d, y_base, midx) %>%
    group_by(d) %>%
    mutate(y.position = y_base + base_off[as.character(d)] + within_step * (row_number() - 1)) %>%
    ungroup()
}
prepare_tests_tblstars <- function(tests) {
  tests %>%
    mutate(
      p_adj = p.adjust(p, method = "holm"),
      p_label = dplyr::case_when(
        p_adj < .01 ~ "***",
        p_adj < .05 ~ "**",
        p_adj < .10 ~ "*",
        TRUE        ~ "ns"
      )
    ) %>%
    dplyr::filter(p_label != "ns") %>%
    space_brackets()
}

# Summary stats per group
sum_df_corr <- data_final %>%
  filter(!is.na(AImediaencounterYes), !is.na(Treatment)) %>%
  group_by(Treatment) %>%
  summarise(
    n         = n(),
    successes = sum(AImediaencounterYes == 1),
    mean_corr = successes / n,
    se        = sqrt(mean_corr * (1 - mean_corr) / n),
    ci        = 1.96 * se,
    .groups   = "drop"
  )

# Pairwise proportion tests (self-contained)
levs <- levels(data_final$Treatment)
cm <- combn(levs, 2)
pair_df <- data.frame(group1 = cm[1,], group2 = cm[2,], stringsAsFactors = FALSE)

get_counts_corr <- function(g) {
  row <- sum_df_corr %>% filter(Treatment == g)
  list(x = row$successes, n = row$n, mean = row$mean_corr,
       ytop = row$mean_corr + row$ci, idx = match(g, levs))
}

tests_corr <- purrr::map_dfr(seq_len(nrow(pair_df)), function(i){
  g1 <- pair_df$group1[i]; g2 <- pair_df$group2[i]
  a <- get_counts_corr(g1); b <- get_counts_corr(g2)
  tst <- prop.test(x = c(a$x, b$x), n = c(a$n, b$n), correct = FALSE)
  tibble::tibble(
    group1 = g1, group2 = g2, p = tst$p.value,
    y_base = max(a$ytop, b$ytop),
    d      = abs(a$idx - b$idx),
    midx   = (a$idx + b$idx)/2
  )
})

# Holm stars + drop ns + stagger
tests_corr <- prepare_tests_tblstars(tests_corr)

# Plot (fixed top; safe when no significant pairs)
y_max_corr <- 0.70
bar_top_corr <- max(sum_df_corr$mean_corr + sum_df_corr$ci, na.rm = TRUE) + 0.02
if (nrow(tests_corr) > 0) {
  from_corr <- range(tests_corr$y.position, na.rm = TRUE)
  if (diff(from_corr) > 0 && y_max_corr > bar_top_corr + 0.02) {
    tests_corr$y.position <- scales::rescale(tests_corr$y.position,
                                             from = from_corr,
                                             to   = c(bar_top_corr, y_max_corr - 0.02))
  } else {
    tests_corr$y.position <- pmin(pmax(tests_corr$y.position, bar_top_corr), y_max_corr - 0.02)
  }
}

p_aimediaecounter <- ggplot(sum_df_corr, aes(x = Treatment, y = mean_corr)) +
  geom_col(width = 0.7, fill = "#2c4e11") +
  geom_errorbar(aes(ymin = pmax(0, mean_corr - ci),
                    ymax = pmin(1, mean_corr + ci)),
                width = 0.15, linewidth = 0.5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, y_max_corr)) +
  labs(x = "", y = "Prop. above Median HiglyAIfamiliar") +
  theme_minimal(base_size = 12) +
  theme(plot.margin = margin(10, 30, 10, 10)) +
  coord_cartesian(clip = "off")

p_aimediaecounter2 <- if (nrow(tests_corr) == 0) {
  p_aimediaecounter
} else {
  p_aimediaecounter + ggpubr::stat_pvalue_manual(
    tests_corr,
    label = "p_label",          # stars (***, **, *)
    xmin  = "group1",
    xmax  = "group2",
    y.position = "y.position",
    tip.length = 0.01,
    size = 2.9
  )
}
p_aimediaecounter2
ggsave("figures/Fig_AImediaencounterYes.pdf", p_aimediaecounter2, width = 7, height = 5)

#### 2. demand effects are significant for sub-sample who correctly recognize AI/non-AI #### 

# ---- Regression models ----
testAI <- t.test(Demand ~ AI, data_final)
testAI
testLabel <- t.test(Demand ~ Label, data_final)
testLabel

# --- baseline LPM (OLS) ---
d1_ols <- feols(Demand ~ AI*Label, data = data_final, vcov = "HC1")

# --- OLS with full controls ---
d2_ols <- feols(Demand ~ AI*Label +
                  Gender + Age + Ethnicity + UKborn + Region + Income + Employment +
                  Education + Political + Party + InterestNews + ConsumptionNews +
                  Newsletter + TrustMedia + AIfamiliar + AIencounter,
                data = data_final, vcov = "HC1")

# --- baseline Logit ---
d1_logit <- feglm(Demand ~ AI*Label, data = data_final, family = "logit", vcov = "HC1")

# --- Logit with full controls ---
d2_logit <- feglm(Demand ~ AI*Label +
                    Gender + Age + Ethnicity + UKborn + Region + Income + Employment +
                    Education + Political + Party + InterestNews + ConsumptionNews +
                    Newsletter + TrustMedia + AIfamiliar + AIencounter,
                  data = data_final, family = "logit", vcov = "HC1")

# --- heterogeneous LPM (OLS) ---
d1_ols_ht <- feols(Demand ~ AI*Label*OriginCorrect, data = data_final, vcov = "HC1")

# --- heterogeneous OLS with full controls ---
d2_ols_ht <- feols(Demand ~ AI*Label*OriginCorrect +
                  Gender + Age + Ethnicity + UKborn + Region + Income + Employment +
                  Education + Political + Party + InterestNews + ConsumptionNews +
                  Newsletter + TrustMedia + AIfamiliar + AIencounter,
                data = data_final, vcov = "HC1")

# --- heterogeneous Logit ---
d1_logit_ht <- feglm(Demand ~ AI*Label*OriginCorrect, data = data_final, family = "logit", vcov = "HC1")

# --- heterogeneous Logit with full controls ---
d2_logit_ht <- feglm(Demand ~ AI*Label*OriginCorrect +
                    Gender + Age + Ethnicity + UKborn + Region + Income + Employment +
                    Education + Political + Party + InterestNews + ConsumptionNews +
                    Newsletter + TrustMedia + AIfamiliar + AIencounter,
                  data = data_final, family = "logit", vcov = "HC1")

# --- Display neatly ---
etable(d1_ols, d2_ols, d1_logit, d2_logit, d1_ols_ht, d2_ols_ht, d1_logit_ht, d2_logit_ht,
       #headers = c("OLS", "OLS + Controls", "Logit", "Logit + Controls"),
       digits = 3,
       fitstat = c("pr2", "n"), 
       tex = TRUE,
       file = "tables/table_newsdemand.tex")

# --- EXTRA: OLS with MORE controls ---
d2_olse <- feols(Demand ~ AI*Label + ArticleTime +
                  Gender + Age + Ethnicity + UKborn + Region + Income + Employment +
                  Education + Political + Party + InterestNews + ConsumptionNews +
                  Newsletter + TrustMedia + AIfamiliar + AIencounter,
                data = data_final, vcov = "HC1")

# --- EXTRA: Logit with MORE controls ---
d2_logite <- feglm(Demand ~ AI*Label + ArticleTime +
                    Gender + Age + Ethnicity + UKborn + Region + Income + Employment +
                    Education + Political + Party + InterestNews + ConsumptionNews +
                    Newsletter + TrustMedia + AIfamiliar + AIencounter,
                  data = data_final, family = "logit", vcov = "HC1")

# --- EXTRA: heterogeneous OLS with MORE controls ---
d2_ols_hte <- feols(Demand ~ AI*Label*OriginCorrect + ArticleTime +
                     Gender + Age + Ethnicity + UKborn + Region + Income + Employment +
                     Education + Political + Party + InterestNews + ConsumptionNews +
                     Newsletter + TrustMedia + AIfamiliar + AIencounter,
                   data = data_final, vcov = "HC1")

# --- EXTRA: heterogeneous Logit with MORE controls ---
d2_logit_hte <- feglm(Demand ~ AI*Label*OriginCorrect  + ArticleTime +
                       Gender + Age + Ethnicity + UKborn + Region + Income + Employment +
                       Education + Political + Party + InterestNews + ConsumptionNews +
                       Newsletter + TrustMedia + AIfamiliar + AIencounter,
                     data = data_final, family = "logit", vcov = "HC1")

# --- Display neatly ---
etable(d1_ols, d2_olse, d1_logit, d2_logite, d1_ols_ht, d2_ols_hte, d1_logit_ht, d2_logit_hte,
       #headers = c("OLS", "OLS + Controls", "Logit", "Logit + Controls"),
       digits = 3,
       fitstat = c("pr2", "n"), 
       tex = TRUE,
       file = "tables/table_newsdemand_v2.tex")

# Manually create the Mean DV
mean_dv <- round(mean(data_final$Demand, na.rm = TRUE), 3)
mean_dv

meanD_by_treatment <- data_final %>%
  group_by(Treatment) %>%                      # replace with the exact name of your treatment variable
  summarize(
    MeanDV = round(mean(Demand, na.rm = TRUE), 3),
    N = sum(!is.na(Demand))
  )
meanD_by_treatment


# --- correct ai LPM (OLS) ---
d1_ols_corr <- feols(Demand ~ AI*Label, data = subset(data_final, OriginCorrect == 1), vcov = "HC1")

# --- correct ai OLS with full controls ---
d2_ols_corr <- feols(Demand ~ AI*Label +
                     Gender + Age + Ethnicity + UKborn + Region + Income + Employment +
                     Education + Political + Party + InterestNews + ConsumptionNews +
                     Newsletter + TrustMedia + AIfamiliar + AIencounter,
                     data = subset(data_final, OriginCorrect == 1), vcov = "HC1")

# --- Extra: correct ai OLS with MORE controls ---
d2_ols_corre <- feols(Demand ~ AI*Label + ArticleTime +
                       Gender + Age + Ethnicity + UKborn + Region + Income + Employment +
                       Education + Political + Party + InterestNews + ConsumptionNews +
                       Newsletter + TrustMedia + AIfamiliar + AIencounter,
                     data = subset(data_final, OriginCorrect == 1), vcov = "HC1")

# --- correct ai Logit ---
d1_logit_corr<- feglm(Demand ~ AI*Label, data = subset(data_final, OriginCorrect == 1), family = "logit", vcov = "HC1")

# --- correct ai Logit with full controls ---
d2_logit_corr <- feglm(Demand ~ AI*Label +
                       Gender + Age + Ethnicity + UKborn + Region + Income + Employment +
                       Education + Political + Party + InterestNews + ConsumptionNews +
                       Newsletter + TrustMedia + AIfamiliar + AIencounter,
                       data = subset(data_final, OriginCorrect == 1), family = "logit", vcov = "HC1")

# --- Extra: correct ai Logit with MORE controls ---
d2_logit_corre <- feglm(Demand ~ AI*Label  + ArticleTime +
                         Gender + Age + Ethnicity + UKborn + Region + Income + Employment +
                         Education + Political + Party + InterestNews + ConsumptionNews +
                         Newsletter + TrustMedia + AIfamiliar + AIencounter,
                       data = subset(data_final, OriginCorrect == 1), family = "logit", vcov = "HC1")


# --- incorrect ai LPM (OLS) ---
d1_ols_incorr <- feols(Demand ~ AI*Label, data = subset(data_final, OriginCorrect == 0), vcov = "HC1")

# --- incorrect ai OLS with full controls ---
d2_ols_incorr <- feols(Demand ~ AI*Label +
                       Gender + Age + Ethnicity + UKborn + Region + Income + Employment +
                       Education + Political + Party + InterestNews + ConsumptionNews +
                       Newsletter + TrustMedia + AIfamiliar + AIencounter,
                       data = subset(data_final, OriginCorrect == 0), vcov = "HC1")

# --- Extra: incorrect ai OLS with MORE controls ---
d2_ols_incorre <- feols(Demand ~ AI*Label + ArticleTime +
                         Gender + Age + Ethnicity + UKborn + Region + Income + Employment +
                         Education + Political + Party + InterestNews + ConsumptionNews +
                         Newsletter + TrustMedia + AIfamiliar + AIencounter,
                       data = subset(data_final, OriginCorrect == 0), vcov = "HC1")

# --- incorrect ai Logit ---
d1_logit_incorr<- feglm(Demand ~ AI*Label, data = subset(data_final, OriginCorrect == 0), family = "logit", vcov = "HC1")

# --- correct ai Logit with full controls ---
d2_logit_incorr <- feglm(Demand ~ AI*Label +
                         Gender + Age + Ethnicity + UKborn + Region + Income + Employment +
                         Education + Political + Party + InterestNews + ConsumptionNews +
                         Newsletter + TrustMedia + AIfamiliar + AIencounter,
                         data = subset(data_final, OriginCorrect == 0), family = "logit", vcov = "HC1")

# --- Extra: correct ai Logit with More controls ---
d2_logit_incorre <- feglm(Demand ~ AI*Label  + ArticleTime +
                           Gender + Age + Ethnicity + UKborn + Region + Income + Employment +
                           Education + Political + Party + InterestNews + ConsumptionNews +
                           Newsletter + TrustMedia + AIfamiliar + AIencounter,
                         data = subset(data_final, OriginCorrect == 0), family = "logit", vcov = "HC1")

# --- Display neatly ---
etable(d1_ols_corr, d2_ols_corr, d1_logit_corr, d2_logit_corr, d1_ols_incorr, d2_ols_incorr, d1_logit_incorr, d2_logit_incorr,
       #headers = c("OLS", "OLS + Controls", "Logit", "Logit + Controls"),
       digits = 3,
       fitstat = c("pr2", "n"), 
       tex = TRUE,
       file = "tables/table_newsdemand_split.tex")


# --- Display neatly ---
etable(d1_ols_corr, d2_ols_corre, d1_logit_corr, d2_logit_corre, d1_ols_incorr, d2_ols_incorre, d1_logit_incorr, d2_logit_incorre,
       #headers = c("OLS", "OLS + Controls", "Logit", "Logit + Controls"),
       digits = 3,
       fitstat = c("pr2", "n"), 
       tex = TRUE,
       file = "tables/table_newsdemand_split_v2.tex")

# ---------- Demand: three separate graphs (same style as manipulation) ----------
# ---- helpers (match manipulation-graph logic) ----

demand_summary_and_tests <- function(df) {
  # summary by treatment
  sum_df <- df %>%
    filter(!is.na(Demand), !is.na(Treatment)) %>%
    group_by(Treatment) %>%
    summarise(
      n = n(),
      successes = sum(Demand == 1),
      mean_d = successes / n,
      se = sqrt(mean_d * (1 - mean_d) / n),
      ci = 1.96 * se,
      .groups = "drop"
    )
  
  # all pairwise tests with staggered bracket heights
  levs <- levels(df$Treatment)
  levs <- levs[levs %in% sum_df$Treatment]       # keep existing
  cm <- combn(levs, 2)
  pair_df <- data.frame(group1 = cm[1,], group2 = cm[2,], stringsAsFactors = FALSE)
  
  get_counts <- function(g) {
    row <- sum_df %>% filter(Treatment == g)
    list(x = row$successes, n = row$n,
         mean = row$mean_d, ytop = row$mean_d + row$ci,
         idx = match(g, levs))
  }
  
  tests <- do.call(rbind, lapply(seq_len(nrow(pair_df)), function(i) {
    g1 <- pair_df$group1[i]; g2 <- pair_df$group2[i]
    a <- get_counts(g1); b <- get_counts(g2)
    tst <- prop.test(x = c(a$x, b$x), n = c(a$n, b$n), correct = FALSE)
    data.frame(
      group1 = g1,
      group2 = g2,
      p = tst$p.value,
      y_base = max(a$ytop, b$ytop),
      d = abs(a$idx - b$idx),                 # distance 1/2/3
      midx = (a$idx + b$idx)/2,
      stringsAsFactors = FALSE
    )
  }))
  
  tests <- tests %>%
    mutate(
      p_adj = p.adjust(p, method = "holm"),
      p_label = ifelse(p_adj < .001, "p < 0.001",
                       paste0("p = ", formatC(p_adj, format = "f", digits = 3))),
      # tiered heights by distance + tiny within-tier jitter
      y.position = y_base +
        dplyr::case_when(d == 1 ~ 0.05, d == 2 ~ 0.12, d == 3 ~ 0.19, TRUE ~ 0.05) +
        0.01 * rank(midx, ties.method = "first")
    )
  
  list(sum_df = sum_df, tests = tests)
}

plot_demand <- function(sum_df, tests,
                        ylab = "Newsletter Demand (Pr = 1)",
                        bar_fill = "#636363",
                        y_max = NULL) {

  # minimum top needed to cover bars + CIs
  bar_top <- max(sum_df$mean_d + sum_df$ci, na.rm = TRUE) + 0.02

  has_tests <- !is.null(tests) && NROW(tests) > 0

  if (is.null(y_max)) {
    # auto headroom based on brackets (fall back if no tests)
    y_top_needed <- if (has_tests) max(tests$y.position, na.rm = TRUE) + 0.03 else bar_top + 0.03
    y_top <- max(1, y_top_needed)
  } else {
    # force a shorter axis and *compress* bracket positions to fit
    if (has_tests) {
      from <- range(tests$y.position, na.rm = TRUE)
      if (is.finite(diff(from)) && diff(from) > 0 && y_max > bar_top + 0.02) {
        tests$y.position <- scales::rescale(tests$y.position,
                                            from = from,
                                            to   = c(bar_top, y_max - 0.02))
      } else {
        tests$y.position <- pmin(pmax(tests$y.position, bar_top), y_max - 0.02)
      }
    }
    y_top <- y_max
  }

  base <- ggplot(sum_df, aes(x = Treatment, y = mean_d)) +
    geom_col(width = 0.7, fill = bar_fill) +
    geom_errorbar(aes(ymin = pmax(0, mean_d - ci),
                      ymax = pmin(1, mean_d + ci)),
                  width = 0.15, linewidth = 0.5) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       limits = c(0, y_top)) +
    labs(x = "", y = ylab) +
    theme_minimal(base_size = 12) +
    theme(plot.margin = margin(10, 30, 10, 10),
          axis.text.x = element_text(size = 10)) +
    coord_cartesian(clip = "off")

  if (!has_tests) {
    base
  } else {
    base + ggpubr::stat_pvalue_manual(
      tests,
      label = "p_label",
      xmin  = "group1",
      xmax  = "group2",
      y.position = "y.position",
      tip.length = 0.01,
      size = 3.0
    )
  }
}


space_brackets <- function(tests, base_off = c(`1`=0.08, `2`=0.16, `3`=0.24), within_step = 0.02) {
  tests %>%
    arrange(d, y_base, midx) %>%
    group_by(d) %>%
    mutate(y.position = y_base + base_off[as.character(d)] + within_step * (row_number() - 1)) %>%
    ungroup()
}

prepare_tests <- function(tests) {
  tests %>%
    mutate(
      p_adj = p.adjust(p, method = "holm"),
      p_label = dplyr::case_when(
        p_adj < .01  ~ "***",
        p_adj < .05  ~ "**",
        p_adj < .10  ~ "*",
        TRUE         ~ "ns"
      )
    ) %>%
    dplyr::filter(p_label != "ns") %>%  # keep only meaningful pairs
    space_brackets()
}


# -------- 1) All respondents --------
res_all <- demand_summary_and_tests(data_final)
res_all$tests <- prepare_tests(res_all$tests)

p_demand_all <- plot_demand(res_all$sum_df, res_all$tests,
                            ylab = "Newsletter Demand (All)",
                            bar_fill = "#636363", y_max = 0.60)
p_demand_all
ggsave("figures/Fig_Demand_All.pdf", p_demand_all, width = 7, height = 5)

# -------- 2) OriginCorrect == 1 --------
df_corr <- subset(data_final, OriginCorrect == 1)
res_corr <- demand_summary_and_tests(df_corr)
res_corr$tests <- prepare_tests(res_corr$tests)

p_demand_corr <- plot_demand(res_corr$sum_df, res_corr$tests,
                             ylab = "Newsletter Demand (OriginCorrect = 1)",
                             bar_fill = "#1b9e77", y_max = 0.60)
p_demand_corr
ggsave("figures/Fig_Demand_CorrectAI.pdf", p_demand_corr, width = 7, height = 5)

# -------- 3) OriginCorrect == 0 --------
df_incorr <- subset(data_final, OriginCorrect == 0)
res_incorr <- demand_summary_and_tests(df_incorr)
res_incorr$tests <- prepare_tests(res_incorr$tests)

p_demand_incorr <- plot_demand(res_incorr$sum_df, res_incorr$tests,
                               ylab = "Newsletter Demand (OriginCorrect = 0)",
                               bar_fill = "#d95f02", y_max = 0.60)
p_demand_incorr
ggsave("figures/Fig_Demand_IncorrectAI.pdf", p_demand_incorr, width = 7, height = 5)

# -------- 4) Manipulation == 1 --------
df_airecon <- subset(data_final, Manipulation == 1)
res_airecon <- demand_summary_and_tests(df_airecon)
res_airecon$tests <- prepare_tests(res_airecon$tests)

p_demand_airecon <- plot_demand(res_airecon$sum_df, res_airecon$tests,
                             ylab = "Newsletter Demand (Manipulation = 1)",
                             bar_fill = "#7b5e99", y_max = 0.60)
p_demand_airecon
ggsave("figures/Fig_Demand_AIrecon.pdf", p_demand_airecon, width = 7, height = 5)

# -------- 5) Manipulation == 0 --------
df_norecon <- subset(data_final, Manipulation == 0)
res_norecon <- demand_summary_and_tests(df_norecon)
res_norecon$tests <- prepare_tests(res_norecon$tests)

p_demand_norecon <- plot_demand(res_norecon$sum_df, res_norecon$tests,
                               ylab = "Newsletter Demand (Manipulation = 0)",
                               bar_fill = "#b69f02", y_max = 0.60)
p_demand_norecon
ggsave("figures/Fig_Demand_NOrecon.pdf", p_demand_norecon, width = 7, height = 5)

# -------- 6) HiglyAIfamiliar == 1 --------
df_aifam <- subset(data_final, HiglyAIfamiliar == 1)
res_aifam <- demand_summary_and_tests(df_aifam)
res_aifam$tests <- prepare_tests(res_aifam$tests)

p_demand_aifam <- plot_demand(res_aifam$sum_df, res_aifam$tests,
                                ylab = "Newsletter Demand (HiglyAIfamiliar = 1)",
                                bar_fill = "#7b5e99", y_max = 0.60)
p_demand_aifam
ggsave("figures/Fig_Demand_HiglyAIfamiliar.pdf", p_demand_aifam, width = 7, height = 5)

# -------- 8) HiglyAIfamiliar == 0 --------
df_noaifam <- subset(data_final, HiglyAIfamiliar == 0)
res_noaifam <- demand_summary_and_tests(df_noaifam)
res_noaifam$tests <- prepare_tests(res_noaifam$tests)

p_demand_noaifam <- plot_demand(res_noaifam$sum_df, res_noaifam$tests,
                                ylab = "Newsletter Demand (HiglyAIfamiliar = 0)",
                                bar_fill = "#b69f02", y_max = 0.60)
p_demand_noaifam
ggsave("figures/Fig_Demand_NOHiglyAIfamiliar.pdf", p_demand_noaifam, width = 7, height = 5)

# -------- 9) AImediaencounterYes == 1 --------
df_aimedia <- subset(data_final, AImediaencounterYes == 1)
res_aimedia <- demand_summary_and_tests(df_aimedia)
res_aimedia$tests <- prepare_tests(res_aimedia$tests)

p_demand_aimedia <- plot_demand(res_aimedia$sum_df, res_aimedia$tests,
                              ylab = "Newsletter Demand (AImediaencounterYes = 1)",
                              bar_fill = "#7b5e99", y_max = 0.60)
p_demand_aimedia
ggsave("figures/Fig_Demand_AImediaencounterYes.pdf", p_demand_aimedia, width = 7, height = 5)

# -------- 10) AImediaencounterYes == 0 --------
df_noaimedia <- subset(data_final, AImediaencounterYes == 0)
res_noaimedia <- demand_summary_and_tests(df_noaimedia)
res_noaimedia$tests <- prepare_tests(res_noaimedia$tests)

p_demand_noaimedia <- plot_demand(res_noaimedia$sum_df, res_noaimedia$tests,
                                ylab = "Newsletter Demand (AImediaencounterYes = 0)",
                                bar_fill = "#b69f02", y_max = 0.60)
p_demand_noaimedia
ggsave("figures/Fig_Demand_NOAImediaencounterYes.pdf", p_demand_noaimedia, width = 7, height = 5)

#### 3. Mechanisms: Perceptions of Source Quality ####

# ---- Regression models ----
main_mech <- c("Accurate", "Trustworthy", "Quality")
alt_mech  <- c("Bias", "Complex", "Researchertrust", "Entertaining")

# --- DICOTOMOUS versions ---
main_dic <- c("NotAccurate", "NotTrustworthy", "LowQuality")

# store models
models_main <- list()
models_alt  <- list()

# --- Run binary (LPM) and continuous (OLS) versions for main mechanisms ---
for (v in main_dic) {
  f <- as.formula(paste0(v, " ~ AI*Label*OriginCorrect"))
  models_main[[paste0(v, "_base")]] <- feols(f, data=data_final, vcov="HC1")
  
  f_ctrl <- as.formula(paste0(v, " ~ AI*Label*OriginCorrect + ",
                              "Gender + Age + Ethnicity + UKborn + Region + Income + Employment + ",
                              "Education + Political + Party + InterestNews + ConsumptionNews + ",
                              "Newsletter + TrustMedia + AIfamiliar + AIencounter"))
  models_main[[paste0(v, "_ctrl")]] <- feols(f_ctrl, data=data_final, vcov="HC1")
}

for (v in main_mech) {
  f <- as.formula(paste0(v, " ~ AI*Label*OriginCorrect"))
  models_main[[paste0(v, "_base")]] <- feols(f, data=data_final, vcov="HC1")
  
  f_ctrl <- as.formula(paste0(v, " ~ AI*Label*OriginCorrect + ",
                              "Gender + Age + Ethnicity + UKborn + Region + Income + Employment + ",
                              "Education + Political + Party + InterestNews + ConsumptionNews + ",
                              "Newsletter + TrustMedia + AIfamiliar + AIencounter"))
  models_main[[paste0(v, "_ctrl")]] <- feols(f_ctrl, data=data_final, vcov="HC1")
}

# --- Combined z-index for perceived source quality ---
models_main[["z_index_base"]] <- feols(z_index ~ AI*Label*OriginCorrect, data=data_final, vcov="HC1")
models_main[["z_index_ctrl"]] <- feols(z_index ~ AI*Label*OriginCorrect +
                                         Gender + Age + Ethnicity + UKborn + Region + Income + Employment +
                                         Education + Political + Party + InterestNews + ConsumptionNews +
                                         Newsletter + TrustMedia + AIfamiliar + AIencounter,
                                       data=data_final, vcov="HC1")

# --- (optional) alternative perceptions, for appendix ---
for (v in alt_mech) {
  f <- as.formula(paste0(v, " ~ AI*Label*OriginCorrect"))
  models_alt[[paste0(v, "_base")]] <- feols(f, data=data_final, vcov="HC1")
  
  f_ctrl <- as.formula(paste0(v, " ~ AI*Label*OriginCorrect + ",
                              "Gender + Age + Ethnicity + UKborn + Region + Income + Employment + ",
                              "Education + Political + Party + InterestNews + ConsumptionNews + ",
                              "Newsletter + TrustMedia + AIfamiliar + AIencounter"))
  models_alt[[paste0(v, "_ctrl")]] <- feols(f_ctrl, data=data_final, vcov="HC1")
}

# --- Display main results neatly ---
etable(models_main,
       order = c("Accurate_base","Accurate_ctrl",
                 "Trustworthy_base","Trustworthy_ctrl",
                 "Quality_base","Quality_ctrl",
                 "z_index_base","z_index_ctrl"),
       headers = c("OLS", "OLS + Controls"),
       digits = 3,
       fitstat = c("ar2", "n"),
       tex = TRUE,
       file = "tables/table_mechanisms_main.tex")

etable(models_alt,
       headers = c("OLS", "OLS + Controls"),
       digits = 3,
       fitstat = c("ar2", "n"),
       tex = TRUE,
       file = "tables/table_mechanisms_alt.tex")


# -------- helpers: MDV  --------
dv_map <- c(
  # core binaries
  NotAccurate      = "Not accurate (0/1)",
  NotTrustworthy   = "Not trustworthy (0/1)",
  LowQuality       = "Low quality (0/1)",
  NoResearchertrust= "Not trusting researcher (0/1)",
  NoEntertaining   = "Not entertaining (0/1)",
  VComplex         = "Complex/Very complex (0/1)",
  Biased           = "Perceived bias present (0/1)",
  
  # core continuous 1–5
  Accurate         = "Accuracy (1–5)",
  Trustworthy      = "Trustworthiness (1–5)",
  Quality          = "Quality (1–5)",
  Researchertrust  = "Trust in researcher (1–5)",
  Entertaining     = "Entertainment (1–5)",
  Complex          = "Complexity (1–5)",
  Bias             = "Political bias (−2 … +2)",
  
  # z-scores / composites
  z_Accurate       = "Accuracy (z-score)",
  z_Trustworthy    = "Trustworthiness (z-score)",
  z_Quality        = "Quality (z-score)",
  z_index          = "Perceived source quality (z-index)",
  z_Researchertrust= "Trust in researcher (z-score)",
  z_Entertaining   = "Entertainment (z-score)",
  z_Complex        = "Complexity (z-score)",
  z_Bias           = "Political bias (z-score)"
)

# --- compute means in one go ---
dv_means_df <- data_final %>%
  select(all_of(names(dv_map))) %>%
  summarise(across(everything(), ~ mean(as.numeric(.x), na.rm = TRUE))) %>%
  pivot_longer(cols = everything(),
               names_to = "Variable",
               values_to = "Mean") %>%
  mutate(Mean = round(Mean, 3),
         Label = dv_map[Variable]) %>%
  select(Variable, Label, Mean)
# --- view ---
dv_means_df

# ---------- z_index: three separate graphs (same style as demand) ----------

# Summary + pairwise tests for a continuous DV (here: z_index)
zindex_summary_and_tests <- function(df) {
  # summary by treatment
  sum_df <- df %>%
    filter(!is.na(z_index), !is.na(Treatment)) %>%
    group_by(Treatment) %>%
    summarise(
      n = n(),
      mean_z = mean(z_index, na.rm = TRUE),
      sd = sd(z_index, na.rm = TRUE),
      se = sd / sqrt(n),
      ci = 1.96 * se,
      .groups = "drop"
    )
  
  # all pairwise Welch t-tests with staggered bracket heights
  levs <- levels(df$Treatment)
  levs <- levs[levs %in% sum_df$Treatment]
  cm <- combn(levs, 2)
  pair_df <- data.frame(group1 = cm[1,], group2 = cm[2,], stringsAsFactors = FALSE)
  
  # convenience accessor
  get_stats <- function(g) {
    row <- sum_df %>% filter(Treatment == g)
    list(mean = row$mean_z, ytop = row$mean_z + row$ci, idx = match(g, levs))
  }
  
  tests <- do.call(rbind, lapply(seq_len(nrow(pair_df)), function(i) {
    g1 <- pair_df$group1[i]; g2 <- pair_df$group2[i]
    x  <- df$z_index[df$Treatment == g1]
    y  <- df$z_index[df$Treatment == g2]
    tt <- t.test(x, y, var.equal = FALSE)  # Welch
    a <- get_stats(g1); b <- get_stats(g2)
    data.frame(
      group1 = g1, group2 = g2, p = tt$p.value,
      y_base = max(a$ytop, b$ytop),
      d = abs(a$idx - b$idx),
      midx = (a$idx + b$idx)/2,
      stringsAsFactors = FALSE
    )
  }))
  
  tests <- tests %>%
    mutate(
      p_adj = p.adjust(p, method = "holm"),
      p_label = ifelse(p_adj < .001, "p < 0.001",
                       paste0("p = ", formatC(p_adj, format = "f", digits = 3))),
      y.position = y_base +
        dplyr::case_when(d == 1 ~ 0.05, d == 2 ~ 0.12, d == 3 ~ 0.19, TRUE ~ 0.05) +
        0.01 * rank(midx, ties.method = "first")
    )
  
  list(sum_df = sum_df, tests = tests)
}

plot_zindex <- function(sum_df, tests,
                        ylab = "Perceived Source Quality (z-index)",
                        bar_fill = "grey60",
                        y_max = NULL) {
  
  # minimum top needed to cover bars + CIs
  bar_top <- max(sum_df$mean_z + sum_df$ci, na.rm = TRUE) + 0.02
  
  if (is.null(y_max)) {
    y_top_needed <- max(tests$y.position, na.rm = TRUE) + 0.03
    y_top <- y_top_needed
  } else {
    # compress bracket positions to fit under y_max
    from <- range(tests$y.position, na.rm = TRUE)
    if (diff(from) > 0 && y_max > bar_top + 0.02) {
      tests$y.position <- scales::rescale(tests$y.position,
                                          from = from,
                                          to   = c(bar_top, y_max - 0.02))
    } else {
      tests$y.position <- pmin(pmax(tests$y.position, bar_top), y_max - 0.02)
    }
    y_top <- y_max
  }
  
  base <- ggplot(sum_df, aes(x = Treatment, y = mean_z)) +
    geom_col(width = 0.7, fill = bar_fill) +
    geom_errorbar(aes(ymin = mean_z - ci, ymax = mean_z + ci),
                  width = 0.15, linewidth = 0.5) +
    scale_y_continuous(limits = c(min(0, min(sum_df$mean_z - sum_df$ci, na.rm = TRUE) - 0.02), y_top)) +
    labs(x = "", y = ylab) +
    theme_minimal(base_size = 12) +
    theme(plot.margin = margin(10, 30, 10, 10),
          axis.text.x = element_text(size = 10)) +
    coord_cartesian(clip = "off")
  
  base 
}

# ---------- Build & save the three figures (stars & spaced brackets) ----------

# helper to space brackets nicely by distance + within-distance order
space_brackets <- function(tests, base_off = c(`1`=0.08, `2`=0.16, `3`=0.24), within_step = 0.02) {
  tests %>%
    arrange(d, y_base, midx) %>%
    group_by(d) %>%
    mutate(y.position = y_base + base_off[as.character(d)] + within_step * (row_number() - 1)) %>%
    ungroup()
}

# helper to apply Holm correction + stars + filter + spacing
prepare_tests <- function(tests) {
  tests %>%
    mutate(
      p_adj = p.adjust(p, method = "holm"),
      p_label = dplyr::case_when(
        p_adj < .01  ~ "***",
        p_adj < .05  ~ "**",
        p_adj < .10  ~ "*",
        TRUE         ~ "ns"
      )
    ) %>%
    filter(p_label != "ns") %>%   # keep only significant / marginal
    space_brackets()
}

# (1) All respondents
res_z_all <- zindex_summary_and_tests(data_final)
res_z_all$tests <- prepare_tests(res_z_all$tests)

p_z_all <- plot_zindex(res_z_all$sum_df, res_z_all$tests,
                       ylab = "Perceived Source Quality (All, z-index)",
                       bar_fill = "#636363", y_max = 0.40) +
  ggpubr::stat_pvalue_manual(
    res_z_all$tests,
    label = "p_label",
    xmin  = "group1",
    xmax  = "group2",
    y.position = "y.position",
    tip.length = 0.01,
    size = 3.0
  )
p_z_all
ggsave("figures/Fig_zindex_All.pdf", p_z_all, width = 7, height = 5)


# (2) OriginCorrect == 1
df_corr <- subset(data_final, OriginCorrect == 1)
res_z_corr <- zindex_summary_and_tests(df_corr)
res_z_corr$tests <- prepare_tests(res_z_corr$tests)

p_z_corr <- plot_zindex(res_z_corr$sum_df, res_z_corr$tests,
                        ylab = "Perceived Source Quality (z-index)",
                        bar_fill = "#1b9e77", y_max = 0.70) +
  ggpubr::stat_pvalue_manual(
    res_z_corr$tests,
    label = "p_label",
    xmin  = "group1",
    xmax  = "group2",
    y.position = "y.position",
    tip.length = 0.01,
    size = 3.0
  )
p_z_corr
ggsave("figures/Fig_zindex_CorrectAI.pdf", p_z_corr, width = 7, height = 5)


# (3) OriginCorrect == 0
df_incorr <- subset(data_final, OriginCorrect == 0)
res_z_incorr <- zindex_summary_and_tests(df_incorr)
res_z_incorr$tests <- prepare_tests(res_z_incorr$tests)

p_z_incorr <- plot_zindex(res_z_incorr$sum_df, res_z_incorr$tests,
                          ylab = "Perceived Source Quality (z-index)",
                          bar_fill = "#d95f02", y_max = 0.70) +
  ggpubr::stat_pvalue_manual(
    res_z_incorr$tests,
    label = "p_label",
    xmin  = "group1",
    xmax  = "group2",
    y.position = "y.position",
    tip.length = 0.01,
    size = 3.0
  )
p_z_incorr
ggsave("figures/Fig_zindex_IncorrectAI.pdf", p_z_incorr, width = 7, height = 5)
 

# (4) Manipulation == 1
df_airecon <- subset(data_final, Manipulation == 1)
res_z_airecon <- zindex_summary_and_tests(df_airecon)
res_z_airecon$tests <- prepare_tests(res_z_airecon$tests)

p_z_airecon <- plot_zindex(res_z_airecon$sum_df, res_z_airecon$tests,
                           ylab = "Perceived Source Quality (z-index)",
                           bar_fill = "#7b5e99", y_max = 0.70)

if (nrow(res_z_airecon$tests) > 0) {
  p_z_airecon <- p_z_airecon +
    ggpubr::stat_pvalue_manual(res_z_airecon$tests,
                               label = "p_label", xmin = "group1", xmax = "group2",
                               y.position = "y.position", tip.length = 0.01, size = 3
    )
}
p_z_airecon
ggsave("figures/Fig_zindex_AIrecon.pdf", p_z_airecon, width = 7, height = 5)

# (5) Manipulation == 0
df_norecon <- subset(data_final, Manipulation == 0)
res_z_norecon <- zindex_summary_and_tests(df_norecon)
res_z_norecon$tests <- prepare_tests(res_z_norecon$tests)

p_z_norecon <- plot_zindex(res_z_norecon$sum_df, res_z_norecon$tests,
                          ylab = "Perceived Source Quality (z-index)",
                          bar_fill = "#b69f02", y_max = 0.70) +
  ggpubr::stat_pvalue_manual(
    res_z_norecon$tests,
    label = "p_label",
    xmin  = "group1",
    xmax  = "group2",
    y.position = "y.position",
    tip.length = 0.01,
    size = 3.0
  )
p_z_norecon
ggsave("figures/Fig_zindex_NOrecon.pdf", p_z_norecon, width = 7, height = 5)





#### 4. IV approach (robustness): Controls for AI and Label; use AI:Label as the excluded instrument (conservative exclusion) ####


# First stage: randomized assignment terms predict recognition ("Manipulation")
fs <- lm(Manipulation ~ AI * Label, data = data_final)
summary(fs)
coeftest(fs, vcov = vcovHC(fs, type = "HC1"))["AI:Label", ]   # strength of excluded IV

# 2SLS: Manipulation instrumented by AI:Label; AI and Label included as controls
iv_pref_d <- ivreg(Demand ~ Manipulation + AI + Label | AI + Label + AI:Label, data = data_final)
summary(iv_pref_d, vcov = function(x) vcovHC(x, type="HC1"), diagnostics = TRUE)

# 2SLS: Manipulation instrumented by AI:Label; AI and Label included as controls
iv_pref_z <- ivreg(z_index ~ Manipulation + AI + Label | AI + Label + AI:Label, data = data_final)
summary(iv_pref_z, vcov = function(x) vcovHC(x, type="HC1"), diagnostics = TRUE)


# =================== SETTINGS ===================
# Controls (RHS only; appear on both stages in the 'with controls' specs)
controls <- ~ Gender + Age + Ethnicity + UKborn + Region + Income + Employment +
  Education + Political + Party + InterestNews + ConsumptionNews +
  Newsletter + TrustMedia + AIfamiliar + AIencounter
controls_rhs <- as.character(controls)[2]

hc1 <- function(m) vcovHC(m, type = "HC1")

# Formatting helpers (one-line cells)
star <- function(p) ifelse(is.na(p), "", ifelse(p<.01,"***", ifelse(p<.05,"**", ifelse(p<.10,"*",""))))
fmt  <- function(est, se, p){
  if (any(is.na(c(est, se)))) return("")
  paste0(sprintf("%.3f", est), star(p), " (", sprintf("%.3f", se), ")")
}

# Safe robust coeftest (falls back to classical if HC1 fails)
safe_ct <- function(model, vc = function(x) vcovHC(x, type="HC1")) {
  out <- try(coeftest(model, vcov = vc), silent = TRUE)
  if (inherits(out, "try-error")) coeftest(model) else out
}
pick_from_ct <- function(ct, term) {
  rn <- rownames(ct)
  if (is.null(rn) || !(term %in% rn)) return(c(NA, NA, NA))
  c(ct[term, "Estimate"], ct[term, "Std. Error"], ct[term, "Pr(>|t|)"])
}

# Robust DWH p-value (auxiliary regression with first-stage residual)
dwh_p <- function(y, rhs_struct, rhs_inst, data, with_controls = FALSE) {
  # First-stage for the endogenous regressor "Manipulation"
  fs_form <- as.formula(
    if (!with_controls)
      paste0("Manipulation ~ ", rhs_inst)
    else
      paste0("Manipulation ~ ", rhs_inst, " + ", controls_rhs)
  )
  # Use na.exclude (not na.omit) so resid() returns a vector aligned to the
  # original `data` rows (NAs where the row was dropped); na.omit would
  # return a shorter vector and break the transform() / model.frame() below.
  fs_fit <- lm(fs_form, data = data, na.action = na.exclude)
  vhat <- resid(fs_fit)           # first-stage residuals on common sample
  
  # Build augmented structural OLS with residuals
  aug_form <- as.formula(
    if (!with_controls)
      paste0(y, " ~ ", rhs_struct, " + vhat")
    else
      paste0(y, " ~ ", rhs_struct, " + ", controls_rhs, " + vhat")
  )
  df_aug <- model.frame(aug_form, data = transform(data, vhat = vhat), na.action = na.omit)
  aug_fit <- lm(aug_form, data = df_aug)
  
  ct <- safe_ct(aug_fit, hc1)
  if (!("vhat" %in% rownames(ct))) return(NA_real_)
  as.numeric(ct["vhat", "Pr(>|t|)"])
}

# =================== MODELS ===================
# ---------- First stage ----------
fs_no <- lm(Manipulation ~ AI*Label, data = data_final, na.action = na.omit)
fs_wc <- lm(as.formula(paste("Manipulation ~ AI*Label +", controls_rhs)),
            data = data_final, na.action = na.omit)

# ---------- IV: Demand ----------
ivD_no <- ivreg(Demand ~ Manipulation + AI + Label | AI + Label + AI:Label,
                data = data_final, na.action = na.omit)
ivD_wc <- ivreg(as.formula(paste0(
  "Demand ~ Manipulation + AI + Label + ", controls_rhs,
  " | AI + Label + AI:Label + ", controls_rhs)),
  data = data_final, na.action = na.omit)

# ---------- IV: z_index ----------
ivZ_no <- ivreg(z_index ~ Manipulation + AI + Label | AI + Label + AI:Label,
                data = data_final, na.action = na.omit)
ivZ_wc <- ivreg(as.formula(paste0(
  "z_index ~ Manipulation + AI + Label + ", controls_rhs,
  " | AI + Label + AI:Label + ", controls_rhs)),
  data = data_final, na.action = na.omit)

# Classical summaries for Adj R^2 & N
sum_fs_no  <- summary(fs_no)
sum_fs_wc  <- summary(fs_wc)
sum_ivD_no <- summary(ivD_no)
sum_ivD_wc <- summary(ivD_wc)
sum_ivZ_no <- summary(ivZ_no)
sum_ivZ_wc <- summary(ivZ_wc)

# Robust coeftest objects
ct_fs_no  <- safe_ct(fs_no)
ct_fs_wc  <- safe_ct(fs_wc)
ct_ivD_no <- safe_ct(ivD_no)
ct_ivD_wc <- safe_ct(ivD_wc)
ct_ivZ_no <- safe_ct(ivZ_no)
ct_ivZ_wc <- safe_ct(ivZ_wc)

# Weak-IV F from first-stage: F = (robust t on AI:Label)^2
weakF_no <- { tval <- try(unname(ct_fs_no["AI:Label","t value"]), silent=TRUE)
if (inherits(tval,"try-error") || is.na(tval)) NA_real_ else tval^2 }
weakF_wc <- { tval <- try(unname(ct_fs_wc["AI:Label","t value"]), silent=TRUE)
if (inherits(tval,"try-error") || is.na(tval)) NA_real_ else tval^2 }

# Wu–Hausman p-values via robust DWH aux-regression (works for both outcomes)
# Structural RHS and instrument RHS (without controls; controls added internally when needed)
rhs_struct <- "Manipulation + AI + Label"
rhs_inst   <- "AI + Label + AI:Label"

wuD_no <- dwh_p("Demand",  rhs_struct, rhs_inst, data_final, with_controls = FALSE)
wuD_wc <- dwh_p("Demand",  rhs_struct, rhs_inst, data_final, with_controls = TRUE)
wuZ_no <- dwh_p("z_index", rhs_struct, rhs_inst, data_final, with_controls = FALSE)
wuZ_wc <- dwh_p("z_index", rhs_struct, rhs_inst, data_final, with_controls = TRUE)

# ============ EXTRACT & FORMAT CELLS ============
# First stage terms
fs_terms   <- c("AI","Label","AI:Label")
fs_no_rows <- t(sapply(fs_terms, function(tt) pick_from_ct(ct_fs_no, tt)))
fs_wc_rows <- t(sapply(fs_terms, function(tt) pick_from_ct(ct_fs_wc, tt)))

# IV terms
iv_terms     <- c("Manipulation","AI","Label")
ivD_no_rows  <- t(sapply(iv_terms, function(tt) pick_from_ct(ct_ivD_no, tt)))
ivD_wc_rows  <- t(sapply(iv_terms, function(tt) pick_from_ct(ct_ivD_wc, tt)))
ivZ_no_rows  <- t(sapply(iv_terms, function(tt) pick_from_ct(ct_ivZ_no, tt)))
ivZ_wc_rows  <- t(sapply(iv_terms, function(tt) pick_from_ct(ct_ivZ_wc, tt)))

# ============ BUILD TABLE ============
row_labels <- c(
  "AI", "Label", "AI×Label", "Weak-IV F", "Adj. R$^2$", "N",
  "Manipulation", "AI", "Label", "Wu–Hausman p", "Adj. R$^2$", "N",
  "Manipulation", "AI", "Label", "Wu–Hausman p", "Adj. R$^2$", "N"
)

tab <- matrix("", nrow = length(row_labels), ncol = 6)
colnames(tab) <- c("(1) FS no ctrls", "(2) FS + ctrls",
                   "(3) Demand IV no", "(4) Demand IV +",
                   "(5) z_index IV no", "(6) z_index IV +")
r <- 0

# First stage rows
for (i in seq_along(fs_terms)){
  r <- r + 1
  tab[r,1] <- fmt(fs_no_rows[i,1], fs_no_rows[i,2], fs_no_rows[i,3])
  tab[r,2] <- fmt(fs_wc_rows[i,1], fs_wc_rows[i,2], fs_wc_rows[i,3])
}
r <- r + 1
tab[r,1] <- ifelse(is.na(weakF_no), "", sprintf("%.3f", weakF_no))
tab[r,2] <- ifelse(is.na(weakF_wc), "", sprintf("%.3f", weakF_wc))
r <- r + 1
tab[r,1] <- sprintf("%.3f", sum_fs_no$adj.r.squared)
tab[r,2] <- sprintf("%.3f", sum_fs_wc$adj.r.squared)
r <- r + 1
tab[r,1] <- as.character(nobs(fs_no))
tab[r,2] <- as.character(nobs(fs_wc))

# Demand IV rows
for (i in seq_along(iv_terms)){
  r <- r + 1
  tab[r,3] <- fmt(ivD_no_rows[i,1], ivD_no_rows[i,2], ivD_no_rows[i,3])
  tab[r,4] <- fmt(ivD_wc_rows[i,1], ivD_wc_rows[i,2], ivD_wc_rows[i,3])
}
r <- r + 1
tab[r,3] <- ifelse(is.na(wuD_no), "", sprintf("%.4f", wuD_no))
tab[r,4] <- ifelse(is.na(wuD_wc), "", sprintf("%.4f", wuD_wc))
r <- r + 1
tab[r,3] <- sprintf("%.3f", sum_ivD_no$adj.r.squared)
tab[r,4] <- sprintf("%.3f", sum_ivD_wc$adj.r.squared)
r <- r + 1
tab[r,3] <- as.character(nobs(ivD_no))
tab[r,4] <- as.character(nobs(ivD_wc))

# z_index IV rows
for (i in seq_along(iv_terms)){
  r <- r + 1
  tab[r,5] <- fmt(ivZ_no_rows[i,1], ivZ_no_rows[i,2], ivZ_no_rows[i,3])
  tab[r,6] <- fmt(ivZ_wc_rows[i,1], ivZ_wc_rows[i,2], ivZ_wc_rows[i,3])
}
r <- r + 1
tab[r,5] <- ifelse(is.na(wuZ_no), "", sprintf("%.4f", wuZ_no))
tab[r,6] <- ifelse(is.na(wuZ_wc), "", sprintf("%.4f", wuZ_wc))
r <- r + 1
tab[r,5] <- sprintf("%.3f", sum_ivZ_no$adj.r.squared)
tab[r,6] <- sprintf("%.3f", sum_ivZ_wc$adj.r.squared)
r <- r + 1
tab[r,5] <- as.character(nobs(ivZ_no))
tab[r,6] <- as.character(nobs(ivZ_wc))

rownames(tab) <- row_labels

# ------------ Console peek ------------
cat("\n=== First stage (HC1) — key terms ===\n")
print(tab[1:3, 1:2], quote = FALSE)
cat("\nWeak-IV F:", tab[4,1], "(no ctrls),", tab[4,2], "(+ ctrls)\n")
cat("\n=== Demand IV (HC1) — key terms ===\n")
print(tab[7:10, 3:4], quote = FALSE)
cat("\n=== z_index IV (HC1) — key terms ===\n")
print(tab[13:16, 5:6], quote = FALSE)

# ------------ LaTeX output ------------
latex_tab <- kbl(tab, format = "latex", booktabs = TRUE, escape = FALSE,
                 caption = "Recognition first-stage and 2SLS effects on Demand and z\\_index (HC1 SEs)") %>%
  kable_styling(latex_options = c("hold_position")) %>%
  pack_rows("First stage: Manipulation ~ AI × Label", 1, 6) %>%
  pack_rows("Second stage (Demand): 2SLS with instrument AI × Label", 7, 12) %>%
  pack_rows("Second stage (z_index): 2SLS with instrument AI × Label", 13, 18)

cat(latex_tab)
writeLines(latex_tab, "tables/iv_table.tex")





###### ######### ######
#### TEXT analysis ####
###### ######### ######

rm(list = ls())
# (The text-analysis section below intentionally clears the workspace and
# re-loads the raw data; working directory is preserved from the caller.)

data_new <- read.csv(file="DATA/AI labels - Full Study_October 19, 2025_15.58.csv")
data_final <- data_new[-(1:6),]

data_new2 <- read.csv(file="DATA/AI labels_October 15, 2025_21.02.csv")
data_final2 <- data_new2[-(1:17),]

data_final <- rbind(data_final, data_final2)

# Q971 If you were to guess, ?
# Q36.1 Could you please explain your decision regarding the newsletter? Write as much as you want, but two/three sentences are enough.

data_final <- data_final %>% 
  rename(study_is_about = Q971, explain_newsletter_decision = Q36.1, Treatment = T)

# 0) Prep: keep only cleaned text cols (use the renamed vars) ---------------------------

text_df <- data_final %>%
  mutate(
    study_is_about = str_squish(as.character(study_is_about)),
    explain_newsletter_decision = str_squish(as.character(explain_newsletter_decision))
  ) %>%
  select(study_is_about, explain_newsletter_decision)

# 1) Generic keyword classifier + summariser # ---------------------------

classify_by_dict <- function(text, dict, other_label = "Other / Unclear") {
  tx <- tolower(ifelse(is.na(text), "", text))
  dict_rx <- lapply(dict, function(keys) {
    if (length(keys) == 0) NA_character_ else paste0("(", paste(keys, collapse = "|"), ")")
  })
  hit_mat <- sapply(dict_rx, function(rx)
    if (is.na(rx)) rep(FALSE, length(tx)) else stringr::str_detect(tx, rx))
  choice <- apply(hit_mat, 1, function(row) {
    if (any(row)) names(dict)[which(row)[1]] else other_label
  })
  choice
}

summarise_latex <- function(data, var_text, cat_col, example_len = 70, digits_share = 1) {
  data %>%
    group_by(!!rlang::sym(cat_col)) %>%
    summarise(
      Share   = round(100 * n() / nrow(data), digits_share),
      Example = stringr::str_trunc(dplyr::first(!!rlang::sym(var_text)), example_len), #real quotes taken from dataset — specifically from the first non-missing text response found in each category
      .groups = "drop"
    ) %>%
    arrange(desc(Share))
}

export_xtable <- function(df, caption, label, file,
                          align = c("l", "l", "S[table-format=2.1]", "X")) {
  dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
  xt <- xtable(df, caption = caption, label = label, align = align)
  print(xt,
        include.rownames = FALSE,
        caption.placement = "top",
        sanitize.text.function = identity,
        comment = FALSE,
        file = file)
}

# 2) Dictionaries (harmonised) # ---------------------------

dict_topics <- list(
  "AI / Artificial Intelligence"   = c("ai", "artificial", "machine learning", "generated", "gpt", "model"),
  "Media / News / Social media"    = c("news", "media", "social", "article", "platform", "twitter|x", "facebook"),
  "Trust / Credibility / Ethics"   = c("trust", "credib", "ethic", "truth", "honest", "integrity"),
  "Politics / Elections / Opinion" = c("politic", "vote", "election", "opinion", "policy"),
  "Misinformation / Fake news"     = c("fake", "misinfo", "disinfo", "deepfake", "hoax"),
  "Psychology / Perception"        = c("perce", "attitude", "belief", "behavior", "psych"),
  "Marketing / Advertising"        = c("advertis", "persuasion", "marketing", "campaign"),
  "Economics / Finance / Work"     = c("econom", "job", "work", "finance", "money")
  # "Other / Unclear" handled by classifier
)

dict_reasons <- list(
  "Interest in AI / Technology"    = c("ai", "artificial", "machine", "technology", "tech"),
  "Interest in Science / Research" = c("science", "research", "learn", "study", "knowledge"),
  "Curiosity / Novelty"            = c("curious", "interest", "interesting", "novel", "new", "fun"),
  "Trust / Credibility"            = c("trust", "credible", "reliable", "truth"),
  "Skepticism / Disinterest"       = c("not interested", "no interest", "no trust", "doubt", "fake", "boring", "spam"),
  "Time / Effort / Relevance"      = c("no time", "busy", "too long", "irrelevant", "not relevant", "don.?t care"),
  "Other / Unclear"                = character(0)
)

# 3) STUDY TOPIC: classify → summarise → export # ---------------------------

study_tbl <- text_df %>%
  filter(!is.na(study_is_about), study_is_about != "") %>%
  mutate(Category = classify_by_dict(study_is_about, dict_topics, "Other / Unclear")) %>%
  summarise_latex(var_text = "study_is_about", cat_col = "Category", example_len = 70, digits_share = 1)

export_xtable(
  df = study_tbl,
  caption = "Guessed Topic of Experiment",
  label   = "tab:guessed_topic",
  file    = "tables/table_guessed_topic.tex"
)

# 4) NEWSLETTER REASONS: classify → summarise → export # ---------------------------

reasons_tbl <- text_df %>%
  filter(!is.na(explain_newsletter_decision), explain_newsletter_decision != "") %>%
  mutate(Category = classify_by_dict(explain_newsletter_decision, dict_reasons, "Other / Unclear")) %>%
  summarise_latex(var_text = "explain_newsletter_decision", cat_col = "Category", example_len = 70, digits_share = 1)

export_xtable(
  df = reasons_tbl,
  caption = "Reasons for Requesting (or Not Requesting) the Newsletter",
  label   = "tab:newsletter_reasons",
  file    = "tables/table_newsletter_reasons.tex"
)

message("Note: Shares are percentages of non-empty responses. Examples are truncated.")
