# -----------------------------------------------------------------------------
# 0. PARAMETERS
# -----------------------------------------------------------------------------

library(data.table)
library(lubridate)
rm(list = ls())
# NOTE: working directory must be set to the replication-package root
# (the folder containing this script). run_all.R does this automatically.

CSV_PATH <- "DATA/AI labels - Mechs - full_April 8, 2026_07.22.csv"

# -----------------------------------------------------------------------------
# 1. LOAD
#    Qualtrics 3-row header: row1 = col names, row2 = labels, row3 = ImportIds
#    skip=1 drops the label row; we then drop the ImportId row manually.
# -----------------------------------------------------------------------------

cat("Loading data...\n")

raw <- fread(
  CSV_PATH,
  skip = 1,
  colClasses = "character"
)

# Drop ImportId row (first data row after skip)
raw <- raw[-1]

cat(sprintf("  Raw rows loaded: %d\n", nrow(raw)))

# -----------------------------------------------------------------------------
# 2. CLEAN & FILTER
# -----------------------------------------------------------------------------

df <- copy(raw)

setnames(
  df,
  old = c("Start Date", "Response Type", "Finished", "PROLIFIC_PID"),
  new = c("start_date", "status", "finished", "prolific_id"),
  skip_absent = TRUE
)

df[, start_date := ymd_hms(start_date)]
df[, finished := (finished == "True")]

# 2a. Remove Survey Preview rows
df_real <- df[status == "IP Address"]
cat(sprintf("  After removing Survey Preview rows: %d\n", nrow(df_real)))

# 2b. Keep only completed responses
df_complete <- df_real[finished == TRUE]
cat(sprintf("  After keeping Finished == True: %d\n", nrow(df_complete)))

cat(sprintf("\n  >>> USABLE RESPONDENTS (wave 1+2): %d <<<\n\n", nrow(df_complete)))

# -----------------------------------------------------------------------------
# 3. REVIEW COLUMN NAMES
# -----------------------------------------------------------------------------

# Print all variable names with a numeric index
name_key <- data.table(
  index = seq_along(names(df_complete)),
  old_name = names(df_complete)
)

print(name_key)

# -----------------------------------------------------------------------------
# 4. BUILD MAIN ANALYSIS DATASET
# -----------------------------------------------------------------------------

dt_main <- copy(df_complete)

# 4a. Drop obvious admin / useless columns from main file
drop_cols <- c(
  "status",
  "IP Address",
  "Progress",
  "finished",
  "Recipient Last Name",
  "Recipient First Name",
  "Recipient Email",
  "External Data Reference",
  "Location Latitude",
  "Location Longitude",
  "Distribution Channel",
  "User Language",
  "SESSION_ID",
  "Attention"
)

drop_cols <- intersect(drop_cols, names(dt_main))
dt_main[, (drop_cols) := NULL]

# 4b. Collapse timing variables to 4 columns USING COLUMN POSITIONS

# positions of all timing columns
timing_idx <- which(grepl("^Timing - ", names(dt_main)))

# within timing block:
# 1 = First Click, 2 = Last Click, 3 = Page Submit, 4 = Click Count
first_idx  <- timing_idx[seq(1, length(timing_idx), by = 4)]
last_idx   <- timing_idx[seq(2, length(timing_idx), by = 4)]
submit_idx <- timing_idx[seq(3, length(timing_idx), by = 4)]
count_idx  <- timing_idx[seq(4, length(timing_idx), by = 4)]

collapse_one <- function(x) {
  vals <- x[!is.na(x) & x != ""]
  if (length(vals) == 0) return(NA_character_)
  vals[1]
}

dt_main[, timing_first_click := apply(as.data.frame(.SD), 1, collapse_one), .SDcols = first_idx]
dt_main[, timing_last_click  := apply(as.data.frame(.SD), 1, collapse_one), .SDcols = last_idx]
dt_main[, timing_page_submit := apply(as.data.frame(.SD), 1, collapse_one), .SDcols = submit_idx]
dt_main[, timing_click_count := apply(as.data.frame(.SD), 1, collapse_one), .SDcols = count_idx]

# convert to numeric
dt_main[, timing_first_click := as.numeric(timing_first_click)]
dt_main[, timing_last_click  := as.numeric(timing_last_click)]
dt_main[, timing_page_submit := as.numeric(timing_page_submit)]
dt_main[, timing_click_count := as.numeric(timing_click_count)]

# drop all raw timing columns
dt_main <- dt_main[, -timing_idx, with = FALSE]

head(dt_main[, .(T, Image, timing_first_click, timing_last_click, timing_page_submit, timing_click_count)])
summary(dt_main[, .(timing_first_click, timing_last_click, timing_page_submit, timing_click_count)])

# 4c. Rename key variables
rename_map <- c(
  "End Date" = "end_date",
  "Recorded Date" = "recorded_date",
  "Response ID" = "response_id",
  "Duration (in seconds)" = "duration_seconds",
  "Q_RecaptchaScore" = "recaptcha_score",
  "T" = "treatment",
  "Image" = "image_id",
  
  "Welcome!\n\n\nStudy information and consent form \n\n\n\nThank you for your interest in our study. Overall, this study aims to understand how people evaluate and perceive information in the news. The study is anonymous. We will collect no identifying information, and only summary statistics and anonymous answers to open questions will be used. \n\nThe survey contains one attention check and takes around 6-8 minutes. For your time, you will receive £1.10.\n\nPlease note that participation is entirely voluntary, and if you decide to take part, you are free to withdraw at any time without giving a reason.\n\nBy clicking \"I consent\", you confirm that:\n\n1. You have read and understood the information above.\n2. You are 18 years of age or older.\n3. You voluntarily agree to participate in this study.\n\nPlease indicate below whether you consent to take part in this study." = "consent",
  
  "Please paste your Prolific ID here:" = "prolific_id_entry",
  
  "We are interested in the views of the respondents, but we also want to ensure that individuals are reading the question carefully. To show that you have read this question, please answer 'Fairly often', regardless of your true opinion." = "attention_instr",
  
  "What is your sex recorded at birth (i.e. the sex on your original birth certificate)?" = "sex_birth",
  "What is your age?" = "age",
  "What ethnicity describes you the best?" = "ethnicity",
  "Were you born in the UK?" = "born_uk",
  "Which part of the UK are you living at the moment?" = "uk_region",
  "What is your household yearly income (after tax)?" = "income",
  "What is your employment status?" = "employment_status",
  "What is your highest completed education level?" = "education",
  "How would you describe your political orientation?" = "political_orientation",
  "Which of these parties do you most closely identify with?" = "party_id",
  "How interested are you in world news?" = "news_interest",
  "How often do you consume news in a week?" = "news_frequency",
  "Are you currently subscribed to a media newsletter?" = "newsletter_subscribed",
  "For which type of content? (Select all that apply)" = "newsletter_content_type",
  "What type of media do you typically use to consume news? (Select all that apply)" = "news_media_type",
  "Which of the newspaper below have you read at least once in the past month? (Select all that apply)" = "newspapers_read",
  "How much do you trust the news sources you typically consume?" = "trust_news_sources",
  
  "You now have the opportunity to receive a real newsletter that features the top three articles on economics from this newspaper every week for the next month. Would you like to receive it?" = "newsletter_takeup",
  "Based on the article you have just seen:\n\nDo you think the image you have seen was AI-generated?" = "belief_image_ai",
  "How accurate do you expect the newsletter to be?" = "expect_accuracy",
  "How trustworthy do you expect the newsletter to be?" = "expect_trustworthiness",
  "What quality do you expect the newsletter to have?" = "expect_quality",
  "What kind of political bias do you expect the newsletter to have?" = "expect_political_bias",
  "How complex do you expect the newsletter to be?" = "expect_complexity",
  "How entertaining do you expect the newsletter to be?" = "expect_entertainment",
  "Do you think the researchers behind this study are trustworthy?" = "trust_researchers",
  "Could you please explain your decision about whether to receive the newsletter?\nWrite as much as you want." = "takeup_reason_text",
  "If you were to guess, what is this study about?" = "study_guess_text",
  "Do you think the newspaper behind this newsletter uses AI to produce text-based content (e.g. articles)?" = "belief_news_ai_text",
  "Do you think the newspaper behind this newsletter uses AI to produce images (e.g. illustrations or photos)?" = "belief_news_ai_images",
  "How familiar are you with generative AI technologies such as OpenAI and other systems?" = "ai_familiarity",
  "In the past month, how often have you encountered AI-generated content in other media outlets?" = "ai_exposure",
  "What type of AI-generated content did you encounter? (Select all that apply)" = "ai_content_type"
)

old_keep <- intersect(names(rename_map), names(dt_main))
setnames(dt_main, old = old_keep, new = unname(rename_map[old_keep]))

# 4d. Convert date/time variables
datetime_vars <- intersect(c("start_date", "end_date", "recorded_date"), names(dt_main))
for (v in datetime_vars) {
  dt_main[, (v) := ymd_hms(get(v))]
}

# 4e. Convert numeric variables
numeric_vars <- intersect(c("age", "duration_seconds", "recaptcha_score"), names(dt_main))
for (v in numeric_vars) {
  dt_main[, (v) := as.numeric(get(v))]
}

# 4f. Convert categorical variables to factors
factor_vars <- intersect(c(
  "consent",
  "attention_instr",
  "sex_birth",
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
  "newsletter_content_type",
  "news_media_type",
  "newspapers_read",
  "trust_news_sources",
  "newsletter_takeup",
  "belief_image_ai",
  "expect_accuracy",
  "expect_trustworthiness",
  "expect_quality",
  "expect_political_bias",
  "expect_complexity",
  "expect_entertainment",
  "trust_researchers",
  "belief_news_ai_text",
  "belief_news_ai_images",
  "ai_familiarity",
  "ai_exposure",
  "ai_content_type",
  "treatment",
  "image_id"
), names(dt_main))

for (v in factor_vars) {
  dt_main[, (v) := as.factor(get(v))]
}

cat(sprintf("dt_main: %d rows, %d cols\n", nrow(dt_main), ncol(dt_main)))

# 4.g Check remaining long names
data.table(
  index = seq_along(names(dt_main)),
  name = names(dt_main),
  nchar = nchar(names(dt_main))
)[nchar > 40]

# Change those as well
setnames(dt_main, old = names(dt_main)[7],  new = "consent")
setnames(dt_main, old = names(dt_main)[9],  new = "attention_instr")
setnames(dt_main, old = names(dt_main)[27], new = "newsletter_takeup")
setnames(dt_main, old = names(dt_main)[36], new = "takeup_reason_text")

# Check 
lapply(
  dt_main[, .(consent, attention_instr, sex_birth, income, education, newsletter_takeup, treatment, image_id)],
  table,
  useNA = "ifany"
)

# Full check
str(dt_main)

# -----------------------------------------------------------------------------
# 5. SMALL TYPE FIXES
# -----------------------------------------------------------------------------

# plain factors
char_to_factor <- c("consent", "attention_instr", "newsletter_takeup")

for (v in intersect(char_to_factor, names(dt_main))) {
  dt_main[, (v) := as.factor(get(v))]
}
str(dt_main[, .(consent, attention_instr, newsletter_takeup)])

# income
dt_main[, income := factor(
  income,
  levels = c(
    "Less than £10,000",
    "£10,000 - £19,999",
    "£20,000 - £29,999",
    "£30,000 - £39,999",
    "£40,000 - £49,999",
    "£50,000 - £59,999",
    "£60,000 - £69,999",
    "£70,000 - £79,999",
    "£80,000 - £89,999",
    "£90,000 - £99,999",
    "More than £100,000"
  ),
  ordered = TRUE
)]
levels(dt_main$income)

# ai_familiarity
dt_main[, ai_familiarity := factor(
  ai_familiarity,
  levels = c(
    "Not at all familiar",
    "Slightly familiar",
    "Somewhat familiar",
    "Fairly familiar",
    "Very familiar"
  ),
  ordered = TRUE
)]
levels(dt_main$ai_familiarity)


# ordered factor already exists from cleaning; create numeric version for controls
dt_main[, ai_familiarity_num := as.numeric(ai_familiarity)]

# optional high-familiarity dummy for heterogeneity
dt_main[, high_ai_familiarity := ai_familiarity %in% c("Fairly familiar", "Very familiar")]

table(dt_main$ai_familiarity, useNA = "ifany")
table(dt_main$high_ai_familiarity, useNA = "ifany")

# -----------------------------------------------------------------------------
# 6. ORDER OUTCOME / MECHANISM VARIABLES
#    least positive  ---->  most positive
# -----------------------------------------------------------------------------

dt_main[, expect_accuracy := factor(
  expect_accuracy,
  levels = c(
    "Not trustworthy at all",
    "Not trustworthy",
    "Somewhat trustworthy",
    "Trustworthy",
    "Very trustworthy"
  ),
  ordered = TRUE
)]
levels(dt_main$expect_accuracy)

dt_main[, expect_trustworthiness := factor(
  expect_trustworthiness,
  levels = c(
    "Not trustworthy at all",
    "Not trustworthy",
    "Somewhat trustworthy",
    "Trustworthy",
    "Very trustworthy"
  ),
  ordered = TRUE
)]
levels(dt_main$expect_trustworthiness)

dt_main[, expect_quality := factor(
  expect_quality,
  levels = c(
    "Very low",
    "Low",
    "Medium",
    "High",
    "Very high"
  ),
  ordered = TRUE
)]
levels(dt_main$expect_quality)

dt_main[, expect_political_bias := factor(
  expect_political_bias,
  levels = c(
    "Very left-wing bias",
    "Left-wing bias",
    "No bias/Neutral",
    "Right-wing bias",
    "Very right-wing bias"
  ),
  ordered = TRUE
)]
levels(dt_main$expect_political_bias)

dt_main[, expect_complexity := factor(
  expect_complexity,
  levels = c(
    "Not complex at all",
    "Not complex",
    "Somewhat complex",
    "Complex",
    "Very complex"
  ),
  ordered = TRUE
)]
levels(dt_main$expect_complexity)

dt_main[, expect_entertainment := factor(
  expect_entertainment,
  levels = c(
    "Not entertaining at all",
    "Not entertaining",
    "Somewhat entertaining",
    "Entertaining",
    "Very entertaining"
  ),
  ordered = TRUE
)]
levels(dt_main$expect_entertainment)

dt_main[, trust_researchers := factor(
  trust_researchers,
  levels = c(
    "Not trustworthy at all",
    "Not trustworthy",
    "Somewhat trustworthy",
    "Trustworthy",
    "Very trustworthy"
  ),
  ordered = TRUE
)]
levels(dt_main$trust_researchers)

# check that your recoding did not create NAs because of a label mismatch:
sapply(
  dt_main[, .(
    expect_accuracy,
    expect_trustworthiness,
    expect_quality,
    expect_political_bias,
    expect_complexity,
    expect_entertainment,
    trust_researchers
  )],
  function(x) sum(is.na(x))
)

#check final ordering 
lapply(
  dt_main[, .(
    expect_accuracy,
    expect_trustworthiness,
    expect_quality,
    expect_political_bias,
    expect_complexity,
    expect_entertainment,
    trust_researchers
  )],
  levels
)

# Check whether expect_accuracy and expect_trustworthiness are nearly identical.
table(dt_main$expect_accuracy, dt_main$expect_trustworthiness, useNA = "ifany")
mean(dt_main$expect_accuracy == dt_main$expect_trustworthiness, na.rm = TRUE)
prop.table(table(dt_main$expect_accuracy, dt_main$expect_trustworthiness), 1)


# -----------------------------------------------------------------------------
# 7. NUMERIC MECHANISM VARIABLES + Z-SCORES + INDEX
#    (kept congruent with the earlier analysis)
# -----------------------------------------------------------------------------

# 7a. Numeric versions of ordered mechanism variables
dt_main[, expect_accuracy_num := as.numeric(expect_accuracy)]
dt_main[, expect_trustworthiness_num := as.numeric(expect_trustworthiness)]
dt_main[, expect_quality_num := as.numeric(expect_quality)]
dt_main[, expect_complexity_num := as.numeric(expect_complexity)]
dt_main[, expect_entertainment_num := as.numeric(expect_entertainment)]
dt_main[, trust_researchers_num := as.numeric(trust_researchers)]

# Political bias: directional scale
# with your current ordering:
# Very left-wing bias = 1 ... No bias/Neutral = 3 ... Very right-wing bias = 5
# so convert to -2 ... +2
dt_main[, expect_political_bias_num := as.numeric(expect_political_bias) - 3]

# 7b. Binary mechanism dummies (same logic as earlier analysis)
dt_main[, not_accurate := as.integer(expect_accuracy_num %in% c(1, 2))]
dt_main[, not_trustworthy := as.integer(expect_trustworthiness_num %in% c(1, 2))]
dt_main[, low_quality := as.integer(expect_quality_num %in% c(1, 2))]
dt_main[, biased := as.integer(!is.na(expect_political_bias_num) & expect_political_bias_num != 0)]
dt_main[, not_entertaining := as.integer(expect_entertainment_num %in% c(1, 2))]
dt_main[, very_complex := as.integer(expect_complexity_num %in% c(4, 5))]
dt_main[, no_researcher_trust := as.integer(trust_researchers_num %in% c(1, 2))]

# 7c. Z-scores for mechanism variables
z_cols <- c(
  "expect_accuracy_num",
  "expect_trustworthiness_num",
  "expect_quality_num",
  "expect_political_bias_num",
  "expect_complexity_num",
  "trust_researchers_num",
  "expect_entertainment_num"
)

for (v in z_cols) {
  dt_main[, (paste0("z_", v)) := as.numeric(scale(get(v)))]
}

# 7d. Main perceived-quality index:
# same structure as earlier analysis = row mean of z(accuracy, trustworthiness, quality)
dt_main[, z_index := rowMeans(.SD, na.rm = TRUE),
        .SDcols = c("z_expect_accuracy_num",
                    "z_expect_trustworthiness_num",
                    "z_expect_quality_num")]


# Check
summary(dt_main[, .(
  expect_accuracy_num,
  expect_trustworthiness_num,
  expect_quality_num,
  expect_political_bias_num,
  expect_complexity_num,
  trust_researchers_num,
  expect_entertainment_num,
  z_index
)])

lapply(
  dt_main[, .(
    not_accurate,
    not_trustworthy,
    low_quality,
    biased,
    not_entertaining,
    very_complex,
    no_researcher_trust
  )],
  table,
  useNA = "ifany"
)

# -----------------------------------------------------------------------------
# 8. CONVERT YES/NO VARIABLES TO LOGICAL
# -----------------------------------------------------------------------------

yes_no_vars <- c(
  "newsletter_subscribed",
  "newsletter_takeup",
  "belief_image_ai",
  "born_uk"
)

for (v in intersect(yes_no_vars, names(dt_main))) {
  dt_main[, (v) := fifelse(
    get(v) == "Yes", TRUE,
    fifelse(get(v) == "No", FALSE, NA)
  )]
}

str(dt_main[, .(
  newsletter_subscribed,
  newsletter_takeup,
  belief_image_ai,
  born_uk
)])

lapply(
  dt_main[, .(
    newsletter_subscribed,
    newsletter_takeup,
    belief_image_ai,
    born_uk
  )],
  table,
  useNA = "ifany"
)

# -----------------------------------------------------------------------------
# 9. CORRECT RECOGNITION VARIABLE
# -----------------------------------------------------------------------------

# actual AI image shown
dt_main[, actual_ai := treatment %in% c("EAI", "LEAI", "HAI", "LHAI")]

# correct recognition:
# TRUE if respondent correctly identifies whether image was AI or not
dt_main[, recognition_correct := (belief_image_ai == actual_ai)]

# quick checks
table(dt_main$actual_ai, useNA = "ifany")
table(dt_main$recognition_correct, useNA = "ifany")
table(dt_main$actual_ai, dt_main$belief_image_ai, useNA = "ifany")

### Final check and SAVE

# check mismatches
sum(dt_main$prolific_id_entry != dt_main$prolific_id, na.rm = TRUE)
dt_main[prolific_id_entry != prolific_id, .(prolific_id_entry, prolific_id)][1:20]

# last check 
str(dt_main)

# Save
fwrite(dt_main, "DATA/ai_labels_mechs_cleaned.csv")
saveRDS(dt_main, "DATA/ai_labels_mechs_cleaned.rds")
