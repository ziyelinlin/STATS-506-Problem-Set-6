set.seed(123)
n <- 3e6

## 1. Demographics
age <- pmin(pmax(rnorm(n, mean = 30, sd = 8), 16), 70)

employment_status <- factor(
  sample(c("student","full_time","part_time","unemployed"),
         n, TRUE, prob = c(0.35,0.35,0.2,0.1))
)

country <- factor(
  sample(c("US","India","Lithuania","Germany","Nigeria","Other"),
         n, TRUE, prob = c(6, 5, .1, 3, .2, 10))
)

device_type <- factor(
  sample(c("desktop","laptop","tablet","phone"),
         n, TRUE, prob = c(0.25,0.4,0.1,0.25))
)

course_difficulty <- factor(
  sample(c("intro","intermediate","advanced"),
         n, TRUE, prob = c(0.5,0.3,0.2)),
  ordered = TRUE
)

## 2. Prior GPA
base_gpa <- 3.0 + 0.2 * (age - 30) / 10 +
  ifelse(employment_status == "student", 0.1, 0) -
  ifelse(employment_status == "unemployed", 0.05, 0)

prior_gpa <- pmin(pmax(rnorm(n, mean = base_gpa, sd = 0.4), 0), 4)

## 3. Weekly study hours
study_mean <- 5 + ifelse(device_type %in% c("desktop","laptop"), 1.5, 0) +
  ifelse(employment_status == "full_time", -1, 0) +
  ifelse(employment_status == "student", 1.2, 0) +
  ifelse(course_difficulty == "advanced", 1.5, 0) +
  ifelse(course_difficulty == "intro", -0.5, 0)

weekly_study_hours <- pmax(rnorm(n, mean = study_mean, sd = 2), 0)

## 4. Engagement
lambda_forum <- pmax(0.2 + 0.15 * weekly_study_hours + 0.5 * (prior_gpa - 3) +
                       ifelse(course_difficulty == "advanced", 1, 0),
                     0.05)

forum_posts <- rpois(n, lambda_forum)

lambda_quiz <- pmax(1 + 0.25 * weekly_study_hours + 0.8 * (prior_gpa - 2.5) +
                      ifelse(course_difficulty == "advanced", 0.5, 0) +
                      ifelse(course_difficulty == "intro", -0.3, 0),
                    0.1)

quiz_attempts <- rpois(n, lambda_quiz)

session_minutes <- pmax(weekly_study_hours * 60 *
                          ifelse(device_type %in% c("phone","tablet"), 0.8, 1.0) *
                          exp(rnorm(n, 0, 0.15)),
                        10)

## 5. Paid subscription
logit_paid <- -1.2 + 0.6 * (prior_gpa - 3) + 0.25 * (weekly_study_hours - 5) +
  ifelse(country %in% c("US","Germany"), 0.5, 0) -
  ifelse(country == "Nigeria", 0.4, 0)

p_paid <- plogis(logit_paid)
paid_subscription <- rbinom(n, 1, p_paid)

## 6. Final score
difficulty_penalty <- ifelse(course_difficulty=="intro", -5,
                             ifelse(course_difficulty=="advanced", 5, 0))

raw_score <- 60 + 10 * (prior_gpa - 3) + 1.5 * weekly_study_hours +
  0.4 * forum_posts + 0.2 * quiz_attempts + difficulty_penalty + rnorm(n, 0, 8)

final_score <- pmin(pmax(raw_score, 0), 100)

## 7. Completion
logit_complete <- -4 + 0.06 * final_score + 0.02 * forum_posts +
  0.01 * quiz_attempts + 0.6 * paid_subscription -
  0.2 * (course_difficulty == "advanced")

p_complete <- plogis(logit_complete)
completed_course <- rbinom(n, 1, p_complete)

## 8. Dropout
dropout_base <- 0.6 - 0.02 * weekly_study_hours - 0.01 * forum_posts -
  0.01 * quiz_attempts - 0.03 * (final_score - 60) / 10 +
  0.3 * (course_difficulty == "advanced")

dropout_base <- pmin(pmax(dropout_base, 0.05), 0.95)
early_dropout <- rbinom(n, 1, dropout_base)

time_to_dropout <- rep(NA_real_, n)
mask <- (early_dropout == 1 & completed_course == 0)
time_to_dropout[mask] <- pmax(rgamma(sum(mask), shape = 2, rate = 0.5), 0.5)
early_dropout[completed_course == 1] <- 0

## Final dataset
df <- data.frame(
  age,
  country,
  device_type,
  employment_status,
  course_difficulty,
  prior_gpa,
  weekly_study_hours,
  forum_posts,
  quiz_attempts,
  session_minutes,
  paid_subscription,
  final_score,
  completed_course,
  early_dropout,
  time_to_dropout,
  stringsAsFactors = FALSE
)

rm(age,
   country,
   device_type,
   employment_status,
   course_difficulty,
   prior_gpa,
   weekly_study_hours,
   forum_posts,
   quiz_attempts,
   session_minutes,
   paid_subscription,
   final_score,
   completed_course,
   early_dropout,
   time_to_dropout,
   base_gpa,
   difficulty_penalty,
   dropout_base,
   lambda_quiz,
   lambda_forum,
   logit_complete,
   logit_paid,
   mask,
   n,
   p_complete,
   p_paid,
   raw_score,
   study_mean)

cat("Data.frame `df` is", format(object.size(df), units = "MB"), "\n")