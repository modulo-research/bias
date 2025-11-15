
library(lme4)
library(tidyverse)
library(lmerTest)
library(ggplot2)
library(dplyr)
library(MuMIn)
library(stats)
library(TOSTER)

data <- read.csv("data_format_1.csv", stringsAsFactors = FALSE)

data$extra_incentive <- ifelse(data$extra_incentive == "True", TRUE, FALSE)
data$show_debate <- ifelse(data$show_debate == "True", TRUE, FALSE)
data$lolo_attempted <- ifelse(data$lolo_attempted == "True", TRUE, FALSE)
data$lolo_correct <- ifelse(data$lolo_correct == "True", TRUE, FALSE)
data$model_correct <- ifelse(data$model_correct == "True", TRUE, FALSE)

data <- data %>%
  mutate(across(c(numeracy, crt2, confidence), ~na_if(., "None")))

data <- data %>%
  mutate(across(c(numeracy, crt2, confidence), as.numeric))

convertCharacterToFactor <- function(df) {
  for (col in colnames(df)) {
    if (is.character(df[[col]])) {
      df[[col]] <- as.factor(df[[col]])
    }
  }
  return(df)
}

data$q_order <- as.factor(data$q_order)
data <- convertCharacterToFactor(data)

printDataTypes <- function(data) {
  for (col in colnames(data)) {
    cat("Column '", col, "' is of type '", class(data[[col]]), "'\n")
  }
}

printDataTypes(data)

result <- data %>%
  filter(timepoint == 1) %>%
  group_by(participant_id) %>%
  summarise(mean_logodds = mean(logodds_assigned_to_correct, na.rm = TRUE))

mean_logodds_vector <- result$mean_logodds

result <- data %>%
  filter(timepoint == 1) %>%
  group_by(participant_id) %>%
  summarise(mean_correctness = mean(correctness, na.rm = TRUE))

mean_correctness_vector <- result$mean_correctness

print(paste('Timepoint == 1, mean correctness', mean(mean_correctness_vector)))
print(paste('Timepoint == 1, correctness sd', sd(mean_correctness_vector)))

print(paste('Timepoint == 1, mean logodds', mean(mean_logodds_vector)))
print(paste('Timepoint == 1, logodds sd', sd(mean_logodds_vector)))

print('-------')

result <- data %>%
  filter(timepoint == 0) %>%
  group_by(participant_id) %>%
  summarise(mean_logodds = mean(logodds_assigned_to_correct, na.rm = TRUE))

mean_logodds_vector <- result$mean_logodds

result <- data %>%
  filter(timepoint == 0) %>%
  group_by(participant_id) %>%
  summarise(mean_correctness = mean(correctness, na.rm = TRUE))

mean_correctness_vector <- result$mean_correctness

print(paste('Timepoint == 0, mean correctness', mean(mean_correctness_vector)))
print(paste('Timepoint == 0, correctness sd', sd(mean_correctness_vector)))

print(paste('Timepoint == 0, mean logodds', mean(mean_logodds_vector)))
print(paste('Timepoint == 0, logodds sd', sd(mean_logodds_vector)))

result <- data %>%
  filter(model_correct == FALSE) %>%
  filter(timepoint == 1) %>%
  group_by(participant_id) %>%
  summarise(mean_correctness = mean(correctness, na.rm = TRUE))


##################################

# Model for H1: The probability that participants assign to the correct answer will increase after interacting with the model.
# Model for H6: Compared to participants in the "arguments absent" group, participants in the "arguments present" group will have a greater increase in the probability that they assign to the correct answer after interacting with the model.

# Simplified model
model_h1_h6_simplified <- lm(logodds_assigned_to_correct ~ timepoint * show_debate + numeracy + total_logic_score, data = data)
summary(model_h1_h6_simplified)

# Hypotheses H2 and H7

logistic_model_simplified <- glm(correctness ~ timepoint * show_debate + numeracy + total_logic_score, 
                                 data = data, 
                                 family = binomial(link="logit"))
summary(logistic_model_simplified)


##################################

# Hypothesis H4
# Simplified
model_H4_simplified <- lm(logodds_assigned_to_model_guess ~ timepoint + numeracy + total_logic_score, data = data)
summary(model_H4_simplified)

######

# Hypothesis H3
df_acc <- data %>%
  filter(timepoint == 1) %>%
  group_by(participant_id) %>%
  summarise(accuracy = mean(correctness),
            extra_incentive = first(extra_incentive),
            show_debate = first(show_debate),
            numeracy = first(numeracy),
            crt2 = first(crt2),
            filler_score = first(filler_score),
            core_score = first(core_score),
            total_logic_score = first(total_logic_score),
            lolo_attempted = first(lolo_attempted),
            lolo_correct = first(lolo_correct))

cor.test(df_acc$accuracy, as.integer(df_acc$lolo_correct))
cor.test(df_acc$accuracy, df_acc$numeracy)
cor.test(df_acc$accuracy, df_acc$crt2)
cor.test(df_acc$accuracy, df_acc$total_logic_score)
cor.test(df_acc$numeracy, df_acc$total_logic_score)
######

# Hypothesis H5
change_in_logodds_assigned_to_model_guess <- data$logodds_assigned_to_model_guess[data$timepoint == 1] - data$logodds_assigned_to_model_guess[data$timepoint == 0]
change_in_logodds_assigned_to_model_guess <- rep(change_in_logodds_assigned_to_model_guess, each = 2)
data$change_in_logodds_assigned_to_model_guess <- change_in_logodds_assigned_to_model_guess

data_one_timepoint_only <- filter(data, timepoint == 1)


model_H5_simplified <- lm(change_in_logodds_assigned_to_model_guess ~ model_correct * show_debate + numeracy + total_logic_score, data = data_one_timepoint_only)
summary(model_H5_simplified)


####

# Hypothesis H8

model_H8_simplified <- lm(change_in_logodds_assigned_to_model_guess ~ model_correct * show_debate + numeracy + total_logic_score, data = data_one_timepoint_only)
summary(model_H8_simplified)

