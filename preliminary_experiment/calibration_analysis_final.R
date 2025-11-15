library(tidyverse)

data <- read.csv("calibration.csv", stringsAsFactors = FALSE)

data$extra_incentive <- ifelse(data$extra_incentive == "True", TRUE, FALSE)
data$show_debate <- ifelse(data$show_debate == "True", TRUE, FALSE)
data$exploratory_calibration_1 <- as.numeric(ifelse(data$exploratory_calibration_1 == "None", NA, data$exploratory_calibration_1))
data$participant_id <- as.factor(data$participant_id)

printDataTypes <- function(data) {
  for (col in colnames(data)) {
    cat("Column '", col, "' is of type '", class(data[[col]]), "'\n")
  }
}

printDataTypes(data)

cor.test(data$exploratory_calibration_1, data$exploratory_calibration_2)
cor.test(data$brier_score, data$exploratory_calibration_1)
cor.test(data$brier_score, data$exploratory_calibration_2)

model <- lm(data$exploratory_calibration_1 ~ data$extra_incentive * data$show_debate)
print(summary(model))
model <- lm(data$exploratory_calibration_2 ~ data$extra_incentive * data$show_debate)
print(summary(model))
model <- lm(data$brier_score ~ data$extra_incentive * data$show_debate)
print(summary(model))

t_result <- t.test(data$brier_score ~ data$show_debate)
print(t_result)

t_result <- t.test(data$brier_score ~ data$show_debate)
print(t_result)

