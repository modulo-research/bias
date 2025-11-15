
library(lmerTest)
library(dplyr)
library(ggplot2)

data <- read.csv("tidy_data.csv", stringsAsFactors = FALSE)

data <- data %>%
  mutate(
    participant_code = as.factor(participant_code),
    group = as.factor(group),
    medical_experience = as.numeric(medical_experience),
    legal_experience = as.numeric(legal_experience),
    conlang_experience = as.numeric(conlang_experience),
    native_english = as.numeric(native_english),
    item_id = as.factor(item_id),
    topic = as.factor(topic),
    timepoint = as.factor(timepoint),
    correct_answer = as.factor(correct_answer),
    probability_assigned_to_a = as.numeric(probability_assigned_to_a),
    logit = as.numeric(logit)
  )

data <- data %>% filter(group != 'no assistance')

data <- data %>%
  mutate(
    medical_experience = ifelse(medical_experience == "", NA, medical_experience),
    legal_experience = ifelse(legal_experience == "", NA, legal_experience),
    conlang_experience = ifelse(conlang_experience == "", NA, conlang_experience),
    native_english = ifelse(native_english == "", NA, native_english)
  ) %>%
  filter(
    !is.na(medical_experience),
    !is.na(legal_experience),
    !is.na(conlang_experience),
    !is.na(native_english)
  )

data <- data %>% filter(timepoint != 'intermediate')
data$timepoint <- factor(data$timepoint, levels = c("before", "after"))
levels(data$timepoint)

model <- lmer(
  logit ~ group * timepoint + medical_experience + legal_experience +
    conlang_experience + native_english +
    (1 | participant_code) + (1 | topic/item_id),
  data = data
)

summary(model)

confint(model, parm = "groupintervention:timepointbefore", method = "Wald")

exp(confint(model, parm = "groupintervention:timepointbefore", method = "Wald"))

library(emmeans)

emm <- emmeans(model, ~ group * timepoint)

# Contrast vector:  (-1, 1, 1, -1) = (C_before – C_after) – (I_before – I_after)
did   <- contrast(
  emm,
  method = list("intervention minus control change" = c(-1, 1, 1, -1)),
  adjust = "none"
)

summary(did, infer = TRUE)

##########

med_mean <- mean(data$medical_experience, na.rm = TRUE)
leg_mean <- mean(data$legal_experience, na.rm = TRUE)
con_mean <- mean(data$conlang_experience, na.rm = TRUE)
nat_mean <- mean(data$native_english, na.rm = TRUE)

pred_df <- expand.grid(
  group = c("control", "intervention"), 
  timepoint = c("before", "after"), 
  medical_experience  = med_mean,
  legal_experience    = leg_mean,
  conlang_experience  = con_mean,
  native_english      = nat_mean
)

pred_df$logit_pred <- predict(model, newdata = pred_df, re.form = NA)

pred_df$prob_pred <- plogis(pred_df$logit_pred)   # convert logit to probability

ggplot(pred_df, aes(x = timepoint, y = prob_pred, color = group, group = group)) +
  geom_point(position = position_dodge(width = 0.2), size = 3) +
  geom_line(position = position_dodge(width = 0.2), linewidth = 1) +
  labs(
    x = "Timepoint",
    y = "Predicted Log-Odds of\nProbability Assigned to Correct Answer",
    color = "Group"
  ) +
  theme_minimal(base_size = 14)
