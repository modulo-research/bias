library(lmerTest)
library(dplyr)

data <- read.csv("tidy_data.csv", stringsAsFactors = FALSE)

data <- data %>%
  mutate(
    participant_code = as.factor(participant_code),
    wave = if_else(
      as.integer(str_remove(as.character(participant_code), "^P")) <= 67,
      1L,
      2L
    ),
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

model_wave1 <- lmer(
  logit ~ group * timepoint + medical_experience + legal_experience +
    conlang_experience + native_english +
    (1 | participant_code) + (1 | topic/item_id),
  data = filter(data, wave == 1)
)
summary(model_wave1)

model_wave2 <- lmer(
  logit ~ group * timepoint + medical_experience + legal_experience +
    conlang_experience + native_english +
    (1 | participant_code) + (1 | topic/item_id),
  data = filter(data, wave == 2)
)
summary(model_wave2)
