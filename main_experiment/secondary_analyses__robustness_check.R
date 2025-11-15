library(lmerTest)
library(dplyr)

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

data <- data %>% filter(timepoint != 'intermediate')
data$timepoint <- factor(data$timepoint, levels = c("before", "after"))
levels(data$timepoint)

model <- lmer(
  logit ~ group * timepoint + 
    (1 | participant_code) + (1 | topic/item_id),
  data = data
)

summary(model)