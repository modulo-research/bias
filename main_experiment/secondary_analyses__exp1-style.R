library(lmerTest)
library(dplyr)
library(tidyverse)

data <- read.csv("tidy_data_logit_diffs.csv", stringsAsFactors = FALSE)

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
    llm_answer = factor(llm_answer, levels = c("A", "B")),
    correct_answer = factor(correct_answer, levels = c("A", "B")),
    model_correct = as.numeric(model_correct),
    llm_logit_difference = as.numeric(llm_logit_difference)
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

model <- lmer(
  llm_logit_difference ~ model_correct * group + medical_experience + legal_experience +
    conlang_experience + native_english +
    (1 | participant_code) + (1 | topic/item_id),
  data = data
)

summary(model)

###########################
library(dplyr)
library(ggplot2)

interaction_df <- data %>%                           
  group_by(model_correct, group) %>%                 
  summarise(
    mean      = mean(llm_logit_difference, na.rm = TRUE),
    sd        = sd(llm_logit_difference,   na.rm = TRUE),
    n         = n(),
    se        = sd / sqrt(n),
    ci_lower  = mean - qt(0.975, df = n - 1) * se,
    ci_upper  = mean + qt(0.975, df = n - 1) * se,
    .groups   = "drop"
  ) %>%
  mutate(
    model_status = factor(model_correct,
                          levels = c(0, 1),
                          labels = c("LLM answer incorrect",
                                     "LLM answer correct"))
  )

p <- ggplot(interaction_df,
            aes(x = model_status,
                y = mean,
                colour = group,
                group = group)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.15) +
  labs(x = NULL,
       y = "Mean logit difference (± 95% CI)",
       colour = "Assistance group",
       title = "Interaction between LLM correctness and assistance condition") +
  scale_colour_brewer(palette = "Set1") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right")

print(p)

ggsave("figures/interaction_plot.png",
       plot   = p,
       width  = 6,
       height = 4,
       units  = "in",
       dpi    = 300)



