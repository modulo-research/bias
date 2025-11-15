
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

# For power analysis
print(paste('Timepoint == 1, mean correctness', mean(mean_correctness_vector)))
print(paste('Timepoint == 1, correctness sd', sd(mean_correctness_vector)))

# For random effects model simulation
print(paste('Timepoint == 1, mean logodds', mean(mean_logodds_vector)))
print(paste('Timepoint == 1, logodds sd', sd(mean_logodds_vector)))

print('-------')

result <- data %>%
  filter(timepoint == 0) %>%
  group_by(participant_id) %>%
  summarise(mean_logodds = mean(logodds_assigned_to_correct, na.rm = TRUE))

mean_logodds_vector <- result$mean_logodds

# Filter, group and calculate mean correctness (accuracy)
result <- data %>%
  filter(timepoint == 0) %>%
  group_by(participant_id) %>%
  summarise(mean_correctness = mean(correctness, na.rm = TRUE))

mean_correctness_vector <- result$mean_correctness

print(paste('Timepoint == 0, mean correctness', mean(mean_correctness_vector)))
print(paste('Timepoint == 0, correctness sd', sd(mean_correctness_vector)))

print(paste('Timepoint == 0, mean logodds', mean(mean_logodds_vector)))
print(paste('Timepoint == 0, logodds sd', sd(mean_logodds_vector)))


# Filter, group and calculate mean correctness (accuracy), timepoint == 1, when model is wrong
result <- data %>%
  filter(model_correct == FALSE) %>%
  filter(timepoint == 1) %>%
  group_by(participant_id) %>%
  summarise(mean_correctness = mean(correctness, na.rm = TRUE))

model_h1_h6_full <- lmer(logodds_assigned_to_correct ~ timepoint * show_debate + numeracy + crt2 + topic + extra_incentive + (1 | q_id) + (1 | participant_id), data = data)
summary(model_h1_h6_full)
r.squaredGLMM(model_h1_h6_full) # the marginal R-squared value gives the proportion of variance explained by fixed effects, while the conditional R-squared value gives the proportion of variance explained by the entire model (both fixed and random effects).

# Simplified model
model_h1_h6_simplified <- lm(logodds_assigned_to_correct ~ timepoint * show_debate, data = data)
summary(model_h1_h6_simplified)

# Interpretation of simplified model: H1

coefficients <- coef(model_h1_h6_simplified)
intercept <- coefficients["(Intercept)"]
timepoint_coef <- coefficients["timepoint"]

log_odds_pre <- intercept
prob_pre <- exp(log_odds_pre) / (1 + exp(log_odds_pre))

log_odds_post <- intercept + timepoint_coef
prob_post <- exp(log_odds_post) / (1 + exp(log_odds_post))

abs_diff <- prob_post - prob_pre
rel_diff <- abs_diff / prob_pre * 100

cat("Predicted probability at pre timepoint:", round(prob_pre, 3), "\n")
cat("Predicted probability at post timepoint:", round(prob_post, 3), "\n")
cat("Absolute change in probability:", round(abs_diff, 3), "\n")
cat("Relative change in probability:", round(rel_diff, 1), "%\n")

# Hypotheses H2 and H7

logistic_model_full <- glmer(correctness ~ timepoint * show_debate + numeracy + crt2 + topic + extra_incentive + (1 | q_id) + (1 | participant_id), 
                        data = data, 
                        family = binomial(link="logit")) # Fails to converge

logistic_model_simplified <- glm(correctness ~ timepoint * show_debate, 
                                 data = data, 
                                 family = binomial(link="logit"))
summary(logistic_model_simplified)


coefficients <- coef(logistic_model_simplified)
intercept <- coefficients["(Intercept)"]
timepoint_coef <- coefficients["timepoint"]

# Calculate probabilities for pre-test (timepoint = 0)
log_odds_pre <- intercept
prob_pre <- plogis(log_odds_pre)  # same as exp(log_odds_pre) / (1 + exp(log_odds_pre))

# Calculate probabilities for post-test (timepoint = 1)
log_odds_post <- intercept + timepoint_coef
prob_post <- plogis(log_odds_post)

cat("Probability of correct response at pre-test:", round(prob_pre * 100, 1), "%\n")
cat("Probability of correct response at post-test:", round(prob_post * 100, 1), "%\n")
cat("Increase in probability (percentage points):", round((prob_post - prob_pre) * 100, 1), "\n")
cat("Relative increase in probability:", round(((prob_post - prob_pre) / prob_pre) * 100, 1), "%\n")


###########################
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

t_test <- t.test(df_acc$accuracy, mu = 0.75)

print(t_test)     # significantly WORSE than 75%

# Were people more accurate if they had an extra incentive?
# or if they saw the debate?
incentive_test <- t.test(accuracy ~ extra_incentive, data=df_acc)
print(incentive_test)
debate_test <- t.test(accuracy ~ show_debate, data=df_acc)
print(debate_test)

df_high_numeracy <- df_acc %>%
  filter(numeracy == 7)
t_test <- t.test(df_high_numeracy$accuracy, mu = 0.75)
print(t_test)
debate_test <- t.test(accuracy ~ show_debate, data=df_high_numeracy)
print(debate_test)

##################################
# Create the accuracy plots
##################################

create_accuracy_df <- function(data_subset) {
  accuracy_df <- data_subset %>%
    filter(timepoint == 1) %>%
    group_by(participant_id) %>%
    summarise(
      accuracy = mean(correctness, na.rm = TRUE),
      extra_incentive = first(extra_incentive),
      show_debate = first(show_debate),
      numeracy = first(numeracy),
      crt2 = first(crt2),
      filler_score = first(filler_score),
      core_score = first(core_score),
      total_logic_score = first(total_logic_score),
      lolo_attempted = first(lolo_attempted),
      lolo_correct = first(lolo_correct),
      n_questions = n()  # Count of questions in this subset for this participant
    )
  return(accuracy_df)
}

data_model_correct <- data %>% filter(model_correct == TRUE)
data_model_incorrect <- data %>% filter(model_correct == FALSE)

df_acc_model_correct <- create_accuracy_df(data_model_correct)
df_acc_model_incorrect <- create_accuracy_df(data_model_incorrect)

names(df_acc_model_correct)[names(df_acc_model_correct) == "accuracy"] <- "accuracy_model_correct"
names(df_acc_model_incorrect)[names(df_acc_model_incorrect) == "accuracy"] <- "accuracy_model_incorrect"

cat("Summary for questions where model was correct:\n")
print(summary(df_acc_model_correct$accuracy_model_correct))
cat("\nNumber of participants with model-correct questions:", nrow(df_acc_model_correct), "\n")
cat("Mean participant accuracy when model is correct:", mean(df_acc_model_correct$accuracy_model_correct, na.rm = TRUE), "\n")

cat("\nSummary for questions where model was incorrect:\n")
print(summary(df_acc_model_incorrect$accuracy_model_incorrect))
cat("\nNumber of participants with model-incorrect questions:", nrow(df_acc_model_incorrect), "\n")
cat("Mean participant accuracy when model is incorrect:", mean(df_acc_model_incorrect$accuracy_model_incorrect, na.rm = TRUE), "\n")

prepare_condition_data <- function(df, acc_column, show_debate_value, condition_name) {
  df %>%
    filter(show_debate == show_debate_value) %>%
    summarise(
      mean_accuracy = mean(!!sym(acc_column), na.rm = TRUE),
      se = sd(!!sym(acc_column), na.rm = TRUE) / sqrt(sum(!is.na(!!sym(acc_column)))),
      n = sum(!is.na(!!sym(acc_column))),
      condition = condition_name
    ) %>%
    mutate(
      lower_ci = mean_accuracy - qt(0.975, n-1) * se,
      upper_ci = mean_accuracy + qt(0.975, n-1) * se
    )
}

conditions <- list(
  list(df = df_acc_model_incorrect, acc_column = "accuracy_model_incorrect", 
       show_debate = FALSE, name = "Arguments absent,\nLLM's guess incorrect"),
  list(df = df_acc_model_correct, acc_column = "accuracy_model_correct", 
       show_debate = FALSE, name = "Arguments absent,\nLLM's guess correct"),
  list(df = df_acc_model_incorrect, acc_column = "accuracy_model_incorrect", 
       show_debate = TRUE, name = "Arguments present,\nLLM's guess incorrect"),
  list(df = df_acc_model_correct, acc_column = "accuracy_model_correct", 
       show_debate = TRUE, name = "Arguments present,\nLLM's guess correct")
)

plot_data <- map_dfr(conditions, function(cond) {
  prepare_condition_data(cond$df, cond$acc_column, cond$show_debate, cond$name)
})

plot_data$condition <- factor(plot_data$condition, 
                              levels = c("Arguments absent,\nLLM's guess incorrect", 
                                         "Arguments absent,\nLLM's guess correct",
                                         "Arguments present,\nLLM's guess incorrect",
                                         "Arguments present,\nLLM's guess correct"))

bar_colors <- c(
  "Arguments absent,\nLLM's guess incorrect" = "#D3D3D3",
  "Arguments absent,\nLLM's guess correct"   = "#808080",
  "Arguments present,\nLLM's guess incorrect" = "#A4E5E3",
  "Arguments present,\nLLM's guess correct"   = "#4FBDBA"
)

p <- ggplot(plot_data, aes(x = condition, y = mean_accuracy, fill = condition)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), 
                width = 0.2, position = position_dodge(0.7)) +
  scale_fill_manual(values = bar_colors) +
  labs(y = "Participant accuracy\n", x = "") +
  theme_minimal() +
  theme(
    text = element_text(size = 24),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = "none"
  ) +
  scale_y_continuous(limits = c(0, 1))

print(p)

ggsave("figures/acc_plot.png", plot = p, width = 922/72, height = 672/72, units = "in", dpi = 300)

cat("Sample sizes for each condition:\n")
print(plot_data %>% select(condition, n))


############

# Extract the subset of data where the LLM's guess was incorrect
incorrect_data <- df_acc_model_incorrect

# Perform t-test comparing accuracy between arguments present vs absent groups
# when the LLM's guess was incorrect
t_test_result <- t.test(
  accuracy_model_incorrect ~ show_debate, 
  data = incorrect_data
)

print(t_test_result)

group_means <- incorrect_data %>%
  group_by(show_debate) %>%
  summarise(
    mean_accuracy = mean(accuracy_model_incorrect, na.rm = TRUE),
    sd = sd(accuracy_model_incorrect, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n)
  ) %>%
  mutate(
    group_label = ifelse(show_debate, "Arguments present", "Arguments absent")
  )

print(group_means)

##################################

# Hypothesis H4
# Original
model_H4_full <- lmer(logodds_assigned_to_model_guess ~ timepoint + numeracy + crt2 + topic + extra_incentive + (1 | q_id) + (1 | participant_id), data = data)
summary(model_H4_full)

# Simplified
model_H4_simplified <- lm(logodds_assigned_to_model_guess ~ timepoint, data = data)
summary(model_H4_simplified)


coefficients <- coef(model_H4_simplified)
intercept <- coefficients["(Intercept)"]
timepoint_coef <- coefficients["timepoint"]

log_odds_pre <- intercept
prob_pre <- exp(log_odds_pre) / (1 + exp(log_odds_pre))

log_odds_post <- intercept + timepoint_coef
prob_post <- exp(log_odds_post) / (1 + exp(log_odds_post))

abs_diff <- prob_post - prob_pre
rel_diff <- abs_diff / prob_pre * 100

cat("Predicted probability assigned to model guess at pre timepoint:", round(prob_pre, 3), "\n")
cat("Predicted probability assigned to model guess at post timepoint:", round(prob_post, 3), "\n")
cat("Absolute change in probability:", round(abs_diff, 3), "\n")
cat("Relative change in probability:", round(rel_diff, 1), "%\n")



###

confint(model_H4, "timepoint", level = 0.95)

###

# The following wasn't in your preregistered hypotheses, so it's exploratory, 
# but does actually show an interaction with timepoint and show_debate:
# people trust the model less when they see the debate.
# So that's kind of interesting.
model_H4b <- lm(logodds_assigned_to_model_guess ~ timepoint * show_debate + numeracy + crt2 + topic + extra_incentive, data = data)
summary(model_H4b)

# Calculate means and standard errors
interaction_df <- data %>%
  group_by(timepoint, show_debate) %>%
  summarise(mean = mean(logodds_assigned_to_model_guess, na.rm = TRUE),
            se = sd(logodds_assigned_to_model_guess, na.rm = TRUE) / sqrt(n()), 
            .groups = "drop")

# Convert 'timepoint' to a factor for plotting
interaction_df$timepoint <- factor(interaction_df$timepoint)

# Create interaction plot
ggplot(interaction_df, aes(x = timepoint, y = mean, color = show_debate)) +
  geom_line(aes(group = show_debate), size = 1.5) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
  labs(x = "Timepoint", y = "Log Odds Assigned to Model Guess", 
       color = "Show Debate") +
  theme_minimal()


######

# Hypothesis H5 - Original code
# Create a variable for the change in log odds
change_in_logodds_assigned_to_model_guess <- data$logodds_assigned_to_model_guess[data$timepoint == 1] - data$logodds_assigned_to_model_guess[data$timepoint == 0]
change_in_logodds_assigned_to_model_guess <- rep(change_in_logodds_assigned_to_model_guess, each = 2)
data$change_in_logodds_assigned_to_model_guess <- change_in_logodds_assigned_to_model_guess

# Since our DV is now a difference between timepoints, it doesn't make sense to continue having
# a separate datapoint for each timepoint for this analysis
data_one_timepoint_only <- filter(data, timepoint == 1)

cat("============================================\n")
cat("ANALYSIS FOR COMPLETE DATASET\n")
cat("============================================\n")

model_H5_full <- lmer(change_in_logodds_assigned_to_model_guess ~ model_correct * show_debate + numeracy + crt2 + topic + extra_incentive + (1 | q_id) + (1 | participant_id), 
                      data = data_one_timepoint_only)
cat("Full Mixed-Effects Model Summary:\n")
print(summary(model_H5_full))

cat("\nConfidence Interval for model_correctTRUE:\n")
print(confint(model_H5_full, "model_correctTRUE", level = 0.95))

model_H5_simplified <- lm(change_in_logodds_assigned_to_model_guess ~ model_correct * show_debate, data = data_one_timepoint_only)
cat("\nSimplified Model Summary:\n")
print(summary(model_H5_simplified))

cat("\n\n============================================\n")
cat("ANALYSIS FOR SUBSET: ARGUMENTS PRESENT (show_debate == TRUE)\n")
cat("============================================\n")

data_debate_true <- filter(data_one_timepoint_only, show_debate == TRUE)

model_H5_full_debate_true <- lmer(change_in_logodds_assigned_to_model_guess ~ model_correct + numeracy + crt2 + topic + extra_incentive + (1 | q_id) + (1 | participant_id), 
                                  data = data_debate_true)
cat("Full Mixed-Effects Model Summary (Arguments Present):\n")
print(summary(model_H5_full_debate_true))

cat("\nConfidence Interval for model_correctTRUE:\n")
print(confint(model_H5_full_debate_true, "model_correctTRUE", level = 0.95))

model_H5_simplified_debate_true <- lm(change_in_logodds_assigned_to_model_guess ~ model_correct, data = data_debate_true)
cat("\nSimplified Model Summary (Arguments Present):\n")
print(summary(model_H5_simplified_debate_true))

cat("\n\n============================================\n")
cat("ANALYSIS FOR SUBSET: ARGUMENTS ABSENT (show_debate == FALSE)\n")
cat("============================================\n")

data_debate_false <- filter(data_one_timepoint_only, show_debate == FALSE)

model_H5_full_debate_false <- lmer(change_in_logodds_assigned_to_model_guess ~ model_correct + numeracy + crt2 + topic + extra_incentive + (1 | q_id) + (1 | participant_id), 
                                   data = data_debate_false)
cat("Full Mixed-Effects Model Summary (Arguments Absent):\n")
print(summary(model_H5_full_debate_false))

cat("\nConfidence Interval for model_correctTRUE:\n")
print(confint(model_H5_full_debate_false, "model_correctTRUE", level = 0.95))

model_H5_simplified_debate_false <- lm(change_in_logodds_assigned_to_model_guess ~ model_correct, data = data_debate_false)
cat("\nSimplified Model Summary (Arguments Absent):\n")
print(summary(model_H5_simplified_debate_false))

cat("\n\n============================================\n")
cat("COMPARING EFFECT SIZES BETWEEN CONDITIONS\n")
cat("============================================\n")

cat("Effect of model_correctTRUE when arguments present (simplified model):\n")
print(coef(model_H5_simplified_debate_true)["model_correctTRUE"])

cat("\nEffect of model_correctTRUE when arguments absent (simplified model):\n")
print(coef(model_H5_simplified_debate_false)["model_correctTRUE"])

effect_present <- coef(model_H5_simplified_debate_true)["model_correctTRUE"]
effect_absent <- coef(model_H5_simplified_debate_false)["model_correctTRUE"]
pct_difference <- (effect_present - effect_absent) / effect_absent * 100

cat("\nPercentage difference in effect sizes: ", round(pct_difference, 2), "%\n", sep="")


#######################################

summary_data_model_correct <- data %>%
  group_by(model_correct) %>%
  summarise(
    mean_change = mean(change_in_logodds_assigned_to_model_guess, na.rm = TRUE),
    sd_change = sd(change_in_logodds_assigned_to_model_guess, na.rm = TRUE),
    n = n(),
    se_change = sd_change / sqrt(n),
    ci_change = qt(0.975, df = n - 1) * se_change
  )

t_test_result <- t.test(change_in_logodds_assigned_to_model_guess ~ model_correct, 
                        data = data,
                        var.equal = FALSE)

print(t_test_result)

ggplot(summary_data_model_correct, aes(x = model_correct, y = mean_change)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_change - ci_change, ymax = mean_change + ci_change), width = 0.2) +
  ylab("Mean change in logodds assigned to model's guess") +
  xlab("Is the model correct?")

###############################################

# Plotting as an interaction instead

interaction_df <- data %>%
  group_by(timepoint, model_correct) %>%
  summarise(mean = mean(logodds_assigned_to_model_guess, na.rm = TRUE),
            sd = sd(logodds_assigned_to_model_guess, na.rm = TRUE),
            n = n(),
            se = sd / sqrt(n),
            ci_lower = mean - qt(0.975, df = n - 1) * se,
            ci_upper = mean + qt(0.975, df = n - 1) * se,
            .groups = "drop")
interaction_df$timepoint <- factor(interaction_df$timepoint)

interaction_df$model_status <- ifelse(interaction_df$model_correct, "LLM's guess is correct", "LLM's guess is incorrect")

p = ggplot(interaction_df, aes(x = timepoint, y = mean, color = model_status)) +
  geom_line(aes(group = model_status), size = 1.5) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  labs(x = "", y = "Log Odds Assigned to LLM's Guess\n") +
  scale_x_discrete(labels = c("Before interacting\nwith LLM", "After interacting\nwith LLM")) +
  scale_color_manual(values = c("LLM's guess is incorrect" = "#8f3228", "LLM's guess is correct" = "#72e081")) +
  ylim(-0.25, 2.25) +
  guides(color = guide_legend(title = NULL)) +
  theme_minimal() +
  theme(
    text = element_text(size = 32),
    legend.text = element_text(size = 24),
    legend.position = "bottom"
  )

ggsave("figures/h5_plot.png", plot = p, width = 922/72, height = 672/72, units = "in", dpi = 300)

library(gridExtra)

create_panel_plots <- function(data) {
  plot_list <- list()
  
  subsets <- list(
    list(condition = quote(show_debate == TRUE), name = "Arguments present"),
    list(condition = quote(show_debate == FALSE), name = "Arguments absent")
  )

  for (i in seq_along(subsets)) {
    subset_data <- data %>%
      filter(eval(subsets[[i]]$condition))
    
    subset_interaction_df <- subset_data %>%
      group_by(timepoint, model_correct) %>%
      summarise(mean = mean(logodds_assigned_to_model_guess, na.rm = TRUE),
                sd = sd(logodds_assigned_to_model_guess, na.rm = TRUE),
                n = n(),
                se = sd / sqrt(n),
                ci_lower = mean - qt(0.975, df = n - 1) * se,
                ci_upper = mean + qt(0.975, df = n - 1) * se,
                .groups = "drop")
    
    subset_interaction_df$timepoint <- factor(subset_interaction_df$timepoint)
    
    subset_interaction_df$model_status <- ifelse(subset_interaction_df$model_correct, 
                                                 "LLM's guess is correct", "LLM's guess is incorrect")
    
    p <- ggplot(subset_interaction_df, aes(x = timepoint, y = mean, color = model_status)) +
      geom_line(aes(group = model_status), size = 1.5) +
      geom_point(size = 3) +
      geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
      labs(x = "", y = if(i == 1) "Log Odds Assigned to LLM's Guess\n" else "",
           title = subsets[[i]]$name) +
      scale_x_discrete(labels = c("Before interacting\nwith LLM", "After interacting\nwith LLM")) +
      scale_color_manual(values = c("LLM's guess is correct" = "#72e081", "LLM's guess is incorrect" = "#8f3228")) +
      ylim(-0.50, 2.25) +
      theme_minimal() +
      theme(
        text = element_text(size = 32),
        axis.title = element_text(size = 32),
        axis.text = element_text(size = 32),
        plot.title = element_text(size = 32),
        legend.position = "none"
      )
    
    plot_list[[i]] <- p
  }
  
  legend_plot <- ggplot(subset_interaction_df, aes(x = timepoint, y = mean, color = model_status)) +
    geom_line(aes(group = model_status), size = 1.5) +
    geom_point(size = 3) +
    scale_color_manual(values = c("LLM's guess is correct" = "#72e081", "LLM's guess is incorrect" = "#8f3228")) +
    theme_minimal() +
    theme(
      text = element_text(size = 36),
      legend.position = "bottom"
    ) + 
    guides(color = guide_legend(title = NULL))
  
  legend <- get_legend(legend_plot)
  
  panel_plot <- grid.arrange(
    arrangeGrob(
      plot_list[[1]], plot_list[[2]],
      ncol = 2
    ),
    legend,
    heights = c(10, 1)
  )
  
  ggsave("figures/h5_panel_plot.png", plot = panel_plot, 
         width = 1600/72, height = 800/72, units = "in", dpi = 300)
  
  return(panel_plot)
}

get_legend <- function(a.gplot) {
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

panel_plot <- create_panel_plots(data)


######################################


# There's a similar difference if we group by show_debate rather than model_correct
summary_data_debate_shown <- data %>%
  group_by(show_debate) %>%
  summarise(
    mean_change = mean(change_in_logodds_assigned_to_model_guess, na.rm = TRUE),
    sd_change = sd(change_in_logodds_assigned_to_model_guess, na.rm = TRUE),
    n = n(),
    se_change = sd_change / sqrt(n),
    ci_change = qt(0.975, df = n - 1) * se_change
  )

t_test_result <- t.test(change_in_logodds_assigned_to_model_guess ~ show_debate, 
                        data = data,
                        var.equal = FALSE)

print(t_test_result)

# Plot
ggplot(summary_data_debate_shown, aes(x = show_debate, y = mean_change)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_change - ci_change, ymax = mean_change + ci_change), width = 0.2) +
  ylab("Mean change in logodds assigned to model's guess") +
  xlab("Debate shown")


####

# Hypothesis H8

model_H8_full <- lmer(change_in_logodds_assigned_to_model_guess ~ model_correct * show_debate + numeracy + crt2 + topic + extra_incentive + (1 | q_id) + (1 | participant_id), data = data_one_timepoint_only)
summary(model_H8_full)

model_H8_simplified <- lm(change_in_logodds_assigned_to_model_guess ~ model_correct * show_debate, data = data_one_timepoint_only)
summary(model_H8_simplified)

new_data <- expand.grid(
  model_correct = c(FALSE, TRUE),
  show_debate = c(FALSE, TRUE)
)

predictions <- predict(model_H8_simplified, newdata = new_data, interval = "confidence", level = 0.95)

plot_data <- cbind(new_data, predictions)

plot_data$model_correct <- factor(plot_data$model_correct, 
                                  levels = c(FALSE, TRUE),
                                  labels = c("Model Incorrect", "Model Correct"))
plot_data$show_debate <- factor(plot_data$show_debate,
                                levels = c(FALSE, TRUE),
                                labels = c("No Debate Shown", "Debate Shown"))

ggplot(plot_data, aes(x = model_correct, y = fit, color = show_debate, group = show_debate)) +
  geom_point(size = 3, position = position_dodge(width = 0.2)) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2, position = position_dodge(width = 0.2)) +
  geom_line(position = position_dodge(width = 0.2)) +
  labs(
    title = "Interaction Effect: Model Correctness × Debate Visibility",
    subtitle = "Effect on Change in Log Odds Assigned to Model Guess",
    x = "",
    y = "Change in Log Odds",
    color = "Condition"
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12)
  ) +
  scale_color_brewer(palette = "Set1")

##########

# Secondary analyses
  
# "The primary analyses will also be conducted for subgroups corresponding to the two levels of `incentive`."

data_incentive_true <- data[data$extra_incentive == TRUE, ]
data_incentive_false <- data[data$extra_incentive == FALSE, ]
df_acc_incentive_true <- df_acc[df_acc$extra_incentive == TRUE, ]
df_acc_incentive_false <- df_acc[df_acc$extra_incentive == FALSE, ]
data_one_timepoint_only_incentive_true <- data_one_timepoint_only[data_one_timepoint_only$extra_incentive == TRUE, ]
data_one_timepoint_only_incentive_false <- data_one_timepoint_only[data_one_timepoint_only$extra_incentive == FALSE, ]


model_h1_h6_full <- lmer(logodds_assigned_to_correct ~ timepoint * show_debate + numeracy + crt2 + topic + extra_incentive + (1 | q_id) + (1 | participant_id), 
                         data = data_incentive_true)
summary(model_h1_h6_full)

model_h1_h6_simplified <- lm(logodds_assigned_to_correct ~ timepoint * show_debate, 
                             data = data_incentive_true)
summary(model_h1_h6_simplified)

model_h1_h6_full <- lmer(logodds_assigned_to_correct ~ timepoint * show_debate + numeracy + crt2 + topic + extra_incentive + (1 | q_id) + (1 | participant_id), 
                         data = data_incentive_false)
summary(model_h1_h6_full)

model_h1_h6_simplified <- lm(logodds_assigned_to_correct ~ timepoint * show_debate, 
                             data = data_incentive_false)
summary(model_h1_h6_simplified)

logistic_model_full <- glmer(correctness ~ timepoint * show_debate + numeracy + crt2 + topic + extra_incentive + (1 | q_id) + (1 | participant_id), 
                             data = data_incentive_true, 
                             family = binomial(link="logit")) # Fails to converge

logistic_model_simplified <- glm(correctness ~ timepoint * show_debate, 
                                 data = data_incentive_true, 
                                 family = binomial(link="logit"))
summary(logistic_model_simplified)

logistic_model_full <- glmer(correctness ~ timepoint * show_debate + numeracy + crt2 + topic + extra_incentive + (1 | q_id) + (1 | participant_id), 
                             data = data_incentive_false, 
                             family = binomial(link="logit")) # Fails to converge

logistic_model_simplified <- glm(correctness ~ timepoint * show_debate, 
                                 data = data_incentive_false, 
                                 family = binomial(link="logit"))
summary(logistic_model_simplified)

t_test <- t.test(df_acc_incentive_true$accuracy, mu = 0.75)
print(t_test)

t_test <- t.test(df_acc_incentive_false$accuracy, mu = 0.75)
print(t_test)

model_H4_full <- lmer(logodds_assigned_to_model_guess ~ timepoint + numeracy + crt2 + topic + extra_incentive + (1 | q_id) + (1 | participant_id), 
                      data = data_incentive_true)
summary(model_H4_full)

model_H4_simplified <- lm(logodds_assigned_to_model_guess ~ timepoint, 
                          data = data_incentive_true)
summary(model_H4_simplified)

model_H4_full <- lmer(logodds_assigned_to_model_guess ~ timepoint + numeracy + crt2 + topic + extra_incentive + (1 | q_id) + (1 | participant_id), 
                      data = data_incentive_false)
summary(model_H4_full)

model_H4_simplified <- lm(logodds_assigned_to_model_guess ~ timepoint, 
                          data = data_incentive_false)
summary(model_H4_simplified)

model_H5_full <- lmer(change_in_logodds_assigned_to_model_guess ~ model_correct * show_debate + numeracy + crt2 + topic + extra_incentive + (1 | q_id) + (1 | participant_id), 
                      data = data_one_timepoint_only_incentive_true)
summary(model_H5_full)

model_H5_simplified <- lm(change_in_logodds_assigned_to_model_guess ~ model_correct * show_debate, 
                          data = data_one_timepoint_only_incentive_true)
summary(model_H5_simplified)

model_H5_full <- lmer(change_in_logodds_assigned_to_model_guess ~ model_correct * show_debate + numeracy + crt2 + topic + extra_incentive + (1 | q_id) + (1 | participant_id), 
                      data = data_one_timepoint_only_incentive_false)
summary(model_H5_full)

model_H5_simplified <- lm(change_in_logodds_assigned_to_model_guess ~ model_correct * show_debate, 
                          data = data_one_timepoint_only_incentive_false)
summary(model_H5_simplified)

model_H8_full <- lmer(change_in_logodds_assigned_to_model_guess ~ model_correct * show_debate + numeracy + crt2 + topic + extra_incentive + (1 | q_id) + (1 | participant_id), 
                      data = data_one_timepoint_only_incentive_true)
summary(model_H8_full)

model_H8_simplified <- lm(change_in_logodds_assigned_to_model_guess ~ model_correct * show_debate, 
                          data = data_one_timepoint_only_incentive_true)
summary(model_H8_simplified)

model_H8_full <- lmer(change_in_logodds_assigned_to_model_guess ~ model_correct * show_debate + numeracy + crt2 + topic + extra_incentive + (1 | q_id) + (1 | participant_id), 
                      data = data_one_timepoint_only_incentive_false)
summary(model_H8_full)

model_H8_simplified <- lm(change_in_logodds_assigned_to_model_guess ~ model_correct * show_debate, 
                          data = data_one_timepoint_only_incentive_false)
summary(model_H8_simplified)



# "We will also do subgroup analyses corresponding to the analyses for H3 and H4, looking at whether these hypotheses 
#  hold separately for the subgroup of participants in each protocol group."

data_show_debate <- data[data$show_debate == TRUE, ]
data_hide_debate <- data[data$show_debate == FALSE, ]
df_acc_show_debate <- df_acc[df_acc$show_debate == TRUE, ]
df_acc_hide_debate <- df_acc[df_acc$show_debate == FALSE, ]

t_test <- t.test(df_acc_show_debate$accuracy, mu = 0.75)
print(t_test)

t_test <- t.test(df_acc_hide_debate$accuracy, mu = 0.75)
print(t_test)

model_H4_full <- lmer(logodds_assigned_to_model_guess ~ timepoint + numeracy + crt2 + topic + extra_incentive + (1 | q_id) + (1 | participant_id), 
                      data = data_show_debate)
summary(model_H4_full)

model_H4_simplified <- lm(logodds_assigned_to_model_guess ~ timepoint, 
                          data = data_hide_debate)
summary(model_H4_simplified)

# "We will also test whether there are differences in correctness, logodds_assigned_to_correct, or logodds_assigned_to_model_guess, 
# between participants assigned to the two different levels of `incentive` using the models correctness ~ incentive * protocol, 
# logodds_assigned_to_correct ~ incentive * protocol, and logodds_assigned_to_model_guess ~ incentive * protocol, respectively."

filtered_data <- data %>%
  filter(timepoint == 1) %>%
  mutate(went_with_model_answer = ifelse(logodds_assigned_to_model_guess < 0, 0, 1))

incentive_test <- glm(correctness ~ extra_incentive * show_debate, 
                                 data = filtered_data, 
                                 family = binomial(link="logit"))
summary(incentive_test)

incentive_test <- glm(logodds_assigned_to_correct ~ extra_incentive * show_debate, data=filtered_data)
summary(incentive_test)
incentive_test <- glm(logodds_assigned_to_model_guess ~ extra_incentive * show_debate, data=filtered_data)
summary(incentive_test)


# We do not expect participants’ initial unassisted accuracy to exceed chance. 
# We will test whether participants’ initial accuracy is significantly higher 
# than chance (both overall accuracy and accuracy on the subset of questions on 
# which participants indicate an initial confidence rating of higher than 
# “just guessing”), and conduct an equivalence test with a predefined 
# equivalence bound of ±5% to determine if we can reject the null hypothesis 
# that participants' initial accuracy is not equivalent to chance. 

# overall accuracy
df_initial_acc <- data %>%
  filter(timepoint == 0) %>%
  group_by(participant_id) %>%
  summarise(accuracy = mean(correctness))

# accuracy on the subset of questions where confidence > "just guessing"
df_initial_filtered_acc <- data %>%
  filter(timepoint == 0) %>%
  filter(confidence > 1) %>%
  group_by(participant_id) %>%
  summarise(accuracy = mean(correctness))

# Overall accuracy equivalence test -- hm, looks like accuracy is actually 
# significantly > 0.5, at ~0.55

t_test <- t.test(df_initial_acc$accuracy, mu = 0.5)
print(t_test)

t_test <- t.test(df_initial_acc$accuracy, mu = 0.75)
print(t_test)

equivalence_test <- TOSTone.raw(mean(df_initial_acc$accuracy), 
                                mu = 0.5, 
                                sd = sd(df_initial_acc$accuracy),
                                n=nrow(df_initial_acc), 
                                low_eqbound = -0.05, 
                                high_eqbound = 0.05)

print(equivalence_test)

# Accuracy on the subset of questions where confidence > "just guessing" 
# is higher still at ~.60

t_test <- t.test(df_initial_filtered_acc$accuracy, mu = 0.5)
print(t_test)

t_test <- t.test(df_initial_filtered_acc$accuracy, mu = 0.75)
print(t_test)

equivalence_test_filtered <- TOSTone.raw(mean(df_initial_filtered_acc$accuracy), 
                                         mu = 0.5, 
                                         sd = sd(df_initial_filtered_acc$accuracy),
                                         n=nrow(df_initial_filtered_acc), 
                                         low_eqbound = -0.05, 
                                         high_eqbound = 0.05)

print(equivalence_test_filtered)

#########

# How often did we go with the model's guess?
filtered_data <- data %>%
  filter(timepoint == 1) %>%
  mutate(went_with_model_answer = ifelse(logodds_assigned_to_model_guess < 0, 0, 1))

mean_val <- mean(filtered_data$went_with_model_answer)

conf_int <- t.test(filtered_data$went_with_model_answer)$conf.int

print(paste("Mean of 'went_with_model_answer':", mean_val))
print(paste("95% Confidence Interval for 'went_with_model_answer':", conf_int[1], '-', conf_int[2]))

# Were people more apt to go with the model's guess if they had an extra incentive?
# Or if they saw the debate?
incentive_test <- t.test(went_with_model_answer ~ extra_incentive, data=filtered_data)
print(incentive_test)
debate_test <- t.test(went_with_model_answer ~ show_debate, data=filtered_data)
print(debate_test)  # yeah, there is a difference if they saw the debate, that corroborates the interaction we saw previously.


# "We will also test whether there are differences in correctness, logodds_assigned_to_correct, or logodds_assigned_to_model_guess, 
# between participants assigned to the two different levels of `incentive` using the models correctness ~ incentive * protocol, 
# logodds_assigned_to_correct ~ incentive * protocol, and logodds_assigned_to_model_guess ~ incentive * protocol, respectively."
# [And let's do the same for debate...]

incentive_test <- t.test(logodds_assigned_to_correct ~ extra_incentive, data=filtered_data)
print(incentive_test)
debate_test <- t.test(logodds_assigned_to_correct ~ show_debate, data=filtered_data)
print(debate_test)
incentive_test <- t.test(logodds_assigned_to_model_guess ~ extra_incentive, data=filtered_data)
print(incentive_test)
debate_test <- t.test(logodds_assigned_to_model_guess ~ show_debate, data=filtered_data)
print(debate_test) # yup

incentive_model <- glm(correctness ~ extra_incentive, data=filtered_data, family=binomial(link="logit"))
summary(incentive_model)

debate_model <- glm(correctness ~ show_debate, data=filtered_data, family=binomial(link="logit"))
summary(debate_model)



#############

filtered_data$model_correct_binary <- as.integer(filtered_data$model_correct > 0)

correlation <- cor.test(filtered_data$model_correct_binary, filtered_data$logodds_assigned_to_model_guess)

print(correlation)

t_result <- t.test(filtered_data$logodds_assigned_to_model_guess ~ filtered_data$model_correct_binary)

print(t_result)

# ANOVA

model <- aov(filtered_data$logodds_assigned_to_model_guess ~ filtered_data$model_correct * filtered_data$show_debate)

summary_model <- summary(model)

print(summary_model)

interaction.plot(filtered_data$show_debate, filtered_data$model_correct, filtered_data$logodds_assigned_to_model_guess)

data_summary <- filtered_data %>%
  group_by(model_correct, show_debate) %>%
  summarise(mean_logodds = mean(logodds_assigned_to_model_guess, na.rm = TRUE),
            se_logodds = sd(logodds_assigned_to_model_guess, na.rm = TRUE) / sqrt(n()))

debate_labels <- c("FALSE" = "Debate not shown", "TRUE" = "Debate shown")
model_labels <- c("FALSE" = "Model is incorrect", "TRUE" = "Model is correct")

ggplot(data_summary, aes(x = model_correct, y = mean_logodds, group = show_debate, color = show_debate)) +
  geom_line() +
  geom_errorbar(aes(ymin = mean_logodds - se_logodds, ymax = mean_logodds + se_logodds), width = 0.2) +
  labs(title = "Interaction Plot", x = "Model Correct", y = "Mean Log Odds Assigned to Model's Guess", color = "Show Debate") +
  scale_color_discrete(labels = debate_labels) +
  scale_x_discrete(labels = model_labels) + 
  theme_minimal()

####################################


model <- aov(filtered_data$logodds_assigned_to_correct ~ filtered_data$model_correct * filtered_data$show_debate)

summary_model <- summary(model)

print(summary_model)

interaction.plot(filtered_data$show_debate, filtered_data$model_correct, filtered_data$logodds_assigned_to_correct)

data_summary <- filtered_data %>%
  group_by(model_correct, show_debate) %>%
  summarise(mean_logodds = mean(logodds_assigned_to_correct, na.rm = TRUE),
            se_logodds = sd(logodds_assigned_to_correct, na.rm = TRUE) / sqrt(n()))

ggplot(data_summary, aes(x = model_correct, y = mean_logodds, group = show_debate, color = show_debate)) +
  geom_line() +
  geom_errorbar(aes(ymin = mean_logodds - se_logodds, ymax = mean_logodds + se_logodds), width = 0.2) +
  labs(title = "Interaction Plot", x = "Model Correct", y = "Mean Log Odds Assigned to Correct Answer", color = "Show Debate") +
  theme_minimal()

####################################

data_summary <- filtered_data %>%
  group_by(model_correct, show_debate) %>%
  summarise(mean_logodds = mean(logodds_assigned_to_correct, na.rm = TRUE),
            .groups = "drop")

change_when_model_correct <- abs(diff(data_summary$mean_logodds[data_summary$model_correct == TRUE]))
change_when_model_incorrect <- abs(diff(data_summary$mean_logodds[data_summary$model_correct == FALSE]))

t_test_result <- t.test(c(change_when_model_correct), c(change_when_model_incorrect))
print(t_test_result)

interaction_model <- lm(logodds_assigned_to_correct ~ show_debate * model_correct, data = filtered_data)
summary(interaction_model)

####################################

model <- aov(filtered_data$change_in_logodds_assigned_to_model_guess ~ filtered_data$model_correct * filtered_data$show_debate)

summary_model <- summary(model)

print(summary_model)

interaction.plot(filtered_data$show_debate, filtered_data$model_correct, filtered_data$change_in_logodds_assigned_to_model_guess)

data_summary <- filtered_data %>%
  group_by(model_correct, show_debate) %>%
  summarise(mean_logodds = mean(change_in_logodds_assigned_to_model_guess, na.rm = TRUE),
            se_logodds = sd(change_in_logodds_assigned_to_model_guess, na.rm = TRUE) / sqrt(n()))

ggplot(data_summary, aes(x = model_correct, y = mean_logodds, group = show_debate, color = show_debate)) +
  geom_line() +
  geom_errorbar(aes(ymin = mean_logodds - se_logodds, ymax = mean_logodds + se_logodds), width = 0.2) +
  labs(title = "Interaction Plot", x = "Model Correct", y = "Change in Logodds Assigned to Model Guess", color = "Show Debate") +
  theme_minimal()

#################
# So: IF you are/aren't shown a debate, AND we reduce/increase the log odds you 
# assign to the model's guess by X, there is some X that will maximize
# accuracy. (this will be a different value depending on whether you are/aren't
# shown a debate). Try two-fold cross-validation to minimize overfitting (variance).

#################

print(cor.test(filtered_data$numeracy, filtered_data$logodds_assigned_to_correct))  # weak sig. (p = .03)
print(cor.test(filtered_data$crt2, filtered_data$logodds_assigned_to_correct))      # not sig.
print(cor.test(filtered_data$filler_score, filtered_data$logodds_assigned_to_correct)) # not sig.
print(cor.test(filtered_data$core_score, filtered_data$logodds_assigned_to_correct))  # not sig (p = .06)
print(cor.test(filtered_data$total_logic_score, filtered_data$logodds_assigned_to_correct)) # not sig. (p = .054)
t_test <- t.test(logodds_assigned_to_correct ~ lolo_attempted, data=filtered_data) # sig. (.04)
print(t_test)
t_test <- t.test(logodds_assigned_to_correct ~ lolo_correct, data=filtered_data) # not sig.
print(t_test)

model <- lmer(logodds_assigned_to_correct ~ numeracy + crt2 + total_logic_score + lolo_correct + (1 | q_id), data = filtered_data) # OK
summary(model)

print(cor.test(df_acc$numeracy, df_acc$accuracy))  # p < .001
print(cor.test(df_acc$crt2, df_acc$accuracy))      # none
print(cor.test(df_acc$filler_score, df_acc$accuracy)) # none
print(cor.test(df_acc$core_score, df_acc$accuracy))  # not sig (p = .052)
print(cor.test(df_acc$total_logic_score, df_acc$accuracy)) # p = .04
t_test <- t.test(accuracy ~ lolo_attempted, data=df_acc) # not sig
print(t_test)
t_test <- t.test(accuracy ~ lolo_correct, data=df_acc) # not sig
print(t_test)

print(cor.test(filtered_data$confidence, filtered_data$logodds_assigned_to_correct)) # confidence correlated to correctness
print(cor.test(filtered_data$confidence, filtered_data$correctness))

#####################################

# We do not expect participants’ initial unassisted accuracy to exceed chance. 
# We will test whether participants’ initial accuracy is significantly higher 
# than chance (both overall accuracy and accuracy on the subset of questions on 
# which participants indicate an initial confidence rating of higher than 
# “just guessing”), and conduct an equivalence test with a predefined 
# equivalence bound of ±5% to determine if we can reject the null hypothesis 
# that participants' initial accuracy is not equivalent to chance. 

# overall accuracy
df_initial_acc <- data %>%
  filter(timepoint == 0) %>%
  group_by(participant_id) %>%
  summarise(accuracy = mean(correctness))

# accuracy on the subset of questions where confidence > "just guessing"
df_initial_filtered_acc <- data %>%
  filter(timepoint == 0) %>%
  filter(confidence > 1) %>%
  group_by(participant_id) %>%
  summarise(accuracy = mean(correctness))

# Overall accuracy equivalence test -- hm, looks like accuracy is actually 
# significantly > 0.5, at ~0.55

t_test <- t.test(df_initial_acc$accuracy, mu = 0.5)
print(t_test)

equivalence_test <- TOSTone.raw(mean(df_initial_acc$accuracy), 
                                mu = 0.5, 
                                sd = sd(df_initial_acc$accuracy),
                                n=nrow(df_initial_acc), 
                                low_eqbound = -0.05, 
                                high_eqbound = 0.05)

print(equivalence_test)

# Accuracy on the subset of questions where confidence > "just guessing" 
# is higher still at ~.60

t_test <- t.test(df_initial_filtered_acc$accuracy, mu = 0.5)
print(t_test)

equivalence_test_filtered <- TOSTone.raw(mean(df_initial_filtered_acc$accuracy), 
                                mu = 0.5, 
                                sd = sd(df_initial_filtered_acc$accuracy),
                                n=nrow(df_initial_filtered_acc), 
                                low_eqbound = -0.05, 
                                high_eqbound = 0.05)

print(equivalence_test_filtered)

#####################################

# Wisdom of crowds - normal, confidence-weighted (both likert scale confidence, and distance-from-0 (abs(logodds_assigned_to_correct)) confidence
# What if you treat half the sample as a dev set, fit a logistic regression to correctness, and treat the remainder as your test set? What accuracy can you achieve?


data$scalar_confidence = abs(data$logodds_assigned_to_correct)
df_show_debate <- data %>%
  filter(show_debate == TRUE) %>%
  filter(timepoint == 1)
df_hide_debate <- data %>%
  filter(show_debate == FALSE) %>%
  filter(timepoint == 1)

cor.test(df_show_debate$logodds_assigned_to_correct, df_show_debate$scalar_confidence)
cor.test(df_hide_debate$logodds_assigned_to_correct, df_hide_debate$scalar_confidence)
cor.test(df_show_debate$confidence, df_show_debate$scalar_confidence)
cor.test(df_hide_debate$confidence, df_hide_debate$scalar_confidence)

# Ah, this is revealing:
hist(df_hide_debate$scalar_confidence)
hist(df_show_debate$scalar_confidence)