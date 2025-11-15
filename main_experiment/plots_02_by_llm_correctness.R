library(tidyverse)
library(dplyr)

# Load the data, ensuring correct data types
data <- read.csv("tidy_data_expanded.csv", stringsAsFactors = FALSE)

# Convert columns to appropriate data types
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
    probability_assigned_to_a = as.numeric(probability_assigned_to_a),
    probability_assigned_to_b = as.numeric(probability_assigned_to_b),
    correct_answer = factor(correct_answer, levels = c("A", "B")),
    logit = as.numeric(logit),
    probability_assigned_to_correct_answer = as.numeric(probability_assigned_to_correct_answer),
    is_correct = as.numeric(is_correct),
    llm_answer = factor(llm_answer, levels = c("A", "B")),
    probability_assigned_to_llm_answer = as.numeric(probability_assigned_to_llm_answer),
    favored_llm_answer = as.numeric(favored_llm_answer),
    llm_answer_logit = as.numeric(llm_answer_logit),
    correct_answer_logit = as.numeric(correct_answer_logit)
  )

# Exclude rows where group == 'no assistance'
data <- data %>%
  filter(group != "no assistance")


#########################################################
# Analogue to H5 Plot
#########################################################

# Calculate means and 95% confidence intervals
interaction_df <- data %>%
  group_by(timepoint, llm_answer == correct_answer) %>%
  rename(model_correct = `llm_answer == correct_answer`) %>%
  summarise(mean = mean(llm_answer_logit, na.rm = TRUE),
            sd = sd(llm_answer_logit, na.rm = TRUE),
            n = n(),
            se = sd / sqrt(n),
            ci_lower = mean - qt(0.975, df = n - 1) * se,
            ci_upper = mean + qt(0.975, df = n - 1) * se,
            .groups = "drop")

# Ensure timepoint is a factor with correct order
interaction_df$timepoint <- factor(interaction_df$timepoint, 
                                   levels = c("before", "intermediate", "after"))

# Create a new variable with the desired labels
interaction_df$model_status <- ifelse(interaction_df$model_correct, 
                                      "Consultant's answer is correct   ", 
                                      "Consultant's answer is incorrect")

# Create the plot
p = ggplot(interaction_df, aes(x = timepoint, y = mean, color = model_status)) +
  geom_line(aes(group = model_status), size = 1.5) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  labs(x = "", y = "Log Odds Assigned to Consultant's answer\n") +
  scale_x_discrete(labels = c("Before\nLLM guidance", 
                              "After\nLLM guidance", 
                              "After\nLLM guidance\nand research")) +
  scale_color_manual(values = c("Consultant's answer is incorrect" = "#8f3228", 
                                "Consultant's answer is correct   " = "#72e081")) +
  ylim(-0.25, 2.25) +
  guides(color = guide_legend(title = NULL)) +
  theme_minimal() +
  theme(
    text = element_text(size = 24)
  )

ggsave("figures/exp2_analogue_to_h5_plot.png", plot = p, width = 922/72, height = 672/72, units = "in", dpi = 300)

#########################################################
# Analogue to H5 Panel Plot
#########################################################

library(gridExtra)

# Function to create panel plots for specific subsets of data
create_panel_plots <- function(data) {
  # Create list to store individual plots
  plot_list <- list()
  
  # Define the subsets we want to create
  subsets <- list(
    list(condition = quote(group == "control"), name = "Control group"),
    list(condition = quote(group == "intervention"), name = "Intervention group")
  )
  
  # Create each subplot without a legend
  for (i in seq_along(subsets)) {
    # Subset the data based on condition
    subset_data <- data %>%
      filter(eval(subsets[[i]]$condition))
    
    # Calculate means and standard errors for the subset
    subset_interaction_df <- subset_data %>%
      group_by(timepoint, llm_answer == correct_answer) %>%
      rename(model_correct = `llm_answer == correct_answer`) %>%
      summarise(mean = mean(llm_answer_logit, na.rm = TRUE),
                sd = sd(llm_answer_logit, na.rm = TRUE),
                n = n(),
                se = sd / sqrt(n),
                ci_lower = mean - qt(0.975, df = n - 1) * se,
                ci_upper = mean + qt(0.975, df = n - 1) * se,
                .groups = "drop")
    
    # Ensure timepoint is a factor with correct order
    subset_interaction_df$timepoint <- factor(subset_interaction_df$timepoint, 
                                              levels = c("before", "intermediate", "after"))
    
    # Create the model_status variable
    subset_interaction_df$model_status <- ifelse(subset_interaction_df$model_correct, 
                                                 "Consultant's answer is correct   ", "Consultant's answer is incorrect")
    
    # Create the plot without legend
    p <- ggplot(subset_interaction_df, aes(x = timepoint, y = mean, color = model_status)) +
      geom_line(aes(group = model_status), size = 1.5) +
      geom_point(size = 3) +
      geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
      labs(x = "", y = if(i == 1) "Log Odds Assigned to Consultant's Answer\n" else "",
           title = subsets[[i]]$name) +
      scale_x_discrete(labels = c("Before LLM\nguidance", 
                                  "After LLM\nguidance", 
                                  "After LLM\nguidance\nand research")) +
      scale_color_manual(values = c("Consultant's answer is correct   " = "#72e081", 
                                    "Consultant's answer is incorrect" = "#8f3228")) +
      ylim(-0.50, 2.35) +
      theme_minimal() +
      theme(
        text = element_text(size = 32),
        legend.position = "none"  # Remove legend from individual plots
      )
    
    plot_list[[i]] <- p
  }
  
  # Extract the legend from one plot
  # First, create a plot with a legend
  legend_plot <- ggplot(subset_interaction_df, aes(x = timepoint, y = mean, color = model_status)) +
    geom_line(aes(group = model_status), size = 1.5) +
    geom_point(size = 3) +
    scale_color_manual(values = c("Consultant's answer is correct   " = "#72e081", 
                                  "Consultant's answer is incorrect" = "#8f3228")) +
    theme_minimal() +
    theme(
      text = element_text(size = 32),
      legend.position = "bottom"
    ) + 
    guides(color = guide_legend(title = NULL))
  
  # Extract the legend
  legend <- get_legend(legend_plot)
  
  # Combine plots in a grid with shared legend at the bottom
  panel_plot <- grid.arrange(
    arrangeGrob(
      plot_list[[1]], plot_list[[2]],
      ncol = 2
    ),
    legend,
    heights = c(10, 1)
  )
  
  # Save the combined panel plot
  ggsave("figures/exp2_analogue_to_h5_panel_plot.png", plot = panel_plot, 
         width = 1600/72, height = 800/72, units = "in", dpi = 300)
  
  return(panel_plot)
}

get_legend <- function(a.gplot) {
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# Call the function to create and save the panel plot
panel_plot <- create_panel_plots(data)
panel_plot

#########################################################
# Analogue to Accuracy Plot
#########################################################


# Function to create accuracy dataframe for a filtered subset of data
create_accuracy_df <- function(data_subset) {
  accuracy_df <- data_subset %>%
    filter(timepoint == "after") %>%  # Filter to final timepoint
    group_by(participant_code) %>%    # Group by participant_code instead of participant_id
    summarise(
      accuracy = mean(is_correct, na.rm = TRUE),  # Use is_correct column
      group = first(group),          # Use group instead of show_debate
      n_questions = n()              # Count of questions in this subset for this participant
    )
  return(accuracy_df)
}

# Create a column to identify if llm_answer matches correct_answer
data <- data %>%
  mutate(model_correct = (llm_answer == correct_answer))

# Create datasets filtered by model correctness
data_model_correct <- data %>% filter(model_correct == TRUE)
data_model_incorrect <- data %>% filter(model_correct == FALSE)

# Create accuracy dataframes using our function
df_acc_model_correct <- create_accuracy_df(data_model_correct)
df_acc_model_incorrect <- create_accuracy_df(data_model_incorrect)

# Add descriptive names to make the analysis clearer
names(df_acc_model_correct)[names(df_acc_model_correct) == "accuracy"] <- "accuracy_model_correct"
names(df_acc_model_incorrect)[names(df_acc_model_incorrect) == "accuracy"] <- "accuracy_model_incorrect"

# Print summary statistics
cat("Summary for questions where model was correct:\n")
print(summary(df_acc_model_correct$accuracy_model_correct))
cat("\nNumber of participants with model-correct questions:", nrow(df_acc_model_correct), "\n")
cat("Mean participant accuracy when model is correct:", mean(df_acc_model_correct$accuracy_model_correct, na.rm = TRUE), "\n")

cat("\nSummary for questions where model was incorrect:\n")
print(summary(df_acc_model_incorrect$accuracy_model_incorrect)) 
cat("\nNumber of participants with model-incorrect questions:", nrow(df_acc_model_incorrect), "\n")
cat("Mean participant accuracy when model is incorrect:", mean(df_acc_model_incorrect$accuracy_model_incorrect, na.rm = TRUE), "\n")

# Function to prepare summary statistics for a condition
prepare_condition_data <- function(df, acc_column, group_value, condition_name) {
  df %>%
    filter(group == group_value) %>%  # Filter by group instead of show_debate
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

# Define condition parameters with updated condition names
conditions <- list(
  list(df = df_acc_model_incorrect, acc_column = "accuracy_model_incorrect", 
       group = "control", name = "Control group,\nConsultant incorrect"),
  list(df = df_acc_model_correct, acc_column = "accuracy_model_correct", 
       group = "control", name = "Control group,\nConsultant correct"),
  list(df = df_acc_model_incorrect, acc_column = "accuracy_model_incorrect", 
       group = "intervention", name = "Intervention group,\nConsultant incorrect"),
  list(df = df_acc_model_correct, acc_column = "accuracy_model_correct", 
       group = "intervention", name = "Intervention group,\nConsultant correct")
)

# Generate data for all conditions using map and bind_rows
plot_data <- map_dfr(conditions, function(cond) {
  prepare_condition_data(cond$df, cond$acc_column, cond$group, cond$name)
})

# Set the condition order with updated condition names
plot_data$condition <- factor(plot_data$condition, 
                              levels = c("Control group,\nConsultant incorrect", 
                                         "Control group,\nConsultant correct",
                                         "Intervention group,\nConsultant incorrect",
                                         "Intervention group,\nConsultant correct"))

# Define bar colors (keeping the same color scheme)
bar_colors <- c(
  "Control group,\nConsultant incorrect" = "#D3D3D3",  # Light gray
  "Control group,\nConsultant correct"   = "#808080",  # Medium gray
  "Intervention group,\nConsultant incorrect" = "#A4E5E3",  # Lighter teal
  "Intervention group,\nConsultant correct"   = "#4FBDBA"   # Original teal
)

# Create the plot
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

# Print the plot
print(p)

# Save the plot
ggsave("figures/exp2_analogue_to_acc_plot.png", plot = p, width = 922/72, height = 672/72, units = "in", dpi = 300)

p

# Print sample sizes
cat("Sample sizes for each condition:\n")
print(plot_data %>% select(condition, n))