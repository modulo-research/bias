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
# Wave Plot for Appendix
#########################################################

library(gridExtra)

# Function to create panel plots for specific subsets of data
create_panel_plots <- function(data) {
  # Create list to store individual plots
  plot_list <- list()
  
  # Define the subsets we want to create
  subsets <- list(
    list(condition = quote(wave == 1), name = "Wave 1"),
    list(condition = quote(wave == 2), name = "Wave 2")
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
  ggsave("figures/exp2_appendix_wave_plot.png", plot = panel_plot, 
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
