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
# Study 2 Initial Plot
#########################################################

# Calculate means and 95% confidence intervals
interaction_df <- data %>%
  group_by(timepoint, group) %>%
  summarise(mean = mean(correct_answer_logit, na.rm = TRUE),
            sd = sd(correct_answer_logit, na.rm = TRUE),
            n = n(),
            se = sd / sqrt(n),
            ci_lower = mean - qt(0.975, df = n - 1) * se,
            ci_upper = mean + qt(0.975, df = n - 1) * se,
            .groups = "drop")

# Ensure timepoint is a factor with correct order
interaction_df$timepoint <- factor(interaction_df$timepoint, 
                                   levels = c("before", "intermediate", "after"))

# Create the plot
p = ggplot(interaction_df, aes(x = timepoint, y = mean, color = group)) +
  geom_line(aes(group = group), size = 1.5) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  labs(x = "", y = "Log Odds Assigned to Correct Answer\n") +
  scale_x_discrete(labels = c("Before\nLLM guidance", 
                              "After\nLLM guidance", 
                              "After\nLLM guidance\nand research")) +
  scale_color_manual(values = c("control" = "#808080", 
                                "intervention" = "#4FBDBA"),
                     labels = c("control" = "Control", 
                                "intervention" = "Intervention")) +
  ylim(-0.25, 2.25) +
  guides(color = guide_legend(title = NULL)) +
  theme_minimal() +
  theme(
    text = element_text(size = 30),
    legend.text = element_text(size = 24),
    legend.position = "bottom"
  )

ggsave("figures/exp2-logit-correct-by-timepoint-and-group.png", plot = p, width = 922/72, height = 672/72, units = "in", dpi = 300)

p_big <- p +
  labs(x = "", y = "Log Odds Assigned\nto Correct Answer\n") +
  scale_x_discrete(labels = c("Before LLM\nguidance", 
                              "After LLM\nguidance", 
                              "After LLM\nguidance and\nresearch")) + 
  theme(
    axis.text.x  = element_text(size = 34),
    axis.text.y  = element_text(size = 34),
    axis.title.y = element_text(size = 36, margin = margin(r = 12)),
    legend.text  = element_text(size = 34)
  )

ggsave("figures/exp2-logit-correct-by-timepoint-and-group_biglabels.png",
       plot = p_big,
       width = 922/72, height = 672/72, units = "in", dpi = 300)

#################

# ------------------------------------------------------------
# Packages
# ------------------------------------------------------------
library(tidyverse)
library(gridExtra)

# ------------------------------------------------------------
# Helper to pull a legend out of a ggplot object -------------
# ------------------------------------------------------------
get_legend <- function(a.gplot) {
  ggplotGrob(a.gplot)$grobs |>
    purrr::keep(~ identical(.x$name, "guide-box")) |>
    purrr::pluck(1)
}

# ------------------------------------------------------------
# Function: make two‑panel plot split by wave ----------------
# ------------------------------------------------------------
create_wave_panel_plot <- function(data, out_file = "figures/exp2-wave-panel-plot.png") {
  
  # common colour palette for the two groups
  group_cols <- c(control = "#808080",
                  intervention = "#4FBDBA")
  
  # empty list to store the two sub‑plots
  plot_list <- vector("list", 2)
  
  # iterate over the two waves -------------------------------------------------
  for (w in 1:2) {
    
    # --- summarise data for this wave ----------------------------------------
    df_wave <- data |>
      dplyr::filter(wave == w) |>
      group_by(timepoint, group) |>
      summarise(mean      = mean(correct_answer_logit, na.rm = TRUE),
                sd        = sd(correct_answer_logit,   na.rm = TRUE),
                n         = n(),
                se        = sd / sqrt(n),
                ci_lower  = mean - qt(0.975, df = n - 1) * se,
                ci_upper  = mean + qt(0.975, df = n - 1) * se,
                .groups   = "drop") |>
      mutate(timepoint = factor(timepoint,
                                levels = c("before", "intermediate", "after")))
    
    # --- build the subplot ----------------------------------------------------
    plot_list[[w]] <- ggplot(df_wave,
                             aes(x = timepoint, y = mean, colour = group)) +
      geom_line(aes(group = group), size = 1.5) +
      geom_point(size = 3) +
      geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
      scale_x_discrete(labels = c("Before\nLLM guidance",
                                  "After\nLLM guidance",
                                  "After\nLLM guidance\nand research")) +
      scale_colour_manual(values = group_cols,
                          labels  = c(control      = "Control",
                                      intervention = "Intervention")) +
      labs(x = "",
           y = if (w == 1) "Log Odds Assigned to Correct Answer\n" else "",
           title = paste("Wave", w)) +
      ylim(-0.25, 2.25) +
      theme_minimal() +
      theme(text = element_text(size = 32),
            legend.position = "none")    # hide legend here
  }
  
  # ------------------------------------------------------------
  # Extract a legend from one of the sub‑plots -----------------
  # ------------------------------------------------------------
  
  legend <- get_legend(
    plot_list[[1]] +
      theme(legend.position = "bottom") +
      guides(colour = guide_legend(title = NULL))   # <- removes “group” title
  )
  
  
  # ------------------------------------------------------------
  # Arrange two sub‑plots plus shared legend -------------------
  # ------------------------------------------------------------
  panel_plot <- grid.arrange(
    arrangeGrob(plot_list[[1]], plot_list[[2]], ncol = 2),
    legend,
    heights = c(10, 1)
  )
  
  # ------------------------------------------------------------
  # Save to disk (adjust width/height as you like) -------------
  # ------------------------------------------------------------
  ggsave(out_file,
         plot   = panel_plot,
         width  = 1600/72, height = 800/72, units = "in", dpi = 300)
  
  invisible(panel_plot)
}

# ----------------------------------------------------------------
# Call the function (assumes your `data` object is already loaded)
# ----------------------------------------------------------------
create_wave_panel_plot(data)
