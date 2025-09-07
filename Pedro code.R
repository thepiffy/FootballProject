library(worldfootballR)
library(tidyverse)
library(ggsoccer)
library(patchwork)

pedro_understat <- understat_player_shots(player_url = "https://understat.com/player/8272")

# Filter to 2024/25 season
pedro_2425 <- pedro_understat %>% filter(season == "2024")
pedro_2526 <- pedro_understat %>% filter(season == "2025")
pedro_2324 <- pedro_understat %>% filter(season == "2023")

# Opta-style plotting function
plot_shotmap_opta <- function(data, player_name) {
  data %>%
    mutate(
      x = X * 120,
      y = (1 - Y) * 80,
      outcome = case_when(
        result == "Goal" ~ "Goal",
        result %in% c("SavedShot", "BlockedShot") ~ "Saved/Blocked",
        TRUE ~ "Missed"
      )
    ) %>%
    ggplot(aes(x = x, y = y)) +
    # Pitch background
    annotate_pitch(dimensions = pitch_statsbomb, colour = "white", fill = "darkgreen") +
    theme_pitch() +
    # Shots
    geom_point(aes(size = xG, fill = outcome), shape = 21, colour = "black", alpha = 0.9) +
    scale_fill_manual(values = c("Goal" = "red", "Saved/Blocked" = "skyblue", "Missed" = "white")) +
    scale_size(range = c(2, 10)) +
    coord_flip(xlim = c(49, 121), ylim = c(-1, 81)) +
    labs(title = paste(player_name, "- Shots 2022/23"), size = "xG", fill = "") +
    theme(
      plot.background = element_rect(fill = "darkgreen", colour = NA),
      panel.background = element_rect(fill = "darkgreen", colour = NA),
      legend.background = element_rect(fill = "darkgreen", colour = NA),
      text = element_text(colour = "white"),
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      legend.position = "bottom"
    )
}

# Create Opta-style plots
pedro_plot_2324 <- plot_shotmap_opta(pedro_2324, "Pedro 23/24")
pedro_plot_2425 <- plot_shotmap_opta(pedro_2425, "Pedro 24/25")
pedro_plot_2526 <- plot_shotmap_opta(pedro_2526, "Pedro 25/26")

pedro_plot_2324 + pedro_plot_2425 + pedro_plot_2526

# Function to calculate low xG conversion stats per game
calculate_low_xg_conversion <- function(data, player_name) {
  low_xg_shots <- data %>% 
    filter(xG < 0.2)
  
  total_shots <- nrow(low_xg_shots)
  goals <- sum(low_xg_shots$result == "Goal")
  games_played <- n_distinct(data$match_id)  # count matches in that season
  
  shots_per_game <- ifelse(games_played > 0, total_shots / games_played, 0)
  goals_per_game <- ifelse(games_played > 0, goals / games_played, 0)
  conversion_rate <- ifelse(total_shots > 0, (goals / total_shots) * 100, 0)
  
  return(data.frame(
    player = player_name,
    games_played = games_played,
    low_xg_shots_per_game = round(shots_per_game, 2),
    goals_from_low_xg_per_game = round(goals_per_game, 2),
    conversion_rate = round(conversion_rate, 2)
  ))
}

# Calculate stats for João Pedro (all 3 seasons)
all_players_stats <- bind_rows(
  calculate_low_xg_conversion(pedro_2324, "João Pedro 23/24"),
  calculate_low_xg_conversion(pedro_2425, "João Pedro 24/25"),
  calculate_low_xg_conversion(pedro_2526, "João Pedro 25/26")
)

# Print summary table
cat("Shot Conversion Analysis per Game for Shots < 0.2 xG (João Pedro)\n")
cat("=================================================================\n")
print(all_players_stats)

# Prepare data for dual bar chart (per-game values)
chart_data <- all_players_stats %>%
  select(player, low_xg_shots_per_game, conversion_rate) %>%
  pivot_longer(cols = c(low_xg_shots_per_game, conversion_rate), 
               names_to = "metric", 
               values_to = "value") %>%
  mutate(
    metric_label = case_when(
      metric == "low_xg_shots_per_game" ~ "Low xG Shots per Game",
      metric == "conversion_rate" ~ "Conversion Rate (%)"
    ),
    # Scale shots to align visually with percentages
    scaled_value = ifelse(metric == "low_xg_shots_per_game", value * 10, value) # multiply to show together
  )

# Dual bar chart
dual_bar_plot <- chart_data %>%
  ggplot(aes(x = reorder(player, -scaled_value), y = scaled_value, fill = metric_label)) +
  geom_col(position = "dodge", alpha = 0.8, color = "black", width = 0.7) +
  geom_text(aes(label = ifelse(metric == "low_xg_shots_per_game", 
                               paste0(value, " shots/gm"), 
                               paste0(round(value, 1), "%"))), 
            position = position_dodge(width = 0.7), 
            vjust = -0.3, size = 3.5, fontface = "bold") +
  scale_fill_manual(values = c("Low xG Shots per Game" = "#3498db", "Conversion Rate (%)" = "#e74c3c")) +
  labs(
    title = "Low xG Shots per Game vs Conversion Rate",
    subtitle = "João Pedro | 23/24, 24/25, 25/26 Seasons",
    x = "Season",
    y = "Value (Shots/gm scaled ×10 | Conversion Rate %)",
    fill = "Metric",
    caption = "Data from Understat"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "grey60"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 8, color = "grey60")
  ) +
  ylim(0, max(chart_data$scaled_value) * 1.15)

print(dual_bar_plot)

# Alternative: separate charts
shots_chart <- all_players_stats %>%
  ggplot(aes(x = reorder(player, -low_xg_shots_per_game), y = low_xg_shots_per_game, fill = player)) +
  geom_col(alpha = 0.8, color = "black", show.legend = FALSE) +
  geom_text(aes(label = low_xg_shots_per_game), vjust = -0.3, fontface = "bold") +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Low xG Shots per Game (< 0.2)", x = "Season", y = "Shots per Game") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"))

conversion_chart <- all_players_stats %>%
  ggplot(aes(x = reorder(player, -conversion_rate), y = conversion_rate, fill = player)) +
  geom_col(alpha = 0.8, color = "black", show.legend = FALSE) +
  geom_text(aes(label = paste0(conversion_rate, "%")), vjust = -0.3, fontface = "bold") +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Conversion Rate (%)", x = "Season", y = "Conversion Rate (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"))

combined_charts <- shots_chart + conversion_chart +
  plot_annotation(
    title = "Low xG Shot Analysis per Game (< 0.2 xG) - João Pedro",
    theme = theme(plot.title = element_text(size = 16, hjust = 0.5, face = "bold"))
  )

print(combined_charts)
