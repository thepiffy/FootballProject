required_pkgs <- c("worldfootballR","dplyr","purrr","stringr","tibble","tidyr",
                   "ggplot2","forcats","scales","ggrepel","viridis")
to_install <- setdiff(required_pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")
invisible(lapply(required_pkgs, library, character.only = TRUE))

theme_nice <- function() {
  ggplot2::theme_minimal() +
    theme(
      plot.title = element_text(face="bold", size=18, margin=margin(b=8)),
      plot.subtitle = element_text(size=12, colour="#555555", margin=margin(b=12)),
      plot.background = element_rect(fill = "#fafafa", color = NA),
      panel.background = element_rect(fill = "#fafafa", color = NA),
      axis.title = element_blank(),
      axis.text  = element_text(size=11, face="bold"),
      axis.text.y = element_text(hjust = 1),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_line(linetype = "dotted", color = "#cccccc"),
      legend.position = "none",
      plot.margin = margin(15,20,15,15)
    )
}

players_all <- tibble::tribble(
  ~player,                      ~season_end_year, ~player_url,                                                    ~era,
  "Ousmane Dembélé (24/25)",     2025,             "https://fbref.com/en/players/b19db005/Ousmane-Dembele",        "Modern",
  "Karim Benzema (21/22)",       2022,             "https://fbref.com/en/players/70d74ece/Karim-Benzema",          "Modern",
  "Luís Figo (99/00)",           2000,             "https://fbref.com/en/players/f20f347f/Luis-Figo",              "Classic",
  "Michael Owen (00/01)",        2001,             "https://fbref.com/en/players/88b3f52c/Michael-Owen",           "Classic",
  "Ronaldo Nazário (02/03)",     2003,             "https://fbref.com/en/players/57feb553/Ronaldo",                "Classic",
  "Andriy Shevchenko (04/05)",   2005,             "https://fbref.com/en/players/32eb0f30/Andriy-Shevchenko",      "Classic",
  "Ronaldinho (04/05)",          2005,             "https://fbref.com/en/players/34edcab0/Ronaldinho",             "Classic",
  "Kaká (06/07)",                2007,             "https://fbref.com/en/players/b2d6ea70/Kaka",                   "Classic"
)

players_modern <- players_all %>% filter(season_end_year >= 2017)

season_string <- function(end_year) paste0(end_year - 1, "-", end_year)
num <- function(x) suppressWarnings(as.numeric(x))

safe_pull <- function(url, stat_type, tries = 3, pause = 2) {
  for (i in seq_len(tries)) {
    out <- tryCatch({
      result <- fb_player_season_stats(player_url = url, stat_type = stat_type)
      if (!is.null(result) && nrow(result) > 0) return(result)
      NULL
    }, error = function(e) NULL)
    if (!is.null(out)) return(out)
    Sys.sleep(pause * i)
  }
  tibble()
}

filter_season_flexible <- function(df, ssn, end_year) {
  if (nrow(df) == 0) return(df)
  if ("Season" %in% names(df)) {
    filtered <- dplyr::filter(df, Season == ssn)
    if (nrow(filtered) > 0) return(filtered)
    filtered <- dplyr::filter(df, grepl(as.character(end_year), Season))
    if (nrow(filtered) > 0) return(filtered)
    return(df %>% slice(1))
  }
  df %>% slice(1)
}

extract_per90 <- function(df, value_cols, minute_cols = c("Min_Time", "Mins_Per_90", "90s")) {
  if (nrow(df) == 0) return(NA_real_)
  for (min_col in minute_cols) {
    if (min_col %in% names(df)) {
      for (val_col in value_cols) {
        if (val_col %in% names(df)) {
          vals <- sum(num(df[[val_col]]), na.rm = TRUE)
          mins <- sum(num(df[[min_col]]), na.rm = TRUE)
          if (min_col %in% c("Mins_Per_90", "90s")) {
            if (!is.na(mins) && mins > 0) return(vals / mins)
          } else {
            if (!is.na(mins) && mins > 0) return((vals / mins) * 90)
          }
        }
      }
    }
  }
  NA_real_
}

extract_basic <- function(player, season_end_year, player_url, era) {
  ssn <- season_string(season_end_year)
  std <- safe_pull(player_url, "standard") %>% filter_season_flexible(ssn, season_end_year)
  tibble::tibble(
    player = player,
    era = era,
    gls90  = extract_per90(std, c("Gls", "Goals", "Gls_Standard")),
    ast90  = extract_per90(std, c("Ast", "Assists", "Ast_Standard"))
  )
}

extract_advanced <- function(player, season_end_year, player_url, era) {
  ssn <- season_string(season_end_year)
  pas <- safe_pull(player_url, "passing")
  Sys.sleep(1)
  def <- safe_pull(player_url, "defense")
  Sys.sleep(1)
  pos <- safe_pull(player_url, "possession")
  
  pas <- filter_season_flexible(pas, ssn, season_end_year)
  def <- filter_season_flexible(def, ssn, season_end_year)
  pos <- filter_season_flexible(pos, ssn, season_end_year)
  
  tibble::tibble(
    player = player,
    tklw90 = extract_per90(def, c("TklW_Tackles", "TklW", "Tackles_Won")),
    prgc90 = extract_per90(pos, c("PrgC_Carries", "PrgC", "Progressive_Carries")),
    prgp90 = extract_per90(pas, c("PrgP", "Progressive_Passes", "PrgP_Total"))
  )
}

basic_metrics <- purrr::pmap_dfr(players_all, extract_basic)
advanced_metrics <- purrr::pmap_dfr(players_modern, extract_advanced)

metrics <- basic_metrics %>% 
  left_join(advanced_metrics, by = "player")

plot_bars <- function(df, metric_col, metric_name, all_players = TRUE) {
  df <- df %>% 
    filter(!is.na(!!sym(metric_col))) %>%
    mutate(player = forcats::fct_reorder(player, !!sym(metric_col)))
  
  if (nrow(df) == 0) return(NULL)
  
  xmax <- max(df[[metric_col]], na.rm = TRUE)
  
  era_colors <- c("Modern" = "#E63946", "Classic" = "#457B9D")
  
  ggplot(df, aes(x = !!sym(metric_col), y = player, fill = era)) +
    geom_col(width = 0.75, alpha = 0.9) +
    geom_text(aes(label = scales::number(!!sym(metric_col), accuracy = 0.01)),
              hjust = -0.1, size = 4, fontface = "bold", color = "#2d2d2d") +
    scale_fill_manual(values = era_colors) +
    coord_cartesian(xlim = c(0, xmax * 1.2)) +
    labs(
      title = metric_name,
      subtitle = if(all_players) "All Players • FBref Season Stats" else "Modern Era (2017+) • FBref Season Stats"
    ) +
    theme_nice()
}

plot_scatter <- function(df, x_col, y_col, x_lab, y_lab) {
  df <- df %>% filter(!is.na(!!sym(x_col)), !is.na(!!sym(y_col)))
  if (nrow(df) == 0) return(NULL)
  
  era_colors <- c("Modern" = "#E63946", "Classic" = "#457B9D")
  
  ggplot(df, aes(x = !!sym(x_col), y = !!sym(y_col), color = era, label = player)) +
    geom_point(size = 5, alpha = 0.8) +
    geom_text_repel(size = 3.5, fontface = "bold", max.overlaps = 20, 
                    box.padding = 0.5, point.padding = 0.3) +
    scale_color_manual(values = era_colors) +
    labs(title = paste(y_lab, "vs", x_lab),
         subtitle = "All Players • FBref Season Stats",
         x = x_lab, y = y_lab) +
    theme_nice() +
    theme(axis.title = element_text(size = 11, face = "bold"),
          legend.position = "top",
          legend.title = element_blank(),
          legend.text = element_text(size = 11, face = "bold"))
}

plot_radar <- function(df) {
  df_modern <- df %>% 
    filter(!is.na(tklw90), !is.na(prgc90), !is.na(prgp90)) %>%
    mutate(across(c(gls90, ast90, tklw90, prgc90, prgp90), 
                  ~scales::rescale(., to = c(0, 100))))
  
  if (nrow(df_modern) == 0) return(NULL)
  
  df_long <- df_modern %>%
    select(player, gls90, ast90, tklw90, prgc90, prgp90) %>%
    pivot_longer(-player, names_to = "metric", values_to = "value") %>%
    mutate(metric = recode(metric,
                           gls90 = "Goals",
                           ast90 = "Assists", 
                           tklw90 = "Tackles",
                           prgc90 = "Prog. Carries",
                           prgp90 = "Prog. Passes"))
  
  ggplot(df_long, aes(x = metric, y = value, group = player, color = player)) +
    geom_polygon(fill = NA, size = 1.2, alpha = 0.3) +
    geom_point(size = 3) +
    coord_polar() +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
    scale_color_viridis_d(option = "turbo") +
    labs(title = "Modern Players: Complete Profile Comparison",
         subtitle = "All metrics scaled 0-100 per player maximum") +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 18, margin = margin(b = 8)),
      plot.subtitle = element_text(size = 12, colour = "#555555", margin = margin(b = 12)),
      plot.background = element_rect(fill = "#fafafa", color = NA),
      panel.background = element_rect(fill = "#fafafa", color = NA),
      axis.text.y = element_blank(),
      axis.text.x = element_text(size = 11, face = "bold"),
      legend.position = "right",
      legend.title = element_blank(),
      legend.text = element_text(size = 10),
      panel.grid.major = element_line(color = "#cccccc", linetype = "dotted"),
      plot.margin = margin(15, 15, 15, 15)
    )
}

cat("\n=== GOALS PER 90 ===\n")
print(plot_bars(metrics, "gls90", "Goals per 90 Minutes", TRUE))

cat("\n=== ASSISTS PER 90 ===\n")
print(plot_bars(metrics, "ast90", "Assists per 90 Minutes", TRUE))

cat("\n=== GOALS VS ASSISTS ===\n")
print(plot_scatter(metrics, "gls90", "ast90", "Goals per 90", "Assists per 90"))

cat("\n=== TACKLES WON PER 90 ===\n")
print(plot_bars(metrics, "tklw90", "Tackles Won per 90 Minutes", FALSE))

cat("\n=== PROGRESSIVE CARRIES PER 90 ===\n")
print(plot_bars(metrics, "prgc90", "Progressive Carries per 90 Minutes", FALSE))

cat("\n=== PROGRESSIVE PASSES PER 90 ===\n")
print(plot_bars(metrics, "prgp90", "Progressive Passes per 90 Minutes", FALSE))

cat("\n=== RADAR CHART ===\n")
print(plot_radar(metrics))

cat("\n=== COMPLETE DATA TABLE ===\n")
print(metrics)