# ============================== #
# Liverpool under Arne Slot — PPG, xG, Shots (worldfootballR; FREE)
# 24/25 pre-title vs post-title, and 25/26 to date (played games only)
# Team URLs fetched via: fb_teams_urls("https://fbref.com/en/comps/9/Premier-League-Stats")
# ============================== #

required_pkgs <- c("worldfootballR","dplyr","tidyr","tibble","stringr","lubridate","zoo","purrr")
to_install <- setdiff(required_pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")
invisible(lapply(required_pkgs, library, character.only = TRUE))

# ---------- Helpers ----------
pts_from_score <- function(gf, ga) ifelse(gf > ga, 3L, ifelse(gf == ga, 1L, 0L))

as_event_dt <- function(date_chr, time_chr) {
  dt <- suppressWarnings(ymd_hm(paste(date_chr, time_chr), tz = "UTC"))
  if (anyNA(dt)) {
    dt_na <- is.na(dt)
    if (any(dt_na)) dt[dt_na] <- as_datetime(ymd(date_chr[dt_na], tz = "UTC"))
  }
  dt
}

compute_clinch_index <- function(match_df, total_games = 38L) {
  events <- match_df %>%
    mutate(
      event_dt = as_event_dt(Date, Time),
      match_id = MatchURL,
      home = Home, away = Away,
      hg = as.integer(HomeGoals), ag = as.integer(AwayGoals)
    ) %>%
    arrange(event_dt, match_id) %>%
    select(event_dt, match_id, home, away, hg, ag)
  
  incr <- events %>% mutate(h_pts = pts_from_score(hg, ag), a_pts = pts_from_score(ag, hg))
  teams <- sort(unique(c(incr$home, incr$away)))
  cum_pts <- setNames(integer(length(teams)), teams)
  cum_gp  <- setNames(integer(length(teams)), teams)
  
  clinch_idx <- NA_integer_
  for (i in seq_len(nrow(incr))) {
    h <- incr$home[i]; a <- incr$away[i]
    cum_pts[h] <- cum_pts[h] + incr$h_pts[i]
    cum_pts[a] <- cum_pts[a] + incr$a_pts[i]
    cum_gp[h]  <- cum_gp[h]  + 1L
    cum_gp[a]  <- cum_gp[a]  + 1L
    if ("Liverpool" %in% names(cum_pts)) {
      lfc_pts <- cum_pts["Liverpool"]
      other <- setdiff(names(cum_pts), "Liverpool")
      if (length(other)) {
        max_other <- max(cum_pts[other] + 3L * pmax(0L, total_games - cum_gp[other]))
        if (lfc_pts > max_other) { clinch_idx <- i; break }
      }
    }
  }
  list(clinch_idx = clinch_idx, events = events)
}

lfc_match_frame <- function(match_df) {
  match_df %>%
    transmute(
      match_id = MatchURL,
      date_time = as_event_dt(Date, Time),
      home_team = Home, away_team = Away,
      home_goals = suppressWarnings(as.integer(HomeGoals)),
      away_goals = suppressWarnings(as.integer(AwayGoals)),
      home_xg = suppressWarnings(as.numeric(Home_xG)),
      away_xg = suppressWarnings(as.numeric(Away_xG))
    ) %>%
    mutate(
      side = case_when(home_team == "Liverpool" ~ "H", away_team == "Liverpool" ~ "A", TRUE ~ NA_character_),
      opp  = if_else(side == "H", away_team, home_team),
      gf   = if_else(side == "H", home_goals, away_goals),
      ga   = if_else(side == "H", away_goals, home_goals),
      xg_f = if_else(side == "H", home_xg, away_xg),
      xg_a = if_else(side == "H", away_xg, home_xg),
      pts  = pts_from_score(gf, ga),
      gd   = gf - ga,
      xg_diff = xg_f - xg_a
    ) %>%
    filter(side %in% c("H","A")) %>%
    arrange(date_time) %>%
    mutate(md = row_number()) %>%
    select(match_id, date_time, md, opp, gf, ga, gd, pts, xg_f, xg_a, xg_diff, side)
}

# Shots logs (version-safe). We:
# 1) fetch team URLs from the PL page
# 2) pick Liverpool's URL
# 3) call whichever shooting-log function your version exposes
get_lfc_shooting <- function(season_end_year) {
  comp_url <- "https://fbref.com/en/comps/9/Premier-League-Stats"
  team_urls <- fb_teams_urls(comp_url)      # per your required usage
  # team_urls may be a character vector; keep it vector-safe
  lfc_url <- team_urls[stringr::str_detect(team_urls, "Liverpool")][1]
  
  # Try modern consolidated API first; fall back to older name; pass season_end_year if supported
  sh <- tryCatch(
    {
      # attempt with season_end_year arg
      tryCatch(
        fb_team_match_log_stats(team_urls = lfc_url, stat_type = "shooting", season_end_year = season_end_year),
        error = function(e) fb_team_match_log_stats(team_urls = lfc_url, stat_type = "shooting")
      )
    },
    error = function(e1) {
      tryCatch(
        fb_team_match_log_shooting(team_urls = lfc_url, season_end_year = season_end_year),
        error = function(e2) fb_team_match_log_shooting(team_urls = lfc_url)
      )
    }
  )
  
  # Normalize likely columns across versions
  nm <- names(sh)
  opp_col <- if ("Opponent" %in% nm) "Opponent" else nm[stringr::str_detect(nm, "Opponent|Opp")]
  comp_col <- if ("Comp" %in% nm) "Comp" else nm[stringr::str_detect(nm, "Comp")]
  shots_col <- dplyr::coalesce(
    which(nm == "Sh") %>% {if(length(.)==0) NA_integer_ else .},
    which(stringr::str_detect(nm, "^Sh(_|$)|Shots$"))[1]
  )
  sot_col <- dplyr::coalesce(
    which(nm == "SoT") %>% {if(length(.)==0) NA_integer_ else .},
    which(stringr::str_detect(nm, "SoT|Shots_on_Target"))[1]
  )
  shots_nm <- nm[shots_col]
  sot_nm   <- if (!is.na(sot_col)) nm[sot_col] else NA_character_
  
  sh %>%
    mutate(date_time = as_datetime(ymd(.data$Date), tz = "UTC")) %>%
    filter(.data[[comp_col]] %in% c("Premier League","English Premier League")) %>%
    transmute(
      date_time,
      opp = .data[[opp_col]],
      shots_f = suppressWarnings(as.numeric(.data[[shots_nm]])),
      sot_f   = if (!is.na(sot_nm)) suppressWarnings(as.numeric(.data[[sot_nm]])) else NA_real_
    )
}

# ---------- Load league results ----------
pl_2425 <- fb_match_results(country="ENG", gender="M", season_end_year=2025, tier="1st")
pl_2526 <- fb_match_results(country="ENG", gender="M", season_end_year=2026, tier="1st")

# ---------- 24/25 clinch ----------
clinch <- compute_clinch_index(pl_2425, total_games = 38L)
clinch_idx <- clinch$clinch_idx
events_2425 <- clinch$events
clinch_dt <- if (!is.na(clinch_idx)) events_2425$event_dt[clinch_idx] else NA

# ---------- Liverpool frames ----------
lfc_2425 <- lfc_match_frame(pl_2425)
lfc_2526 <- lfc_match_frame(pl_2526)

# Event order for split
lfc_2425 <- lfc_2425 %>% left_join(events_2425 %>% mutate(event_idx = row_number()) %>% select(match_id, event_idx), by = "match_id")
lfc_2425_pre  <- if (!is.na(clinch_idx)) filter(lfc_2425, event_idx <  clinch_idx) else lfc_2425[0,]
lfc_2425_post <- if (!is.na(clinch_idx)) filter(lfc_2425, event_idx >= clinch_idx) else lfc_2425[0,]

# ---------- Shooting logs (from PL page URL, per your requirement) ----------
shoot_2425 <- get_lfc_shooting(2025)
shoot_2526 <- get_lfc_shooting(2026)

merge_shots <- function(df, shots_tbl) {
  df %>%
    left_join(shots_tbl, by = c("date_time","opp")) %>%
    mutate(xg_per_shot = if_else(!is.na(shots_f) & shots_f > 0, xg_f / shots_f, NA_real_))
}

lfc_2425_pre  <- merge_shots(lfc_2425_pre,  shoot_2425)
lfc_2425_post <- merge_shots(lfc_2425_post, shoot_2425)
lfc_2526      <- merge_shots(lfc_2526,      shoot_2526)

# ---------- Played-only filter ----------
played_only <- function(df) df %>% filter(!is.na(pts), !is.na(gf), !is.na(ga))

# ---------- Segment aggregations ----------
seg_agg <- function(df) {
  d <- played_only(df)
  gms <- nrow(d)
  if (!gms) return(tibble(
    games=0, points=0, ppg=NA_real_, gd_pg=NA_real_,
    xg_for_pg=NA_real_, xg_ag_pg=NA_real_,
    shots_pg=NA_real_, xg_per_shot=NA_real_
  ))
  tibble(
    games = gms,
    points = sum(d$pts),
    ppg = round(sum(d$pts)/gms, 3),
    gd_pg = round(mean(d$gd), 2),
    xg_for_pg = round(mean(d$xg_f, na.rm = TRUE), 2),
    xg_ag_pg  = round(mean(d$xg_a, na.rm = TRUE), 2),
    shots_pg  = round(mean(d$shots_f, na.rm = TRUE), 1),
    xg_per_shot = round(mean(d$xg_per_shot, na.rm = TRUE), 3)
  )
}

a_pre   <- seg_agg(lfc_2425_pre)
a_post  <- seg_agg(lfc_2425_post)
b_2526  <- seg_agg(lfc_2526)

# ---------- 2×3 outputs ----------
tbl_ppg_gd <- tibble::tibble(
  Metric = c("PPG","GD per game"),
  `24/25 Pre-title`  = c(a_pre$ppg, a_pre$gd_pg),
  `24/25 Post-title` = c(a_post$ppg, a_post$gd_pg),
  `25/26 (to date)`  = c(b_2526$ppg, b_2526$gd_pg)
)

tbl_xg <- tibble::tibble(
  Metric = c("xG for / game","xG against / game"),
  `24/25 Pre-title`  = c(a_pre$xg_for_pg, a_pre$xg_ag_pg),
  `24/25 Post-title` = c(a_post$xg_for_pg, a_post$xg_ag_pg),
  `25/26 (to date)`  = c(b_2526$xg_for_pg, b_2526$xg_ag_pg)
)

tbl_shots_quality <- tibble::tibble(
  Metric = c("Shots / game","xG per shot"),
  `24/25 Pre-title`  = c(a_pre$shots_pg, a_pre$xg_per_shot),
  `24/25 Post-title` = c(a_post$shots_pg, a_post$xg_per_shot),
  `25/26 (to date)`  = c(b_2526$shots_pg, b_2526$xg_per_shot)
)

# ---------- Output ----------
cat("\n================= TITLE CLINCH (2024/25) =================\n")
if (is.na(clinch_idx)) {
  cat("Clinch not determinable.\n")
} else {
  cat("Liverpool mathematically clinched on: ", format(clinch_dt, "%Y-%m-%d %H:%M:%S UTC"), "\n")
}

cat("\n===== 2x3: PPG & GD per game =====\n"); print(tbl_ppg_gd)
cat("\n===== 2x3: xG for/against per game =====\n"); print(tbl_xg)
cat("\n===== 2x3: Shots & xG/shot =====\n"); print(tbl_shots_quality)

# Context roll (played-only)
lfc_2526_roll <- lfc_2526 %>%
  filter(!is.na(pts)) %>%
  arrange(date_time) %>%
  mutate(
    ppg_last7 = if (nrow(.) >= 7) zoo::rollapply(pts, 7, function(x) sum(x)/7, align="right", fill=NA_real_) else NA_real_,
    xg_diff_last7 = if (nrow(.) >= 7) zoo::rollapply(xg_diff, 7, mean, align="right", fill=NA_real_) else NA_real_
  ) %>%
  select(md, date_time, opp, gf, ga, pts, xg_diff, ppg_last7, xg_diff_last7)
cat("\n====== 2025/26 Rolling 7-match form (PLAYED ONLY) ========\n")
print(tail(lfc_2526_roll, 15))

# ============================== #
# Liverpool shotmaps + per-game tables (Understat FOR-shots only)
# + Verification vs FBref xG/game
# Windows: 24/25 pre-title, 24/25 post-title, 25/26 to date
# ============================== #

# ---- Install/Load ----
if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
if (!requireNamespace("worldfootballR", quietly = TRUE)) devtools::install_github("JaseZiv/worldfootballR")
suppressPackageStartupMessages({
  library(worldfootballR)
  library(dplyr); library(tidyr); library(tibble)
  library(stringr); library(lubridate)
  library(ggplot2); library(ggsoccer); library(patchwork)
})

# ---- Helpers ----
as_event_dt <- function(date_chr, time_chr) {
  dt <- suppressWarnings(ymd_hm(paste(date_chr, time_chr), tz = "UTC"))
  if (anyNA(dt)) {
    dt_na <- is.na(dt)
    if (any(dt_na)) dt[dt_na] <- as_datetime(ymd(date_chr[dt_na], tz = "UTC"))
  }
  dt
}
pts_from_score <- function(gf, ga) ifelse(gf > ga, 3L, ifelse(gf == ga, 1L, 0L))

compute_clinch_dt_2425 <- function() {
  pl <- fb_match_results(country="ENG", gender="M", season_end_year=2025, tier="1st")
  events <- pl %>%
    mutate(
      event_dt = as_event_dt(Date, Time),
      match_id = MatchURL,
      home = Home, away = Away,
      hg = as.integer(HomeGoals), ag = as.integer(AwayGoals)
    ) %>%
    arrange(event_dt, match_id)
  incr <- events %>% mutate(h_pts = pts_from_score(hg, ag), a_pts = pts_from_score(ag, hg))
  teams <- sort(unique(c(incr$home, incr$away))); cum_pts <- setNames(integer(length(teams)), teams); cum_gp <- setNames(integer(length(teams)), teams)
  clinch_idx <- NA_integer_
  for (i in seq_len(nrow(incr))) {
    h <- incr$home[i]; a <- incr$away[i]
    cum_pts[h] <- cum_pts[h] + incr$h_pts[i]; cum_pts[a] <- cum_pts[a] + incr$a_pts[i]
    cum_gp[h]  <- cum_gp[h]  + 1L;            cum_gp[a]  <- cum_gp[a]  + 1L
    if ("Liverpool" %in% names(cum_pts)) {
      lfc_pts <- cum_pts["Liverpool"]; other <- setdiff(names(cum_pts), "Liverpool")
      if (length(other)) {
        max_other <- max(cum_pts[other] + 3L * pmax(0L, 38L - cum_gp[other]))
        if (lfc_pts > max_other) { clinch_idx <- i; break }
      }
    }
  }
  if (is.na(clinch_idx)) NA else events$event_dt[clinch_idx]
}

# Normalize Understat team-season shots; FILTER to Liverpool FOR-shots only
norm_and_filter_lfc_for <- function(df) {
  nm <- names(df)
  stopifnot(all(c("X","Y","xG","result","date","home_away","home_team","away_team") %in% nm))
  df %>%
    mutate(
      X = as.numeric(X), Y = as.numeric(Y), xG = as.numeric(xG),
      date_time = suppressWarnings(lubridate::ymd_hms(date, tz = "UTC")),
      x_sb = X * 120, y_sb = Y * 80,
      is_goal = result == "Goal",
      is_lfc_for = (home_away == "h" & home_team == "Liverpool") |
        (home_away == "a" & away_team == "Liverpool")
    ) %>%
    filter(is_lfc_for) %>%
    select(match_id, date_time, xG, x_sb, y_sb, is_goal)
}

theme_pitch <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    theme(panel.grid = element_blank(), axis.text = element_blank(), axis.title = element_blank(),
          plot.title = element_text(face="bold", size=16, margin=margin(b=2)),
          plot.subtitle = element_text(size=11, colour="#555"),
          plot.caption = element_text(size=9, colour="#777"))
}
plot_shotmap <- function(df, title, subtitle = NULL) {
  if (!nrow(df)) {
    return(ggplot() +
             ggsoccer::annotate_pitch(dimensions = ggsoccer::pitch_statsbomb, colour = "#999999") +
             coord_fixed(xlim = c(60, 120), ylim = c(0, 80)) +
             labs(title = title, subtitle = paste0(subtitle, " | No shots"),
                  caption = "Shots: Understat via worldfootballR") +
             theme_pitch())
  }
  df2 <- df %>% mutate(outcome = if_else(is_goal, "Goal", "Miss"))
  ggplot(df2, aes(x = x_sb, y = y_sb)) +
    ggsoccer::annotate_pitch(dimensions = ggsoccer::pitch_statsbomb, colour = "#999999") +
    coord_fixed(xlim = c(60, 120), ylim = c(0, 80)) +
    geom_point(aes(size = xG, fill = outcome),
               alpha = 0.6, shape = 21, color = "#222222", stroke = 0.2) +
    scale_fill_manual(values = c("Goal" = "#1f77b4", "Miss" = "#d62728"), name = NULL) +
    scale_size_continuous(range = c(1.0, 4.0), breaks = c(0.05, 0.15, 0.30), name = "xG") +
    labs(title = title, subtitle = subtitle,
         caption = "Shots: Understat via worldfootballR; Clinch: FBref via worldfootballR") +
    theme_pitch() +
    guides(fill = guide_legend(override.aes = list(size = 3, alpha = 0.9)))
}

# Per-game summary (xG pg, Goals pg, (G−xG) pg, Shots pg) using FOR-shots only
per_game_table <- function(df_window) {
  if (!nrow(df_window)) {
    return(tibble(Metric = c("xG per game","Goals per game","(Goals − xG) per game","Shots per game"),
                  Value = NA_real_))
  }
  per_match <- df_window %>%
    group_by(match_id) %>%
    summarise(xg_match = sum(xG, na.rm = TRUE),
              goals_match = sum(is_goal, na.rm = TRUE),
              shots_match = n(), .groups = "drop")
  tibble(
    `xG per game`           = mean(per_match$xg_match, na.rm = TRUE),
    `Goals per game`        = mean(per_match$goals_match, na.rm = TRUE),
    `(Goals − xG) per game` = mean(per_match$goals_match - per_match$xg_match, na.rm = TRUE),
    `Shots per game`        = mean(per_match$shots_match, na.rm = TRUE)
  ) %>%
    tidyr::pivot_longer(everything(), names_to = "Metric", values_to = "Value") %>%
    mutate(Value = round(Value, 3))
}

# ---- Clinch time (24/25) ----
clinch_dt <- compute_clinch_dt_2425()
if (is.na(clinch_dt)) clinch_dt <- as.POSIXct("2025-04-27 16:30:00", tz = "UTC")

# ---- Understat pulls (team-season) ----
lfc_url_2425 <- "https://understat.com/team/Liverpool/2024"  # 24/25
lfc_url_2526 <- "https://understat.com/team/Liverpool/2025"  # 25/26 (to date)

shots_2425_raw <- understat_team_season_shots(team_url = lfc_url_2425)
shots_2526_raw <- understat_team_season_shots(team_url = lfc_url_2526)

# Keep Liverpool FOR-shots only
shots_2425 <- norm_and_filter_lfc_for(shots_2425_raw)
shots_2526 <- norm_and_filter_lfc_for(shots_2526_raw)

# Split windows
shots_2425_pre  <- shots_2425 %>% filter(date_time <  clinch_dt)
shots_2425_post <- shots_2425 %>% filter(date_time >= clinch_dt)
shots_2526_now  <- shots_2526

# Subtitles
summ_txt <- function(d) {
  tibble(shots = nrow(d), goals = sum(d$is_goal, na.rm = TRUE), xg = sum(d$xG, na.rm = TRUE)) %>%
    mutate(txt = sprintf("Shots: %d | Goals: %d | xG: %.2f", shots, goals, xg)) %>% pull(txt)
}
sub_pre  <- summ_txt(shots_2425_pre)
sub_post <- summ_txt(shots_2425_post)
sub_2526 <- summ_txt(shots_2526_now)

# ---- Shotmaps ----
p1 <- plot_shotmap(shots_2425_pre,   "Liverpool Shotmap — 2024/25 (Pre-Title)",  sub_pre)
p2 <- plot_shotmap(shots_2425_post,  "Liverpool Shotmap — 2024/25 (Post-Title)", sub_post)
p3 <- plot_shotmap(shots_2526_now,   "Liverpool Shotmap — 2025/26 (To Date)",    sub_2526)

print((p1 | p2 | p3) + plot_annotation(
  title = "Liverpool Shot Profiles Across Key Windows",
  subtitle = "Filled circles: Blue=Goal, Red=Miss; size ∝ xG; attacking left → right",
  theme = theme(plot.title = element_text(face="bold", size=18))
))

# ---- Per-game tables (FOR-shots only) ----
tbl_pre  <- per_game_table(shots_2425_pre)  %>% rename(`24/25 Pre-title` := Value)
tbl_post <- per_game_table(shots_2425_post) %>% rename(`24/25 Post-title` := Value)
tbl_2526 <- per_game_table(shots_2526_now)  %>% rename(`25/26 (to date)` := Value)
metrics_tbl <- tbl_pre %>% left_join(tbl_post, by = "Metric") %>% left_join(tbl_2526, by = "Metric")

cat("\n===== Per-Game Metrics (Understat; Liverpool FOR-shots only) =====\n")
print(metrics_tbl)

# ================= VERIFICATION vs FBref =================
lfc_match_frame <- function(match_df) {
  match_df %>%
    transmute(
      match_id = MatchURL,
      date_time = as_event_dt(Date, Time),
      home_team = Home, away_team = Away,
      home_xg = suppressWarnings(as.numeric(Home_xG)),
      away_xg = suppressWarnings(as.numeric(Away_xG))
    ) %>%
    mutate(
      side = case_when(home_team == "Liverpool" ~ "H", away_team == "Liverpool" ~ "A", TRUE ~ NA_character_),
      xg_f = if_else(side == "H", home_xg, away_xg)
    ) %>%
    filter(side %in% c("H","A")) %>%
    arrange(date_time)
}

pl_2425 <- fb_match_results(country="ENG", gender="M", season_end_year=2025, tier="1st")
pl_2526 <- fb_match_results(country="ENG", gender="M", season_end_year=2026, tier="1st")

lfc_2425_res <- lfc_match_frame(pl_2425)
lfc_2526_res <- lfc_match_frame(pl_2526)

# Split by clinch datetime
lfc_2425_pre_res  <- lfc_2425_res %>% filter(date_time <  clinch_dt)
lfc_2425_post_res <- lfc_2425_res %>% filter(date_time >= clinch_dt)

fbref_tbl <- tibble(
  Metric = "xG per game",
  `24/25 Pre-title`  = round(mean(lfc_2425_pre_res$xg_f,  na.rm = TRUE), 3),
  `24/25 Post-title` = round(mean(lfc_2425_post_res$xg_f, na.rm = TRUE), 3),
  `25/26 (to date)`  = round(mean(lfc_2526_res$xg_f,      na.rm = TRUE), 3)
)

cat("\n===== Cross-check: FBref xG per game (Liverpool FOR) =====\n")
print(fbref_tbl)

cat("\nNOTE: Understat and FBref xG models differ, so the levels won't match exactly.\nBut they should be in the same ballpark. If Understat xG/game is ~double FBref, you were counting both teams before; the filter above fixes that.\n")
