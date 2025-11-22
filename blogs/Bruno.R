############################################################
# Bruno Fernandes 24/25 PL share-of-team analysis
# Dataset: https://www.kaggle.com/datasets/siddhrajthakor/fbref-premier-league-202425-player-stats-dataset
############################################################

library(dplyr)
library(readr)
library(stringr)
library(tibble)

## 1. Load data --------------------------------------------------------------

fbref_path <- "fbref_PL_2024-25.csv"  # change if your name differs

pl_raw <- read_csv(fbref_path, show_col_types = FALSE)

## 2. Normalise & map relevant columns ---------------------------------------
# Your columns (from your message):
# "Rk","Player","Nation","Pos","Squad","Age","Born","MP","Starts","Min","90s",
# "Gls...12","Ast...13","G+A...14","G-PK...15","PK","PKatt","CrdY","CrdR",
# "xG...20","npxG...21","xAG...22","npxG+xAG...23","PrgC","PrgP","PrgR",
# "Gls...27","Ast...28","G+A...29","G-PK...30","G+A-PK","xG...32","xAG...33",
# "xG+xAG","npxG...35","npxG+xAG...36"

pl <- pl_raw |>
  rename_with(str_to_lower) |>
  rename(
    player    = player,
    squad     = squad,
    pos       = pos,
    minutes   = min,
    prgc      = prgc,
    prgp      = prgp,
    xg        = `xg...20`,     # total xG
    npxg      = `npxg...21`,   # total non-pen xG
    xag       = `xag...22`,    # total xAG
    ast_total = `ast...13`,    # total assists
    ast_90    = `ast...28`,    # assists per 90
    xag_90    = `xag...33`     # xAG per 90
  )

pl_players <- pl

## 3. Identify Bruno, United, forwards, GKs ----------------------------------

bruno_name  <- "Bruno Fernandes"
united_name <- "Manchester Utd"  # adjust if your CSV uses a different label

bruno <- pl_players |>
  filter(player == bruno_name, squad == united_name)

if (nrow(bruno) == 0) {
  stop("Bruno Fernandes not found. Check 'player'/'squad' values (try unique(pl_players$squad)).")
}

# Forwards to compare
mu_forwards_vec <- c(
  "Antony",
  "Alejandro Garnacho",
  "Rasmus Højlund",
  "Joshua Zirkzee",
  "Amad Diallo"
)

mu_forwards <- pl_players |>
  filter(squad == united_name, player %in% mu_forwards_vec)

# Identify goalkeepers (for minutes ranking only)
pl_players <- pl_players |>
  mutate(is_gk = str_detect(pos, "GK"))

## 4. Team totals (for share-of-team) ----------------------------------------

team_totals <- pl_players |>
  group_by(squad) |>
  summarise(
    team_minutes = sum(minutes, na.rm = TRUE),
    team_xg      = sum(xg,      na.rm = TRUE),
    team_npxg    = sum(npxg,    na.rm = TRUE),
    team_xag     = sum(xag,     na.rm = TRUE),
    team_prgc    = sum(prgc,    na.rm = TRUE),
    team_prgp    = sum(prgp,    na.rm = TRUE),
    .groups = "drop"
  )

united_totals <- team_totals |>
  filter(squad == united_name)

if (nrow(united_totals) == 0) {
  stop("No team totals found for Manchester Utd – check squad naming.")
}

## 5. League minutes ranking (outfield only) ---------------------------------

minutes_ranked_outfield <- pl_players |>
  filter(!is_gk, !is.na(minutes)) |>
  arrange(desc(minutes)) |>
  mutate(minutes_rank_outfield = row_number())

bruno_minutes_row <- minutes_ranked_outfield |>
  filter(player == bruno_name, squad == united_name)

if (nrow(bruno_minutes_row) == 0) {
  stop("Bruno not found in outfield minutes ranking – check position/squad.")
}

bruno_minutes            <- bruno_minutes_row$minutes[1]
bruno_minutes_rank_out   <- bruno_minutes_row$minutes_rank_outfield[1]

bruno_minutes_league_tbl <- minutes_ranked_outfield |>
  select(minutes_rank_outfield, player, squad, minutes) |>
  arrange(minutes_rank_outfield)

## 6. League progression rankings (PrgC & PrgP) ------------------------------

prgc_ranked <- pl_players |>
  filter(!is.na(prgc)) |>
  arrange(desc(prgc)) |>
  mutate(prgc_rank = row_number())

prgp_ranked <- pl_players |>
  filter(!is.na(prgp)) |>
  arrange(desc(prgp)) |>
  mutate(prgp_rank = row_number())

bruno_prgc_row <- prgc_ranked |>
  filter(player == bruno_name, squad == united_name)

bruno_prgp_row <- prgp_ranked |>
  filter(player == bruno_name, squad == united_name)

bruno_prgc      <- bruno_prgc_row$prgc[1]
bruno_prgc_rank <- bruno_prgc_row$prgc_rank[1]

bruno_prgp      <- bruno_prgp_row$prgp[1]
bruno_prgp_rank <- bruno_prgp_row$prgp_rank[1]

bruno_league_ranks <- tibble(
  metric      = c("Minutes (outfield)", "Progressive carries", "Progressive passes"),
  value       = c(bruno_minutes,        bruno_prgc,            bruno_prgp),
  league_rank = c(bruno_minutes_rank_out, bruno_prgc_rank,     bruno_prgp_rank)
)

## 7. Bruno share-of-team metrics --------------------------------------------

u_tot <- united_totals  # shorthand

bruno_team_share <- tibble(
  metric       = c("Minutes", "xG", "npxG", "xAG", "PrgC", "PrgP"),
  player_value = c(bruno$minutes[1], bruno$xg[1], bruno$npxg[1],
                   bruno$xag[1], bruno$prgc[1], bruno$prgp[1]),
  team_total   = c(u_tot$team_minutes[1], u_tot$team_xg[1], u_tot$team_npxg[1],
                   u_tot$team_xag[1], u_tot$team_prgc[1], u_tot$team_prgp[1])
) |>
  mutate(
    share_pct = round(100 * player_value / team_total, 1)
  )

## 8. United forwards: share-of-team for the same metrics --------------------

mu_forwards_share <- mu_forwards |>
  mutate(
    minutes_share_pct = 100 * minutes / u_tot$team_minutes[1],
    xg_share_pct      = 100 * xg      / u_tot$team_xg[1],
    npxg_share_pct    = 100 * npxg    / u_tot$team_npxg[1],
    xag_share_pct     = 100 * xag     / u_tot$team_xag[1],
    prgc_share_pct    = 100 * prgc    / u_tot$team_prgc[1],
    prgp_share_pct    = 100 * prgp    / u_tot$team_prgp[1]
  ) |>
  select(
    player,
    minutes, minutes_share_pct,
    xg,      xg_share_pct,
    npxg,    npxg_share_pct,
    xag,     xag_share_pct,
    prgc,    prgc_share_pct,
    prgp,    prgp_share_pct
  ) |>
  arrange(desc(npxg_share_pct))

# Group-level forwards vs team summary (optional but useful in article)
forwards_share_summary <- mu_forwards |>
  summarise(
    forwards_minutes = sum(minutes, na.rm = TRUE),
    forwards_xg      = sum(xg,      na.rm = TRUE),
    forwards_npxg    = sum(npxg,    na.rm = TRUE),
    forwards_xag     = sum(xag,     na.rm = TRUE),
    forwards_prgc    = sum(prgc,    na.rm = TRUE),
    forwards_prgp    = sum(prgp,    na.rm = TRUE)
  ) |>
  mutate(
    team_minutes = u_tot$team_minutes[1],
    team_xg      = u_tot$team_xg[1],
    team_npxg    = u_tot$team_npxg[1],
    team_xag     = u_tot$team_xag[1],
    team_prgc    = u_tot$team_prgc[1],
    team_prgp    = u_tot$team_prgp[1],
    forwards_minutes_share_pct = 100 * forwards_minutes / team_minutes,
    forwards_xg_share_pct      = 100 * forwards_xg      / team_xg,
    forwards_npxg_share_pct    = 100 * forwards_npxg    / team_npxg,
    forwards_xag_share_pct     = 100 * forwards_xag     / team_xag,
    forwards_prgc_share_pct    = 100 * forwards_prgc    / team_prgc,
    forwards_prgp_share_pct    = 100 * forwards_prgp    / team_prgp
  )

## 9. Assists vs xAG (kept as per-90, not share-of-team) ---------------------

bruno_assist_profile <- tibble(
  player     = bruno_name,
  squad      = united_name,
  minutes    = bruno$minutes[1],
  ast_total  = bruno$ast_total[1],
  xag_total  = bruno$xag[1],
  ast_per90  = bruno$ast_90[1],
  xag_per90  = bruno$xag_90[1]
)

## 10. Final output (easy to read) -------------------------------------------

cat("\n=== Bruno Fernandes: League Ranks (Outfield Minutes, Progression) ===\n")
print(bruno_league_ranks)

cat("\n=== Bruno Fernandes: Share of Manchester United Totals (%) ===\n")
print(bruno_team_share)

cat("\n=== Manchester United Forwards: Share of Team Totals (%) ===\n")
print(mu_forwards_share)

cat("\n=== United Forwards Group vs Team: Share Summary (%) ===\n")
print(forwards_share_summary)

cat("\n=== Bruno Fernandes: Assists vs xAG Profile ===\n")
print(bruno_assist_profile)

bruno_results <- list(
  bruno_league_ranks      = bruno_league_ranks,
  bruno_team_share        = bruno_team_share,
  mu_forwards_share       = mu_forwards_share,
  forwards_share_summary  = forwards_share_summary,
  bruno_assist_profile    = bruno_assist_profile,
  minutes_league_outfield = bruno_minutes_league_tbl
)

bruno_results
############################################################
