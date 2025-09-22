# ===== Robust Chelsea strikers script (defensive, single-copy) =====
# Paste into RStudio

# --- packages ---
required_pkgs <- c(
  "worldfootballR","dplyr","purrr","stringr","ggplot2","ggrepel","scales",
  "tidyr","gridExtra","stringi","tibble"
)
missing_pkgs <- required_pkgs[!required_pkgs %in% installed.packages()[,"Package"]]
if (length(missing_pkgs)) {
  message("Missing packages: ", paste(missing_pkgs, collapse = ", "),
          ". Install them if you want; script will attempt to run but some features may be limited.")
}

suppressPackageStartupMessages({
  library(worldfootballR)
  library(dplyr)
  library(purrr)
  library(stringr)
  library(ggplot2)
  library(ggrepel)
  library(scales)
  library(tidyr)
  library(gridExtra)
  library(stringi)
  library(tibble)
})

options(stringsAsFactors = FALSE)
time_pause <- 1.2

# small helpers -------------------------------------------------------
`%||%` <- function(a, b) if (!is.null(a)) a else b

safe_call <- function(f, ..., context = NULL, quiet = TRUE) {
  tryCatch({
    Sys.sleep(time_pause)
    res <- f(...)
    if (!quiet && !is.null(context)) message("✓ OK: ", context)
    res
  }, error = function(e) {
    if (!quiet && !is.null(context)) message("✗ Failed (", context, "): ", conditionMessage(e))
    NULL
  })
}

# returns a single numeric scalar (first non-missing) or NA_real_
clean_numeric <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_real_)
  # if it's a vector, take first non-NA element
  xv <- as.character(x)
  if (all(is.na(xv))) return(NA_real_)
  xv_first <- xv[which(!is.na(xv) & xv != "")[1]]
  if (is.na(xv_first) || xv_first == "") return(NA_real_)
  num <- str_replace_all(xv_first, "[^0-9\\.\\-]", "")
  if (num == "" || is.na(num)) return(NA_real_)
  suppressWarnings(as.numeric(num))
}

# Normalize names for robust matching
normalize_name <- function(x) {
  if (is.null(x) || is.na(x)) return("")
  x2 <- as.character(x)
  x2 <- stringi::stri_trans_general(x2, "Latin-ASCII")  # remove diacritics
  x2 <- tolower(x2)
  x2 <- str_replace_all(x2, "[^a-z0-9 ]", " ")
  x2 <- str_squish(x2)
  x2
}

# season fallback url
season_fbref_url <- function(season_start) {
  season_end <- season_start + 1
  paste0("https://fbref.com/en/comps/9/", season_start, "-", season_end, "/", season_start, "-", season_end, "-Premier-League-Stats")
}

# tm fee parsing (unchanged semantics)
parse_tm_fee <- function(fee_string) {
  if (is.null(fee_string) || is.na(fee_string)) return(NULL)
  fee_text <- str_trim(as.character(fee_string))
  lower <- tolower(fee_text)
  if (lower %in% c("", "free", "loan", "undisclosed", "-", "returned")) return(NULL)
  cur <- NA_character_
  if (str_detect(fee_text, "£")) cur <- "GBP"
  if (str_detect(fee_text, "€")) cur <- "EUR"
  if (str_detect(fee_text, "\\$")) cur <- "USD"
  num_text <- str_extract(fee_text, "[0-9,]+\\.?[0-9]*\\s*[mkMK]?")
  if (is.na(num_text)) return(NULL)
  num_clean <- str_replace_all(num_text, ",", "")
  mult <- 1
  if (str_detect(num_clean, regex("[mM]$"))) mult <- 1e6
  if (str_detect(num_clean, regex("[kK]$"))) mult <- 1e3
  numeric_part <- as.numeric(str_replace_all(num_clean, "[mMkK\\s]", ""))
  if (is.na(numeric_part)) return(NULL)
  list(raw = numeric_part * mult, currency = cur, raw_text = fee_text)
}

# players (your list)
player_debut_seasons <- c(
  "Hernan Crespo" = 2003, "Adrian Mutu" = 2003, "Didier Drogba" = 2004,
  "Mateja Kezman" = 2004, "Andriy Shevchenko" = 2006, "Claudio Pizarro" = 2007,
  "Nicolas Anelka" = 2008, "Daniel Sturridge" = 2009, "Fernando Torres" = 2011,
  "Romelu Lukaku" = 2011, "Demba Ba" = 2013, "Samuel Eto'o" = 2013,
  "Diego Costa" = 2014, "Loïc Rémy" = 2014, "Radamel Falcao" = 2015,
  "Michy Batshuayi" = 2016, "Álvaro Morata" = 2017, "Olivier Giroud" = 2018,
  "Gonzalo Higuaín" = 2019, "Timo Werner" = 2020, "Pierre-Emerick Aubameyang" = 2022,
  "Joao Pedro" = 2025
)

# small helper to safely extract numeric from a one-row df
get_single_numeric <- function(df_row, col_patterns_regex) {
  if (is.null(df_row) || nrow(df_row) == 0) return(NA_real_)
  cols <- names(df_row)[str_detect(names(df_row), regex(col_patterns_regex, ignore_case = TRUE))]
  if (length(cols) == 0) return(NA_real_)
  for (cname in cols) {
    val <- tryCatch(df_row[[cname]][1], error = function(e) NA)
    nval <- clean_numeric(val)
    if (!is.na(nval)) return(nval)
  }
  NA_real_
}

# -------------------------------
# Main robust collector (rewritten)
# -------------------------------
get_player_stats_robust <- function(player_name, season_start) {
  season_end <- season_start + 1
  message("\n--- ", player_name, " (", season_start, "/", season_end, ") ---")
  result <- tibble::tibble(
    player = player_name,
    season_start = season_start,
    season_end = season_end,
    goals = NA_real_,
    assists = NA_real_,
    minutes = NA_real_,
    prog_passes = NA_real_,
    prog_carries = NA_real_,
    data_source = NA_character_
  )
  
  # 1) league URL
  league_urls_try <- safe_call(function() {
    if ("fb_league_urls" %in% ls(getNamespace("worldfootballR"))) {
      worldfootballR::fb_league_urls(country = "ENG", gender = "M", season_end_year = season_end, tier = "1st")
    } else NULL
  }, context = paste0("fb_league_urls ", season_end), quiet = TRUE)
  
  league_url <- if (!is.null(league_urls_try) && length(league_urls_try) > 0) {
    idx <- which(str_detect(tolower(league_urls_try), "premier"))
    if (length(idx) == 0) idx <- 1
    league_urls_try[idx[1]]
  } else {
    season_fbref_url(season_start)
  }
  
  message("Using league URL: ", league_url)
  
  # 2) team urls
  team_urls <- safe_call(function(url) {
    if ("fb_teams_urls" %in% ls(getNamespace("worldfootballR"))) {
      out <- NULL
      try(out <- worldfootballR::fb_teams_urls(league_url = url), silent = TRUE)
      if (is.null(out)) {
        try(out <- worldfootballR::fb_teams_urls(country = "ENG", season_end_year = season_end, tier = "1st"), silent = TRUE)
      }
      out
    } else NULL
  }, league_url, context = paste0("fb_teams_urls for ", league_url), quiet = TRUE)
  
  if (is.null(team_urls) || length(team_urls) == 0) {
    message("fb_teams_urls returned nothing for league_url: ", league_url)
    return(result)
  }
  
  chelsea_idx <- which(str_detect(tolower(team_urls), "chelsea"))
  if (length(chelsea_idx) == 0) {
    message("No Chelsea URL in team URLs; showing first few:")
    message(paste(head(team_urls, 10), collapse = "\n"))
    return(result)
  }
  chelsea_url <- team_urls[chelsea_idx[1]]
  message("Chelsea URL: ", chelsea_url)
  
  # 3) standard table (goals, minutes)
  std <- safe_call(worldfootballR::fb_team_player_stats, team_urls = chelsea_url, stat_type = "standard",
                   context = paste0("fbref standard ", season_end), quiet = TRUE)
  target_norm <- normalize_name(player_name)
  if (!is.null(std) && nrow(std) > 0) {
    # find player column(s)
    player_cols <- names(std)[str_detect(names(std), regex("player|name", ignore_case = TRUE))]
    if (length(player_cols) == 0) player_cols <- names(std)[1]
    matched <- FALSE
    for (pc in player_cols) {
      col_vals <- as.character(std[[pc]])
      col_norm <- vapply(col_vals, normalize_name, FUN.VALUE = character(1))
      hits <- which(str_detect(col_norm, fixed(target_norm)))
      if (length(hits) == 0) hits <- agrep(target_norm, col_norm, max.distance = 0.15)
      if (length(hits) > 0) {
        row <- std[hits[1], , drop = FALSE]
        # extract goals, assists, minutes conservatively
        result$goals <- get_single_numeric(row, "^Gls$|^Goals?$|^G$")
        result$assists <- get_single_numeric(row, "^Ast$|^Assists?$")
        # minutes can be "Min" or "90s"
        mins1 <- get_single_numeric(row, "^Min$|^Minutes?$")
        if (!is.na(mins1)) result$minutes <- mins1 else {
          n90 <- get_single_numeric(row, "^90s$|^90$")
          if (!is.na(n90)) result$minutes <- n90 * 90
        }
        result$data_source <- paste0(result$data_source, ifelse(is.na(result$data_source) || result$data_source == "", "", "_"), "fbref_standard")
        message("Matched (fbref standard): ", as.character(row[[player_cols[1]]][1]))
        matched <- TRUE
        break
      }
    }
    if (!matched) message("No direct match found in fbref standard for ", player_name)
  } else {
    message("fbref standard table missing or empty for ", season_start)
  }
  
  # 4) progressive passes (passing_types) - more reliable for PrgP
  pass_types <- safe_call(worldfootballR::fb_team_player_stats, team_urls = chelsea_url, stat_type = "passing_types",
                          context = paste0("fbref passing_types ", season_end), quiet = TRUE)
  if (!is.null(pass_types) && nrow(pass_types) > 0) {
    # find player row similarly
    player_col_pt <- names(pass_types)[str_detect(names(pass_types), regex("player|name", ignore_case = TRUE))]
    if (length(player_col_pt) == 0) player_col_pt <- names(pass_types)[1]
    col_vals <- as.character(pass_types[[player_col_pt[1]]])
    col_norm <- vapply(col_vals, normalize_name, FUN.VALUE = character(1))
    hits <- which(str_detect(col_norm, fixed(target_norm)))
    if (length(hits) == 0) hits <- agrep(target_norm, col_norm, max.distance = 0.15)
    if (length(hits) > 0) {
      row <- pass_types[hits[1], , drop = FALSE]
      result$prog_passes <- get_single_numeric(row, "PrgP|Progressive.*Pass|Prog.*Pass")
      result$data_source <- paste0(result$data_source, ifelse(is.na(result$data_source) || result$data_source == "", "", "_"), "fbref_pass_types")
      message("Matched (fbref passing_types) for progressive passes.")
    }
  }
  
  # 5) progressive carries from possession
  pos <- safe_call(worldfootballR::fb_team_player_stats, team_urls = chelsea_url, stat_type = "possession",
                   context = paste0("fbref possession ", season_end), quiet = TRUE)
  if (!is.null(pos) && nrow(pos) > 0) {
    player_col_pos <- names(pos)[str_detect(names(pos), regex("player|name", ignore_case = TRUE))]
    if (length(player_col_pos) == 0) player_col_pos <- names(pos)[1]
    col_vals <- as.character(pos[[player_col_pos[1]]])
    col_norm <- vapply(col_vals, normalize_name, FUN.VALUE = character(1))
    hits <- which(str_detect(col_norm, fixed(target_norm)))
    if (length(hits) == 0) hits <- agrep(target_norm, col_norm, max.distance = 0.15)
    if (length(hits) > 0) {
      row <- pos[hits[1], , drop = FALSE]
      result$prog_carries <- get_single_numeric(row, "PrgC|Progressive.*Carr|Prog.*Carr")
      result$data_source <- paste0(result$data_source, ifelse(is.na(result$data_source) || result$data_source == "", "", "_"), "fbref_possession")
      message("Matched (fbref possession) for progressive carries.")
    }
  }
  
  # 6) shooting table as an extra place for assists & shooting stats (shots, etc.)
  shoot <- safe_call(worldfootballR::fb_team_player_stats, team_urls = chelsea_url, stat_type = "shooting",
                     context = paste0("fbref shooting ", season_end), quiet = TRUE)
  if (!is.null(shoot) && nrow(shoot) > 0) {
    player_col_sh <- names(shoot)[str_detect(names(shoot), regex("player|name", ignore_case = TRUE))]
    if (length(player_col_sh) == 0) player_col_sh <- names(shoot)[1]
    col_vals <- as.character(shoot[[player_col_sh[1]]])
    col_norm <- vapply(col_vals, normalize_name, FUN.VALUE = character(1))
    hits <- which(str_detect(col_norm, fixed(target_norm)))
    if (length(hits) == 0) hits <- agrep(target_norm, col_norm, max.distance = 0.15)
    if (length(hits) > 0) {
      row <- shoot[hits[1], , drop = FALSE]
      # if assists are missing from standard, try here
      if (is.na(result$assists) || result$assists == 0) {
        result$assists <- get_single_numeric(row, "Ast|Assist")
        if (!is.na(result$assists)) result$data_source <- paste0(result$data_source, ifelse(is.na(result$data_source) || result$data_source == "", "", "_"), "fbref_shooting")
      }
      # For shot-related metrics, prefer FBref shooting to Understat
      # e.g., shots, xG columns exist in shooting table (if needed, extract here)
      message("Checked fbref shooting table for ", player_name)
    }
  }
  
  # 7) Understat fallback for missing essential metrics (use only if still NA)
  if (season_start >= 2014 && (is.na(result$goals) || is.na(result$assists) || is.na(result$minutes))) {
    message("Understat fallback for ", season_start)
    us_url <- paste0("https://understat.com/team/Chelsea/", season_start)
    us_stats <- safe_call(worldfootballR::understat_team_players_stats, team_url = us_url, context = paste0("understat ", season_start), quiet = TRUE)
    if (!is.null(us_stats) && nrow(us_stats) > 0) {
      col <- "player_name"
      col_vals <- as.character(us_stats[[col]])
      col_norm <- vapply(col_vals, normalize_name, FUN.VALUE = character(1))
      hits <- which(str_detect(col_norm, fixed(target_norm)))
      if (length(hits) == 0) hits <- agrep(target_norm, col_norm, max.distance = 0.15)
      if (length(hits) > 0) {
        row <- us_stats[hits[1], , drop = FALSE]
        if (is.na(result$goals)) result$goals <- get_single_numeric(row, "goals?")
        if (is.na(result$assists)) result$assists <- get_single_numeric(row, "assists?")
        if (is.na(result$minutes)) {
          # understat's minutes column may be 'time' or 'minutes'
          mins <- get_single_numeric(row, "time|minutes")
          if (!is.na(mins)) result$minutes <- mins
          else {
            games <- get_single_numeric(row, "games|apps|appearances")
            if (!is.na(games) && games > 0) result$minutes <- games * 65
          }
        }
        result$data_source <- paste0(result$data_source, ifelse(is.na(result$data_source) || result$data_source == "", "", "_"), "understat")
        message("Matched (understat): ", as.character(row[[col]][1]))
      } else {
        message("No Understat match for ", player_name, " season ", season_start)
      }
    } else {
      message("understat returned no data for ", season_start)
    }
  }
  
  # final return row
  result
}

# ---- transfer fee collector (best-effort) ----
get_fee_and_inflate <- function(player_name, season_start, to_year = 2025) {
  message("Getting transfer fee for ", player_name, " (", season_start, ")...")
  out <- tibble::tibble(
    player = player_name,
    season_start = season_start,
    fee_2025_gbp = NA_real_,
    fee_currency = NA_character_,
    fee_raw_text = NA_character_
  )
  tm_urls <- safe_call(function() {
    if ("tm_league_team_urls" %in% ls(getNamespace("worldfootballR"))) {
      worldfootballR::tm_league_team_urls(country_name = "England", start_year = season_start)
    } else NULL
  }, context = paste0("tm_league_team_urls ", season_start), quiet = TRUE)
  if (is.null(tm_urls) || length(tm_urls) == 0) {
    message("tm_league_team_urls not available for ", season_start)
    return(out)
  }
  chelsea_idx <- which(str_detect(tolower(tm_urls), "chelsea"))
  if (length(chelsea_idx) == 0) {
    message("No Chelsea TM URL for season ", season_start)
    return(out)
  }
  chelsea_url <- tm_urls[chelsea_idx[1]]
  transfers <- safe_call(worldfootballR::tm_team_transfers, team_url = chelsea_url, transfer_window = "all",
                         context = paste0("tm_team_transfers ", season_start), quiet = TRUE)
  if (is.null(transfers) || nrow(transfers) == 0) return(out)
  player_cols <- names(transfers)[str_detect(names(transfers), regex("player|name", ignore_case = TRUE))]
  if (length(player_cols) == 0) player_cols <- names(transfers)[1]
  target_norm <- normalize_name(player_name)
  found_row <- NULL
  for (col in player_cols) {
    vals <- as.character(transfers[[col]])
    vals_norm <- vapply(vals, normalize_name, FUN.VALUE = character(1))
    hits <- which(str_detect(vals_norm, fixed(target_norm)))
    if (length(hits) == 0) hits <- agrep(target_norm, vals_norm, max.distance = 0.15)
    if (length(hits) > 0) {
      found_row <- transfers[hits[1], , drop = FALSE]
      break
    }
  }
  if (is.null(found_row)) {
    message("Player not found in Transfermarkt transfers for season ", season_start)
    return(out)
  }
  fee_cols <- names(found_row)[str_detect(names(found_row), regex("fee|transfer|value", ignore_case = TRUE))]
  fee_text <- NA_character_
  for (fc in fee_cols) {
    v <- as.character(found_row[[fc]][1])
    if (!is.na(v) && v != "") { fee_text <- v; break }
  }
  if (is.na(fee_text)) return(out)
  out$fee_raw_text <- fee_text
  parsed <- parse_tm_fee(fee_text)
  if (is.null(parsed)) return(out)
  out$fee_currency <- parsed$currency %||% NA_character_
  amount_gbp <- parsed$raw
  if (!is.null(parsed$currency) && !is.na(parsed$currency) && parsed$currency != "GBP") {
    if (requireNamespace("priceR", quietly = TRUE)) {
      try({
        if ("convert_currencies" %in% ls(getNamespace("priceR"))) {
          amount_gbp <- priceR::convert_currencies(parsed$raw, from = parsed$currency, to = "GBP", date = paste0(season_start, "-07-01"))
        }
      }, silent = TRUE)
    } else {
      message("priceR not present; skipping currency conversion")
    }
  }
  inflated <- amount_gbp
  if (requireNamespace("priceR", quietly = TRUE) && exists("adjust_for_inflation", where = asNamespace("priceR"), inherits = FALSE)) {
    try({
      inflated <- priceR::adjust_for_inflation(amount_gbp, from_date = season_start, country = "United Kingdom", to_date = to_year)
    }, silent = TRUE)
  } else {
    # leave as-is
  }
  out$fee_2025_gbp <- as.numeric(inflated)
  out
}

# ---------------------
# Run collection
# ---------------------
player_names <- names(player_debut_seasons)
season_starts <- as.integer(unname(player_debut_seasons))

message("Collecting player stats...")
all_stats <- purrr::map2_dfr(player_names, season_starts, ~ get_player_stats_robust(player_name = .x, season_start = .y))

message("\n=== COLLECTING TRANSFER FEES ===")
fee_data <- purrr::map2_dfr(player_names, season_starts, ~ get_fee_and_inflate(player_name = .x, season_start = .y))

# Join and per-90s
all_data <- all_stats %>%
  left_join(fee_data %>% select(player, season_start, fee_2025_gbp, fee_currency, fee_raw_text), by = c("player", "season_start")) %>%
  mutate(
    player = ifelse(player == "Joao Pedro", "João Pedro (25/26)", player),
    fee_2025_gbp_millions = ifelse(!is.na(fee_2025_gbp), fee_2025_gbp / 1e6, NA_real_)
  ) %>%
  mutate(
    goals_per90 = ifelse(!is.na(minutes) & minutes > 0, goals / minutes * 90, NA_real_),
    assists_per90 = ifelse(!is.na(minutes) & minutes > 0, assists / minutes * 90, NA_real_),
    prog_passes_per90 = ifelse(!is.na(minutes) & minutes > 0, prog_passes / minutes * 90, NA_real_),
    prog_carries_per90 = ifelse(!is.na(minutes) & minutes > 0, prog_carries / minutes * 90, NA_real_)
  )

# summary
message("\n=== DATA SUMMARY ===")
print(all_data %>%
        select(player, season_start, goals, assists, minutes, goals_per90, assists_per90, prog_passes_per90, prog_carries_per90, fee_2025_gbp_millions, data_source) %>%
        arrange(desc(season_start)))

message("Players with goal data:", sum(!is.na(all_data$goals)))
message("Players with fees:", sum(!is.na(all_data$fee_20
                                         