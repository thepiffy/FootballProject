# ============================================================
# FBref 429-safe fetch: throttle + exponential backoff + caching
# Paste once at top of script and use fbref_read_html(url) instead of read_html(url)
# ============================================================

suppressPackageStartupMessages({
  library(rvest)
  library(httr2)
  library(digest)
})

# Simple disk cache (so reruns don't re-hit FBref)
.fbref_cache_dir <- "fbref_cache"
if (!dir.exists(.fbref_cache_dir)) dir.create(.fbref_cache_dir, recursive = TRUE)

cache_path <- function(url) file.path(.fbref_cache_dir, paste0(digest(url, algo = "md5"), ".html"))

# Global minimum delay between requests (seconds)
.fbref_min_delay <- 12

# Call this before *every* network request
fbref_pause <- function(min_delay = .fbref_min_delay, jitter = 6) {
  Sys.sleep(min_delay + runif(1, 0, jitter))
}

# Robust HTML fetcher for FBref with:
# - browser-like headers
# - retry/backoff on 429
# - cache
fbref_read_html <- function(url,
                            max_tries = 8,
                            base_sleep = 15,
                            cache = TRUE,
                            force_refresh = FALSE) {

  stopifnot(is.character(url), length(url) == 1, nzchar(url))

  cp <- cache_path(url)
  if (cache && !force_refresh && file.exists(cp)) {
    return(read_html(cp))
  }

  ua <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/123 Safari/537.36"

  for (i in seq_len(max_tries)) {
    fbref_pause()

    req <- request(url) |>
      req_user_agent(ua) |>
      req_headers(
        "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
        "Accept-Language" = "en-US,en;q=0.9",
        "Connection" = "keep-alive",
        "Upgrade-Insecure-Requests" = "1"
      )

    resp <- tryCatch(req_perform(req), error = function(e) NULL)

    if (is.null(resp)) {
      # network error: backoff
      Sys.sleep(base_sleep * (2^(i-1)) + runif(1, 0, 3))
      next
    }

    status <- resp_status(resp)

    if (status == 200) {
      body <- resp_body_string(resp)
      if (cache) writeLines(body, cp, useBytes = TRUE)
      return(read_html(cp))
    }

    if (status == 429) {
      # Respect Retry-After if present
      ra <- resp_headers(resp)[["retry-after"]]
      wait <- suppressWarnings(as.numeric(ra))
      if (is.na(wait)) wait <- base_sleep * (2^(i-1))
      wait <- wait + runif(1, 0, 5)
      message(sprintf("[FBref] 429 rate limit. Backing off %.1fs (try %d/%d)", wait, i, max_tries))
      Sys.sleep(wait)
      next
    }

    # 403/other statuses: don't hammer. backoff and try again a few times, then fail.
    wait <- base_sleep * (2^(i-1)) + runif(1, 0, 5)
    message(sprintf("[FBref] HTTP %d. Waiting %.1fs (try %d/%d)", status, wait, i, max_tries))
    Sys.sleep(wait)
  }

  stop(sprintf("Failed to fetch %s after %d tries (likely rate-limited).", url, max_tries))
}

# ============================================================
# OPTIONAL: safer squad URL extraction directly from comp page
# (Use this if fb_teams_urls is unstable)
# ============================================================

fbref_comp_squad_urls <- function(comp_url) {
  page <- fbref_read_html(comp_url)
  hrefs <- page |> html_elements("a") |> html_attr("href")
  hrefs <- unique(hrefs[!is.na(hrefs)])
  squad <- hrefs[grepl("^/en/squads/", hrefs)]
  unique(paste0("https://fbref.com", squad))
}

# ============================================================
# Liverpool 24/25 vs 25/26 diagnostics (FBref via worldfootballR)
# One copy-paste script. Robust to your fork where URL functions return character vectors.
#
# KEY PATCHES vs your original:
# 1) Force season-specific squad URLs (fixes silent "wrong roster" pulls).
# 2) Bundle join uses player_url when present; falls back to name only if needed.
# 3) Avoid namespace collisions: use fbref_read_html() only for manual scraping.
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(stringr)
  library(tidyr)
  library(janitor)
  library(readr)
  library(tibble)
  library(worldfootballR)
})

# -----------------------------
# SETTINGS
# -----------------------------
season_then_end <- 2025  # 24/25
season_now_end  <- 2026  # 25/26
time_pause <- 4

# -----------------------------
# HELPERS
# -----------------------------
msg <- function(...) message(sprintf(...))
norm_name <- function(x) str_to_lower(str_squish(x))

safe_clean_names <- function(x) {
  if (is.null(x)) return(tibble())
  if (!inherits(x, "data.frame")) {
    x2 <- tryCatch(as.data.frame(x), error = function(e) NULL)
    if (is.null(x2)) return(tibble())
    x <- x2
  }
  if (is.null(colnames(x)) && is.null(dimnames(x))) return(tibble())
  if (nrow(x) == 0) return(tibble())
  janitor::clean_names(x)
}

safe_call_tbl <- function(expr, label = "call") {
  out <- tryCatch(expr, error = function(e) {
    msg("[WARN] %s failed: %s", label, e$message)
    NULL
  })
  safe_clean_names(out)
}

# -----------------------------
# SEASON URL NORMALIZER (critical)
# -----------------------------
season_str <- function(season_end_year) sprintf("%d-%d", season_end_year - 1L, season_end_year)

ensure_season_squad_url <- function(squad_url, season_end_year) {
  stopifnot(is.character(squad_url), length(squad_url) == 1, nzchar(squad_url))
  ss <- season_str(season_end_year)

  # Already season scoped: /en/squads/<id>/<yyyy-yyyy>/<yyyy-yyyy>-<Club>-Stats
  if (grepl(sprintf("/en/squads/[^/]+/%s/%s-", ss, ss), squad_url)) return(squad_url)

  # Base form: /en/squads/<id>/<Club>-Stats  -> inject season segment
  m <- stringr::str_match(squad_url, "^(https?://fbref\\.com/en/squads/[^/]+/)([^/]+)$")
  if (!is.na(m[1,2])) {
    base <- m[1,2]
    tail <- m[1,3]
    return(paste0(base, ss, "/", ss, "-", tail))
  }

  msg("[WARN] couldn't season-normalize squad URL: %s", squad_url)
  squad_url
}

# fb_league_urls() in your fork returns a single character URL
get_league_url <- function(country, season_end_year, tier = "1st", gender = "M") {
  raw <- tryCatch(
    fb_league_urls(country = country, gender = gender, season_end_year = season_end_year, tier = tier),
    error = function(e) NULL
  )
  if (is.character(raw) && length(raw) >= 1 && nzchar(raw[1])) return(raw[1])
  # fallback if another version returns a table
  lu <- safe_clean_names(raw)
  if (nrow(lu) == 0) stop("fb_league_urls() returned no usable URL.")
  url_col <- NA_character_
  for (nm in names(lu)) {
    v <- lu[[nm]]
    if (is.character(v) && any(grepl("fbref\\.com", v, ignore.case = TRUE), na.rm = TRUE)) { url_col <- nm; break }
  }
  if (is.na(url_col)) stop(sprintf("Could not find URL column. Columns: %s", paste(names(lu), collapse=", ")))
  lu[[url_col]][1]
}

# fb_teams_urls() in your fork returns character vector of squad URLs
get_team_squad_url <- function(league_url, team_name) {

  # ------------- helpers -------------
  normalize_to_base_comp_url <- function(u) {
    if (!is.character(u) || length(u) == 0 || is.na(u)) return(u)

    # already base form
    if (grepl("https?://fbref\\.com/en/comps/\\d+/[^/]+-Stats$", u)) return(u)

    # season form: /en/comps/<id>/<yyyy-yyyy>/<yyyy-yyyy>-<comp>-Stats
    m <- stringr::str_match(u, "https?://fbref\\.com/en/comps/(\\d+)/(\\d{4}-\\d{4})/\\2-(.+-Stats)$")
    if (!is.na(m[1,2])) {
      comp_id <- m[1,2]
      slug    <- m[1,4]
      return(sprintf("https://fbref.com/en/comps/%s/%s", comp_id, slug))
    }
    u
  }

  team_slug <- function(x) {
    x %>%
      stringr::str_squish() %>%
      stringr::str_replace_all("&", "and") %>%
      stringr::str_replace_all("'", "") %>%
      stringr::str_replace_all("\\.", "") %>%
      stringr::str_replace_all("\\s+", "-")
  }

  # handle FBref naming quirks
  team_map <- c(
    "Newcastle Utd"="Newcastle United",
    "Spurs"="Tottenham Hotspur",
    "Wolves"="Wolverhampton Wanderers"
  )
  team_norm <- if (team_name %in% names(team_map)) team_map[[team_name]] else team_name
  slug <- team_slug(team_norm)

  pick_from_urls <- function(urls) {
    if (!is.character(urls) || length(urls) == 0) return(NA_character_)
    hit <- urls[grepl(paste0("/", slug, "-Stats$"), urls, ignore.case = TRUE)]
    if (length(hit) == 0) {
      # fallback to partial slug match
      hit <- urls[grepl(slug, urls, ignore.case = TRUE)]
    }
    if (length(hit) == 0) return(NA_character_)
    hit[1]
  }

  # ------------- attempt 1: fb_teams_urls(season_or_base_url) -------------
  raw1 <- tryCatch(worldfootballR::fb_teams_urls(league_url), error = function(e) character())
  ans1 <- pick_from_urls(raw1)
  if (!is.na(ans1)) return(ans1)

  # ------------- attempt 2: fb_teams_urls(normalized base comp url) -------------
  base_url <- normalize_to_base_comp_url(league_url)
  raw2 <- tryCatch(worldfootballR::fb_teams_urls(base_url), error = function(e) character())
  ans2 <- pick_from_urls(raw2)
  if (!is.na(ans2)) return(ans2)

  # ------------- attempt 3: html scrape of the comp page to extract /en/squads/... links -------------
  page <- tryCatch(fbref_read_html(base_url), error = function(e) NULL)
  if (!is.null(page)) {
    links <- page %>%
      rvest::html_elements("a") %>%
      rvest::html_attr("href") %>%
      unique()

    squad_links <- links[grepl("^/en/squads/", links)]
    squad_urls  <- paste0("https://fbref.com", squad_links)

    ans3 <- pick_from_urls(squad_urls)
    if (!is.na(ans3)) return(ans3)
  }

  stop(sprintf(
    "Could not find squad URL for team '%s'.\nTried:\n- fb_teams_urls(%s)\n- fb_teams_urls(%s)\n- html scrape of %s\n\nTip: print fb_teams_urls(%s) to see what it returns right now.",
    team_name, league_url, base_url, base_url, base_url
  ))
}

# Get a player URL list from a squad page (schema varies)
get_player_url_from_squad <- function(squad_url, player_name) {
  raw <- tryCatch(fb_player_urls(squad_url), error = function(e) NULL)
  if (is.null(raw)) return(NA_character_)

  if (inherits(raw, "data.frame")) {
    pu <- safe_clean_names(raw)
    name_col <- intersect(names(pu), c("player","player_name","name"))[1]
    url_col  <- intersect(names(pu), c("player_url","url","href","player_href"))[1]
    if (is.na(name_col) || is.na(url_col)) return(NA_character_)
    row <- pu %>% filter(norm_name(.data[[name_col]]) == norm_name(player_name)) %>% slice(1)
    if (nrow(row)==0) return(NA_character_)
    return(row[[url_col]][1])
  }

  if (is.character(raw)) {
    msg("[WARN] fb_player_urls() returned character vector; name->URL matching not implemented for that mode.")
    return(NA_character_)
  }

  NA_character_
}

# Pull one season table for all players in a team
get_team_player_table <- function(team_url, stat_type) {
  safe_call_tbl(
    fb_team_player_stats(team_urls = team_url, stat_type = stat_type, time_pause = time_pause),
    label = sprintf("fb_team_player_stats(%s,%s)", team_url, stat_type)
  )
}

# --- Bundle join: use player_url when present; fallback to normalized name
infer_key <- function(df) {
  if ("player_url" %in% names(df) && any(nzchar(df$player_url), na.rm = TRUE)) return(df$player_url)
  if ("player" %in% names(df)) return(norm_name(df$player))
  rep(NA_character_, nrow(df))
}

get_team_bundle <- function(team_url, stat_types = c("standard","shooting","passing","defense","possession","playing_time","misc")) {
  tabs <- map(stat_types, ~ get_team_player_table(team_url, .x))
  names(tabs) <- stat_types

  pt <- tabs[["playing_time"]]
  if (nrow(pt) == 0) return(tibble())
  player_col <- intersect(names(pt), c("player"))[1]
  if (is.na(player_col)) return(tibble())

  min_col <- intersect(names(pt), c("min_playing_time","minutes","min"))[1]
  minutes <- if (is.na(min_col)) rep(NA_real_, nrow(pt)) else suppressWarnings(as.numeric(pt[[min_col]]))

  out <- pt %>%
    transmute(
      player = .data[[player_col]],
      player_url = if ("player_url" %in% names(pt)) .data[["player_url"]] else NA_character_,
      key = infer_key(pt),
      minutes = minutes,
      nineties = minutes / 90
    )

  for (st in setdiff(stat_types, "playing_time")) {
    df <- tabs[[st]]
    if (nrow(df) == 0) next
    pcol <- intersect(names(df), c("player"))[1]
    if (is.na(pcol)) next

    df2 <- df %>%
      mutate(key = infer_key(.)) %>%
      rename(player = .data[[pcol]]) %>%
      rename_with(~ paste0(st, "__", .x), -c(player, player_url, key))

    out <- out %>% left_join(df2, by = "key")
  }

  out %>% mutate(nineties = minutes / 90)
}

# =============================
# NA-REDUCTION PATCH
# =============================

find_player_row <- function(bundle, player_name, max_edit = 2) {
  if (is.null(bundle) || nrow(bundle) == 0) return(NULL)

  pn <- norm_name(player_name)
  players <- bundle$player
  pnorm <- norm_name(players)

  idx <- which(pnorm == pn)
  if (length(idx) >= 1) return(bundle[idx[1], , drop = FALSE])

  last <- tail(strsplit(pn, "\\s+")[[1]], 1)
  idx2 <- which(stringr::str_detect(pnorm, stringr::fixed(last)))
  if (length(idx2) >= 1) return(bundle[idx2[1], , drop = FALSE])

  d <- adist(pn, pnorm)
  j <- which.min(d)
  if (length(j) == 1 && is.finite(d[j]) && d[j] <= max_edit) return(bundle[j, , drop = FALSE])

  NULL
}

get_p90 <- function(row_df, prefix, candidates) {
  stopifnot(nrow(row_df) == 1)

  cols <- paste0(prefix, "__", candidates)
  hit <- cols[cols %in% names(row_df)][1]

  if (is.na(hit)) {
    if (prefix == "passing" && any(candidates %in% c("x_a_expected","xa","xag","x_ag"))) {
      hit2 <- c("passing__x_a_expected", "passing__x_ag", "standard__x_ag_expected", "standard__x_ag")[
        c("passing__x_a_expected", "passing__x_ag", "standard__x_ag_expected", "standard__x_ag") %in% names(row_df)
      ][1]
      if (!is.na(hit2)) hit <- hit2
    }

    if (prefix == "passing" && any(candidates %in% c("crs_pa","crs"))) {
      hit2 <- c("passing__crs_pa", "misc__crs")[c("passing__crs_pa","misc__crs") %in% names(row_df)][1]
      if (!is.na(hit2)) hit <- hit2
    }

    if (any(candidates %in% c("prg_p","prg_p_progression","prg_p_progression"))) {
      hit2 <- c("passing__prg_p", "standard__prg_p_progression")[c("passing__prg_p","standard__prg_p_progression") %in% names(row_df)][1]
      if (!is.na(hit2)) hit <- hit2
    }
    if (any(candidates %in% c("prg_c_carries","prg_c_progression"))) {
      hit2 <- c("possession__prg_c_carries", "standard__prg_c_progression")[c("possession__prg_c_carries","standard__prg_c_progression") %in% names(row_df)][1]
      if (!is.na(hit2)) hit <- hit2
    }
  }

  if (is.na(hit)) return(NA_real_)

  val <- suppressWarnings(as.numeric(row_df[[hit]]))
  n90 <- suppressWarnings(as.numeric(row_df$nineties))

  ifelse(is.na(n90) | n90 == 0, NA_real_, val / n90)
}

metrics_from_team_bundle <- function(bundle, player_name, metric_map) {
  row <- find_player_row(bundle, player_name, max_edit = 2)
  if (is.null(row) || nrow(row) == 0) {
    return(tibble::tibble(player = player_name, minutes = NA_real_, nineties = NA_real_))
  }

  out <- tibble::tibble(
    player   = row$player,
    minutes  = suppressWarnings(as.numeric(row$minutes)),
    nineties = suppressWarnings(as.numeric(row$nineties))
  )

  for (m in names(metric_map)) {
    spec <- metric_map[[m]]
    out[[m]] <- get_p90(row, spec$stat_type, spec$candidates)
  }

  out
}

# National team season stats
pull_player_season_stats <- function(player_url, stat_type, national = FALSE) {
  safe_call_tbl(
    fb_player_season_stats(player_url = player_url, stat_type = stat_type, national = national, time_pause = time_pause),
    label = sprintf("fb_player_season_stats(%s,%s,national=%s)", player_url, stat_type, national)
  )
}

get_national_metrics <- function(player_url, player_name, country_name, season_end_year, metric_map) {
  stat_types <- unique(c(map_chr(metric_map, ~.x$stat_type), "playing_time"))
  tabs <- map(stat_types, ~ pull_player_season_stats(player_url, .x, national = TRUE))
  names(tabs) <- stat_types

  pt <- tabs[["playing_time"]]
  if (nrow(pt)==0) return(tibble(player=player_name, minutes=NA_real_, nineties=NA_real_))

  season_col <- intersect(names(pt), c("season","season_end_year","season_end"))[1]
  squad_col  <- intersect(names(pt), c("squad","team"))[1]
  min_col    <- intersect(names(pt), c("minutes","min"))[1]
  if (is.na(season_col) || is.na(squad_col) || is.na(min_col)) {
    return(tibble(player=player_name, minutes=NA_real_, nineties=NA_real_))
  }

  pt2 <- pt %>%
    mutate(
      season_end = suppressWarnings(as.integer(str_extract(as.character(.data[[season_col]]), "\\d{4}$"))),
      squad_norm = norm_name(.data[[squad_col]]),
      minutes = suppressWarnings(as.numeric(.data[[min_col]]))
    ) %>%
    filter(season_end == season_end_year, squad_norm == norm_name(country_name)) %>%
    slice(1)

  if (nrow(pt2)==0) return(tibble(player=player_name, minutes=NA_real_, nineties=NA_real_))

  nineties <- pt2$minutes / 90
  out <- tibble(player = player_name, minutes = pt2$minutes, nineties = nineties)

  for (m in names(metric_map)) {
    spec <- metric_map[[m]]
    df <- tabs[[spec$stat_type]]
    if (nrow(df)==0) { out[[m]] <- NA_real_; next }

    season_col2 <- intersect(names(df), c("season","season_end_year","season_end"))[1]
    squad_col2  <- intersect(names(df), c("squad","team"))[1]
    if (is.na(season_col2) || is.na(squad_col2)) { out[[m]] <- NA_real_; next }

    df2 <- df %>%
      mutate(
        season_end = suppressWarnings(as.integer(str_extract(as.character(.data[[season_col2]]), "\\d{4}$"))),
        squad_norm = norm_name(.data[[squad_col2]])
      ) %>%
      filter(season_end == season_end_year, squad_norm == norm_name(country_name)) %>%
      slice(1)

    if (nrow(df2)==0) { out[[m]] <- NA_real_; next }

    hit <- spec$candidates[spec$candidates %in% names(df2)][1]
    if (is.na(hit)) { out[[m]] <- NA_real_; next }

    val <- suppressWarnings(as.numeric(df2[[hit]]))
    out[[m]] <- ifelse(is.na(nineties) | nineties == 0, NA_real_, val / nineties)
  }

  out
}

# -----------------------------
# METRIC MAPS (unchanged from your draft)
# -----------------------------
metric_salah <- list(
  goals_p90        = list(stat_type = "standard",   candidates = c("gls")),
  assists_p90      = list(stat_type = "standard",   candidates = c("ast")),
  xg_p90           = list(stat_type = "standard",   candidates = c("x_g_expected")),
  xa_p90           = list(stat_type = "passing",    candidates = c("x_a_expected")),
  tackles_p90      = list(stat_type = "defense",    candidates = c("tkl_tackles")),
  dribbles_p90     = list(stat_type = "possession", candidates = c("succ_take_ons")),
  prog_passes_p90  = list(stat_type = "passing",    candidates = c("prg_p")),
  prog_carries_p90 = list(stat_type = "possession", candidates = c("prg_c_carries"))
)

metric_macca <- list(
  assists_p90      = list(stat_type = "standard",   candidates = c("ast")),
  xa_p90           = list(stat_type = "passing",    candidates = c("x_a_expected")),
  dribbles_p90     = list(stat_type = "possession", candidates = c("succ_take_ons")),
  prog_passes_p90  = list(stat_type = "passing",    candidates = c("prg_p")),
  prog_carries_p90 = list(stat_type = "possession", candidates = c("prg_c_carries")),
  interceptions_p90= list(stat_type = "defense",    candidates = c("int")),
  tackles_p90      = list(stat_type = "defense",    candidates = c("tkl_tackles")),
  distance_travelled_p90 = list(stat_type = "passing", candidates = c("tot_dist_total"))
)

metric_vvd <- list(
  tackles_p90        = list(stat_type = "defense", candidates = c("tkl_tackles")),
  successful_tackles_p90 = list(stat_type = "defense", candidates = c("tkl_w_tackles")),
  interceptions_p90  = list(stat_type = "defense", candidates = c("int")),
  successful_interceptions_p90 = list(stat_type = "defense", candidates = c("int")),
  blocks_p90         = list(stat_type = "defense", candidates = c("blocks_blocks")),
  successful_blocks_p90 = list(stat_type = "defense", candidates = c("blocks_blocks"))
)

metric_wirtz <- list(
  assists_p90      = list(stat_type = "standard",   candidates = c("ast")),
  goals_p90        = list(stat_type = "standard",   candidates = c("gls")),
  prog_passes_p90  = list(stat_type = "passing",    candidates = c("prg_p")),
  prog_carries_p90 = list(stat_type = "possession", candidates = c("prg_c_carries")),
  xa_p90           = list(stat_type = "passing",    candidates = c("x_a_expected")),
  xg_p90           = list(stat_type = "standard",   candidates = c("x_g_expected"))
)

metric_frimpong <- list(
  assists_p90      = list(stat_type = "standard", candidates = c("ast")),
  tackles_p90      = list(stat_type = "defense",  candidates = c("tkl_tackles")),
  tackles_won_p90  = list(stat_type = "defense",  candidates = c("tkl_w_tackles")),
  crosses_p90      = list(stat_type = "passing",  candidates = c("crs_pa")),
  xa_p90           = list(stat_type = "passing",  candidates = c("x_a_expected"))
)

metric_kerkez <- metric_frimpong

metric_isak <- list(
  goals_p90               = list(stat_type = "standard",   candidates = c("gls")),
  xg_p90                  = list(stat_type = "standard",   candidates = c("x_g_expected")),
  progressive_assists_p90 = list(stat_type = "passing",    candidates = c("kp")),
  shots_p90               = list(stat_type = "shooting",   candidates = c("sh_standard")),
  shots_on_target_p90     = list(stat_type = "shooting",   candidates = c("so_t_standard")),
  touches_in_box_p90      = list(stat_type = "possession", candidates = c("att_pen_touches"))
)

metric_ekitike <- metric_isak

# -----------------------------
# Aerial duels proxy helpers
# -----------------------------
add_aerial_duels <- function(bundle, player_name) {
  row <- bundle %>% dplyr::filter(norm_name(player) == norm_name(player_name)) %>% dplyr::slice(1)
  if (nrow(row) == 0) return(list(duels_p90 = NA_real_, duels_won_p90 = NA_real_, duels_lost_p90 = NA_real_))

  aw <- get_p90(row, "misc", c("won_aerial_duels"))
  al <- get_p90(row, "misc", c("lost_aerial_duels"))

  list(
    duels_won_p90  = aw,
    duels_lost_p90 = al,
    duels_p90      = ifelse(is.na(aw) & is.na(al), NA_real_, dplyr::coalesce(aw, 0) + dplyr::coalesce(al, 0))
  )
}

# -----------------------------
# TEAM URLS FOR REQUIRED SEASONS
# -----------------------------
league_epl_then <- get_league_url("ENG", season_then_end, tier="1st")
league_epl_now  <- get_league_url("ENG", season_now_end,  tier="1st")
league_bun_then <- get_league_url("GER", season_then_end, tier="1st")
league_bun_now  <- get_league_url("GER", season_now_end,  tier="1st")

squad_liv_then <- ensure_season_squad_url(get_team_squad_url(league_epl_then, "Liverpool"), season_then_end)
squad_liv_now  <- ensure_season_squad_url(get_team_squad_url(league_epl_now,  "Liverpool"), season_now_end)

squad_b04_then <- ensure_season_squad_url(get_team_squad_url(league_bun_then, "Bayer Leverkusen"), season_then_end)
squad_bou_then <- ensure_season_squad_url(get_team_squad_url(league_epl_then, "Bournemouth"), season_then_end)
squad_new_then <- ensure_season_squad_url(get_team_squad_url(league_epl_then, "Newcastle Utd"), season_then_end)
squad_sge_then <- ensure_season_squad_url(get_team_squad_url(league_bun_then, "Eintracht Frankfurt"), season_then_end)

# -----------------------------
# BUILD TEAM BUNDLES (club seasons)
# -----------------------------
club_stat_types <- c("standard","shooting","passing","defense","possession","playing_time","misc")

bundle_liv_then <- get_team_bundle(squad_liv_then, stat_types = club_stat_types)
bundle_liv_now  <- get_team_bundle(squad_liv_now,  stat_types = club_stat_types)

bundle_b04_then <- get_team_bundle(squad_b04_then, stat_types = club_stat_types)
bundle_bou_then <- get_team_bundle(squad_bou_then, stat_types = club_stat_types)
bundle_new_then <- get_team_bundle(squad_new_then, stat_types = club_stat_types)
bundle_sge_then <- get_team_bundle(squad_sge_then, stat_types = club_stat_types)

# -----------------------------
# PLAYER URLS (only needed for national-team pulls)
# -----------------------------
get_liv_player_url <- function(player_name) get_player_url_from_squad(squad_liv_now, player_name)

player_urls_for_nationals <- c(
  "Mohamed Salah" = get_liv_player_url("Mohamed Salah"),
  "Alexis Mac Allister" = get_liv_player_url("Alexis Mac Allister"),
  "Virgil van Dijk" = get_liv_player_url("Virgil van Dijk"),
  "Florian Wirtz" = get_liv_player_url("Florian Wirtz"),
  "Jeremie Frimpong" = get_liv_player_url("Jeremie Frimpong"),
  "Milos Kerkez" = get_liv_player_url("Milos Kerkez"),
  "Alexander Isak" = get_liv_player_url("Alexander Isak"),
  "Hugo Ekitike" = get_liv_player_url("Hugo Ekitike")
)

player_nations <- c(
  "Mohamed Salah"="Egypt",
  "Alexis Mac Allister"="Argentina",
  "Virgil van Dijk"="Netherlands",
  "Florian Wirtz"="Germany",
  "Jeremie Frimpong"="Netherlands",
  "Milos Kerkez"="Hungary",
  "Alexander Isak"="Sweden",
  "Hugo Ekitike"="France"
)

# -----------------------------
# LEVEL 1: Liverpool incumbents (24/25 vs 25/26) per 90
# -----------------------------
inc_then <- bind_rows(
  metrics_from_team_bundle(bundle_liv_then, "Mohamed Salah", metric_salah) %>% mutate(context="club", squad="Liverpool", season_end_year=season_then_end),
  metrics_from_team_bundle(bundle_liv_then, "Alexis Mac Allister", metric_macca) %>% mutate(context="club", squad="Liverpool", season_end_year=season_then_end),
  metrics_from_team_bundle(bundle_liv_then, "Virgil van Dijk", metric_vvd) %>% mutate(context="club", squad="Liverpool", season_end_year=season_then_end)
)

vvd_duels_then <- add_aerial_duels(bundle_liv_then, "Virgil van Dijk")
inc_then <- inc_then %>%
  mutate(
    duels_p90 = ifelse(player=="Virgil van Dijk", vvd_duels_then$duels_p90, NA_real_),
    duels_won_p90 = ifelse(player=="Virgil van Dijk", vvd_duels_then$duels_won_p90, NA_real_),
    duels_lost_p90 = ifelse(player=="Virgil van Dijk", vvd_duels_then$duels_lost_p90, NA_real_)
  )

inc_now <- bind_rows(
  metrics_from_team_bundle(bundle_liv_now, "Mohamed Salah", metric_salah) %>% mutate(context="club", squad="Liverpool", season_end_year=season_now_end),
  metrics_from_team_bundle(bundle_liv_now, "Alexis Mac Allister", metric_macca) %>% mutate(context="club", squad="Liverpool", season_end_year=season_now_end),
  metrics_from_team_bundle(bundle_liv_now, "Virgil van Dijk", metric_vvd) %>% mutate(context="club", squad="Liverpool", season_end_year=season_now_end)
)

vvd_duels_now <- add_aerial_duels(bundle_liv_now, "Virgil van Dijk")
inc_now <- inc_now %>%
  mutate(
    duels_p90 = ifelse(player=="Virgil van Dijk", vvd_duels_now$duels_p90, NA_real_),
    duels_won_p90 = ifelse(player=="Virgil van Dijk", vvd_duels_now$duels_won_p90, NA_real_),
    duels_lost_p90 = ifelse(player=="Virgil van Dijk", vvd_duels_now$duels_lost_p90, NA_real_)
  )

incumbents_comp <- inc_then %>%
  select(-any_of(c("minutes","nineties","context","squad","season_end_year"))) %>%
  rename_with(~ paste0(.x, "_2425"), -player) %>%
  full_join(
    inc_now %>%
      select(-any_of(c("minutes","nineties","context","squad","season_end_year"))) %>%
      rename_with(~ paste0(.x, "_2526"), -player),
    by="player"
  ) %>%
  mutate(section="1) Incumbents: Liverpool 24/25 vs 25/26") %>%
  select(section, player, everything())

# -----------------------------
# LEVEL 2: New signings (24/25 previous club vs 25/26 Liverpool) per 90
# -----------------------------
sign_then <- bind_rows(
  metrics_from_team_bundle(bundle_b04_then, "Florian Wirtz", metric_wirtz) %>% mutate(context="club", squad="Bayer Leverkusen", season_end_year=season_then_end),
  metrics_from_team_bundle(bundle_b04_then, "Jeremie Frimpong", metric_frimpong) %>% mutate(context="club", squad="Bayer Leverkusen", season_end_year=season_then_end),
  metrics_from_team_bundle(bundle_bou_then, "Milos Kerkez", metric_kerkez) %>% mutate(context="club", squad="Bournemouth", season_end_year=season_then_end),
  metrics_from_team_bundle(bundle_new_then, "Alexander Isak", metric_isak) %>% mutate(context="club", squad="Newcastle Utd", season_end_year=season_then_end),
  metrics_from_team_bundle(bundle_sge_then, "Hugo Ekitike", metric_ekitike) %>% mutate(context="club", squad="Eintracht Frankfurt", season_end_year=season_then_end)
)

frimp_duels_then <- add_aerial_duels(bundle_b04_then, "Jeremie Frimpong")
kerkez_duels_then <- add_aerial_duels(bundle_bou_then, "Milos Kerkez")
sign_then <- sign_then %>%
  mutate(
    duels_p90 = case_when(
      player=="Jeremie Frimpong" ~ frimp_duels_then$duels_p90,
      player=="Milos Kerkez" ~ kerkez_duels_then$duels_p90,
      TRUE ~ NA_real_
    ),
    duels_won_p90 = case_when(
      player=="Jeremie Frimpong" ~ frimp_duels_then$duels_won_p90,
      player=="Milos Kerkez" ~ kerkez_duels_then$duels_won_p90,
      TRUE ~ NA_real_
    ),
    duels_lost_p90 = case_when(
      player=="Jeremie Frimpong" ~ frimp_duels_then$duels_lost_p90,
      player=="Milos Kerkez" ~ kerkez_duels_then$duels_lost_p90,
      TRUE ~ NA_real_
    )
  )

sign_now <- bind_rows(
  metrics_from_team_bundle(bundle_liv_now, "Florian Wirtz", metric_wirtz) %>% mutate(context="club", squad="Liverpool", season_end_year=season_now_end),
  metrics_from_team_bundle(bundle_liv_now, "Jeremie Frimpong", metric_frimpong) %>% mutate(context="club", squad="Liverpool", season_end_year=season_now_end),
  metrics_from_team_bundle(bundle_liv_now, "Milos Kerkez", metric_kerkez) %>% mutate(context="club", squad="Liverpool", season_end_year=season_now_end),
  metrics_from_team_bundle(bundle_liv_now, "Alexander Isak", metric_isak) %>% mutate(context="club", squad="Liverpool", season_end_year=season_now_end),
  metrics_from_team_bundle(bundle_liv_now, "Hugo Ekitike", metric_ekitike) %>% mutate(context="club", squad="Liverpool", season_end_year=season_now_end)
)

frimp_duels_now <- add_aerial_duels(bundle_liv_now, "Jeremie Frimpong")
kerkez_duels_now <- add_aerial_duels(bundle_liv_now, "Milos Kerkez")
sign_now <- sign_now %>%
  mutate(
    duels_p90 = case_when(
      player=="Jeremie Frimpong" ~ frimp_duels_now$duels_p90,
      player=="Milos Kerkez" ~ kerkez_duels_now$duels_p90,
      TRUE ~ NA_real_
    ),
    duels_won_p90 = case_when(
      player=="Jeremie Frimpong" ~ frimp_duels_now$duels_won_p90,
      player=="Milos Kerkez" ~ kerkez_duels_now$duels_won_p90,
      TRUE ~ NA_real_
    ),
    duels_lost_p90 = case_when(
      player=="Jeremie Frimpong" ~ frimp_duels_now$duels_lost_p90,
      player=="Milos Kerkez" ~ kerkez_duels_now$duels_lost_p90,
      TRUE ~ NA_real_
    )
  )

signings_comp <- sign_then %>%
  select(-any_of(c("minutes","nineties","context","squad","season_end_year"))) %>%
  rename_with(~ paste0(.x, "_2425_prevclub"), -player) %>%
  full_join(
    sign_now %>%
      select(-any_of(c("minutes","nineties","context","squad","season_end_year"))) %>%
      rename_with(~ paste0(.x, "_2526_liverpool"), -player),
    by="player"
  ) %>%
  mutate(section="2) New signings: 24/25 previous club vs 25/26 Liverpool") %>%
  select(section, player, everything())

# -----------------------------
# LEVEL 3: System test — 25/26 Liverpool vs 25/26 National Team
# -----------------------------
player_metric_map <- list(
  "Mohamed Salah" = metric_salah,
  "Alexis Mac Allister" = metric_macca,
  "Virgil van Dijk" = metric_vvd,
  "Florian Wirtz" = metric_wirtz,
  "Jeremie Frimpong" = metric_frimpong,
  "Milos Kerkez" = metric_kerkez,
  "Alexander Isak" = metric_isak,
  "Hugo Ekitike" = metric_ekitike
)

system_rows <- imap_dfr(player_metric_map, function(mmap, pname) {
  club_row <- metrics_from_team_bundle(bundle_liv_now, pname, mmap) %>%
    mutate(context="club", squad="Liverpool", season_end_year=season_now_end)

  if (pname %in% c("Virgil van Dijk","Jeremie Frimpong","Milos Kerkez")) {
    dd <- add_aerial_duels(bundle_liv_now, pname)
    club_row <- club_row %>%
      mutate(duels_p90=dd$duels_p90, duels_won_p90=dd$duels_won_p90, duels_lost_p90=dd$duels_lost_p90)
  }

  purl <- unname(player_urls_for_nationals[[pname]])
  nat  <- unname(player_nations[[pname]])
  nat_row <- if (is.na(purl) || is.na(nat)) {
    tibble(player=pname, minutes=NA_real_, nineties=NA_real_)
  } else {
    get_national_metrics(purl, pname, nat, season_now_end, mmap)
  }
  nat_row <- nat_row %>% mutate(context="national", squad=nat, season_end_year=season_now_end)

  club_row %>%
    select(-any_of(c("nineties"))) %>%
    rename_with(~ paste0(.x, "_liverpool_2526"), -player) %>%
    full_join(
      nat_row %>%
        select(-any_of(c("nineties"))) %>%
        rename_with(~ paste0(.x, "_national_2526"), -player),
      by="player"
    ) %>%
    mutate(section="3) System test: 25/26 Liverpool vs 25/26 National Team") %>%
    select(section, player, everything())
})

# -----------------------------
# OUTPUTS
# -----------------------------
all_outputs <- bind_rows(incumbents_comp, signings_comp, system_rows)

# =========================
# DIAGNOSTIC: what columns did FBref actually return?
# =========================
diag_team_tables <- function(team_url, label) {
  stat_types <- c("standard","shooting","passing","defense","possession","playing_time","misc")
  cat("\n\n====================\n", label, "\n", team_url, "\n====================\n")

  for (st in stat_types) {
    df <- tryCatch(
      janitor::clean_names(worldfootballR::fb_team_player_stats(team_urls = team_url, stat_type = st, time_pause = time_pause)),
      error = function(e) tibble::tibble()
    )

    cat("\n---", st, "---\n")
    cat("rows:", nrow(df), " cols:", ncol(df), "\n")
    if (nrow(df) > 0) {
      print(head(names(df), 60))
      pcol <- intersect(names(df), c("player"))[1]
      if (!is.na(pcol)) print(df %>% dplyr::select(dplyr::all_of(pcol)) %>% head(10))
    } else {
      cat("EMPTY TABLE\n")
    }
  }
}

diag_team_tables(squad_liv_then, "Liverpool 24/25")
diag_team_tables(squad_liv_now,  "Liverpool 25/26")

write_csv(incumbents_comp, "liverpool_level1_incumbents_2425_vs_2526.csv")
write_csv(signings_comp, "liverpool_level2_signings_prevclub_2425_vs_liverpool_2526.csv")
write_csv(system_rows, "liverpool_level3_system_test_liverpool_vs_national_2526.csv")
write_csv(all_outputs, "liverpool_all_levels_output.csv")

# ============================================================
# CLEAN 2-BAR PER PLAYER GRAPHS (24/25 vs 25/26 Liverpool)
# One facet per player, two bars per metric.
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(scales)
})

norm_name <- function(x) stringr::str_to_lower(stringr::str_squish(x))

theme_article <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title.position = "plot",
      plot.title = element_text(face="bold"),
      plot.subtitle = element_text(margin = margin(b=8)),
      panel.grid.minor = element_blank(),
      legend.position = "top",
      legend.title = element_blank(),
      strip.text = element_text(face="bold")
    )
}

save_plot <- function(p, filename, w=12, h=7, dpi=320) {
  ggsave(filename, plot=p, width=w, height=h, dpi=dpi)
  invisible(filename)
}

# ------------------------------------------------------------
# Build tidy 2-bar data for ONE metric:
# - 24/25 from incumbents_comp (or signings_comp prevclub)
# - 25/26 Liverpool from system_rows
# Robust to missing columns (won't crash)
# ------------------------------------------------------------
build_metric_2bar <- function(metric_base, players = NULL) {
  
  inc_col <- paste0(metric_base, "_2425")
  sig_col <- paste0(metric_base, "_2425_prevclub")
  liv_col <- paste0(metric_base, "_liverpool_2526")
  
  inc_2425 <- incumbents_comp %>%
    select(player, any_of(inc_col)) %>%
    mutate(value_2425 = if (inc_col %in% names(.)) .data[[inc_col]] else NA_real_) %>%
    select(player, value_2425) %>%
    mutate(src="inc")
  
  sig_2425 <- signings_comp %>%
    select(player, any_of(sig_col)) %>%
    mutate(value_2425 = if (sig_col %in% names(.)) .data[[sig_col]] else NA_real_) %>%
    select(player, value_2425) %>%
    mutate(src="sig")
  
  s2425 <- bind_rows(inc_2425, sig_2425) %>%
    group_by(player) %>%
    summarise(
      value_2425 = dplyr::coalesce(
        value_2425[src=="inc"][1],
        value_2425[src=="sig"][1]
      ),
      .groups="drop"
    )
  
  s2526 <- system_rows %>%
    select(player, any_of(liv_col)) %>%
    mutate(value_liv = if (liv_col %in% names(.)) suppressWarnings(as.numeric(.data[[liv_col]])) else NA_real_) %>%
    select(player, value_liv)
  
  out <- s2425 %>%
    full_join(s2526, by="player") %>%
    mutate(player = as.character(player))
  
  if (!is.null(players)) {
    out <- out %>% filter(norm_name(player) %in% norm_name(players))
  }
  
  out_long <- out %>%
    transmute(
      player,
      `24/25` = suppressWarnings(as.numeric(value_2425)),
      `25/26 Liverpool` = suppressWarnings(as.numeric(value_liv))
    ) %>%
    pivot_longer(-player, names_to="season", values_to="value") %>%
    mutate(season = factor(season, levels=c("24/25","25/26 Liverpool")))
  
  out_long
}

# ------------------------------------------------------------
# Plot one metric: 2 bars per player, faceted
# ------------------------------------------------------------
plot_metric_2bar <- function(metric_base,
                             title,
                             subtitle = "24/25 vs 25/26 Liverpool",
                             ylab = NULL,
                             players = NULL,
                             facet_cols = 2,
                             drop_missing_players = TRUE,
                             annotate_values = TRUE) {
  
  df <- build_metric_2bar(metric_base = metric_base, players = players)
  
  if (drop_missing_players) {
    keep_players <- df %>%
      group_by(player) %>%
      summarise(any_data = any(!is.na(value)), .groups="drop") %>%
      filter(any_data) %>%
      pull(player)
    df <- df %>% filter(player %in% keep_players)
  }
  
  if (nrow(df) == 0) {
    return(ggplot() + labs(title=title, subtitle="No data available.") + theme_article())
  }
  
  p <- ggplot(df, aes(x=season, y=value, fill=season)) +
    geom_col(width=0.7, na.rm=TRUE) +
    facet_wrap(~ player, ncol = facet_cols, scales = "free_y") +
    scale_y_continuous(labels=number_format(accuracy=0.01), expand=expansion(mult=c(0,0.12))) +
    labs(title=title, subtitle=subtitle, x=NULL, y=ylab) +
    theme_article() +
    theme(
      axis.text.x = element_text(angle=15, hjust=1),
      legend.position = "none"
    )
  
  if (annotate_values) {
    p <- p +
      geom_text(
        aes(label = ifelse(is.na(value), "", sprintf("%.2f", value))),
        vjust = -0.35,
        size = 3.2,
        na.rm = TRUE
      )
  }
  
  p
}

# ------------------------------------------------------------
# Plot ALL metrics in a pack (returns list of ggplots)
# It will SKIP metrics that don't exist in both contexts.
# ------------------------------------------------------------
plot_all_metrics_pack <- function(players,
                                  metrics_named,
                                  facet_cols = 2) {
  
  plots <- list()
  
  for (nm in names(metrics_named)) {
    metric_base <- metrics_named[[nm]]
    
    # sanity: must exist in at least one 24/25 source AND in 25/26 liverpool
    has_2425 <- (paste0(metric_base, "_2425") %in% names(incumbents_comp)) ||
      (paste0(metric_base, "_2425_prevclub") %in% names(signings_comp))
    has_2526 <- (paste0(metric_base, "_liverpool_2526") %in% names(system_rows))
    
    if (!has_2425 || !has_2526) {
      message(sprintf("[SKIP] %-28s -> %s (missing 24/25=%s or 25/26 Liverpool=%s)",
                      nm, metric_base, has_2425, has_2526))
      next
    }
    
    plots[[nm]] <- plot_metric_2bar(
      metric_base = metric_base,
      title = nm,
      subtitle = "24/25 vs 25/26 Liverpool",
      ylab = nm,
      players = players,
      facet_cols = facet_cols,
      drop_missing_players = TRUE,
      annotate_values = TRUE
    )
  }
  
  plots
}

# ============================================================
# RUN: generate graphs for all metrics
# ============================================================

players_all <- c(
  "Mohamed Salah","Alexis Mac Allister","Virgil van Dijk",
  "Florian Wirtz","Jeremie Frimpong","Milos Kerkez",
  "Alexander Isak","Hugo Ekitike"
)

metrics_pack <- list(
  "Goals / 90"               = "goals_p90",
  "Assists / 90"             = "assists_p90",
  "xG / 90"                  = "xg_p90",
  "xA / 90"                  = "xa_p90",
  "Progressive passes / 90"  = "prog_passes_p90",
  "Progressive carries / 90" = "prog_carries_p90",
  "Tackles / 90"             = "tackles_p90",
  "Interceptions / 90"       = "interceptions_p90",
  "Blocks / 90"              = "blocks_p90",
  "Crosses / 90"             = "crosses_p90",
  "Shots / 90"               = "shots_p90",
  "Touches in box / 90"      = "touches_in_box_p90",
  "Key passes (KP) / 90"     = "progressive_assists_p90",  # your proxy
  "Aerial duels / 90"        = "duels_p90",
  "Aerials won / 90"         = "duels_won_p90",
  "Aerials lost / 90"        = "duels_lost_p90"
)

# Print a batch of metric plots in order

plot_batch <- function(players, metrics_named, facet_cols = 2, ncol = 1) {
  plots <- lapply(names(metrics_named), function(nm) {
    metric_base <- metrics_named[[nm]]
    plot_metric_2bar(
      metric_base = metric_base,
      title = nm,
      subtitle = "24/25 vs 25/26 Liverpool",
      ylab = nm,
      players = players,
      facet_cols = facet_cols,
      drop_missing_players = TRUE,
      annotate_values = TRUE
    )
  })
  patchwork::wrap_plots(plots, ncol = ncol)
}

players_all <- c(
  "Mohamed Salah","Alexis Mac Allister","Virgil van Dijk",
  "Florian Wirtz","Jeremie Frimpong","Milos Kerkez",
  "Alexander Isak","Hugo Ekitike"
)


# These six lines are to be run separately and produce six separate plots

plot_batch(players_all, list("Goals / 90"="goals_p90", "Assists / 90"="assists_p90"), facet_cols = 2)

plot_batch(players_all, list("xG / 90"="xg_p90", "xA / 90"="xa_p90"), facet_cols = 2)

plot_batch(players_all, list("Progressive passes / 90"="prog_passes_p90", "Progressive carries / 90"="prog_carries_p90", "Key passes (KP) / 90"="progressive_assists_p90"), facet_cols = 2)




message("[OK] Built 2-bar per player charts for all available metrics.")


msg("\nPROXIES / LIMITATIONS (don’t lie to yourself):")
msg("- 'progressive_assists' is proxied with Key Passes (KP). FBref doesn't provide progressive assists as a season stat.")
msg("- 'duels' is proxied with aerial duels (aerials won/lost).")
msg("- 'successful interceptions/blocks' aren't separate stats; mapped to base interceptions/blocks.")
msg("- International season tables may be missing/partial on FBref => expect NA for some players.")
