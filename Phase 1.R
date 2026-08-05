# ===========================================================================
# FPL MINUTES MODEL — PHASE 1: ASSEMBLE, AUDIT, AND BUILD THE TRUE CALENDAR
# ---------------------------------------------------------------------------
# HOW TO USE
#   Run CHECKPOINT 0 every session, then one checkpoint at a time. Each ends
#   by printing a bounded diagnostic between:
#       vvvvv PASTE FROM HERE vvvvv   ...   ^^^^^ PASTE TO HERE ^^^^^
#
#   Every checkpoint reloads its input from cache/, so all of them are safe to
#   re-run after an edit. Nothing mutates its own input in place.
#
# WHAT CHANGED FROM THE FIRST DRAFT (all found by running it)
#   CP1  Assistant Manager elements (2024-25) are not players — removed.
#        FPL's goalkeeper code is "GKP" in 101 rows of 2021-22; normalised
#        BEFORE filtering, or real goalkeepers get silently deleted.
#   CP2  2019-20 has 47 gameweeks, not 38 — COVID blanked GW31-38 and nine new
#        gameweeks were created for the restart. 2022-23 has no GW7 (cancelled
#        after the death of Queen Elizabeth II).
#   CP3  Duplicate player-fixture rows are POSTPONEMENT ARTEFACTS: a 0-minute
#        placeholder at the original date plus the real row at the replayed
#        date, sharing a fixture id. Keeping the first row kept the placeholder
#        32 times out of 32. Now keeps the latest kickoff.
#   CP4  NEW AND LOAD-BEARING. Rest days computed from FPL data alone are
#        Premier-League-to-Premier-League and miss every European and cup
#        match. 22.4% of PL matches have a hidden fixture in the gap, and rest
#        is overstated by 1.1 days on average. At identical measured rest of
#        6-8 days, clubs with a hidden fixture make 2.36 XI changes against
#        1.90 without, and heavy rotation triples (12.6% vs 4.1%).
#
# CALENDAR COVERAGE (worldfootballR_data froze on 2025-01-09)
#   complete : 2020-21 .. 2023-24
#   partial  : 2024-25 (spring knockouts missing)
#   absent   : 2025-26, and 2019-20 has no `team` column to join on
# ===========================================================================


# ===========================================================================
# CHECKPOINT 0 — SETUP  (run every session)
# ===========================================================================

pkgs <- c("dplyr", "tidyr", "purrr", "readr", "stringr", "stringi",
          "lubridate", "worldfootballR")
missing <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(missing)) install.packages(missing)
invisible(lapply(pkgs, library, character.only = TRUE))

ARCHIVE_BASE <- "https://raw.githubusercontent.com/vaastav/Fantasy-Premier-League/master/data/"

SEASONS <- c("2019-20", "2020-21", "2021-22", "2022-23",
             "2023-24", "2024-25", "2025-26")

EXPECTED_GWS <- c("2019-20" = 47, "2020-21" = 38, "2021-22" = 38,
                  "2022-23" = 38, "2023-24" = 38, "2024-25" = 38,
                  "2025-26" = 38)

# Seasons where the non-league calendar is complete enough to trust.
CALENDAR_SEASONS <- c("2020-21", "2021-22", "2022-23", "2023-24")

VALID_POSITIONS <- c("GK", "DEF", "MID", "FWD")

for (d in c("data", "audit", "cache")) dir.create(d, showWarnings = FALSE)


# --- Reporting helpers ------------------------------------------------------

cp_open <- function(n, title) {
  cat("\nvvvvv PASTE FROM HERE vvvvv\n")
  cat("### CHECKPOINT ", n, " — ", title, "\n", sep = "")
  cat("### R ", paste(R.version$major, R.version$minor, sep = "."),
      " | ", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n", sep = "")
}
cp_close   <- function() cat("^^^^^ PASTE TO HERE ^^^^^\n\n")
cp_section <- function(t) cat("\n-- ", t, " --\n", sep = "")

cp_table <- function(df, max_rows = 20, digits = 3) {
  df <- as.data.frame(df)
  if (nrow(df) == 0) { cat("(no rows)\n"); return(invisible()) }
  num <- vapply(df, is.numeric, logical(1))
  df[num] <- lapply(df[num], function(x) round(x, digits))
  print(utils::head(df, max_rows), row.names = FALSE)
  if (nrow(df) > max_rows) cat("... ", nrow(df) - max_rows, " more rows\n", sep = "")
  invisible()
}

cp_kv <- function(...) {
  kv <- list(...)
  for (k in names(kv)) cat(k, ": ", paste(kv[[k]], collapse = ", "), "\n", sep = "")
}

presence_pattern <- function(p) {
  vapply(p, function(x) if (is.na(x)) "-" else as.character(min(9L, floor(x * 10))),
         character(1)) |> paste(collapse = "")
}

collapse_unique <- function(x) {
  x <- as.character(x); x <- x[!is.na(x)]
  if (!length(x)) NA_character_ else paste(sort(unique(x)), collapse = "/")
}


# ===========================================================================
# CHECKPOINT 1 — FETCH, CLEAN LABELS, COLUMN COVERAGE
# ---------------------------------------------------------------------------
# Everything is read as character. Column TYPES drift between seasons and
# readr's guesser only inspects the first 1000 rows, so a surprise would
# silently drop values. Coercion happens once, deliberately, at CP2.
# ===========================================================================

RAW_CACHE <- "cache/fpl_raw.rds"

read_season_gws <- function(season) {
  url <- paste0(ARCHIVE_BASE, season, "/gws/merged_gw.csv")
  message("fetching ", season)
  out <- try(read_csv(url, col_types = cols(.default = col_character()),
                      progress = FALSE), silent = TRUE)
  if (inherits(out, "try-error")) { warning("FAILED: ", season); return(NULL) }
  mutate(out, season = season)
}

if (!file.exists(RAW_CACHE)) {
  saveRDS(map(SEASONS, read_season_gws) |> compact() |> bind_rows(), RAW_CACHE)
}

# Always start from the untouched cache, so this block is idempotent.
fpl_raw_untouched <- readRDS(RAW_CACHE)

# FPL's API short name for goalkeepers is "GKP"; the scraper mostly rewrites it
# to "GK" but not always (101 rows in 2021-22). Normalise BEFORE filtering on
# position, or those goalkeepers vanish without a warning.
fpl_raw <- fpl_raw_untouched |>
  mutate(position = recode(position, "GKP" = "GK"))

# Assistant Manager elements were added for the 2024-25 chip. They have no
# minutes and no football position, and would otherwise inflate the
# zero-minute rate, the player counts, and the identity roster.
# `is.na(position)` keeps 2019-20 alive — that season has no position column.
am_rows <- sum(fpl_raw$position == "AM", na.rm = TRUE)
fpl_raw <- filter(fpl_raw, is.na(position) | position %in% VALID_POSITIONS)

cp_open(1, "fetch, clean labels, column coverage")

cp_kv("seasons requested" = length(SEASONS),
      "seasons returned"  = n_distinct(fpl_raw$season),
      "raw rows"          = format(nrow(fpl_raw_untouched), big.mark = ","),
      "AM rows removed"   = am_rows,
      "rows after"        = format(nrow(fpl_raw), big.mark = ","),
      "columns"           = ncol(fpl_raw))

cp_section("rows per season")
cp_table(count(fpl_raw, season, name = "rows"), max_rows = 10)

cp_section("position labels present")
cp_table(count(fpl_raw_untouched, season, position, name = "rows") |>
           filter(!position %in% VALID_POSITIONS | is.na(position)),
         max_rows = 12)

cp_section("column presence by season ('-' absent, 0-9 = decile non-NA)")
cov_pattern <- fpl_raw |>
  group_by(season) |>
  summarise(across(everything(), ~ mean(!is.na(.x))), .groups = "drop") |>
  pivot_longer(-season, names_to = "column", values_to = "p") |>
  pivot_wider(names_from = season, values_from = p) |>
  rowwise() |>
  mutate(pattern = presence_pattern(c_across(any_of(SEASONS)))) |>
  ungroup() |>
  select(column, pattern) |>
  arrange(pattern, column)
cp_table(cov_pattern, max_rows = 60)

cp_close()

# Known from the coverage table:
#   starts, expected_*      : 2022-23 onwards only
#   position, team, xP      : 2020-21 onwards only
#   tackles/recoveries/CBI  : 2025-26 only (new defensive contribution scoring)


# ===========================================================================
# CHECKPOINT 2 — TYPING AND SEASON PROFILE
# ===========================================================================

INT_COLS <- c("GW", "element", "fixture", "opponent_team", "minutes",
              "goals_scored", "assists", "clean_sheets", "goals_conceded",
              "own_goals", "penalties_saved", "penalties_missed",
              "yellow_cards", "red_cards", "saves", "bonus", "bps",
              "total_points", "value", "selected", "transfers_in",
              "transfers_out", "transfers_balance", "team_h_score",
              "team_a_score", "starts", "round")

DBL_COLS <- c("influence", "creativity", "threat", "ict_index", "xP",
              "expected_goals", "expected_assists",
              "expected_goal_involvements", "expected_goals_conceded")

# Values that exist as text but will not parse as numbers are destroyed
# silently by as.integer/as.numeric. Count them before letting that happen.
coercion_report <- map_dfr(intersect(c(INT_COLS, DBL_COLS), names(fpl_raw)),
                           function(cl) {
                             before <- sum(!is.na(fpl_raw[[cl]]) & fpl_raw[[cl]] != "")
                             after  <- sum(!is.na(suppressWarnings(as.numeric(fpl_raw[[cl]]))))
                             tibble(column = cl, non_empty = before, parsed = after, lost = before - after)
                           }) |>
  filter(lost > 0)

fpl <- fpl_raw |>
  mutate(across(any_of(INT_COLS), ~ suppressWarnings(as.integer(.x))),
         across(any_of(DBL_COLS), ~ suppressWarnings(as.numeric(.x))),
         was_home     = as.logical(was_home),
         kickoff_time = ymd_hms(kickoff_time, tz = "UTC"),
         price        = value / 10) |>
  rename(gw = GW)

season_profile <- fpl |>
  group_by(season) |>
  summarise(rows = n(), players = n_distinct(element),
            n_gws = n_distinct(gw), max_gw = max(gw, na.rm = TRUE),
            pct_zero_mins = mean(minutes == 0, na.rm = TRUE),
            total_minutes = sum(minutes, na.rm = TRUE),
            max_minutes   = max(minutes, na.rm = TRUE),
            no_kickoff    = sum(is.na(kickoff_time)),
            .groups = "drop") |>
  mutate(expected_gw = EXPECTED_GWS[season], gw_ok = max_gw == expected_gw)

saveRDS(fpl, "cache/fpl_typed.rds")

cp_open(2, "typing and season profile")

cp_section("values lost to coercion (empty is good)")
cp_table(coercion_report, max_rows = 15)

cp_section("season profile")
cp_table(season_profile, max_rows = 10)

cp_section("hard checks")
cp_kv("gameweek counts as expected" = all(season_profile$gw_ok),
      "any season under 500k minutes" = any(season_profile$total_minutes < 5e5),
      "max minutes in one row" = max(fpl$minutes, na.rm = TRUE),
      "rows with no kickoff" = sum(is.na(fpl$kickoff_time)),
      "rows with NA gw" = sum(is.na(fpl$gw)),
      "missing gameweeks 2022-23" = paste(setdiff(1:38,
                                                  unique(fpl$gw[fpl$season == "2022-23"])), collapse = ", "))

cp_close()

# 2019-20: 38 distinct gameweeks with a max of 47. GW30-38 are empty — blanked
# when COVID stopped play after GW29 — and GW39-47 hold the restart fixtures.
# 2022-23: GW7 has no rows; the round was cancelled outright.
# Total minutes sit at 747-749k every season against a theoretical 752,400
# (380 matches x 22 players x 90). The ~3k shortfall is red cards. Seven
# seasons within 0.3% of each other means the minutes data is complete.


# ===========================================================================
# CHECKPOINT 3 — GRAIN: DUPLICATES, DOUBLES, BLANKS, ORDERING
# ---------------------------------------------------------------------------
# DECISION: fixture-level is the primary grain. A minutes model predicts
# whether someone plays in a MATCH. Two matches in a gameweek are two separate
# selection decisions with different rest, different opponents, and a manager
# far more likely to rotate between them. Collapsing them destroys the signal —
# a player who plays 90+0 would look identical to one who plays 45+45.
# ===========================================================================

fpl <- readRDS("cache/fpl_typed.rds")

# Any fixture appearing under more than one gameweek is a postponement.
fixture_span <- fpl |>
  group_by(season, fixture) |>
  summarise(n_gws = n_distinct(gw),
            gws = paste(sort(unique(gw)), collapse = "/"),
            first_ko = min(kickoff_time), last_ko = max(kickoff_time),
            gap_days = as.numeric(difftime(max(kickoff_time), min(kickoff_time),
                                           units = "days")),
            rows = n(), .groups = "drop") |>
  filter(n_gws > 1)

true_duplicates <- fpl |>
  count(season, element, fixture, name = "n") |>
  filter(n > 1)

before_rows <- nrow(fpl)

# Keep the LATEST kickoff, not the first. A postponed fixture leaves a
# 0-minute placeholder row at the original date and the real row at the
# replayed date, both under the same fixture id. Keeping the first row kept the
# placeholder every time — 32 players who played 90 minutes recorded as 0.
# max(minutes) cannot separate the pairs where both rows are 0, but the
# placeholder still has to go or it corrupts rest days.
fpl <- fpl |>
  arrange(season, element, fixture, desc(kickoff_time)) |>
  distinct(season, element, fixture, .keep_all = TRUE)

dgw <- fpl |>
  count(season, element, gw, name = "fixtures_in_gw") |>
  filter(fixtures_in_gw > 1)

# Ordering features. `gw` is comparable neither across seasons (2019-20 runs to
# 47) nor within one (2022-23 has no GW7), so match_index does the ordinal
# work. is_restart flags gaps long enough to invalidate a rolling window.
fpl <- fpl |>
  arrange(season, element, kickoff_time) |>
  group_by(season, element) |>
  mutate(match_index = row_number(),
         days_since_prev = as.numeric(difftime(kickoff_time, lag(kickoff_time),
                                               units = "days")),
         is_restart = !is.na(days_since_prev) & days_since_prev > 30) |>
  ungroup()

saveRDS(fpl, "cache/fpl_grain.rds")

cp_open(3, "grain")

cp_section("postponed fixtures (spanning more than one gameweek)")
cp_table(select(fixture_span, season, fixture, gws, first_ko, last_ko,
                gap_days, rows), max_rows = 15)

cp_kv("duplicate player-fixture groups" = nrow(true_duplicates),
      "rows removed" = before_rows - nrow(fpl),
      "rows remaining" = format(nrow(fpl), big.mark = ","))

cp_section("double gameweeks by season")
cp_table(dgw |> group_by(season) |>
           summarise(player_gws_doubled = n(), gws_affected = n_distinct(gw),
                     max_fixtures = max(fixtures_in_gw), .groups = "drop"),
         max_rows = 10)

cp_section("clubs playing twice in a gameweek (distinct fixtures per club)")
cp_table(fpl |> filter(!is.na(team)) |>
           distinct(season, gw, team, fixture) |>
           count(season, gw, team, name = "fixtures") |>
           filter(fixtures > 1) |>
           count(season, gw, name = "clubs_doubled") |>
           arrange(desc(clubs_doubled)), max_rows = 12)

cp_section("blank gameweeks (fewer than 20 clubs playing)")
cp_table(fpl |> filter(!is.na(team)) |> distinct(season, gw, team) |>
           count(season, gw, name = "clubs_playing") |>
           filter(clubs_playing < 20) |>
           count(season, name = "blank_gameweeks"), max_rows = 10)

cp_section("long breaks (30+ days) affecting 50+ players")
cp_table(fpl |> filter(is_restart) |> count(season, gw, name = "resuming") |>
           filter(resuming > 50) |> arrange(season, gw), max_rows = 10)

cp_close()

# Gameweek-level aggregation, for FPL scoring questions only — never the
# modelling grain. Counting stats sum; snapshot stats take the pre-first-match
# value, since they describe the state at the deadline.
SUM_COLS <- c("minutes", "goals_scored", "assists", "clean_sheets",
              "goals_conceded", "own_goals", "penalties_saved",
              "penalties_missed", "yellow_cards", "red_cards", "saves",
              "bonus", "bps", "total_points", "starts", "expected_goals",
              "expected_assists", "expected_goal_involvements",
              "expected_goals_conceded")
SNAP_COLS <- c("value", "price", "selected", "transfers_in", "transfers_out",
               "transfers_balance")

to_gameweek_level <- function(df) {
  df |>
    arrange(season, element, gw, kickoff_time) |>
    group_by(season, element, gw) |>
    summarise(n_fixtures = n(),
              across(any_of(SUM_COLS),  ~ sum(.x, na.rm = TRUE)),
              across(any_of(SNAP_COLS), ~ first(.x)),
              kickoff_first = min(kickoff_time),
              kickoff_last  = max(kickoff_time),
              .groups = "drop")
}


# ===========================================================================
# CHECKPOINT 4 — THE TRUE FIXTURE CALENDAR
# ---------------------------------------------------------------------------
# WHY THIS EXISTS. Rest computed from `fpl` alone is Premier-League-to-
# Premier-League. European and cup matches are invisible, so a club that played
# a Europa League tie on Thursday and a league match on Sunday is recorded as
# having had a full week off.
#
# Measured on 2020-21..2023-24: 22.4% of PL matches have a hidden fixture in
# the gap, and mean rest is overstated by ~1.1 days. Critically the error is
# ENTIRELY in the long-rest buckets — nothing fits inside a 3-day gap — so it
# contaminates the CONTROL group, not the treatment group. At a nominally
# identical 6-8 days of rest, clubs with a hidden fixture made 2.36 XI changes
# against 1.90 without, and heavy rotation went 12.6% vs 4.1%.
#
# Live FBref scraping returns 403. The load_* functions read pre-scraped data
# from the worldfootballR_data GitHub repo instead and are not blocked.
#
# THREE REST MEASURES, ALL KEPT — they answer different questions and must not
# be used interchangeably:
#
#   club_rest_pl     days since the CLUB's previous Premier League match.
#                    What the league fixture list gave them. Per club.
#   true_rest        days since the club's previous match in ANY competition.
#                    What the squad actually got. Per club.
#   days_since_prev  days since THIS PLAYER's previous appearance. Diverges
#                    from the club measures for anyone who transferred between
#                    PL clubs mid-season — their previous row belongs to their
#                    old club. Per player.
#
# The earlier draft compared true_rest against days_since_prev, mixing a club
# measure with a player one. That produced impossible cells (true rest longer
# than measured rest, which cannot happen when the calendar is a superset of
# league fixtures) and inflated the blind-spot figure from 22.4% to 23.5%.
# The cross-tab below must be strictly lower-triangular.
# ===========================================================================

fpl <- readRDS("cache/fpl_grain.rds")

COMP_CACHE    <- "cache/comp_results.rds"
SEASON_OF_END <- c("2021" = "2020-21", "2022" = "2021-22",
                   "2023" = "2022-23", "2024" = "2023-24")

if (!file.exists(COMP_CACHE)) {
  saveRDS(load_match_comp_results(
    comp_name = c("UEFA Champions League", "UEFA Europa League",
                  "UEFA Europa Conference League", "UEFA Conference League",
                  "FA Cup", "EFL Cup")), COMP_CACHE)
}
comp_results <- readRDS(COMP_CACHE)


# --- A. CLUB NAMES ----------------------------------------------------------
# FBref tags clubs with a country code, prepended for away and appended for
# home ("it Roma" and "Roma it" are the same club). The code identifies English
# clubs directly, so no guessing is needed.

extract_country <- function(x) {
  coalesce(str_extract(x, "^[a-z]{2,3}(?=\\s)"),
           str_extract(x, "(?<=\\s)[a-z]{2,3}$"))
}
strip_country <- function(x) {
  x |> str_remove("^[a-z]{2,3}\\s+") |> str_remove("\\s+[a-z]{2,3}$") |> str_squish()
}

# FBref spelling on the left, FPL short name on the right. A MISSING ENTRY IS
# INVISIBLE — the club's cup matches are dropped and its true_rest silently
# equals its club_rest_pl, making it look like a club that never plays midweek.
# The C1 check below is the only thing that catches that.
club_map <- tribble(
  ~fbref,                 ~team,
  "Manchester City",      "Man City",
  "Manchester Utd",       "Man Utd",
  "Tottenham",            "Spurs",
  "Newcastle Utd",        "Newcastle",
  "Nott'ham Forest",      "Nott'm Forest",
  "Leicester City",       "Leicester",
  "West Bromwich Albion", "West Brom",
  "West Brom",            "West Brom",
  "Norwich City",         "Norwich",
  "Luton Town",           "Luton",
  "Ipswich Town",         "Ipswich",
  "Leeds United",         "Leeds",
  "Sheffield Utd",        "Sheffield Utd"
)

pl_clubs_by_season <- fpl |>
  filter(!is.na(team), season %in% CALENDAR_SEASONS) |>
  distinct(season, team)

other_long <- comp_results |>
  filter(Competition_Name %in% c("UEFA Champions League", "UEFA Europa League",
                                 "UEFA Conference League",
                                 "UEFA Europa Conference League",
                                 "FA Cup", "EFL Cup"),
         as.character(Season_End_Year) %in% names(SEASON_OF_END)) |>
  select(competition = Competition_Name, Season_End_Year, Date, Home, Away) |>
  pivot_longer(c(Home, Away), names_to = "venue", values_to = "club_raw") |>
  filter(!is.na(Date), !is.na(club_raw)) |>
  mutate(season  = SEASON_OF_END[as.character(Season_End_Year)],
         date    = as.Date(Date),
         country = extract_country(club_raw),
         club    = strip_country(club_raw),
         # Domestic cups carry no country code, so a missing one there is English.
         is_english = country == "eng" |
           (is.na(country) & competition %in% c("FA Cup", "EFL Cup")))

# semi_join on SEASON-SPECIFIC club lists. The all-seasons club set would
# import (say) Sunderland's FA Cup ties from their Championship years.
other_eng <- other_long |>
  filter(is_english) |>
  left_join(club_map, by = c("club" = "fbref")) |>
  mutate(team = coalesce(team, club)) |>
  semi_join(pl_clubs_by_season, by = c("season", "team")) |>
  distinct(season, team, date, competition)


# --- B. THE CALENDAR --------------------------------------------------------

pl_dates <- fpl |>
  filter(!is.na(team), season %in% CALENDAR_SEASONS) |>
  distinct(season, team, fixture, kickoff_time) |>
  transmute(season, team, date = as.Date(kickoff_time),
            competition = "Premier League")

calendar <- bind_rows(pl_dates, other_eng) |>
  distinct(season, team, date, competition) |>
  arrange(season, team, date)


# --- C. THE TWO CLUB-LEVEL REST MEASURES ------------------------------------
# Both computed per club, on the same rows, so they are directly comparable.

club_rest <- calendar |>
  group_by(season, team) |>
  mutate(true_rest = as.numeric(date - lag(date))) |>
  ungroup() |>
  filter(competition == "Premier League") |>
  select(season, team, date, true_rest) |>
  # club_rest_pl uses PL fixtures only — the same series with the cup and
  # European dates removed before lagging.
  left_join(
    pl_dates |>
      distinct(season, team, date) |>
      arrange(season, team, date) |>
      group_by(season, team) |>
      mutate(club_rest_pl = as.numeric(date - lag(date))) |>
      ungroup(),
    by = c("season", "team", "date")
  )

# One row per club-match. Guards against a silent fan-out on the join below.
stopifnot(nrow(club_rest) == nrow(distinct(club_rest, season, team, date)))
# true_rest can never exceed club_rest_pl: the calendar is a superset.
stopifnot(all(club_rest$true_rest <= club_rest$club_rest_pl + 1e-6, na.rm = TRUE))


# --- D. ATTACH TO PLAYER ROWS -----------------------------------------------
# Seasons outside CALENDAR_SEASONS get NA plus a flag, so nothing downstream
# mistakes a missing calendar for a well-rested club.

fpl <- fpl |>
  mutate(date = as.Date(kickoff_time)) |>
  left_join(club_rest, by = c("season", "team", "date")) |>
  mutate(
    calendar_complete = season %in% CALENDAR_SEASONS,
    hidden_fixture  = !is.na(true_rest) & !is.na(club_rest_pl) &
      true_rest < club_rest_pl - 0.5,
    short_rest_true = !is.na(true_rest) & true_rest < 4,
    # How much a PL-only measure overstates rest, in days. Zero when the club
    # had a genuinely free midweek.
    rest_overstatement = club_rest_pl - true_rest
  )

saveRDS(calendar,  "cache/calendar.rds")
saveRDS(club_rest, "cache/club_rest.rds")
saveRDS(fpl,       "cache/fpl_calendar.rds")


# --- DIAGNOSTICS ------------------------------------------------------------

cp_open(4, "true fixture calendar")

cp_section("C1. PL club-seasons with NO cup or European match (MUST be empty)")
cp_table(anti_join(pl_clubs_by_season, distinct(other_eng, season, team),
                   by = c("season", "team")), max_rows = 20)

cp_section("C2. matches per club-season, all competitions")
cp_table(calendar |> count(season, team, name = "matches") |>
           group_by(season) |>
           summarise(min = min(matches), mean = round(mean(matches), 1),
                     max = max(matches), busiest = team[which.max(matches)],
                     .groups = "drop"), max_rows = 6)

cp_section("C3. size of the blind spot (club-level, one row per PL match)")
cp_table(club_rest |>
           filter(!is.na(true_rest), !is.na(club_rest_pl), club_rest_pl <= 30) |>
           summarise(pl_matches = n(),
                     hidden = sum(true_rest < club_rest_pl - 0.5),
                     pct = round(100 * mean(true_rest < club_rest_pl - 0.5), 1),
                     mean_pl_only = round(mean(club_rest_pl), 2),
                     mean_true    = round(mean(true_rest), 2),
                     mean_overstatement = round(mean(club_rest_pl - true_rest), 2)),
         max_rows = 3)

cp_section("C4. cross-tab — MUST be lower-triangular (no cells above diagonal)")
cp_table(club_rest |>
           filter(!is.na(true_rest), !is.na(club_rest_pl), club_rest_pl <= 30) |>
           mutate(pl_only = cut(club_rest_pl, c(0, 3.5, 4.5, 5.5, 7.5, Inf),
                                labels = c("<=3", "4", "5", "6-7", "8+")),
                  true    = cut(true_rest, c(0, 3.5, 4.5, 5.5, 7.5, Inf),
                                labels = c("<=3", "4", "5", "6-7", "8+"))) |>
           count(pl_only, true) |>
           pivot_wider(names_from = true, values_from = n,
                       names_prefix = "true_"),
         max_rows = 8)

cp_section("C5. blind spot by season")
cp_table(club_rest |>
           filter(!is.na(true_rest), !is.na(club_rest_pl), club_rest_pl <= 30) |>
           group_by(season) |>
           summarise(matches = n(),
                     pct_hidden = round(100 * mean(true_rest < club_rest_pl - 0.5), 1),
                     mean_overstatement = round(mean(club_rest_pl - true_rest), 2),
                     .groups = "drop"), max_rows = 6)

cp_section("C6. which clubs are most affected (2023-24)")
cp_table(club_rest |>
           filter(season == "2023-24", !is.na(true_rest), !is.na(club_rest_pl)) |>
           group_by(team) |>
           summarise(matches = n(),
                     hidden = sum(true_rest < club_rest_pl - 0.5),
                     pct_hidden = round(100 * mean(true_rest < club_rest_pl - 0.5), 1),
                     .groups = "drop") |>
           arrange(desc(pct_hidden)), max_rows = 20)

cp_section("C7. player rows where the player gap differs from the club gap")
cat("expected: mid-season transfers between PL clubs\n\n")
cp_table(fpl |>
           filter(calendar_complete, !is.na(club_rest_pl), !is.na(days_since_prev)) |>
           summarise(rows = n(),
                     differ = sum(abs(days_since_prev - club_rest_pl) > 0.5),
                     pct = round(100 * mean(abs(days_since_prev - club_rest_pl) > 0.5), 2)),
         max_rows = 3)

cp_close()


# ===========================================================================
# CHECKPOINT 5 — PERSISTENT PLAYER IDENTITY
# ---------------------------------------------------------------------------
# `element` is reassigned every season. Without a stable id there is no
# "started 80% of matches last season" feature and no grouping variable for the
# player random effect.
#
# TWO BUGS FIXED FROM THE FIRST DRAFT, both found by reading CP5's own output:
#
#   1. TRIAGE ORDER. `any_dormant` was tested before `teams_differ`, so a
#      genuinely different player who happened to record no minutes was
#      labelled a duplicate registration. Alvaro Fernandez (Brentford GK,
#      1080 mins) and Alvaro Fernandez (Man Utd DEF, 0 mins) are two people.
#      Different clubs means different people, played or not.
#
#   2. DISAMBIGUATION WAS INSUFFICIENT AND UNSTABLE.
#      (a) Position alone cannot separate two players who share a name AND a
#          position — Ben Davies (Spurs, DEF) and Ben Davies (Liverpool, DEF)
#          both became `ben_davies__def` and were merged into one career.
#          Club is now appended when position does not separate them.
#      (b) Suffixing only in seasons where a collision occurs splits a player
#          across seasons: Ben Davies (Liverpool) first appears in 2020-21, so
#          Spurs' Ben Davies would be `ben_davies` in 2019-20 and
#          `ben_davies__def__spurs` afterwards. A key that collides in ANY
#          season is now suffixed in EVERY season.
# ===========================================================================

fpl <- readRDS("cache/fpl_calendar.rds")

clean_player_name <- function(x) {
  x |>
    str_replace("_\\d+$", "") |>          # older seasons append the element id
    str_replace_all("_", " ") |>
    stri_trans_general("Latin-ASCII") |>  # Odegaard
    str_replace_all("[^A-Za-z ]", " ") |>
    str_squish() |> str_to_lower()
}

# Sorting the tokens makes the key invariant to name order, which handles
# "Son Heung-min" / "Heung-min Son" without a lookup table. The keys read
# oddly as a result (van Dijk becomes dijk_van_virgil) but that is cosmetic.
make_name_key <- function(x) {
  vapply(str_split(clean_player_name(x), " "),
         function(t) paste(sort(t), collapse = "_"), character(1))
}

slug <- function(x) tolower(str_replace_all(coalesce(x, "unk"), "[^A-Za-z]", ""))

# One row per (season, element) — NOT per (season, element, team), or a
# mid-season transfer becomes two rows and registers as a name collision.
roster <- fpl |>
  count(season, element, name, name = "n_rows") |>
  group_by(season, element) |>
  slice_max(n_rows, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(season, element, name) |>
  mutate(name_clean = clean_player_name(name),
         name_key   = make_name_key(name)) |>
  left_join(
    fpl |> group_by(season, element) |>
      summarise(across(any_of(c("team", "position")), collapse_unique),
                appearances = n(), total_minutes = sum(minutes, na.rm = TRUE),
                first_gw = min(gw, na.rm = TRUE), last_gw = max(gw, na.rm = TRUE),
                .groups = "drop"),
    by = c("season", "element"))


# --- A. COLLISIONS ----------------------------------------------------------
# A real collision is two DIFFERENT elements sharing a key in one season.
# Order matters: club first, because different clubs settle it outright.

collision_triage <- roster |>
  group_by(season, name_key) |>
  filter(n_distinct(element) > 1) |>
  mutate(n_elements     = n_distinct(element),
         teams_differ   = n_distinct(team) > 1,
         positions_differ = n_distinct(position) > 1,
         any_dormant    = any(total_minutes == 0),
         ranges_overlap = max(first_gw) <= min(last_gw),
         verdict = case_when(
           teams_differ & positions_differ ~ "SPLIT_club_and_position",
           teams_differ                          ~ "SPLIT_same_position",
           any_dormant                                    ~ "MERGE_one_never_played",
           !ranges_overlap                                ~ "MERGE_disjoint_spells",
           TRUE                                           ~ "REVIEW")) |>
  ungroup()


# --- B. DISAMBIGUATION ------------------------------------------------------
# Any key that collides in ANY season is suffixed in EVERY season, so the uid
# is stable across a player's whole history.

colliding_keys <- roster |>
  group_by(season, name_key) |>
  filter(n_distinct(element) > 1) |>
  ungroup() |>
  distinct(name_key) |>
  pull(name_key)

# Within a colliding key and season, does position separate the elements? If
# two share it, the club has to go into the suffix as well.
pos_ambiguous_keys <- roster |>
  filter(name_key %in% colliding_keys) |>
  group_by(season, name_key, position) |>
  filter(n_distinct(element) > 1) |>
  ungroup() |>
  distinct(name_key) |>
  pull(name_key)

roster <- roster |>
  mutate(
    collides = name_key %in% colliding_keys,
    disambiguated = case_when(
      !collides                          ~ name_key,
      name_key %in% pos_ambiguous_keys   ~ paste(name_key, slug(position),
                                                 slug(team), sep = "__"),
      TRUE                               ~ paste(name_key, slug(position), sep = "__")
    ))


# --- C. CROSS-SEASON ALIASES ------------------------------------------------
# Same player under two spellings. Confirm each of these before trusting it —
# they are inferred from edit distance, not verified.
uid_aliases <- tribble(
  ~variant,                       ~canonical,
  "yarmoliuk_yegor",              "yarmolyuk_yegor",
  "da_ferreira_joao_pedro_silva", "ferreira_joao_pedro_silva",
  "fin_stevens",                  "finley_stevens",
  "joe_whitworth",                "joseph_whitworth",
  "aji_alese",                    "ajibola_alese"
)

# For anything the alias table cannot express (a club suffix that changed, say).
player_overrides <- tribble(
  ~season,   ~element, ~player_uid,
  "2019-20",      329, "ben_davies__def__spurs"
)

player_map <- roster |>
  left_join(player_overrides, by = c("season", "element")) |>
  mutate(player_uid = coalesce(player_uid, disambiguated)) |>
  left_join(uid_aliases, by = c("player_uid" = "variant")) |>
  mutate(player_uid = coalesce(canonical, player_uid)) |>
  select(-canonical)

# A uid must never map to two elements in one season — that is a merge.
merged_check <- player_map |>
  count(season, player_uid, name = "elements") |>
  filter(elements > 1)
stopifnot(nrow(merged_check) == 0)

singletons <- player_map |> distinct(player_uid, season) |>
  count(player_uid, name = "n") |> filter(n == 1) |> pull(player_uid)

surname_of <- function(k) vapply(str_split(k, "_"),
                                 function(t) t[length(t)], character(1))

fuzzy <- tibble(uid = singletons, sn = surname_of(singletons)) |>
  group_by(sn) |> filter(n() > 1) |>
  summarise(pairs = list(as.data.frame(t(combn(uid, 2)))), .groups = "drop") |>
  mutate(pairs = map(pairs, ~ setNames(.x, c("a", "b")))) |>
  unnest(pairs) |>
  mutate(dist = map2_dbl(a, b, ~ adist(.x, .y)[1, 1])) |>
  filter(dist <= 4) |> arrange(dist)

fpl <- left_join(fpl, select(player_map, season, element, player_uid, name_clean),
                 by = c("season", "element"))
stopifnot(!any(is.na(fpl$player_uid)))

saveRDS(player_map, "cache/player_map.rds")
saveRDS(fpl, "cache/fpl_identified.rds")
write_csv(collision_triage, "audit/name_collisions.csv")
write_csv(fuzzy, "audit/fuzzy_name_candidates.csv")


# --- DIAGNOSTICS ------------------------------------------------------------

# Clubs collapse twice (once per season, once per uid), so a season string
# like "Brighton/Newcastle" survives as a single value. Split before uniquing.
clubs_of <- function(x) {
  x |> na.omit() |> str_split("/") |> unlist() |> unique() |> sort() |>
    paste(collapse = "/")
}

cp_open(5, "player identity")

cp_kv("roster rows" = nrow(roster),
      "distinct season-element pairs" = n_distinct(paste(roster$season, roster$element)),
      "distinct player_uids" = n_distinct(player_map$player_uid),
      "keys colliding in some season" = length(colliding_keys),
      "keys needing a club suffix" = length(pos_ambiguous_keys),
      "aliases applied" = nrow(uid_aliases),
      "uids in one season only" = length(singletons))

cp_section("A1. collision verdicts")
cp_table(collision_triage |> distinct(season, name_key, verdict) |>
           count(verdict, name = "keys"), max_rows = 8)

cp_section("A2. every collision, with evidence")
cp_table(collision_triage |> arrange(name_key, season) |>
           select(season, name_key, element, any_of(c("team", "position")),
                  total_minutes, verdict), max_rows = 30)

cp_section("A3. the uids those collisions now resolve to (MUST all differ)")
cp_table(player_map |>
           filter(name_key %in% collision_triage$name_key) |>
           arrange(name_key, season) |>
           select(season, name_key, element, any_of(c("team", "position")),
                  player_uid), max_rows = 30)

cp_section("B1. seasons per uid")
cp_table(player_map |> distinct(player_uid, season) |> count(player_uid) |>
           count(n, name = "uids") |> rename(seasons_present = n), max_rows = 10)

cp_section("B2. uids in the most seasons — should be recognisable long-servers")
cp_table(player_map |> group_by(player_uid) |>
           summarise(seasons = n_distinct(season),
                     total_minutes = sum(total_minutes, na.rm = TRUE),
                     clubs = clubs_of(team), .groups = "drop") |>
           arrange(desc(seasons), desc(total_minutes)), max_rows = 20)

cp_section("B3. remaining fuzzy pairs after aliases")
cp_table(select(fuzzy, a, b, dist), max_rows = 30)

cp_close()

# ===========================================================================
# CHECKPOINT 6 — LEAKAGE AUDIT
# ---------------------------------------------------------------------------
# For every column: would I have known this value BEFORE the deadline?
# Anything answering "no" that gets used as a feature will inflate validation
# scores and then collapse in live use.
#
# CORRECTED FROM THE PREVIOUS VERSION — the verdict was inverted.
#
# The first draft treated "the change from t to t+1 correlates with row t+1's
# transfers" as evidence of a POST-HOC scrape. It is the opposite. If
# selected[t] is ownership AT DEADLINE t, then
#
#     selected[t+1] - selected[t]  ==  net transfers in the window before
#                                      deadline t+1  ==  transfers_balance[t+1]
#
# which is close to an identity. So cor_next near 1 is the SIGNATURE of a
# deadline snapshot. Observed: 0.95 for ownership, 0.447 vs 0.339 for value —
# weaker for value only because price moves are quantised to +/-0.1 and capped.
#
# Consequence: value, price, selected and the transfer columns are all
# pre-deadline and safe UNLAGGED. Only xP gets lagged (see section C).
# ===========================================================================

fpl <- readRDS("cache/fpl_identified.rds")

OUTCOME_COLS <- c("total_points", "bps", "bonus", "minutes", "goals_scored",
                  "assists", "clean_sheets", "goals_conceded", "own_goals",
                  "penalties_saved", "penalties_missed", "yellow_cards",
                  "red_cards", "saves", "starts", "influence", "creativity",
                  "threat", "ict_index", "expected_goals", "expected_assists",
                  "expected_goal_involvements", "expected_goals_conceded",
                  "team_h_score", "team_a_score", "defensive_contribution",
                  "tackles", "recoveries", "clearances_blocks_interceptions")

SNAPSHOT_COLS <- c("value", "price", "selected", "transfers_in",
                   "transfers_out", "transfers_balance")

SAFE_COLS <- c("season", "gw", "element", "player_uid", "name", "name_clean",
               "team", "position", "opponent_team", "fixture", "kickoff_time",
               "date", "was_home", "match_index", "days_since_prev",
               "is_restart", "club_rest_pl", "true_rest", "hidden_fixture",
               "short_rest_true", "rest_overstatement", "calendar_complete")

DEAD_COLS <- c("mng_clean_sheets", "mng_draw", "mng_goals_scored", "mng_loss",
               "mng_underdog_draw", "mng_underdog_win", "mng_win",
               "modified", "round")


# --- A. CORRELATION WITH THE SAME-ROW OUTCOME ------------------------------
# A pre-deadline variable can legitimately correlate with points — good players
# cost more and are more owned. So this flags candidates, it does not convict.

leak_corr <- fpl |>
  filter(!is.na(total_points)) |>
  summarise(across(any_of(c(SNAPSHOT_COLS, "xP")),
                   ~ suppressWarnings(cor(.x, total_points,
                                          use = "pairwise.complete.obs")))) |>
  pivot_longer(everything(), names_to = "column", values_to = "cor_points") |>
  arrange(desc(abs(cor_points)))


# --- B. SNAPSHOT TIMING -----------------------------------------------------
# One row per player-gameweek: value and transfers are gameweek-level and
# identical across both fixtures of a double. Consecutive gameweeks only, so
# the 2019-20 restart gap and blank weeks cannot distort the lag.

gw_level <- fpl |>
  group_by(season, player_uid, gw) |>
  summarise(value = first(value), selected = first(selected),
            transfers_balance = first(transfers_balance), .groups = "drop") |>
  arrange(season, player_uid, gw)

timing_test <- function(col) {
  gw_level |>
    filter(!is.na(.data[[col]]), !is.na(transfers_balance)) |>
    group_by(season, player_uid) |>
    mutate(gw_gap = lead(gw) - gw,
           change_next = lead(.data[[col]]) - .data[[col]],
           tb_this = transfers_balance,
           tb_next = lead(transfers_balance)) |>
    ungroup() |>
    filter(gw_gap == 1) |>
    summarise(column = col, pairs = n(),
              cor_this = round(cor(change_next, tb_this, use = "complete.obs"), 3),
              cor_next = round(cor(change_next, tb_next, use = "complete.obs"), 3))
}

snapshot_timing <- bind_rows(timing_test("value"), timing_test("selected")) |>
  mutate(verdict = if_else(
    abs(cor_next) > abs(cor_this),
    "DEADLINE snapshot — safe unlagged",
    "POST-HOC — lag one gameweek"))


# --- C. IS xP PRE-DEADLINE? -------------------------------------------------
# xP is scraped from FPL's ep_this. Two tests, because the crude one misleads.
#
# C1 (crude) looks damning: most zero-minute rows have xP of exactly 0. But
#    that population is dominated by fringe players who were never going to
#    play, for whom 0 is the correct PRE-match forecast.
#
# C2 (sharp) restricts to genuinely surprising benchings — started 60+ last
#    match, played 0 this one. A forecast with hindsight would zero these too.
#    Observed: 39% zeros and 5.2% above 4, against 72.8% and 0.3% in C1. xP
#    gets caught out on exactly the rows a real forecast should, so it is
#    probably pre-deadline. The remaining zeros are most likely injuries and
#    suspensions, which FPL flags before the deadline and prices at zero
#    legitimately.
#
# VERDICT: lag it anyway. Mean xP of 1.1 for last week's starters is lower than
# a clean forecast would give, and being wrong here produces a model that
# validates well and fails live. Second reason: xP is FPL's own projection and
# already contains an implicit minutes model, so using it same-row means partly
# learning to copy FPL's opinion — which muddies the Phase 2 baselines.

xp_crude <- fpl |>
  filter(!is.na(xP), season >= "2022-23") |>
  mutate(played = minutes > 0) |>
  group_by(played) |>
  summarise(rows = n(), mean_xP = round(mean(xP), 2),
            pct_xP_zero = round(100 * mean(xP == 0), 1),
            pct_xP_over_4 = round(100 * mean(xP > 4), 1), .groups = "drop")

xp_sharp <- fpl |>
  filter(season >= "2022-23", !is.na(xP)) |>
  arrange(season, player_uid, kickoff_time) |>
  group_by(season, player_uid) |>
  mutate(started_prev = lag(minutes) >= 60) |>
  ungroup() |>
  filter(started_prev, minutes == 0) |>
  summarise(rows = n(), mean_xP = round(mean(xP), 2),
            pct_xP_zero = round(100 * mean(xP == 0), 1),
            pct_xP_over_4 = round(100 * mean(xP > 4), 1))


# --- D. CLASSIFY EVERY COLUMN ----------------------------------------------

audit_table <- tibble(column = names(fpl)) |>
  mutate(category = case_when(
    column %in% OUTCOME_COLS  ~ "OUTCOME — target only, never a same-row feature",
    column == "xP"            ~ "LAG — ambiguous timing, use xP_lag1",
    column %in% SNAPSHOT_COLS ~ "SNAPSHOT — deadline value, safe unlagged",
    column %in% SAFE_COLS     ~ "SAFE — known before the deadline",
    column %in% DEAD_COLS     ~ "DROP — empty or redundant",
    TRUE                      ~ "UNCLASSIFIED")) |>
  left_join(leak_corr, by = "column") |>
  arrange(category, column)

write_csv(audit_table, "audit/leakage_audit.csv")

# Written record, to commit alongside the data.
writeLines(c(
  "# Leakage audit",
  paste0("_Generated ", format(Sys.time(), "%Y-%m-%d %H:%M"), "_"),
  "",
  "## Method",
  "For every column: would this value have been known before the deadline?",
  "",
  "## Snapshot timing test",
  "If a column holds the value AT deadline t, its change from t to t+1 equals",
  "the transfer traffic recorded at t+1 — so a high correlation with the NEXT",
  "row indicates a deadline snapshot, not a post-hoc scrape.",
  "",
  paste0("- ", snapshot_timing$column, ": cor_this=", snapshot_timing$cor_this,
         ", cor_next=", snapshot_timing$cor_next, " -> ", snapshot_timing$verdict),
  "",
  "## xP",
  paste0("- All zero-minute rows: ", xp_crude$pct_xP_zero[xp_crude$played == FALSE],
         "% have xP exactly 0"),
  paste0("- Surprising benchings only (started 60+ last match): ",
         xp_sharp$pct_xP_zero, "% zeros, ", xp_sharp$pct_xP_over_4, "% above 4"),
  "- Probably pre-deadline, but lagged as a precaution and to keep the Phase 2",
  "  baseline comparison clean.",
  "",
  "## Column classification",
  paste0("- ", audit_table$category, ": ",
         tapply(audit_table$column, audit_table$category,
                function(x) paste(x, collapse = ", "))[audit_table$category]) |>
    unique()
), "audit/leakage_audit.md")


# --- DIAGNOSTICS ------------------------------------------------------------

cp_open(6, "leakage audit")

cp_section("A1. correlation with same-row total_points (a flag, not a verdict)")
cp_table(leak_corr, max_rows = 10)

cp_section("B1. snapshot timing — cor_next > cor_this means DEADLINE value")
cp_table(snapshot_timing, max_rows = 5)

cp_section("C1. xP, all rows (misleading on its own)")
cp_table(xp_crude, max_rows = 5)

cp_section("C2. xP, surprising benchings only (the real test)")
cp_table(xp_sharp, max_rows = 3)

cp_section("D1. column classification")
cp_table(count(audit_table, category, name = "columns"), max_rows = 8)

cp_section("D2. UNCLASSIFIED — decide before Phase 3")
cp_table(filter(audit_table, category == "UNCLASSIFIED"), max_rows = 20)

cp_close()
# ===========================================================================
# CHECKPOINT 7 — FINALISE
# ---------------------------------------------------------------------------
# WHAT CP6 SETTLED
#
# value / price / selected / transfers_*  =  DEADLINE SNAPSHOTS, safe unlagged.
#   The test reads backwards at first glance. If selected[t] is ownership AT
#   deadline t, then selected[t+1] - selected[t] IS the transfer traffic in the
#   window before deadline t+1 — i.e. transfers_balance[t+1]. So a high
#   correlation with the NEXT row (0.95 for ownership, 0.447 vs 0.339 for
#   value) is the signature of a deadline snapshot, not a post-hoc scrape.
#
# xP  =  probably pre-deadline, LAGGED anyway.
#   72.8% of zero-minute rows have xP of exactly 0, but that population is
#   mostly fringe players for whom 0 is the correct pre-match forecast. On
#   genuinely surprising benchings (started 60+ last match, 0 this one) it is
#   39% zeros and 5.2% above 4 — xP gets caught out where a real forecast
#   should. Lagged regardless: the downside of being wrong is a model that
#   validates well and fails live, and xP is FPL's own projection, so using it
#   same-row means partly learning to copy FPL rather than beating it.
#
# WHAT CP7 SETTLED — THE TARGET
#
#   `started_60` (minutes >= 60) is the CANONICAL target for all seasons.
#   `starts` is a cross-check only, NOT the target, because it is unpopulated
#   for the first fifteen gameweeks of 2022-23 — roughly 208 broken rows per
#   gameweek, near the full complement of 220 starters, collapsing to single
#   digits from GW16. In the seasons where `starts` IS reliable the two agree
#   98% of the time, and the residual is genuine starters substituted before
#   the hour. So the proxy is the more trustworthy variable, and using it means
#   all seven seasons are usable — including 2019-22, where the extreme
#   congestion lives.
# ===========================================================================

fpl <- readRDS("cache/fpl_identified.rds")


# --- A. DROP DEAD COLUMNS ---------------------------------------------------

DROP_COLS <- c(
  # Assistant Manager chip, 2024-25 only. The AM elements went at CP1, so
  # these are now entirely NA.
  "mng_clean_sheets", "mng_draw", "mng_goals_scored", "mng_loss",
  "mng_underdog_draw", "mng_underdog_win", "mng_win",
  "modified",   # scraper artifact
  "round"       # duplicates gw
)

dropped <- intersect(DROP_COLS, names(fpl))
fpl <- select(fpl, -any_of(DROP_COLS))


# --- B. LAG ONLY WHAT NEEDS LAGGING -----------------------------------------
# Ordered by kickoff within player-season, so a double gameweek lags to the
# previous FIXTURE — the correct grain for this model.

LAG_COLS <- c("xP")

fpl <- fpl |>
  arrange(season, player_uid, kickoff_time) |>
  group_by(season, player_uid) |>
  mutate(across(any_of(LAG_COLS), lag, .names = "{.col}_lag1")) |>
  ungroup()


# --- C. TARGETS -------------------------------------------------------------

# Where the API `starts` column can be trusted. Flagged rather than patched, so
# the reason stays visible and the cross-check in D2 still works.
starts_reliable <- fpl |>
  filter(!is.na(starts)) |>
  group_by(season, gw) |>
  summarise(broken = sum(starts == 0 & minutes >= 60), .groups = "drop") |>
  mutate(ok = broken < 20)

fpl <- fpl |>
  left_join(select(starts_reliable, season, gw, starts_ok = ok),
            by = c("season", "gw")) |>
  mutate(
    starts_ok = coalesce(starts_ok, FALSE),
    
    # CANONICAL TARGET — available for every season.
    started_60 = minutes >= 60,
    
    # Cross-check only. NA where the API column is unreliable.
    started_api = if_else(starts_ok & !is.na(starts), starts == 1, NA),
    
    played     = minutes > 0,
    appearance = case_when(minutes == 0 ~ "unused",
                           minutes < 60 ~ "cameo",
                           TRUE         ~ "start"),
    
    # Regime flags. Five substitutions returned permanently in 2022-23, which
    # changes the data-generating process for minutes, not just its level.
    five_subs = season >= "2022-23",
    covid_era = season %in% c("2019-20", "2020-21")
  )

# The lagged target is the single strongest predictor in the model and the
# baseline every fancier version has to beat, so it is built here rather than
# left to Phase 3.
fpl <- fpl |>
  arrange(season, player_uid, kickoff_time) |>
  group_by(season, player_uid) |>
  mutate(started_prev = lag(started_60),
         minutes_prev = lag(minutes)) |>
  ungroup()


# --- D. MODELLING BASE ------------------------------------------------------
# OUTCOME columns stay in the table — minutes and started_60 are the TARGET.
# The rule is that they may never be same-row PREDICTORS. Lagged and rolling
# versions, built in Phase 3, are legitimate.

fpl_modelling <- fpl |> select(-any_of(LAG_COLS))

saveRDS(fpl,           "data/fpl_fixture_level.rds")
saveRDS(fpl_modelling, "data/fpl_modelling_base.rds")
write_csv(readRDS("cache/player_map.rds"), "data/player_map.csv")


# --- DIAGNOSTICS ------------------------------------------------------------

# Conditional start rate. The UNCONDITIONAL rate cannot respond to rest —
# exactly eleven players start every match, so rest changes WHICH players
# start, never how many. The question the model asks is: given a player
# started last time, does short rest make him less likely to start again?
rest_effect <- function(df, var) {
  df |>
    filter(calendar_complete, started_prev,
           !is.na(true_rest), !is.na(club_rest_pl)) |>
    mutate(b = cut(.data[[var]], c(0, 3.5, 4.5, 5.5, 7.5, Inf),
                   labels = c("<=3", "4", "5", "6-7", "8+"))) |>
    group_by(b) |>
    summarise(rows = n(),
              pct_start_again = round(100 * mean(started_60), 1),
              .groups = "drop") |>
    mutate(measure = var)
}

cp_open(7, "finalise")

cp_kv("rows" = format(nrow(fpl), big.mark = ","),
      "columns dropped" = paste(dropped, collapse = ", "),
      "columns lagged" = paste(LAG_COLS, collapse = ", "),
      "canonical target" = "started_60 (minutes >= 60)",
      "distinct players" = n_distinct(fpl$player_uid),
      "seasons" = n_distinct(fpl$season),
      "rows with true_rest" = format(sum(!is.na(fpl$true_rest)), big.mark = ","))

cp_section("D1. target distribution by season")
cp_table(fpl |> count(season, appearance) |>
           pivot_wider(names_from = appearance, values_from = n) |>
           mutate(pct_start = round(100 * start / (start + cameo + unused), 1)),
         max_rows = 10)

cp_section("D2. where the API `starts` column is unusable")
cat("broken = starts==0 with 60+ minutes played. 2022-23 GW1-15 is dead.\n\n")
cp_table(starts_reliable |> group_by(season) |>
           summarise(gws = n(), gws_broken = sum(!ok),
                     worst_gw_broken_rows = max(broken),
                     .groups = "drop"), max_rows = 6)

cp_section("D3. agreement where `starts` IS reliable")
cp_table(fpl |> filter(!is.na(started_api)) |>
           group_by(season) |>
           summarise(rows = n(),
                     agree_pct = round(100 * mean(started_api == started_60), 2),
                     started_but_under_60 = sum(started_api & !started_60),
                     sub_but_over_60 = sum(!started_api & started_60),
                     .groups = "drop"), max_rows = 6)

cp_section("D4. congestion exposure, true rest (calendar seasons)")
cp_table(fpl |> filter(calendar_complete, !is.na(true_rest)) |>
           group_by(season) |>
           summarise(rows = n(),
                     pct_short_true = round(100 * mean(true_rest < 4), 1),
                     pct_hidden = round(100 * mean(hidden_fixture), 1),
                     # NA where a club's first league match followed a cup or
                     # European qualifier: true_rest exists, club_rest_pl does not.
                     mean_overstatement = round(mean(rest_overstatement,
                                                     na.rm = TRUE), 2),
                     .groups = "drop"), max_rows = 6)

cp_section("D5. THE HEADLINE — same question, two rest measures")
cat("given a player started last match, does he start again?\n\n")
cp_table(bind_rows(rest_effect(fpl, "club_rest_pl"),
                   rest_effect(fpl, "true_rest")) |>
           select(measure, b, rows, pct_start_again) |>
           pivot_wider(names_from = measure,
                       values_from = c(rows, pct_start_again)),
         max_rows = 8)

cp_section("D6. final column inventory")
cp_table(tibble(column = names(fpl)) |>
           mutate(role = case_when(
             column %in% c("started_60", "minutes", "appearance") ~ "TARGET",
             column %in% OUTCOME_COLS      ~ "OUTCOME (never a same-row feature)",
             str_detect(column, "_lag1$|_prev$") ~ "FEATURE (lagged)",
             column %in% SNAPSHOT_COLS     ~ "FEATURE (deadline snapshot)",
             TRUE                          ~ "FEATURE / key")) |>
           count(role, name = "columns"), max_rows = 6)

cp_close()


# ===========================================================================
# PHASE 1 COMPLETE — WHAT WAS FOUND
# ---------------------------------------------------------------------------
#   1. Assistant Manager elements sitting in the player table (2024-25)
#   2. "GKP" position label deleting 101 real goalkeeper rows (2021-22)
#   3. Postponement placeholders — 32 players recorded as 0 minutes when they
#      had played 90
#   4. Two different Ben Davieses merged into one career
#   5. The leakage verdict inverted, nearly lagging four safe columns
#   6. Rest days blind to every European and cup fixture — 22.8% of league
#      matches, 1.12 days of overstatement, concentrated entirely in the
#      control group
#   7. The `starts` column unpopulated for 2022-23 GW1-15
#
# HEADLINE FINDING
#   Measured on the league calendar alone, 4,596 player-matches follow three
#   days' rest or less. Counted across all competitions it is 7,885 — 72% more.
#   The 6-7 day bucket shrinks from 10,471 to 7,696 because a quarter of those
#   "full week" cases were nothing of the sort. True rest also produces a clean
#   monotonic gradient (75.3 -> 80.8% start-again) where the league-only
#   measure is non-monotonic. Anyone modelling rotation from FPL data alone
#   cannot see most of the congestion.
#
# NEXT — IN ORDER
#   0. THE AVAILABILITY LOGGER. Still not started, and the only unrecoverable
#      item. `status`, `chance_of_playing_next_round` and the news string are
#      live-only and overwritten. Without them a zero-minute row cannot be
#      split into injured / suspended / rotated, and the model learns "zero
#      last week -> zero this week". Twenty lines and a cron job. The season
#      starts 21 August.
#   1. Baselines (Phase 2). Three rolling benchmarks scored by log loss and
#      Brier before any feature work, so you cannot be impressed by a model
#      that fails to beat "started last week".
#   2. European calendar for 2025-26, absent, and 2024-25, partial. Needed
#      before the model runs weekly. Try football-data.org's free tier.
#   3. Decide on 2019-20 — no team or position, so no calendar join and no
#      congestion features. Minutes history only, or drop it.
# ===========================================================================----