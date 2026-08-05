#!/usr/bin/env Rscript
# ===========================================================================
# PHASE 0 — DAILY AVAILABILITY SNAPSHOT
# ---------------------------------------------------------------------------
# WHY THIS EXISTS
#   The archive records that a player got zero minutes. It never records WHY.
#   Injured, suspended, benched-and-unused, dropped, and not-in-squad all look
#   identical in merged_gw.csv. A model trained on that learns "zero last week
#   -> zero this week", which is autocorrelation dressed up as prediction, and
#   it fails hardest at the moment that matters: the week a player returns.
#
#   The availability fields (status, chance_of_playing_*, news) exist ONLY as a
#   live snapshot. FPL overwrites them whenever they change and they cannot be
#   reconstructed afterwards. Every day this does not run is a day of training
#   data that no longer exists anywhere.
#
#   This is the only unrecoverable item in the whole project. Everything else
#   can be rebuilt from archives at any point.
#
# SCHEDULING
#   Twice daily. Team news breaks on Friday mornings for Saturday fixtures, so
#   a single evening capture misses the window where the information actually
#   moves.
#
#   cron (Linux/macOS) — 07:00 and 19:00:
#     0 7,19 * * * cd /path/to/project && /usr/bin/Rscript snapshot_fpl.R \
#       >> logs/snapshot.log 2>&1
#
#   Windows Task Scheduler:
#     Program:   C:\Program Files\R\R-4.6.1\bin\Rscript.exe
#     Arguments: snapshot_fpl.R
#     Start in:  C:\path\to\project
#     Trigger:   daily, repeat every 12 hours
#
#   Check it is actually running: after a week, data/snapshots/ should hold
#   ~14 files. An empty directory means the schedule is not firing, and that
#   failure is silent unless you look.
#
# NOTE: no install.packages() anywhere in this file. A scheduled script must
# never install — it retries on every run and can hang waiting for a mirror.
# ===========================================================================

suppressPackageStartupMessages({
  library(jsonlite)
  library(dplyr)
  library(readr)
})

OUT_DIR <- "data/snapshots"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create("logs",  recursive = TRUE, showWarnings = FALSE)

log_line <- function(...) {
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " ", ..., "\n", sep = "")
}

now <- Sys.time()

# Season runs July to June, so anything from July onwards belongs to the
# season starting that calendar year. Rolls over correctly without editing.
fpl_season <- function(t) {
  y <- as.integer(format(t, "%Y"))
  m <- as.integer(format(t, "%m"))
  if (m >= 7) sprintf("%d-%02d", y, (y + 1) %% 100)
  else        sprintf("%d-%02d", y - 1, y %% 100)
}


# --- FETCH ------------------------------------------------------------------
# Wrapped so a failure logs and exits non-zero rather than dying silently in
# cron. Retried once — the FPL API drops requests occasionally, and a single
# transient failure should not cost a day of data.

fetch_bootstrap <- function(attempts = 2) {
  for (i in seq_len(attempts)) {
    out <- try(
      fromJSON("https://fantasy.premierleague.com/api/bootstrap-static/",
               flatten = TRUE),
      silent = TRUE
    )
    if (!inherits(out, "try-error")) return(out)
    log_line("fetch attempt ", i, " failed: ", attr(out, "condition")$message)
    if (i < attempts) Sys.sleep(30)
  }
  NULL
}

boot <- fetch_bootstrap()

if (is.null(boot)) {
  log_line("FETCH FAILED after retries — no snapshot written")
  quit(status = 1)
}


# --- GAMEWEEK CONTEXT -------------------------------------------------------
# Without this a snapshot means nothing. "Chance of playing 25%" only tells you
# something if you know which deadline it preceded and by how long.
# hours_to_deadline is the most useful field in the file — news six hours out
# is worth far more than news six days out, and it lets you weight a snapshot
# by how settled the information was.

events <- as_tibble(boot$events)

next_ev <- events |> filter(is_next) |> slice(1)
curr_ev <- events |> filter(is_current) |> slice(1)

next_gw <- if (nrow(next_ev)) next_ev$id else NA_integer_
curr_gw <- if (nrow(curr_ev)) curr_ev$id else NA_integer_

next_deadline <- if (nrow(next_ev)) {
  as.POSIXct(next_ev$deadline_time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
} else as.POSIXct(NA)


# --- SNAPSHOT ---------------------------------------------------------------

teams <- boot$teams |> select(team_id = id, team = short_name)

snap <- boot$elements |>
  select(
    element = id, web_name, first_name, second_name,
    team_id = team, element_type,
    
    # THE POINT OF ALL THIS — live-only, overwritten, unrecoverable.
    #   status: a=available, d=doubtful, i=injured, s=suspended,
    #           u=unavailable, n=on loan / not in squad
    status,
    chance_of_playing_this_round,
    chance_of_playing_next_round,
    news, news_added,
    
    # Context that also moves and is cheap to keep alongside.
    now_cost, cost_change_event, selected_by_percent, form,
    minutes, starts, total_points,
    transfers_in_event, transfers_out_event,
    ep_this, ep_next
  ) |>
  left_join(teams, by = "team_id") |>
  mutate(
    # season is essential: element ids reset every year, so without it two
    # seasons of snapshots cannot be told apart or joined to anything.
    season            = fpl_season(now),
    snapshot_time     = now,
    snapshot_date     = as.Date(now),
    current_gw        = curr_gw,
    next_gw           = next_gw,
    next_deadline     = next_deadline,
    hours_to_deadline = round(as.numeric(difftime(next_deadline, now,
                                                  units = "hours")), 2)
  )


# --- WRITE ------------------------------------------------------------------
# One timestamped file per run, so twice-daily captures never overwrite each
# other and a manual re-run cannot destroy an earlier one. Roughly 200KB a day.

stamp <- format(now, "%Y%m%d_%H%M")
path  <- file.path(OUT_DIR, sprintf("bootstrap_%s.csv", stamp))

ok <- try(write_csv(snap, path), silent = TRUE)

if (inherits(ok, "try-error")) {
  log_line("WRITE FAILED: ", attr(ok, "condition")$message)
  quit(status = 1)
}


# --- HEALTH CHECK -----------------------------------------------------------
# Printed to the log so a problem is visible without opening any files.
# Flagged counts sitting at zero for weeks means a broken fetch, not an
# unusually healthy Premier League.

flagged  <- sum(snap$status != "a", na.rm = TRUE)
doubtful <- sum(!is.na(snap$chance_of_playing_next_round) &
                  snap$chance_of_playing_next_round < 100)
with_news <- sum(!is.na(snap$news) & nzchar(snap$news))
files_so_far <- length(list.files(OUT_DIR, pattern = "\\.csv$"))

log_line("OK  season ", snap$season[1],
         " | ", nrow(snap), " players",
         " | next GW ", next_gw,
         " | ", snap$hours_to_deadline[1], "h to deadline")
log_line("    flagged ", flagged,
         " | doubtful ", doubtful,
         " | with news ", with_news,
         " | snapshots on disk ", files_so_far)

# Expect the player count to CLIMB through August as clubs register squads
# (~570 in early preseason, ~700+ by September). A sharp DROP mid-season means
# something is wrong with the fetch.


# ===========================================================================
# READING THE SNAPSHOTS BACK — for Phase 3, not needed yet
# ---------------------------------------------------------------------------
# library(dplyr); library(readr); library(purrr)
#
# snapshots <- list.files("data/snapshots", full.names = TRUE, pattern = "\\.csv$") |>
#   map_dfr(read_csv, col_types = cols(.default = col_character())) |>
#   type_convert()
#
# The row that matters is the LAST snapshot before each deadline — that is the
# information state a manager actually had when picking.
#
# pre_deadline <- snapshots |>
#   filter(hours_to_deadline > 0) |>
#   group_by(season, next_gw, element) |>
#   slice_min(hours_to_deadline, n = 1, with_ties = FALSE) |>
#   ungroup() |>
#   transmute(season, gw = next_gw, element,
#             status, chance_of_playing_next_round, news,
#             hours_before_deadline = hours_to_deadline)
#
# Joined to the fixture-level table on (season, gw, element), this finally
# splits a zero-minute row into:
#
#   status "i" / "s"       -> unavailable, KNOWN before the deadline.
#                             Exclude from the rotation model — predicting that
#                             an injured player will not start is not skill.
#   status "a", 0 minutes  -> ROTATED. This is the population the model exists
#                             to predict, and until now it was indistinguishable
#                             from the rows above.
#   status "d"             -> doubtful. The genuinely uncertain middle, and the
#                             band where a good model earns its keep.
#
# Also worth keeping: the CHANGE in chance_of_playing between snapshots. A
# player moving 25% -> 75% in the 24 hours before a deadline is a much stronger
# signal than either figure on its own.
# ===========================================================================