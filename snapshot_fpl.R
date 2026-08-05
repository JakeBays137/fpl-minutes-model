#!/usr/bin/env Rscript
# ===========================================================================
# PHASE 0 - DAILY AVAILABILITY SNAPSHOT
# Captures status, chance_of_playing and news. These are live-only fields:
# FPL overwrites them whenever they change and they cannot be reconstructed
# later. Every missed run is training data that no longer exists anywhere.
# ===========================================================================

suppressPackageStartupMessages({
  library(jsonlite)
  library(dplyr)
  library(readr)
})

OUT_DIR <- "data/snapshots"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

log_line <- function(...) {
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " ", ..., "\n", sep = "")
}

now <- Sys.time()

# Season runs July to June, so July onward belongs to the season starting
# that calendar year. Rolls over without editing.
fpl_season <- function(t) {
  y <- as.integer(format(t, "%Y"))
  m <- as.integer(format(t, "%m"))
  if (m >= 7) sprintf("%d-%02d", y, (y + 1) %% 100)
  else        sprintf("%d-%02d", y - 1, y %% 100)
}

# Retried once: a single transient failure should not cost a day of data.
fetch_bootstrap <- function(attempts = 2) {
  for (i in seq_len(attempts)) {
    out <- try(
      fromJSON("https://fantasy.premierleague.com/api/bootstrap-static/",
               flatten = TRUE),
      silent = TRUE
    )
    if (!inherits(out, "try-error")) return(out)
    log_line("fetch attempt ", i, " failed")
    if (i < attempts) Sys.sleep(30)
  }
  NULL
}

boot <- fetch_bootstrap()
if (is.null(boot)) {
  log_line("FETCH FAILED - no snapshot written")
  quit(status = 1)
}

# Gameweek context. Without it a snapshot means nothing: a chance_of_playing
# figure only informs you if you know which deadline it preceded, and by how
# long. hours_to_deadline is the most useful field in the file.
events  <- as_tibble(boot$events)
next_ev <- events |> filter(is_next) |> slice(1)
curr_ev <- events |> filter(is_current) |> slice(1)

next_gw <- if (nrow(next_ev)) next_ev$id else NA_integer_
curr_gw <- if (nrow(curr_ev)) curr_ev$id else NA_integer_
next_deadline <- if (nrow(next_ev)) {
  as.POSIXct(next_ev$deadline_time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
} else as.POSIXct(NA)

teams <- boot$teams |> select(team_id = id, team = short_name)

snap <- boot$elements |>
  select(element = id, web_name, first_name, second_name,
         team_id = team, element_type,
         status, chance_of_playing_this_round, chance_of_playing_next_round,
         news, news_added,
         now_cost, cost_change_event, selected_by_percent, form,
         minutes, starts, total_points,
         transfers_in_event, transfers_out_event, ep_this, ep_next) |>
  left_join(teams, by = "team_id") |>
  mutate(season = fpl_season(now),
         snapshot_time = now,
         snapshot_date = as.Date(now),
         current_gw = curr_gw,
         next_gw = next_gw,
         next_deadline = next_deadline,
         hours_to_deadline = round(as.numeric(difftime(next_deadline, now,
                                                       units = "hours")), 2))

# Timestamped so twice-daily runs never overwrite each other. Gzipped because
# every one of these gets committed to the repo.
stamp <- format(now, "%Y%m%d_%H%M")
path  <- file.path(OUT_DIR, sprintf("bootstrap_%s.csv.gz", stamp))
write_csv(snap, path)

flagged   <- sum(snap$status != "a", na.rm = TRUE)
doubtful  <- sum(!is.na(snap$chance_of_playing_next_round) &
                   snap$chance_of_playing_next_round < 100)
with_news <- sum(!is.na(snap$news) & nzchar(snap$news))

log_line("wrote ", nrow(snap), " rows to ", path)
log_line("season ", snap$season[1], " | next GW ", next_gw,
         " | ", snap$hours_to_deadline[1], "h to deadline")
log_line("flagged ", flagged, " | doubtful ", doubtful,
         " | with news ", with_news)

