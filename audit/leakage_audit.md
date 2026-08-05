# Leakage audit
_Generated 2026-08-03 20:04_

## Method
For every column: would this value have been known before the deadline?

## Snapshot timing test
If a column holds the value AT deadline t, its change from t to t+1 equals
the transfer traffic recorded at t+1 — so a high correlation with the NEXT
row indicates a deadline snapshot, not a post-hoc scrape.

- value: cor_this=0.339, cor_next=0.447 -> DEADLINE snapshot — safe unlagged
- selected: cor_this=0.287, cor_next=0.95 -> DEADLINE snapshot — safe unlagged

## xP
- All zero-minute rows: 72.8% have xP exactly 0
- Surprising benchings only (started 60+ last match): 39% zeros, 5.2% above 4
- Probably pre-deadline, but lagged as a precaution and to keep the Phase 2
  baseline comparison clean.

## Column classification
- DROP — empty or redundant: mng_clean_sheets, mng_draw, mng_goals_scored, mng_loss, mng_underdog_draw, mng_underdog_win, mng_win, modified, round
- LAG — ambiguous timing, use xP_lag1: xP
- OUTCOME — target only, never a same-row feature: assists, bonus, bps, clean_sheets, clearances_blocks_interceptions, creativity, defensive_contribution, expected_assists, expected_goal_involvements, expected_goals, expected_goals_conceded, goals_conceded, goals_scored, ict_index, influence, minutes, own_goals, penalties_missed, penalties_saved, recoveries, red_cards, saves, starts, tackles, team_a_score, team_h_score, threat, total_points, yellow_cards
- SAFE — known before the deadline: calendar_complete, club_rest_pl, date, days_since_prev, element, fixture, gw, hidden_fixture, is_restart, kickoff_time, match_index, name, name_clean, opponent_team, player_uid, position, rest_overstatement, season, short_rest_true, team, true_rest, was_home
- SNAPSHOT — deadline value, safe unlagged: price, selected, transfers_balance, transfers_in, transfers_out, value
