# Excludes the big regenerable files but keeps the snapshots, which cannot
# be rebuilt. data/*.rds only matches .rds files directly in data/, so
# data/snapshots/ passes through untouched.
writeLines(c(
  "cache/",
  "data/*.rds",
  "logs/",
  ".Rhistory",
  ".RData",
  ".Rproj.user/"
), ".gitignore")

dir.create(".github/workflows", recursive = TRUE)

file.exists(".gitignore")
dir.exists(".github/workflows")