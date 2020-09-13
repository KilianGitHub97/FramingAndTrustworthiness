pkgs <- c(
  "tidyverse",
  "data.table",
  "qgraph",
  "rstatix"
)
lapply(pkgs, library, character.only = TRUE)
