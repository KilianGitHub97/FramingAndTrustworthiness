# Import ------------------------------------------------------------------

#Libraries
source("Packages.R")

#setwd
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Load data
data <- fread("..//..//data//raw//FramingAndTrust.csv")

# Cleaning ----------------------------------------------------------------

#variable conversion
data$Gender <- as.factor(data$Gender)
data$Condition <- as.factor(data$Condition)
data$RecordedDate <- as.POSIXct(
  data$RecordedDate,
  format = "%d.%m.%Y %H:%M",
  tz = "CET"
  )

#rename variables
data <- data %>%
  rename(
    Duration = "Duration (in seconds)",
    Date = "RecordedDate"
    )

#recode variables
data <- data %>%
  mutate(
    ConsentConfirm = recode(
    ConsentConfirm,
    "Ich habe die aufgefÅhrten Bedingungen gelesen und verstanden. Mit der Auswahl dieser Antwort best"tige ich mein Einverst"ndnis zur Teilnahme an dieser Studie." = 1,
    "Nein, ich bin mit den Bedingungen nicht einverstanden oder habe mich aus anderen GrÅnden entschieden, nicht an der Studie teilzunehmen." = 2
    ))

#delete invalid responses
data <- data %>%
  filter(Duration < 3000) %>%
  filter(Duration > 120) %>%
  filter(Progress == 100) %>%
  filter(ConsentConfirm == 1)

#delete variables
data <- data %>%
  select(
    -ResponseId,
    -Progress,
    -ConsentConfirm
  )

# Export ------------------------------------------------------------------

saveRDS(
  data,
  file = "..//..//data//processed//FramingAndTrust_clean.rds", 
  compress = TRUE
  )