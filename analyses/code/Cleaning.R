# Import ------------------------------------------------------------------

#Libraries
source("Packages.R")

#setwd
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Load data
data <- fread("..//..//data//raw//FramingAndTrust.csv")

# Cleaning ----------------------------------------------------------------

#variable conversions
data$Gender <- as.factor(data$Gender)
data$Condition <- as.factor(data$Condition)

data <- data%>%
  separate(
    col = RecordedDate,
    into = c("Date", "Time"),
    sep = " "
  )
data$Date <- as.Date(data$Date, "%d.%m.%Y")
data$Time <- as.POSIXct(data$Time, "%H:%M", tz = "CET")

#delete variables
data <- data[,ResponseId := NULL]

#rename variables
data <- data %>%
  rename(
    Duration = `Duration (in seconds)`,
    )

#recode variables
data <- data%>%
  mutate(
    
  )
