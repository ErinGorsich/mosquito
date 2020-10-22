# format_sero.R: Format the serological data ready for model fitting.

# Clear the workspace.
rm(list = ls())

# Load in data manipulation libraries.
library(tidyr)
library(dplyr)

# Load in the raw serological cattle data.
sero <- read.csv("../data/raw/ndumo_cattle.csv", header=TRUE)

# Convert the date column to a date type.
sero$DATE <- as.Date(sero$date, format="%d%b%Y")

# Round down the animal age to:
# (i) an integer,
# (ii) maximum of age 9.
sero <- sero %>%
  mutate(AGE = floor(age_y)) %>%
  mutate(AGE = ifelse(AGE > 9, 9, AGE))
df <- sero

# Place into an age group. If age group is unknown,
# mark as -1.
sero <- sero %>%
  mutate(AGE_GROUP = ifelse(is.na(AGE), -1, AGE + 1))

# Start locations from zero for ease of use.
sero <- sero %>%
  mutate(LOCATION_ID = place - 1)

# Get the year, month and epi-week of the reads.
sero <- sero %>%
  mutate(YEAR = as.numeric(format(DATE, format="%Y"))) %>%
  mutate(MONTH = as.numeric(format(DATE, format="%m")))

# Summarise number of animals tested and positive results.
sero <- group_by(sero, LOCATION_ID, YEAR, MONTH, AGE_GROUP) %>%
  summarise(N_TESTED = n(),
            N_POSITIVE = sum(rvf))

# Organise columns of the data.
sero <- sero %>%
  select(LOCATION_ID, N_POSITIVE, N_TESTED, AGE_GROUP, YEAR, MONTH)

# Sort by year, month, age and location.
sero <- sero %>%
  arrange(YEAR, MONTH, LOCATION_ID, AGE_GROUP)

# Write the formatted serological data to file.
write.csv(sero, "../data/seroprev.csv", row.names=FALSE)

############################################################
# Summarise number of animals tested and positive results.
# Erin's additions... 
############################################################
library(lubridate)
t <- data.frame(table(df$animal_id))
length(t[,1]) # 623 unique animals

# Are capture periods distinct?
hist(c(dist(df$DATE)), breaks = 30, main = "",
     xlab = "Pairwise difference (days between sampling)") 
min <- c(dist(df$DATE))
hist(min[min < 50], main = "",
     xlab = "Pairwise difference (days between sampling)") # maybe...

# For some sites more than others... 
par(mfrow = c(3,3))
for (place in unique(df$place)){
  temp <- df[df$place == place,]
  min <- c(dist(temp$DATE))
  hist(min[min < 200], breaks = 30, main = as.character(place), xlab = "Pairwise difference")
}
