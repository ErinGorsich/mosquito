# plot_predictive.R: Visualise the predictive posterior distribution of
# the model fit. For now, just visualise the data.

# Clear the workspace.
rm(list = ls())

# Load the data manipulation and visualisation libraries.
library(ggplot2)
library(tidyr)
library(dplyr)
library(binom)

# Load in the favourite ggplot theme.
source("gg_theme.R")

# Load in the serological data.
sero <- read.csv("../data/seroprev.csv", header=TRUE)

# Label the ID of each data entry and calculate seroprevalence.
sero <- sero %>%
  mutate(DATA_ID = 1:n()) %>%
  mutate(SERO_PREV = N_POSITIVE / N_TESTED) %>%
  mutate(LOWER_CI = binom.confint(N_POSITIVE, N_TESTED, method="exact")$lower) %>%
  mutate(UPPER_CI = binom.confint(N_POSITIVE, N_TESTED, method="exact")$upper)

# Change the group ID.
sero <- sero %>%
  mutate(GROUP_ID = as.numeric(factor(paste(AGE_GROUP, LOCATION_ID, YEAR, MONTH)))) %>%
  mutate(LABEL = factor(paste(LOCATION_ID, YEAR, sprintf("%02d", MONTH), sep="-")))

# Define the age labels.
age_labels <- paste(seq(0, 9), seq(1, 10), sep="–")
age_labels[10] <- "9+"

# Visualise the predictive posterior distributions.
gg_pred <- ggplot(sero, aes(x=AGE_GROUP, y=SERO_PREV, group=DATA_ID)) +
  geom_errorbar(aes(ymin=LOWER_CI, ymax=UPPER_CI), width=0.75, colour="grey75") +
  geom_point(colour="grey60") +
  geom_vline(xintercept=0, linetype="dashed", colour="grey50") +
  scale_x_continuous(breaks=c(-2, -1, seq(1,10)),
                     labels=c("1+", "0+", age_labels)) +
  scale_y_continuous(breaks=c(0, 0.5, 1.0)) +
  xlab("Age group") +
  ylab("Seroprevalence") +
  facet_wrap(~LABEL) +
  gg_theme +
  theme(panel.border=element_rect(colour="black", fill=NA)) +
  theme(axis.text.x = element_text(angle=60, size=8)) +
  theme(legend.position="right")
print(gg_pred)

# just focus on the first location.
gg_pred_first <- filter(sero, LOCATION_ID == 0) %>%
  ggplot(aes(x=AGE_GROUP, y=SERO_PREV, group=DATA_ID)) +
  geom_errorbar(aes(ymin=LOWER_CI, ymax=UPPER_CI), width=0.75, colour="grey75") +
  geom_point(colour="grey60") +
  geom_vline(xintercept=0, linetype="dashed", colour="grey50") +
  scale_x_continuous(breaks=c(-2, -1, seq(1,10)),
                     labels=c("1+", "0+", age_labels)) +
  scale_y_continuous(breaks=c(0, 0.5, 1.0)) +
  xlab("Age group") +
  ylab("Seroprevalence") +
  facet_wrap(~LABEL) +
  gg_theme +
  theme(panel.border=element_rect(colour="black", fill=NA)) +
  theme(axis.text.x = element_text(angle=60, size=8)) +
  theme(legend.position="right")
print(gg_pred_first)
