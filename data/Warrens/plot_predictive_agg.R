# plot_predictive_agg.R: Visualise the predictive posterior distribution of
# the model fit. For now, just visualise the data. Aggregate across age groups.

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
  group_by(LOCATION_ID, YEAR, MONTH) %>%
  summarise(N_POSITIVE = sum(N_POSITIVE),
            N_TESTED = sum(N_TESTED)) %>%
  ungroup() %>%
  mutate(DATA_ID = 1:n()) %>%
  mutate(SERO_PREV = N_POSITIVE / N_TESTED) %>%
  mutate(LOWER_CI = binom.confint(N_POSITIVE, N_TESTED, method="exact")$lower) %>%
  mutate(UPPER_CI = binom.confint(N_POSITIVE, N_TESTED, method="exact")$upper) %>%
  mutate(DATE = as.Date(paste(YEAR, MONTH, 15, sep="-")))

# Change the group ID.
sero <- sero %>%
  mutate(GROUP_ID = as.numeric(factor(paste(LOCATION_ID, YEAR, MONTH)))) %>%
  mutate(LABEL = factor(paste(LOCATION_ID, sep="-")))

# Visualise the predictive posterior distributions.
gg_pred_time <- ggplot(sero, aes(x=DATE, y=SERO_PREV)) +
  geom_errorbar(aes(ymin=LOWER_CI, ymax=UPPER_CI), width=14, colour="grey75") +
  geom_point(colour="grey60") +
  geom_vline(xintercept=0, linetype="dashed", colour="grey50") +
  geom_smooth(method="loess", formula="y~x", se=FALSE) +
  scale_x_date() +
  scale_y_continuous(breaks=c(0, 0.5, 1.0)) +
  xlab("Age group") +
  ylab("Seroprevalence") +
  facet_wrap(~LABEL) +
  gg_theme +
  theme(panel.border=element_rect(colour="black", fill=NA)) +
  theme(axis.text.x = element_text(angle=60, size=8)) +
  theme(legend.position="right")
print(gg_pred_time)
