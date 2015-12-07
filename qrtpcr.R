############################################################################
# Bioramble
# Analyzing quantitative PCR data the tidy way
# by Jesse Lipp
# created: Dec 7, 2015
############################################################################

# --------------------------------------------------------------------------
# Set up environment
# --------------------------------------------------------------------------
# clean-up
rm(list = ls())

# libraries
if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}
if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}
if (!require(tidyr)) {
  install.packages("tidyr")
  library(tidyr)
}
if (!require(broom)) {
  install.packages("broom")
  library(broom)
}

setwd("~/Box Sync/bioramble/data_processing/")

# --------------------------------------------------------------------------
# Generate the data
# --------------------------------------------------------------------------
set.seed(321)
messy <- matrix(rnorm(27, mean = c(20, 25, 21), sd = rep(c(1, 2, 1, 3, 2, 3, 1, 1, 1), each = 3)), nrow = 3)
rownames(messy) <- c("control", "mutant1", "mutant2")
colnames(messy) <- paste(rep(paste0("exp", 1:3), each = 3), paste0("rep", 1:3), sep = "_")

# --------------------------------------------------------------------------
# Data cleaning
# --------------------------------------------------------------------------
tidy <- data.frame(messy) %>%
  # make row names a column
  mutate(genotype = rownames(messy)) %>%
  # make each row a single measurement 
  gather(key = sample, value = measurement, -genotype) %>%
  # make each column a single variable
  separate(col = sample, into = c("experiment", "replicate"), sep = "_")

data <- tidy %>%
  # calculate mean of technical replicates by genotype and experiment
  group_by(genotype, experiment) %>%
  summarise(measurement = mean(measurement)) %>%
  ungroup()

# --------------------------------------------------------------------------
# Statistical analysis
# --------------------------------------------------------------------------
mod <- data %>%
  # set "control" as reference
  mutate(genotype = relevel(factor(genotype), ref = "control")) %>%
  # one-way anova and Tukey's post hoc test
  do(tidy(TukeyHSD(aov(measurement ~ genotype, data = .))))

# --------------------------------------------------------------------------
# Visualization
# --------------------------------------------------------------------------
# genotype will be on the x-axis, measurements on the y-axis
ggplot(data, aes(x = genotype, y = measurement, col = experiment)) + 
  # plot the mean of each genotype as a cross
  stat_summary(fun.y = "mean", geom = "point", color = "black", shape = 3, size = 5) +
  # plot the 95% confidence interval for each genotype
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", color = "black", width = 0.1) + 
  # we we add the averaged measurements for each experiment
  geom_point(shape = 16, size = 5) +
  theme_classic()