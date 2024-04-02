library(tidyverse)
library(ggplot2)
library(knitr)

wages = read.csv("wages.csv")
fuel = read.csv("fuel.csv")

d = wages %>%
  group_by(Age.group) %>%
  reframe(Age.group) %>%
  unique()

View(d)