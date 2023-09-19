library(shiny)
library(rlang)
library(tidyverse)
library(ggplot2)
library(broom)
library(here)
library(shinydashboard)
library(DT)
library(patchwork)

theme_set(theme_bw())

bars <- read_csv("data/bars.csv")
bars <- bars %>% mutate(
  No = Total - Yes,
  y = Yes/Total
)

dframe <- data.frame(expand.grid(
  distance=unique(bars$distance), 
  signal=seq(-10,10, by=0.1),
  intensity = unique(bars$intensity)))  

dataset <- bars


