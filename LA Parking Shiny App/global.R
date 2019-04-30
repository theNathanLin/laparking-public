library(tidyverse)
library(shiny)
library(shinyWidgets)
library(scales)
library(ggmap)

load("la1.RData")
load("la_bw.RData")
load("laparking_2.RData")
load("violations_list.RData")
load("agencies.RData")

states_non_ca <- levels(factor(laparking_2$`RP State Plate`))
states_non_ca <- states_non_ca[!states_non_ca %in% "CA"]
