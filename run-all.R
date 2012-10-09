########################################################
# Author: Diego Valle-Jones
# Website: www.diegovalle.net
# Date Created: Oct 1 2012 17:09:48 2011
# Email: diegovalle at gmail.com
# Purpose: Age-Period-Cohort analysis of homicides in Mexico
# Copyright (c) Diego Valle-Jones. All rights reserved


library(ggplot2)
library(stringr)
library(stringr)
library(car)
library(Hmisc)
library(Cairo)
library(plyr)
library(reshape)
library(doMC)
library(directlabels)
library(zoo)
library(Epi)
registerDoMC(4) ##Change to your number of cores
theme_set(theme_bw())
options(stringAsFactors = FALSE)

##Settings
year.period <- 2003  ## Year of reference for the period
year.cohort <- 1970  ## Year of reference for the cohort and age
sex <- "Total"

source(file.path("src", "load-data.R"))
source(file.path("src", "historic-plot.R"))
source(file.path("src", "utils.R")) #Functions to save plots, etc
source(file.path("src", "since85.R")) #Cohorts for all of mexico since '85
source(file.path("src", "age-period-cohort.R")) #The Age-Period-Cohort model

