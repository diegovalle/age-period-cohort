########################################################
## Author: Diego Valle-Jones
## Website: www.diegovalle.net
## Date Created: Mon Oct  8 20:50:51 2012
## Email: diegovalle at gmail.com
## Purpose:  Load the data sets necessary for the APC model 
## Copyright (c) Diego Valle-Jones. All rights reserved

## A big data frame with homicides and population by age group at the municipality level
load("data/agerates.RData")
## Municipality names and coordinates
mun.heads <- read.csv("data/municipality-heads.csv")
## Data from Sinais on homicides at the national level since 1985
rates85 <- read.csv("data/rates.csv")

## Ignore homicides where the age of the victim was unknown
agerates <- na.omit(agerates)

## Test the data makes sense
ddply(subset(agerates, Sex == "Total"), .(Year), summarise, sum = sum(Homicides))
ddply(subset(agerates, Sex == "Total"), .(Year), summarise,
      sum = sum(Population, na.rm = TRUE))

## Merge the lat/long with the homicide data
agerates <- merge(mun.heads[,c("DistUSBorder", "id")], agerates, by = "id")
agerates$StateCode <- floor(agerates$id / 1000)

## border municipalities
mun200k <- subset(agerates, DistUSBorder <= 200)



mun200k.agerates <- ddply(mun200k, .(Year, StateCode, Sex, AgeGroup),
                          summarise,
                          Homicides = sum(Homicides),
                          Population = sum(Population),
                          .progress = "text")
message("preparing homicides by age group at the state level")
state.agerates <- ddply(agerates, .(Year, StateCode, Sex, AgeGroup),
                        summarise,
                        Homicides = sum(Homicides),
                        Population = sum(Population),
                        .progress = "text",
                        .parallel = TRUE)


##Data for the historic homicide plot
##hh from http://www.mexicomaxico.org/Voto/Homicidios100M.htm
hh <- read.csv("data/historical-homicides.csv")
pop <- read.csv("data/homicides79-11.csv") 
pop$Population <- na.spline(pop$Population)
pop$rate <- pop$Homicides / pop$Population * 10^5

