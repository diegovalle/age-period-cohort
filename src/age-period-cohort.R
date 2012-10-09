


APCClean <- function(df){
  ## Description: 
  ##  Cleans data for the apc.fit function
  ## Args:
  ##   df - the data.frame to clean
  ##
  ## Returns: 
  ##   A nice data.frame with the weird A,D,Y,C names apc.fit uses

  message("Cleaning data for the APC model")
  df <- ddply(df, .(Year, Sex, AgeGroup),
                        summarise,
                        Homicides = sum(Homicides),
                        Population = sum(Population),
                        .progress = "text")
  df <- subset(df, AgeGroup != "85plus")
  df <- subset(df, AgeGroup != "NA")

  df$AgeGroup <- car::recode(as.factor(df$AgeGroup),'"0.4"=2.5;
       "5.9" = 7.5; "10.14" = 12.5; "15.19" = 17.5; "20.24" = 22.5; "25.29" = 27.5;
                        "30.34" = 32.5; "35.39" = 37.5; 
                        "40.44" = 42.5; "45.49" = 47.5; "50.54" = 52.5; "55.59" = 57.5; "60.64"=62.5; "65.69"=67.5; 
                        "70.74" = 72.5; "75.79" = 77.5; "80.84" = 82.5 ;')
  df$AgeGroup <- as.numeric(as.character(df$AgeGroup))
  df$C <- df$Year - df$AgeGroup  
  
  names(df) <- c("P","Sex", "A", "D", "Y", "C")
  df
}

cleanAgeRates <- function(df) {
  ## Description: 
  ##  Age adjusted homicide rates by state
  ## Args:
  ##   df - data.frame with homicides by age group and state

  who <- read.csv("data/who-agegroups.csv")
  who$WHO <- NULL
  df$rate <- with(df, Homicides / Population * 10 ^ 5)
  df <- merge(df, who, all.x = TRUE)
  
  state.ageadjusted <- ddply(df, .(Year, Sex, StateCode),
                             summarise,
                             adjusted.rate = wtd.mean(rate, Mexico))
  state.ageadjusted <- subset(state.ageadjusted, Sex == "Total" & Year <= 2006)
  
  state.ageadjusted <- addAbbrv(state.ageadjusted)
  state.ageadjusted <- ddply(state.ageadjusted, .(StateCode),
                             transform,
                             order = coef(lm(adjusted.rate~Year))[2])
  state.ageadjusted$StateCode <-  with(state.ageadjusted, 
                                       reorder(StateCode, order, mean))
  state.ageadjusted
}

##Fuck you ugly stupid function name!
cleanMun100mi <- function(df) {
  ## Description: 
  ##  Age adjusted homicide rates for all municipalites 200 km from the border 

  who <- read.csv("data/who-agegroups.csv")
  who$WHO <- NULL
    
  mun200k.agerates2 <- ddply(df,
                             .(Year, Sex, AgeGroup),
                             summarise,
                             Homicides = sum(Homicides),
                             Population = sum(Population),
                             .progress = "text"
                             )
  mun200k.agerates2$rate <- with(mun200k.agerates2, Homicides / Population * 10 ^ 5)
  mun200k.agerates2 <- merge(mun200k.agerates2, who, all.x = TRUE)
  
  mun200k.ageadjusted <- ddply(mun200k.agerates2, .(Year, Sex),
                               summarise,
                               adjusted.rate = wtd.mean(rate, Mexico),
                               Homicides = sum(Homicides))

  mun200k.ageadjusted <- subset(mun200k.ageadjusted, Sex == "Total" & Year <= 2006)
  mun200k.ageadjusted
}

##Functions to create 4 plots in 1
Layout <- grid.layout(nrow = 2, ncol = 2, heights = unit(c(1,
    1), c("null", "null")))
##grid.show.layout(Layout)
vplayout <- function(...) {
     grid.newpage()
     pushViewport(viewport(layout = Layout))
}
subplot <- function(x, y) viewport(layout.pos.row=x, layout.pos.col=y)

pr <- function(p1, p2, p3, p4) {
  ##Print the four plots
    vplayout()
    print(p1, vp = subplot(1, 1))
    print(p2, vp = subplot(1, 2))
    print(p3, vp = subplot(2, 1))
    print(p4, vp = subplot(2, 2))
  }

recodeCohorts <- function(df) {
  ## Group generations into nice groups
  df$Cohort <- cut(df$C, c(-Inf,1924, 1929, 1934, 1939,1944,
                           1949,
                           1954, 1959,
                           1964, 1969,
                           1974, 1979,
                           1984, 1989,
                           2010),
                   labels = c( "Cavemen","25-29",
                     "30-34", "35-39",
                     "40-44", "45-49",
                     "50-54", "55-59",
                     "60-64", "65-69",
                     "70-74", "75-79",
                     "80-84", "85-89","none"))
  return(df)
}

rateplots <- function(df, sex = "Male") {
  ## The four classic plots to explore age-period cohort models
  df <- recodeCohorts(df)
  df4charts <- na.omit(subset(df, Sex == sex & P <= 2006))
  p1 <- ggplot(subset(df4charts, !Cohort %in% c("none", "Cavemen")),
               aes(A, D/Y * 10^5, group = Cohort, color = Cohort)) +
         geom_point() +
         geom_smooth(show_guide = FALSE, method = glm, 
                     formula = "y ~ x + I(x ^ 2)", se = FALSE, size = 1) +
         ggtitle("Rates versus age at death\nrates in the same birth-cohort connected.") +
         ylab("rate") +
         xlab("age at death (5 year intervals)") +
         scale_color_grey("cohort", start = .8, end = 0,
                          breaks = c("25-29", "50-54", "85-89"),
                          labels = c("1920-24", "1950-54", "1985-89"))

  p2 <- ggplot(df4charts,
       aes(A, D/Y * 10^5, group = factor(P), color = factor(P))) +
  geom_line( size =1) +
  ggtitle("Rates versus age at death\nrates in the same year connected")+
    ylab("rate") +
      xlab("age at death (5 year intervals)")+
      scale_color_grey("year\nof\ndeath", start = .8, end = 0,
                       breaks = c(1990, 1998, 2006),
                       labels = c(1990, 1998, 2006))


  p3 <- ggplot(df4charts,
       aes(P, D/Y * 10^5, group = factor(A), color = factor(A))) +
  geom_line( size =1)  +
  ggtitle("Rates versus date of death\nrates in the same age group connected")+
    ylab("rate") +
      xlab("date of death")+
      scale_color_grey("age\ngroup", #h=c(90, 180),
                       breaks = c(2.5, 32.5, 67.5),
                       labels = c("0-4", "30-34", "65-69"))

  p4 <- ggplot(df4charts,
       aes(C, D/Y * 10^5, group = factor(A), color = factor(A))) +
  geom_line( size =1)  +
  ggtitle("Rates versus date of birth\nrates in the same age group connected")+
    ylab("rate") +
      xlab("cohort average date of birth")+
      scale_color_grey("age\ngroup", #h=c(90, 180),
                      breaks = c(2.5, 32.5, 67.5),
                       labels = c("0-4", "30-34", "65-69"))

  pr(p1, p2, p3, p4)
}


##Age adjusted homicide rates in the southern states
state.ageadjusted <- cleanAgeRates(state.agerates)
state.ageadjusted$color <- ifelse(state.ageadjusted$StateCode %in% c("Mich",
                                                                     "Mor",
                                                                     "Oax",
                                                                     "Mex",
                                                                     "Gro"),
                                  "blue", "black")

ggplot(state.ageadjusted, 
       aes(Year, adjusted.rate)) +
  geom_line(aes(color = color), show_guide = FALSE) +
  facet_wrap(~ StateCode)+
  ylab("age adjusted homicide rate") +
  xlab("year") +
  scale_color_manual(values = c("black", "#00a9ff")) +
  ggtitle("Age adjusted homicide rates, by state (1990-2006)")
ggsave("graphs/states.png", dpi = 100, height = 6.8, width = 9.6)


#Age adjusted homicide rates in the municipalities 200 km from the border
mun100mi.ageadjusted <-  cleanMun100mi(mun200k.agerates)
ggplot(mun100mi.ageadjusted, 
       aes(Year, adjusted.rate)) +
         geom_line(size = 1.1, color = "#f8766d") +
##geom_line(aes(Year, Homicides)) +
         ylim(0, max(mun100mi.ageadjusted$adjusted.rate)) +
         ylab("age adjusted homicide rate") +
         xlab("year") +
         ggtitle("Age adjusted homicide rates in the municipalities\nat least 200 km from the U.S. border (1990-2006)")
ggsave("graphs/border.png", dpi = 100, height = 5, width = 9)


#Age-Period-Cohort analysis with Epi
national <- APCClean(agerates)
#No border and no southern states
rest <- APCClean(subset(state.agerates,
                         !StateCode %in% c(16,15,20,17,12,
                                          8, 5, 2, 26, 19, 28)))


##t <- ddply(agerates, .(id, Sex, Year), summarise, sum(Population))

##subset(agerates, Population < 10000 & Year == 1990)$id
##agerates$id[agerates$Population < 10000 & agerates$Year == 1990]
##rural <- APCClean(subset(agerates, Population > 10000))


##Mich = 16, Mex = 15, Oax = 20, Mor = 17, Guer = 12
south <- APCClean(subset(state.agerates,
                         StateCode %in% c(16,15,20,17,12)))
##border areas
border <- APCClean(mun200k.agerates)

##The models
##All of Mexico
message("\n\n\n\nAPC model for all of Mexico\n")
ex <- apc.fit(subset(national, Sex == sex & P <=2006),
              npar=c(10,10,10), model="ns", ref.c = year.cohort, parm="ACP", scale=10^5)
apc.plot(ex, ci = TRUE)

##Excluding the violent south and the border
message("\n\n\n\nAPC model excluding the south and border states\n")
ex.rest <- apc.fit(subset(rest, Sex == sex & P <=2006),
              npar=c(10,10,10), model="ns", ref.c = year.cohort, parm="ACP", scale=10^5)
apc.plot(ex.rest, ci = TRUE)


##The violent south
message("\n\n\n\nAPC model for southern Mexico\n")
ex.south <- apc.fit(subset(south, Sex == sex & P <=2006),
              npar=c(9,9,9), model="ns", ref.c = year.cohort,
                    ref.p = year.period,
                    parm="ACP", scale=10^5)
apc.plot(ex.south, ci = TRUE)


## The border region
message("\n\n\n\nAPC model for the border region\n")
ex.border <- apc.fit(subset(border, Sex == sex & P <=2006),
              npar=c(9,9,9), model="ns", ref.c = year.cohort, ref.p = year.period,
                     parm="ACP", scale=10^5)
apc.plot(ex.border, ci = TRUE)

##Save a plot
Cairo("graphs/apc.png", height = 600, width = 960)
apc.plot(ex.south, col = "#00a9ff", ci = TRUE)
apc.lines(ex.border,col="#f8766d", ci = TRUE )
##apc.lines(ex.rest, col="#000000", ci = TRUE )
dev.off()

  
##The four classic plots for the south
Cairo(file = "graphs/south3p.png", width = 960, height = 600)
rateplots(south, "Total")
dev.off()

##The four classic plots for the border
Cairo(file = "graphs/border3p.png", width = 960, height = 600)
rateplots(border, "Total")
dev.off()

##The four classic plots for the border
Cairo(file = "graphs/national3p.png", width = 960, height = 600)
rateplots(national, "Total")
dev.off()




#Convert the result of apc.fit into a data frame
#tyep can be "Coh", "Per" "Age"
## cleanEPI <- function(epi.res, type) {
##   df <- as.data.frame(epi.res[type])
##   names(df) <- c("coh", "crr", "c2.5", "c97.5")
##   df
## }

## border.epi <- cleanEPI(ex.border, "Coh")
## south.epi <- cleanEPI(ex.south, "Coh")
## national.epi <- cleanEPI(ex, "Coh")
## ggplot(subset(border.epi, coh >=1940), aes(coh, crr)) +
##   geom_line(color = "blue") +
##   geom_ribbon(aes(ymin = c2.5, ymax = c97.5), alpha = .3) +
##   geom_line(data = south.epi, aes(coh, crr), color = "red") +
##   geom_ribbon(data = south.epi, aes(ymin = c2.5, ymax = c97.5), alpha = .3) +
##   geom_line(data = national.epi, aes(coh, crr), linetype = 2) +
##   geom_ribbon(data = national.epi, aes(ymin = c2.5, ymax = c97.5), alpha = .3)

## border.epi <- cleanEPI(ex.border, "Age")
## south.epi <- cleanEPI(ex.south, "Age")
## national.epi <- cleanEPI(ex, "Age")
## ggplot(border.epi, aes(coh, crr)) +
##   geom_line(color = "blue") +
##   geom_ribbon(aes(ymin = c2.5, ymax = c97.5), alpha = .3) +
##   geom_line(data = south.epi, aes(coh, crr), color = "red") +
##   geom_ribbon(data = south.epi, aes(ymin = c2.5, ymax = c97.5), alpha = .3) +
##   geom_line(data = national.epi, aes(coh, crr), linetype = 2) +
##   geom_ribbon(data = national.epi, aes(ymin = c2.5, ymax = c97.5), alpha = .3)


## plotEffect <- function(effect) {
##   border.epi <- cleanEPI(ex.border, effect)
##   border.epi$name <- "border" 
##   south.epi <- cleanEPI(ex.south, effect)
##   south.epi$name <- "south" 
##   national.epi <- cleanEPI(ex, effect)
##   national.epi$name <- "national"
  
##   p <- ggplot(rbind(border.epi, south.epi, national.epi), aes(coh, crr, group = name)) +
##     geom_line(aes(color = name)) +
##     geom_ribbon(aes(ymin = c2.5, ymax = c97.5), alpha = .3) 
##   direct.label(p)
## }  
## plotEffect("Per")
## plotEffect("Age")
## plotEffect("Coh") + scale_x_continuous(limits = c(1950, 1985)) + 
##   scale_y_continuous(limits = c(0.2, 6)) +
##   coord_trans(y = "log10")
