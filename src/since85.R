
CohortRatePlot <- function(df, lastyear = 2006) {
  df$group <- cut(df$yobirth, c(-Inf,
                                1949,
                                1954, 1959,
                                1964, 1969,
                                1974, 1979,
                                1984, 1989,
                                2010),
                  labels = c("none", "50-54", "55-59",
                             "60-64", "65-69",
                             "70-74", "75-79",
                             "80-84", "85-89", "none2"))
  df <- subset(df, year <= lastyear & age >= 12 & age <= 60 &
    group != "none" & group != "none2", drop = TRUE)
  df$group <- droplevels(df$group)
  ggplot(df, aes(age, rate, group = group, color = group)) +
    geom_smooth(size = 1.1, method = loess, se = FALSE) +
    geom_point() +
    ##scale_colour_brewer("birth\ncohort", palette="Dark2") +
    scale_color_grey("Cohort", start = .6, end = 0) +
    xlab("age of homicide victim") +
    ylab("homicide rate") +
    labs(title = str_c("Age specific homicide rates in all of Mexico from 1985 to ",
                       lastyear, ", by birth cohort"))
}


CohortRatePlot(rates85, 2006)
SavePlot("age-cohorts-regression", w = 800, h = 500)
#Shows the effect of the drug war on all cohorts
CohortRatePlot(rates85, 2009)
SavePlot("age-cohorts-regression-2009", w = 800, h = 500)
