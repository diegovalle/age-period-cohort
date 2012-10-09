########################################################
## Author: Diego Valle-Jones
## Website: www.diegovalle.net
## Date Created: Tue Oct  9 15:05:43 2012
## Email: diegovalle at gmail.com
## Purpose: Plot with subplot of homicides 1931-2011 
## Copyright (c) Diego Valle-Jones. All rights reserved

full <- function() {
  print(p)
  theme_set(theme_bw(base_size = 8))
  theme_white()
  print(subplot, vp = vp)
  theme_set(theme_bw())
}

theme_white <- function() {
  theme_update(panel.background = element_blank(),
               panel.grid.major = element_blank(),
               plot.margin = unit(c(-1, -1, -1.5, -1.5), "lines"),
               legend.position = "none",
               plot.title = element_text(size = 14),
               ##panel.background = theme_blank(), panel.border = theme_blank(), 
               ##panel.grid.major = theme_blank(), panel.grid.minor = theme_blank(), 
               panel.margin = unit(0, "lines"))
}
vp <- viewport(width = 0.14, height = 0.14, x = .255,
               y = unit(4.6, "lines"), just = c("right",
                                                "bottom"))


p <- ggplot(pop, aes(Year, rate)) +
  geom_rect(xmin = 2006, xmax = 2011,
            ymin=0, ymax=Inf, alpha = .02, fill = "red") +
  geom_rect(xmin = 1986, xmax = 2006,
            ymin=0, ymax=Inf, alpha = .02, fill = "#00a9ff") +
  geom_line(size = 1.1) +
  xlab("year") + ylab("homicide rate") +
  scale_y_continuous(breaks=c(0, 5, 10, 15, 20, 25, 30), limits=c(0,26)) +
  geom_line(data = data.frame(x = 2010:2011, y = c(pop[32,4], 24)),
              aes(x, y), size=1.1, linetype = 1) +
  ggtitle("Homicide rate in Mexico, 1979-2011") +
  theme_bw()+
  annotate("text", x = 2008.5, y = 6, label = "Drug War") +
  annotate("text", x = 1995.5, y = 6, label = "Decline of Violence")
##ggsave("historic.png", plot = p, width = 7, height = 5, dpi = 100)

subplot <- ggplot(hh, aes(year, rate)) +
  geom_rect(xmin = 1979, xmax = 2012,
            ymin=0, ymax=35, alpha = .005, color = "#999999", 
            fill = "#ffffff", size = .1) +
  ##geom_rect(xmin = 1979, xmax = 2011,
  ##          ymin=0, ymax=Inf, alpha = .009, fill = "#f2f2f2") +
  geom_line() +
  labs(panel.border = element_blank()) +
  ggtitle("Full data 1931-2011") +
  xlab(NULL) + ylab(NULL) + scale_x_continuous(breaks = c(1979, 2011)) +
  scale_y_continuous(breaks = c(0, 60), limits = c(0, 70)) 

Cairo("graphs/historic.png", height = 450, width = 700)
full()
dev.off()
##ggsave("graphs/historic.png", width = 7, height = 5, dpi = 100)
