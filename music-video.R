#!/usr/bin/env Rscript
library(ggplot2)
library(scales)
library(MASS)
library(plyr)

anycast <- read.csv('anycast.csv', stringsAsFactors = FALSE)
anycast$as <- anycast$asn_v4
anycast[anycast$asf == 6,'as'] <- anycast[anycast$asf == 6,'asn_v6']
anycast$asn_v4 <- anycast$asn_v6 <- NULL
anycast$af <- factor(anycast$af, levels = c(4, 6))
anycast$dst_city <- factor(anycast$dst_city)

anycast.probe <- ddply(anycast, 'prb_id', function(df) {
  df[order(df$rt)[1],]
})
m <- rlm(rt ~ dist, data = anycast.probe)

frame <- function(df.full, low, high) {
  df <- subset(df.full, dist >= low & dist < high)
  plot(dist ~ dist_theoretical, data = df.full, type = 'n',
       axes = F)
  axis(1)
  axis(2)
  points(df$dist ~ df$dist_theoretical, pch = 21, bg = df$dst_city,
         )
}

N.VERSES <- 7
N.PHRASES <- N.VERSES * 4

dist.step <- round(max(anycast$dist) / N.PHRASES, -2)
for (dist in seq(0, max(anycast$dist) + dist.step, dist.step)) {
  png(sprintf('frames/%08d.png', dist), width = 800, height = 450)
  frame(anycast.probe, dist, dist + dist.step)
  dev.off()
}
