#!/usr/bin/env Rscript
library(MASS)
library(plyr)
library(RColorBrewer)

COLORS <- paste0(brewer.pal(12, 'Set3'), '99')

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
  df <- subset(df.full, rt >= low & rt < high)
  plot(dist ~ dist_theoretical, data = df.full, type = 'n',
       axes = F, xlim = c(0, 12000), ylim = c(0, 12000), asp = 1)
  axis(1, at = seq(0, 12000, 2000))
  axis(2)
  abline(0, 1)
  points(df$dist ~ df$dist_theoretical, pch = 21, bg = COLORS[df$dst_city],
         lwd = 0, cex = sqrt(df$rt))
}

video <- function() {
  N.VERSES <- 7
  N.PHRASES <- N.VERSES * 4

  rt.step <- round(max(anycast$rt) / N.PHRASES, -2)
  for (rt in seq(0, max(anycast$rt) + rt.step, rt.step)) {
    png(sprintf('frames/%08d.png', rt), width = 800, height = 450)
    frame(anycast.probe, rt, rt + rt.step)
    dev.off()
  }
}

#frame(anycast.probe, 0, 12000)
video()
