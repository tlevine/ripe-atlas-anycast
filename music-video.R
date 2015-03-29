#!/usr/bin/env Rscript
library(MASS)
library(plyr)

library(devtools)
unloadNamespace('devtools')
devtools::load_all()

# library(krounq)

anycast <- read.csv('anycast.csv', stringsAsFactors = FALSE)
anycast$as <- anycast$asn_v4
anycast[anycast$asf == 6,'as'] <- anycast[anycast$asf == 6,'asn_v6']
anycast$asn_v4 <- anycast$asn_v6 <- NULL
anycast$af <- factor(anycast$af, levels = c(4, 6))
anycast$dst_city <- factor(anycast$dst_city)
anycast$datetime <- as.POSIXct(anycast$timestamp, origin = '1970-01-01')

anycast.probe <- ddply(anycast, 'prb_id', function(df) {
  df[order(df$rt)[1],]
})

music.step <- 3600 * 3
video.step <- 3600
music.starts <- seq(min(anycast$timestamp), max(anycast$timestamp) + music.step, music.step)
video.starts <- seq(min(anycast$timestamp), max(anycast$timestamp) + video.step, video.step)

video <- function(anycast) {
  for (start in video.starts) {
    png(sprintf('frames/%s.png', start), width = 1600, height = 900)
    df <- subset(anycast, timestamp >= start & timestamp < start + video.step)
    frame(anycast, df)
    dev.off()
  }
}

music <- function(anycast) {
  for (start in music.starts) {
    anycast[anycast$timestamp >= start,'start'] <- start
  }
  df <- anycast[anycast$start == start,]
  do.call(c,lapply(unique(anycast$start),
                   function(start) plot.phrase(df)))
}
# video(anycast)
# music(anycast)
# krounq::play(phrase(anycast.probe, subset(anycast.probe, dst_city == 'LHR')))
