#!/usr/bin/env Rscript
library(MASS)
library(plyr)
library(RColorBrewer)

library(devtools)
unloadNamespace('devtools')
devtools::load_all('../krounq')
source('../krounq/generate-data.R')

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

frame <- function(df, df.full = anycast.probe, color = 'black')
  ggplot(df) +
    aes(y = dist, size = rt,
        x = dist_theoretical_improvement) +
    ggtitle('Targetting any.ca-servers.something') +
    scale_y_continuous('Distance to chosen instance (km)',
                       labels = comma, limits = range(df.full$dist)) +
    scale_x_continuous('Distance farther than the closest instance (km)',
                       labels = comma, limits = range(df.full$dist_theoretical_improvement)) +
    scale_size_continuous('Min response time (ms)', labels = comma) +
    annotate('text', 0.9 * max(df.full$dist_theoretical_improvement), 0.5 * max(df.full$dist), label = 'Indirect routes') +
    annotate('text', 0.1 * max(df.full$dist_theoretical_improvement), 0.5 * max(df.full$dist), label = 'Direct routes') +
    geom_point(alpha = 0.5, color = color)

phrase <- function(df) {
}

colors <- paste0(brewer.pal(12, 'Set3'), '99')
names(colors)[1:length(levels(anycast.probe$dst_city))] <- levels(anycast.probe$dst_city)
for (city in levels(anycast.probe$dst_city)) {
  png(sprintf('frames/%s.png', city), width = 800, height = 450)
  print(frame(subset(anycast.probe, dst_city == city),
              color = colors[city][[1]]))
  dev.off()
}
