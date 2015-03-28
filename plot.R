#!/usr/bin/env Rscript
library(ggplot2)
library(scales)

anycast <- read.csv('anycast.csv', stringsAsFactors = FALSE)
anycast$as <- anycast$asn_v4
anycast[anycast$asf == 6,'as'] <- anycast[anycast$asf == 6,'asn_v6']
anycast$asn_v4 <- anycast$asn_v6 <- NULL
anycast$af <- factor(anycast$af, levels = c(4, 6))

for (prb_id in unique(anycast$prb_id)) {
  anycast[anycast$prb_id == prb_id,'probe.min.rt'] <-
    min(anycast[anycast$prb_id == prb_id,'rt'])
}
prb.rts <- sapply(unique(anycast$prb_id),
                  function(prb_id) min(anycast[anycast$prb_id == prb_id,'rt']))
names(prb.rts) <- unique(anycast$prb_id)

#theta <- anycast$dst_
#anycast$dist <-
#theta = lon2 - lon1
#dist = acos(sin(lat1) × sin(lat2) + cos(lat1) × cos(lat2) × cos(theta))
#if (dist < 0) dist = dist + pi
#dist = dist × 6371.2 

#p <- ggplot(subset(anycast, dist < 10^4 & lts < 500)) +

anycast.nearby <- subset(anycast, rt < 100) # dist < 600)

p1 <- ggplot(anycast) +
  aes(x = dist, y = rt, group = prb_id, color = dst_city) +
  ggtitle('Targetting any.ca-servers.something
Each line is a probe, and each measurement is a dot.') +
  scale_x_continuous('Distance (km)', labels = comma) +
  scale_y_continuous('Response time (ms)', labels = comma) +
  annotate('text', 4200, 10, label = 'Speed of light through fibre') +
  geom_point(alpha = 0.4) + geom_line(alpha = 0.2) +
  geom_abline(intercept = 0, slope = 1.444 * 2 * (1000/299792))

p2 <- ggplot(anycast.nearby) +
  aes(x = dist, y = rt, group = prb_id, color = dst_city) +
  ggtitle('Targetting any.ca-servers.something
Each line is a probe, and each measurement is a dot.') +
  scale_x_continuous('Distance (km)', labels = comma) +
  scale_y_continuous('Response time (ms)', labels = comma) +
# annotate('text', 4200, 10, label = 'Speed of light through fibre') +
  geom_point(alpha = 0.4) + geom_line(alpha = 0.2) +
  aes(label = prb_id) + geom_text() +
  geom_abline(intercept = 0, slope = 1.444 * 2 * (1000/299792))

print(p2)
