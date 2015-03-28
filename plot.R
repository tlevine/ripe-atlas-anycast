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

anycast.nearby <- subset(anycast, rt < 100) # dist < 600)

anycast.probe <- ddply(anycast, 'prb_id', function(df) {
  df[order(df$rt)[1],]
})
m <- rlm(rt ~ dist, data = anycast.probe)

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
  geom_abline(intercept = m$coefficients[[1]],
              slope = m$coefficients[[2]]) +
  geom_abline(intercept = 0, slope = 1.444 * 2 * (1000/299792))

p3 <- ggplot(anycast.probe) +
  aes(x = dist, y = rt, color = dst_city) +
  ggtitle('Targetting any.ca-servers.something
Each line is a probe, and each measurement is a dot.') +
  scale_x_continuous('Distance (km)', labels = comma) +
  scale_y_continuous('Response time (ms)', labels = comma) +
# annotate('text', 4200, 10, label = 'Speed of light through fibre') +
  geom_point(alpha = 0.2, size = 32) +
  geom_abline(intercept = m$coefficients[[1]],
              slope = m$coefficients[[2]]) +
  geom_abline(intercept = 0, slope = 1.444 * 2 * (1000/299792))

p4 <- ggplot(anycast.probe) +
  aes(x = dist, y = rt, color = dst_city,
      size = dist_theoretical_improvement) +
  ggtitle('Targetting any.ca-servers.something
Each line is a probe, and each measurement is a dot.') +
  scale_x_continuous('Distance (km)', labels = comma) +
  scale_y_continuous('Min response time (ms)', labels = comma) +
# annotate('text', 4200, 10, label = 'Speed of light through fibre') +
  geom_point(alpha = 0.2) +
  geom_abline(intercept = m$coefficients[[1]],
              slope = m$coefficients[[2]]) +
  geom_abline(intercept = 0, slope = 1.444 * 2 * (1000/299792))

p5 <- ggplot(anycast.probe) +
  aes(x = dist, size = rt, color = dst_city,
      y = dist - dist_theoretical_improvement) +
  ggtitle('Targetting any.ca-servers.something
Each line is a probe, and each measurement is a dot.') +
  scale_x_continuous('Distance to chosen instance (km)', labels = comma) +
  scale_y_continuous('Distance to closest instance (km)', labels = comma) +
  scale_size_area(max_size = 20) +
 #scale_size_continuous('Min response time (ms)', labels = comma) +
  geom_abline(slope = 1) +
  annotate('text', 5000, 100, label = 'Indirect routes') +
  annotate('text', 200, 1250, label = 'Direct routes') +
  coord_flip() +
  geom_point(alpha = 0.2)

print(p5)
