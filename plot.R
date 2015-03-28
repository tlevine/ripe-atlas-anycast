#!/usr/bin/env Rscript
library(ggplot2)

anycast <- read.csv('anycast.csv', stringsAsFactors = FALSE)
anycast$as <- anycast$asn_v4
anycast[anycast$asf == 6,'as'] <- anycast[anycast$asf == 6,'asn_v6']
anycast$asn_v4 <- anycast$asn_v6 <- NULL
anycast$af <- factor(anycast$af, levels = c(4, 6))

#theta <- anycast$dst_
#anycast$dist <-
#theta = lon2 - lon1
#dist = acos(sin(lat1) × sin(lat2) + cos(lat1) × cos(lat2) × cos(theta))
#if (dist < 0) dist = dist + pi
#dist = dist × 6371.2 

p <- ggplot(subset(anycast, dist < 10^4 & lts < 500)) +
  aes(x = dist, y = lts, group = prb_id,
      label = paste(from, '\n(ASN ', as, ')')) +
  ggtitle('Targetting any.ca-servers.something') +
  geom_line(alpha = 0.2) # + geom_text()

print(p)
