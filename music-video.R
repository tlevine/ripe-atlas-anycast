#!/usr/bin/env Rscript
library(MASS)
library(plyr)

library(devtools)
unloadNamespace('devtools')
devtools::load_all()

devtools::load_all('../krounq')

anycast <- read.csv('anycast.csv', stringsAsFactors = FALSE)
anycast$as <- anycast$asn_v4
anycast[anycast$asf == 6,'as'] <- anycast[anycast$asf == 6,'asn_v6']
anycast$asn_v4 <- anycast$asn_v6 <- NULL
anycast$af <- factor(anycast$af, levels = c(4, 6))
anycast$dst_city <- factor(anycast$dst_city)

anycast.probe <- ddply(anycast, 'prb_id', function(df) {
  df[order(df$rt)[1],]
})

# video(anycast.probe)
krounq::play(phrase(anycast.probe, subset(anycast.probe, dst_city == 'LHR')))
