library(devtools)
unloadNamespace('devtools')
devtools::load_all('../../krounq')

LIGHT.THROUGH.FIBER <- 1.444 * 2 * (1000/299792)
TEMPO <- 216 # multiple of 24, for easy division

drumlike <- function(freq, duration) {
  base <- 0.8 * sawtooth(freq, duration) ^ 3 + runif(duration, -.2, .2)
  duration.left <- min(duration, round(SECOND/8))
  base.left <- base[1:duration.left]
  base.right <- silence(max(0, duration - duration.left))
  c(base.left, base.right)
}


phrase <- function(df.full, df) {
  base.duration <- 2 ^ (2 - floor(nrow(df) / 20))
  # syncopation <- number of probes
  # Response time divided by speed of light through fiber
  .selector <- order(anycast.probe$rt)[round(nrow(anycast.probe)/2)]
  pitch <- df[.selector,'rt'] / (LIGHT.THROUGH.FIBER * df[.selector,'dist'])
  print(pitch)
}
