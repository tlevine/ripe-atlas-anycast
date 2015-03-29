library(ddr)
data(roland)
unloadNamespace('ddr')

library(devtools)
unloadNamespace('devtools')
devtools::load_all('../../krounq')

LIGHT.THROUGH.FIBER <- 1.444 * 2 * (1000/299792)
TEMPO <- 216 # multiple of 24, for easy division

sample.instrument <- function(the.sample) {
  function(., duration) {
    if (duration < length(the.sample)) {
      stop(paste('Duration must be at least', length(the.sample)))
      the.sample[1:duration]
    } else {
      c(the.sample, silence(duration - length(kick)))
    }
  }
}

.norm <- function(x) {
  x / max(abs(x))
}
kick <- sample.instrument(.norm(roland$BD1@left[(1:length(roland$BD1@left)) %% 2 == 0]))
snare <- sample.instrument(.norm(roland$SD0@left))
hihat <- sample.instrument(.norm(roland$HHO@left))

drumlike <- function(freq, duration) {
  base <- 0.8 * sawtooth(freq, duration) ^ 3 + runif(duration, -.2, .2)
  duration.left <- min(duration, round(SECOND/8))
  base.left <- base[1:duration.left]
  base.right <- silence(max(0, duration - duration.left))
  c(base.left, base.right)
}


phrase <- function(df.full, df, base.pitch = 22) {
  # Rhythm speed: Number of measurements
  base.duration <- 2 ^ (4 - floor(nrow(df) / 20))
  speed <- ceiling(nrow(df) / 70)

  # syncopation <- number of probes

  # Response time divided by speed of light through fiber
  .selector <- order(anycast.probe$rt)[round(nrow(anycast.probe)/2)]
  .rt.normalized <- df[.selector,'rt'] / (LIGHT.THROUGH.FIBER * df[.selector,'dist'])
  pitch <- base.pitch + intervals$P1 + (.rt.normalized > 3) * intervals$P4
  
  a <- sequence(durations = base.duration, instrument = snare,
                tempo = TEMPO, beats = 8)
  b <- function(key, speed) {
    starts <- c(1, 2, 3, 4.5, 5, 6, 7, 8, 8.5)
    start.priorities <- c(1, 5, 7, 3, 8, 9, 4, 7, 2)
    sequence(frequencies = P.n(key),
             starts = starts,
             durations = base.duration,
             instrument = drumlike[start.priorities[1:speed]],
             tempo = TEMPO,
             beats = 8)
  }

  c(b(base.pitch, speed), b(pitch, speed)) + a * 3
}
