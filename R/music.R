library(devtools)
unloadNamespace('devtools')
library(ddr)
data(roland)
unloadNamespace('ddr')

devtools::load_all('../../krounq')
#source('generate-data.R')

TEMPO <- 216 # multiple of 24, for easy division
LIGHT.THROUGH.FIBER <- 1.444 * 2 * (1000/299792)

norm <- function(x) {
  x / max(abs(x))
}

drumlike <- function(freq, duration) {
  base <- 0.8 * sawtooth(freq, duration) ^ 3 + runif(duration, -.2, .2)
  duration.left <- min(duration, round(SECOND/8))
  base.left <- base[1:duration.left]
  base.right <- silence(max(0, duration - duration.left))
  c(base.left, base.right)
}

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

kick <- sample.instrument(norm(roland$BD1@left[(1:length(roland$BD1@left)) %% 2 == 0]))
snare <- sample.instrument(norm(roland$SD0@left))
hihat <- sample.instrument(norm(roland$HHO@left))
rim <- sample.instrument(norm(roland$RIM@left))

phrase <- function(key = 30, speed = 2, pickup = NULL, drums = TRUE,
                   rhythm = c(1, 4)) {
  base.duration <- 2 ^ (4 - floor(speed))

  pounding <- sequence(durations = base.duration,
                       instrument = if (drums) snare else rim,
                       tempo = TEMPO, beats = 8)

  f <- rep(key, length(rhythm))
  if (!is.null(pickup))
    f[floor(rhythm) %% 4 == 0] <- key + pickup
  melody <- sequence(frequencies = P.n(f),
                     starts = rhythm,
                     durations = 0.5,
                     instrument = drumlike,
                     tempo = TEMPO,
                     beats = 8)

  rep(2.5 * pounding + melody, 2)
}


RHYTHMS <- list(c(1, 2, 3, 4.5, 5, 6, 7, 8, 8.5),
                c(1, 2, 3, 3 + 2/3, 4 + 1/3, 5, 6, 6.5, 7, 7.5, 8, 8.5),
                c(1, 3, 5, 7), 1:8)

plot.phrase <- function(df) {
  # Response time divided by speed of light through fiber
  if (nrow(df) > 0) {
     .selector <- order(df$rt)[round(nrow(df)/2)]
    .rt.normalized <- df[.selector,'rt'] / (LIGHT.THROUGH.FIBER * df[.selector,'dist'])
  } else {
    .rt.normalized <- NULL
  }

  # Number of different cities
  rhythm <- length(unique(df$dst_city))
  if (rhythm > 4)
    rhythm <- 4

  print(list(key = 30, speed = max(1, nrow(df)),
         pickup = scales$major[round(.rt.normalized)],
         drums = nrow(df) > 0,
         rhythm = RHYTHMS[[rhythm]]))
  phrase(key = 30, speed = max(1, nrow(df)),
         pickup = scales$major[round(.rt.normalized)],
         drums = nrow(df) > 0,
         rhythm = RHYTHMS[[rhythm]])
}
