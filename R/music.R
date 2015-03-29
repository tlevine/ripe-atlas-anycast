library(devtools)
unloadNamespace('devtools')
devtools::load_all('../../krounq')


TEMPO <- 216 # multiple of 24, for easy division
drumlike <- function(freq, duration) {
  base <- 0.8 * sawtooth(freq, duration) ^ 3 + runif(duration, -.2, .2)
  duration.left <- min(duration, round(SECOND/8))
  base.left <- base[1:duration.left]
  base.right <- silence(max(0, duration - duration.left))
  c(base.left, base.right)
}
phrase <- function(df) {
  nrow(df)
}
