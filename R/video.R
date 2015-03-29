library(scales)
library(ggplot2)
library(RColorBrewer)

colors <- paste0(brewer.pal(12, 'Set3'), '99')
names(colors)[1:length(levels(anycast.probe$dst_city))] <- levels(anycast.probe$dst_city)


video <- function(anycast.probe) {
  step <- 3600
  starts <- seq(min(anycast.probe$timestamp), max(anycast.probe$timestamp) + step, step)
  for (start in starts) {
    png(sprintf('frames/%s.png', start), width = 800, height = 450)
    df <- subset(anycast.probe, timestamp >= start & timestamp < start + step)
    frame(anycast.probe, df)
    dev.off()
  }
}

frame <- function(df.full, df) {
  plot(dist ~ dist_theoretical_improvement, data = df.full, type = 'n',
       main = 'Targetting 173.245.58.117 (anycast)',
       axes = F,
       xlab = 'Distance farther than the closest instance (km)',
       ylab = 'Distance to chosen instance (km)',
       sub = 'Each dot is a measurement. Bigger dots have higher latency.',
       xlim = range(df.full$dist_theoretical_improvement),
       ylim = range(df.full$dist))
  points(df$dist ~ df$dist_theoretical_improvement,
         pch = 21, lwd = 0, cex = sqrt(df$rt), bg = colors[df$dst_city])
  text(x = c(0.9, 0.1) * max(df.full$dist_theoretical_improvement),
       y = 0.5 * max(df.full$dist),
       label = c('Indirect routes', 'Direct routes'))
}
