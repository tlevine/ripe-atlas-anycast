library(scales)
library(ggplot2)
library(RColorBrewer)
# colors <- paste0(brewer.pal(12, 'Set3'), '99')
# names(colors)[1:length(levels(anycast.probe$dst_city))] <- levels(anycast.probe$dst_city)


video <- function(anycast.probe) {
  step <- 3600
  starts <- seq(min(anycast.probe$timestamp), max(anycast.probe$timestamp) + step, step)
  for (start in starts) {
    png(sprintf('frames/%s.png', start), width = 800, height = 450)
    df <- subset(anycast.probe, timestamp >= start & timestamp < start + step)
    print(frame(anycast.probe, df))
    dev.off()
  }
}

frame <- function(df.full, df)
  ggplot(df) +
    aes(x = dist_theoretical_improvement, y = dist, size = rt, color = dst_city) +
    ggtitle('Targetting 173.245.58.117 (anycast)') +
    scale_y_continuous('Distance to chosen instance (km)',
                       labels = comma, limits = range(df.full$dist)) +
    scale_x_continuous('Distance farther than the closest instance (km)',
                       labels = comma, limits = range(df.full$dist_theoretical_improvement)) +
    scale_size_continuous('Min response time (ms)', labels = comma) +
    annotate('text', 0.9 * max(df.full$dist_theoretical_improvement), 0.5 * max(df.full$dist), label = 'Indirect routes') +
    annotate('text', 0.1 * max(df.full$dist_theoretical_improvement), 0.5 * max(df.full$dist), label = 'Direct routes') +
    geom_point(alpha = 0.5)
