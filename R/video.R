library(scales)
library(ggplot2)
library(RColorBrewer)

video <- function(anycast.probe) {
  colors <- paste0(brewer.pal(12, 'Set3'), '99')
  names(colors)[1:length(levels(anycast.probe$dst_city))] <- levels(anycast.probe$dst_city)

  step <- as.difftime(60, units = 'mins')
  hours <- as.POSIXct(seq.Date(as.Date('2015-01-01'),as.Date('2015-03-01'), step))
  for (start in starts) {
    png(sprintf('frames/%s.png', start), width = 800, height = 450)
    print(frame(anycast.probe,
                subset(anycast.probe, datetime >= start & datetime < start + step),
                color = colors[city][[1]]))
    dev.off()
  }
}

frame <- function(df.full, df, color = 'black')
  ggplot(df) +
    aes(y = dist, size = rt,
        x = dist_theoretical_improvement) +
    ggtitle('Targetting any.ca-servers.something') +
    scale_y_continuous('Distance to chosen instance (km)',
                       labels = comma, limits = range(df.full$dist)) +
    scale_x_continuous('Distance farther than the closest instance (km)',
                       labels = comma, limits = range(df.full$dist_theoretical_improvement)) +
    scale_size_continuous('Min response time (ms)', labels = comma) +
    annotate('text', 0.9 * max(df.full$dist_theoretical_improvement), 0.5 * max(df.full$dist), label = 'Indirect routes') +
    annotate('text', 0.1 * max(df.full$dist_theoretical_improvement), 0.5 * max(df.full$dist), label = 'Direct routes') +
    geom_point(alpha = 0.5, color = color)
