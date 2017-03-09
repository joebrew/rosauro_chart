if(!require('tidyverse')){
  install.packages('tidyverse')
}
if(!require('ggplot2')){
  install.packages('ggplot2')
}
if(!require('readxl')){
  install.packages('readxl')
}
if(!require('readxl')){
  install.packages('readxl')
}
if(!require('RColorBrewer')){
  install.packages('RColorBrewer')
}
if(!require('plotly')){
  install.packages('plotly')
}

# Read data into R
df <- read_excel('Grafica Joe.xlsx',
                 skip = 1)
names(df)[1] <- 'key'

# Make long
df <- gather(df, time, value, `0h`:`312h`)

# Get hours
df$hours <- as.numeric(gsub('h', '', df$time))

# Get value as percent of first
df <- df %>%
  group_by(key) %>%
  mutate(p = value / first(value) * 100) %>%
  ungroup

# Remove NA values
df <- df %>%
  filter(!is.na(p))

# plot
cols <- colorRampPalette(brewer.pal(n = 9, name = 'Set1'))(length(unique(df$key)))
g <- ggplot(data = df,
            aes(x = hours,
                y = p,
                group = key,
                color = key)) +
  geom_line() +
  labs(x = 'Hours after recruitment',
       y = 'Percentage',
       title = 'Laboratory values over time',
       subtitle = 'As percentage of value upon recruitment') +
  scale_color_manual(name = '',
                     values = cols) +
  geom_hline(yintercept = c(0, 100), alpha = 0.6) +
  scale_y_sqrt(breaks = c(0, 10, 25, 50, 100, 200, 500, 1000, 2000, 3000))
plotly::ggplotly(g)
