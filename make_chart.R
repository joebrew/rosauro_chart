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

# Filter down
df <- df %>%
  filter(key %in% c('HGB', 'Parasitemia', 'WBC'))

# # Rename wbc
# df$key <-
#   ifelse(df$key == 'WBC',
#          'WBCC (leucocytes/microlitre)',
#          # expression(paste0("WBCC (leucocytes/)", mu, 'L')),
#          df$key)
# 
# # Rename parasitemia
# df <- df %>%
#   mutate(key = ifelse(key == 'Parasitemia', 'Parasitemia (parasites/microlitre)',
#                       ifelse(key == 'HGB', 'HGB (g/dL)',
#                              key)))



# plot
cols <- colorRampPalette(brewer.pal(n = 9, name = 'Set1'))(length(unique(df$key)))

# Make a sub dataframe for labeling hgb cut points
hgb <- df %>% filter(grepl('HGB', key))
hgb$time <- as.numeric(gsub('h', '', hgb$time))
# need to interpolate the value at the unknown hours
left <- data_frame(time = seq(0, max(hgb$time), by = 1))
hgb <- left_join(left, hgb)
hgb$p <- zoo::na.approx(object = hgb$p,
                            x = hgb$time)
hgb$value <- zoo::na.approx(object = hgb$value,
                        x = hgb$time)
hgb <- hgb %>% filter(time %in% c(6, 98, 174))
hgb$hours <- hgb$time
hgb$time <- paste0(hgb$time, 'h')

# Get better labels

greeks <- list(
  bquote('HGB (g/dL)'),
  bquote(.('Parasitemia (parasites /') ~ mu*.('L)')),
  # bquote(.('joe'),mu),
  bquote(.('WBCC (Leococytes /') ~ mu*.('L')))

g <- ggplot() +
  geom_line(data = df,
            aes(x = hours,
                y = p,
                group = key,
                color = key)) +
  labs(x = 'Time after recruitment (hours)',
       y = 'Percentage (%)') +
  scale_colour_manual(name = '',
                      values=cols, 
                      labels=greeks) +
  geom_point(data = hgb,
             aes(x = hours,
                 y = p),
             pch = 6,
             size = 2,
             col = cols[1]) #+
  # scale_y_sqrt(breaks = c(0, 10, 25, 50, 100, 200, 500, 1000, 2000, 3000)) 
  g
plotly::ggplotly(g)

