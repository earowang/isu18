## ---- load-pkgs
library(lubridate)
library(tidyverse)
library(tsibble)
library(sugrrants)
# devtools::install_github("heike/ggmapr")
library(ggmapr)

## ---- theme-remark
theme_remark <- function() {
  theme_grey() +
  theme(
    axis.text = element_text(size = 14), 
    strip.text = element_text(size = 16), 
    axis.title = element_text(size = 16),
    legend.title = element_text(size = 16), 
    legend.text = element_text(size = 16),
    legend.position = "bottom"
  )
}
theme_set(theme_remark())

## ---- load-data
flights <- read_rds("data/flights.rds")

## ---- map-airlines-data
origin_dest <- flights %>% 
  distinct(origin, origin_state, dest, dest_state)
write_rds(origin_dest, path = "data/tsibble/origin_dest.rds")

## ---- map-airlines
origin_dest <- read_rds("data/tsibble/origin_dest.rds")
airports <- read_rds("data/airports.rds")
map_dat <- origin_dest %>% 
  left_join(airports, by = c("origin" = "faa")) %>% 
  rename(long = lon) %>% 
  shift(origin_state == "HI", shift_by = c(52.5, 5.5)) %>%
  scale(origin_state == "AK", scale = 0.3, set_to = c(-117, 27)) %>%
  rename(origin_lat = lat, origin_lon = long)  %>% 
  left_join(select(airports, faa, lon, lat), by = c("dest" = "faa")) %>% 
  rename(long = lon) %>% 
  shift(dest_state == "HI", shift_by = c(52.5, 5.5)) %>%
  scale(dest_state == "AK", scale = 0.3, set_to = c(-117, 27)) %>%
  rename(dest_lat = lat, dest_lon = long)

states <- states %>%
  shift(NAME == "Hawaii", shift_by = c(52.5, 5.5)) %>%
  scale(NAME == "Alaska", scale = 0.3, set_to = c(-117, 27)) %>%
  filter(lat > 20)

ggplot() +
  geom_polygon(data= states, aes(x = long, y = lat, group = group), 
    fill = "white", colour = "grey60") +
  geom_curve(data = map_dat, aes(
    x = origin_lon, y = origin_lat, xend = dest_lon, yend = dest_lat
  ), curvature = 0.2, alpha = 0.2, size = 0.5, colour = "#762a83") +
  geom_point(data = map_dat, aes(x = origin_lon, y = origin_lat), 
    colour = "#f1a340", size  = 1.5) +
  ggthemes::theme_map()

## ---- tsibble
flights <- flights %>% 
  as_tsibble(key = id(flight), index = sched_dep_datetime, regular = FALSE,
    validate = FALSE)

## ---- select
flights_sel <- flights %>% 
  select(flight, sched_dep_datetime, dep_delay)
flights_sel

## ---- filter
flights_fil <- flights_sel %>% 
  filter(year(sched_dep_datetime) == 2017)
flights_fil

## ---- tsum
flights_tsum <- flights_fil %>% 
  tsummarise(
    yrmth = yearmonth(sched_dep_datetime),
    avg_delay = mean(dep_delay)
  )
flights_tsum

## ----- n-flights-2017
n_flights <- flights %>% 
  filter(year(sched_dep_datetime) == 2017) %>% 
  mutate(dep_delay_break = case_when(
    dep_delay <= 15 ~ "ontime",
    dep_delay <= 60 ~ "15-60 mins",
    TRUE ~ "60+ mins"
  )) %>% 
  group_by(dep_delay_break) %>% 
  tsummarise(
    floor_date(sched_dep_datetime, unit = "hour"), 
    n_flight = n()
  ) %>% 
  mutate(
    hour = hour(sched_dep_datetime),
    date = as_date(sched_dep_datetime),
    wday = wday(sched_dep_datetime, label = TRUE, week_start = 1)
  )
n_flights

## ---- n-flights-rds
write_rds(as_tibble(n_flights), "data/tsibble/n_flights.rds")

## ---- delayed-facet
n_flights <- read_rds("data/tsibble/n_flights.rds")
avg_n_flights <- n_flights %>% 
  group_by(dep_delay_break, wday, hour) %>% 
  summarise(avg_n_flight = mean(n_flight, na.rm = TRUE), drop = TRUE)

n_flights %>% 
  ggplot() +
  geom_line(aes(x = hour, y = n_flight, group = date), alpha = 0.25, size = 0.4) +
  geom_line(
    aes(x = hour, y = avg_n_flight, colour = dep_delay_break), 
    data = avg_n_flights, size = 1
  ) +
  facet_grid(dep_delay_break ~ wday, scales = "free_y") +
  theme_remark() +
  guides(colour = guide_legend(title = "Depature Delay")) +
  xlab("Time of day") +
  ylab("Number of flights") +
  scale_x_continuous(breaks = seq(6, 23, by = 6)) +
  scale_colour_brewer(palette = "Dark2")

## ---- delay-qtl-data
delay_qtl <- flights %>% 
  filter(
    year(sched_dep_datetime) == 2017,
    hour(sched_dep_datetime) > 4
  ) %>% 
  tsummarise(
    floor_date(sched_dep_datetime, unit = "hour"), 
    qtl50 = quantile(dep_delay, 0.5),
    qtl80 = quantile(dep_delay, 0.8),
    qtl95 = quantile(dep_delay, 0.95)
  ) %>% 
  mutate(
    zero = 0,
    hour = hour(sched_dep_datetime), 
    date = as_date(sched_dep_datetime)
  )
delay_qtl

## ---- delay-qtl-data-rds
write_rds(as_tibble(delay_qtl), "data/tsibble/delay_qtl.rds")

## ---- delay-qtl-cal
delay_qtl <- read_rds("data/tsibble/delay_qtl.rds")
qtl_cal <- delay_qtl %>% 
  frame_calendar(
    x = hour, y = vars(qtl95, qtl80, qtl50, zero), date = date,
  )

break_cols <- c(
  "50%" = "#d7301f", 
  "80%" = "#fc8d59", 
  "95%" = "#fdcc8a"
)

p_qtl <- ggplot(qtl_cal, aes(x = .hour, group = date)) +
  geom_ribbon(aes(ymin = .zero, ymax = .qtl95, fill = "95%")) +
  geom_ribbon(aes(ymin = .zero, ymax = .qtl80, fill = "80%")) +
  geom_ribbon(aes(ymin = .zero, ymax = .qtl50, fill = "50%")) +
  scale_fill_manual(
    name = "Departure delay",
    values = break_cols
  ) +
  theme_remark()
prettify(p_qtl, label.padding = unit(0.12, "lines"))

## ---- plotly-cal
library(plotly)
library(widgetframe)
a <- list(
  title = "",
  zeroline = FALSE,
  autotick = FALSE,
  showticklabels = FALSE,
  showline = FALSE,
  showgrid = FALSE
)

lab_data <- attr(qtl_cal, "label")
lab_data$label <- month.abb
txt_data <- attr(qtl_cal, "text")
txt_data$label <- c("M", "T", "W", "T", "F", "S", "S")

cal_plot <- as_tibble(qtl_cal) %>%
  group_by(date) %>%
  plot_ly(
    x = ~ .hour, hoverinfo = "text",
    width = 700, height = 700
  ) %>%
  add_ribbons(
    ymin = ~ .zero, ymax = ~ .qtl95, color = I("#fdcc8a"),
    text = ~ paste(
      "Departure delay: ", round(qtl95), "<br> Date: ", date,
      "<br> Hour: ", hour
    )
  ) %>% 
  add_ribbons(
    ymin = ~ .zero, ymax = ~ .qtl80, color = I("#fc8d59"),
    text = ~ paste(
      "Departure delay: ", round(qtl80), "<br> Date: ", date,
      "<br> Hour: ", hour
    )
  ) %>% 
  add_ribbons(
    ymin = ~ .zero, ymax = ~ .qtl50, color = I("#d7301f"),
    text = ~ paste(
      "Departure delay: ", round(qtl50), "<br> Date: ", date,
      "<br> Hour: ", hour
    )
  ) %>% 
  add_text(
    x = ~ x, y = ~ y, text = ~ label, data = lab_data,
    color = I("black")
  ) %>%
  add_text(
    x = ~ x, y = ~ y - 0.03, text = ~ label, data = txt_data,
    color = I("black")
  )
p <- layout(cal_plot, showlegend = FALSE, xaxis = a, yaxis = a)
frameWidget(p)
