## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)

## -----------------------------------------------------------------------------
library(orbitr)

## -----------------------------------------------------------------------------
create_system() |>
  add_body("Earth", mass = mass_earth) |>
  add_body("Moon",  mass = mass_moon,
           x = distance_earth_moon,
           vy = speed_moon * cos(5 * pi / 180),
           vz = speed_moon * sin(5 * pi / 180)) |>
  simulate_system(time_step = seconds_per_hour, duration = seconds_per_day * 28) |>
  plot_orbits()

## -----------------------------------------------------------------------------
create_system() |>
  add_body("Earth", mass = mass_earth) |>
  add_body("Moon",  mass = mass_moon, x = distance_earth_moon, vy = speed_moon) |>
  simulate_system(time_step = seconds_per_hour, duration = seconds_per_day * 28) |>
  plot_orbits(three_d = TRUE)

