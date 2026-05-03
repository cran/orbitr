## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)

## -----------------------------------------------------------------------------
library(orbitr)

create_system() |>
  add_body("Earth", mass = mass_earth) |>
  add_body("Moon",  mass = mass_moon, x = distance_earth_moon, vy = speed_moon) |>
  simulate_system(time_step = seconds_per_hour, duration = seconds_per_day * 28) |>
  plot_orbits()

## ----sun-earth-plot-with-sun, message = FALSE---------------------------------
sim <- create_system() |>
  add_sun() |>
  add_body("Earth", mass = mass_earth, x = distance_earth_sun, vy = speed_earth) |>
  simulate_system(time_step = seconds_per_day, duration = seconds_per_year)

sim |>
  plot_orbits() +
  ggplot2::geom_point(
    data = data.frame(x = 0, y = 0),
    ggplot2::aes(x = x, y = y),
    color = "#00BFC4",
    size = 6
  ) +
  ggplot2::labs(title = "Earth-Sun Orbit")

## ----eval = FALSE-------------------------------------------------------------
# animate_system(sim, fps = 15, duration = 5)

## -----------------------------------------------------------------------------
create_system() |>
  add_sun() |>
  add_body("Earth", mass = mass_earth, x = distance_earth_sun, vy = speed_earth) |>
  add_body("Venus", mass = mass_venus, x = distance_venus_sun, vy = speed_venus) |>
  simulate_system(time_step = seconds_per_day, duration = seconds_per_year) |>
  plot_orbits()

## -----------------------------------------------------------------------------
create_system() |>
  add_sun() |>
  add_planet("Earth", parent = "Sun") |>
  add_planet("Mars",  parent = "Sun") |>
  simulate_system(time_step = seconds_per_day, duration = seconds_per_year * 2) |>
  plot_orbits(three_d = FALSE)

## -----------------------------------------------------------------------------
load_solar_system() |>
  simulate_system(time_step = seconds_per_day, duration = seconds_per_year) |>
  plot_orbits(three_d = FALSE)

## -----------------------------------------------------------------------------
load_solar_system() |>
  remove_body(c("Pluto", "Moon")) |>
  simulate_system(time_step = seconds_per_day, duration = seconds_per_year) |>
  plot_orbits(three_d = FALSE)

## -----------------------------------------------------------------------------
sim <- create_system() |>
  add_body("Earth", mass = mass_earth) |>
  add_body("Moon",  mass = mass_moon, x = distance_earth_moon, vy = speed_moon) |>
  simulate_system(time_step = seconds_per_hour, duration = seconds_per_day * 28)

sim

## ----eval = FALSE-------------------------------------------------------------
# save_system(sys, "my_system.rds")
# restored <- load_system("my_system.rds")

## ----eval = FALSE-------------------------------------------------------------
# export_bodies(sys, "bodies.csv")

