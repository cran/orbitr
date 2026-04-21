## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)

## -----------------------------------------------------------------------------
library(orbitr)
library(dplyr)

system <- create_system() |>
  add_body("Star", mass = 1e30) |>
  add_body("Planet", mass = 1e24, x = 1e11, vy = 30000)

verlet <- simulate_system(system, time_step = seconds_per_hour, duration = seconds_per_year, method = "verlet") |>
  mutate(method = "Velocity Verlet")

euler_cromer <- simulate_system(system, time_step = seconds_per_hour, duration = seconds_per_year, method = "euler_cromer") |>
  mutate(method = "Euler-Cromer")

euler <- simulate_system(system, time_step = seconds_per_hour, duration = seconds_per_year, method = "euler") |>
  mutate(method = "Standard Euler")

bind_rows(verlet, euler_cromer, euler) |>
  filter(id == "Planet") |>
  ggplot2::ggplot(ggplot2::aes(x = x, y = y, color = method)) +
  ggplot2::geom_path(alpha = 0.7) +
  ggplot2::coord_equal() +
  ggplot2::theme_minimal()

