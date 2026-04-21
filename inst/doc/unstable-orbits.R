## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)

## -----------------------------------------------------------------------------
library(orbitr)

## ----three-body-sim-----------------------------------------------------------
three_body <- create_system() |>
  add_body("Star A", mass = 1e30, x = 1e11, y = 0, vx = 0, vy = 15000) |>
  add_body("Star B", mass = 1e30, x = -5e10, y = 8.66e10, vx = -12990, vy = -7500) |>
  add_body("Star C", mass = 1e30, x = -5e10, y = -8.66e10, vx = 14000, vy = -8000) |>
  simulate_system(time_step = seconds_per_hour, duration = seconds_per_year * 3)

three_body |> plot_orbits()

## ----eval = FALSE-------------------------------------------------------------
# animate_system(three_body, fps = 15, duration = 6)

