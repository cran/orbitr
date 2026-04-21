## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)

## ----setup, message = FALSE---------------------------------------------------
library(orbitr)
library(ggplot2)
library(dplyr)

## ----eval = FALSE-------------------------------------------------------------
# system <- create_system() |>
#   add_body("Star",   mass = 1e30) |>                # heavy; sits at origin
#   add_body("Planet", mass = 1e24, x = 1e11, vy = ?) # light; on the +x axis

## -----------------------------------------------------------------------------
M    <- 1e30         # central mass (kg)
r    <- 1e11         # starting distance (m)

v_circ <- sqrt(gravitational_constant * M / r)
v_circ

## ----circle-orbit-------------------------------------------------------------
create_system() |>
  add_body("Star",   mass = M) |>
  add_body("Planet", mass = 1e24, x = r, vy = v_circ) |>
  simulate_system(time_step = seconds_per_hour, duration = seconds_per_year) |>
  plot_orbits()

## ----velocity-sweep-----------------------------------------------------------
make_sim <- function(v, label) {
  create_system() |>
    add_body("Star",   mass = M) |>
    add_body("Planet", mass = 1e24, x = r, vy = v) |>
    simulate_system(time_step = seconds_per_hour, duration = seconds_per_year * 2) |>
    filter(id == "Planet") |>
    mutate(case = label)
}

bind_rows(
  make_sim(0.7  * v_circ, "0.7  v_circ  (too slow)"),
  make_sim(1.0  * v_circ, "1.0  v_circ  (circle)"),
  make_sim(1.2  * v_circ, "1.2  v_circ  (ellipse)"),
  make_sim(1.4  * v_circ, "1.4  v_circ  (wider ellipse)")
) |>
  ggplot(aes(x = x, y = y, color = case)) +
  geom_path(linewidth = 0.8) +
  geom_point(x = 0, y = 0, color = "gold", size = 4, inherit.aes = FALSE) +
  coord_equal() +
  theme_minimal() +
  labs(title = "Same starting point, different starting speeds",
       color = NULL)

## ----escape-velocity----------------------------------------------------------
v_esc <- sqrt(2) * v_circ

bind_rows(
  make_sim(0.99 * v_esc, "0.99 v_esc  (very elongated ellipse)"),
  make_sim(1.00 * v_esc, "1.00 v_esc  (parabolic escape)"),
  make_sim(1.10 * v_esc, "1.10 v_esc  (hyperbolic escape)")
) |>
  ggplot(aes(x = x, y = y, color = case)) +
  geom_path(linewidth = 0.8) +
  geom_point(x = 0, y = 0, color = "gold", size = 4, inherit.aes = FALSE) +
  coord_equal() +
  theme_minimal() +
  labs(title = "At escape velocity and above, the orbit opens up",
       color = NULL)

## ----direction----------------------------------------------------------------
bind_rows(
  make_sim(  v_circ, "vy = +v_circ (counterclockwise)"),
  make_sim(- v_circ, "vy = -v_circ (clockwise)")
) |>
  ggplot(aes(x = x, y = y, color = case)) +
  geom_path(linewidth = 0.8) +
  geom_point(x = 0, y = 0, color = "gold", size = 4, inherit.aes = FALSE) +
  coord_equal() +
  theme_minimal() +
  labs(color = NULL)

## ----rotated------------------------------------------------------------------
create_system() |>
  add_body("Star",   mass = M) |>
  add_body("Planet", mass = 1e24, y = r, vx = -v_circ) |>
  simulate_system(time_step = seconds_per_hour, duration = seconds_per_year) |>
  plot_orbits()

