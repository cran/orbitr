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

## ----eccentricity-comparison--------------------------------------------------
make_ecc_orbit <- function(e, label) {
  create_system() |>
    add_sun() |>
    add_body_keplerian(
      "Planet", mass = 1e24, parent = "Sun",
      a = distance_earth_sun, e = e, nu = 0
    ) |>
    simulate_system(time_step = seconds_per_day, duration = seconds_per_year * 2) |>
    filter(id == "Planet") |>
    mutate(case = label)
}

bind_rows(
  make_ecc_orbit(0.0,  "e = 0.0 (circle)"),
  make_ecc_orbit(0.2,  "e = 0.2 (Mercury-like)"),
  make_ecc_orbit(0.5,  "e = 0.5 (comet-like)"),
  make_ecc_orbit(0.85, "e = 0.85 (Halley-like)")
) |>
  ggplot(aes(x = x, y = y, color = case)) +
  geom_path(linewidth = 0.8) +
  geom_point(x = 0, y = 0, color = "gold", size = 4, inherit.aes = FALSE) +
  coord_equal() +
  theme_minimal() +
  labs(title = "Same semi-major axis, different eccentricities",
       color = NULL)

## ----inclination-comparison---------------------------------------------------
make_inc_orbit <- function(inc, label) {
  create_system() |>
    add_sun() |>
    add_body_keplerian(
      "Planet", mass = 1e24, parent = "Sun",
      a = distance_earth_sun, e = 0.1, i = inc, nu = 0
    ) |>
    simulate_system(time_step = seconds_per_day, duration = seconds_per_year) |>
    filter(id == "Planet") |>
    mutate(case = label)
}

bind_rows(
  make_inc_orbit(0,  "i = 0 (flat)"),
  make_inc_orbit(30, "i = 30"),
  make_inc_orbit(60, "i = 60"),
  make_inc_orbit(90, "i = 90 (polar)")
) |>
  ggplot(aes(x = x, y = z, color = case)) +
  geom_path(linewidth = 0.8) +
  coord_equal() +
  theme_minimal() +
  labs(title = "Same orbit, different inclinations (viewed from the side: X vs Z)",
       x = "X (m)", y = "Z (m)", color = NULL)

## ----lan-comparison-----------------------------------------------------------
make_lan_orbit <- function(lan_val, label) {
  create_system() |>
    add_sun() |>
    add_body_keplerian(
      "Planet", mass = 1e24, parent = "Sun",
      a = distance_earth_sun, e = 0.1, i = 45, lan = lan_val, nu = 0
    ) |>
    simulate_system(time_step = seconds_per_day, duration = seconds_per_year) |>
    filter(id == "Planet") |>
    mutate(case = label)
}

bind_rows(
  make_lan_orbit(0,   "LAN = 0"),
  make_lan_orbit(90,  "LAN = 90"),
  make_lan_orbit(180, "LAN = 180")
) |>
  ggplot(aes(x = x, y = y, color = case)) +
  geom_path(linewidth = 0.8) +
  geom_point(x = 0, y = 0, color = "gold", size = 4, inherit.aes = FALSE) +
  coord_equal() +
  theme_minimal() +
  labs(title = "Same orbit tilted in different directions (top-down view)",
       color = NULL)

## ----argpe-comparison---------------------------------------------------------
make_argpe_orbit <- function(argpe_val, label) {
  create_system() |>
    add_sun() |>
    add_body_keplerian(
      "Planet", mass = 1e24, parent = "Sun",
      a = distance_earth_sun, e = 0.5, arg_pe = argpe_val, nu = 0
    ) |>
    simulate_system(time_step = seconds_per_day, duration = seconds_per_year * 2) |>
    filter(id == "Planet") |>
    mutate(case = label)
}

bind_rows(
  make_argpe_orbit(0,   "arg_pe = 0"),
  make_argpe_orbit(90,  "arg_pe = 90"),
  make_argpe_orbit(180, "arg_pe = 180")
) |>
  ggplot(aes(x = x, y = y, color = case)) +
  geom_path(linewidth = 0.8) +
  geom_point(x = 0, y = 0, color = "gold", size = 4, inherit.aes = FALSE) +
  coord_equal() +
  theme_minimal() +
  labs(title = "Same ellipse, rotated within the orbital plane",
       color = NULL)

## ----nu-comparison------------------------------------------------------------
sys_0 <- create_system() |>
  add_sun() |>
  add_body_keplerian("Planet", mass = 1e24, parent = "Sun",
                      a = distance_earth_sun, e = 0.3, nu = 0)

sys_90 <- create_system() |>
  add_sun() |>
  add_body_keplerian("Planet", mass = 1e24, parent = "Sun",
                      a = distance_earth_sun, e = 0.3, nu = 90)

sys_180 <- create_system() |>
  add_sun() |>
  add_body_keplerian("Planet", mass = 1e24, parent = "Sun",
                      a = distance_earth_sun, e = 0.3, nu = 180)

# Plot starting positions
starts <- bind_rows(
  sys_0$bodies |> filter(id == "Planet") |> mutate(case = "nu = 0 (periapsis)"),
  sys_90$bodies |> filter(id == "Planet") |> mutate(case = "nu = 90"),
  sys_180$bodies |> filter(id == "Planet") |> mutate(case = "nu = 180 (apoapsis)")
)

# Full orbit for reference
orbit <- simulate_system(sys_0, time_step = seconds_per_day,
                         duration = seconds_per_year * 2) |>
  filter(id == "Planet")

ggplot() +
  geom_path(data = orbit, aes(x = x, y = y), color = "grey70", linewidth = 0.5) +
  geom_point(data = starts, aes(x = x, y = y, color = case), size = 4) +
  geom_point(x = 0, y = 0, color = "gold", size = 4) +
  coord_equal() +
  theme_minimal() +
  labs(title = "Starting position at different true anomaly values",
       color = NULL)

## ----spread-planets-----------------------------------------------------------
# Spread the inner planets around their orbits
create_system() |>
  add_sun() |>
  add_planet("Mercury", parent = "Sun", nu = 30) |>
  add_planet("Venus",   parent = "Sun", nu = 120) |>
  add_planet("Earth",   parent = "Sun", nu = 210) |>
  add_planet("Mars",    parent = "Sun", nu = 300) |>
  simulate_system(time_step = seconds_per_day, duration = seconds_per_year) |>
  plot_orbits(three_d = FALSE)

## ----what-if-mercury----------------------------------------------------------
# What if Mercury's orbit were circular? How different would it look?
bind_rows(
  create_system() |>
    add_sun() |>
    add_planet("Mercury", parent = "Sun") |>
    simulate_system(time_step = seconds_per_hour * 6, duration = seconds_per_day * 88) |>
    filter(id == "Mercury") |>
    mutate(case = "Real Mercury (e = 0.21)"),
  create_system() |>
    add_sun() |>
    add_planet("Mercury", parent = "Sun", e = 0) |>
    simulate_system(time_step = seconds_per_hour * 6, duration = seconds_per_day * 88) |>
    filter(id == "Mercury") |>
    mutate(case = "Circular Mercury (e = 0)")
) |>
  ggplot(aes(x = x, y = y, color = case)) +
  geom_path(linewidth = 0.8) +
  geom_point(x = 0, y = 0, color = "gold", size = 4, inherit.aes = FALSE) +
  coord_equal() +
  theme_minimal() +
  labs(title = "Mercury: real vs. circular orbit", color = NULL)

