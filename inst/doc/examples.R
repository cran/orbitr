## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)

## ----message = FALSE----------------------------------------------------------
library(orbitr)
library(ggplot2)
library(dplyr)

## ----full-solar-system--------------------------------------------------------
solar <- load_solar_system() |>
  simulate_system(time_step = seconds_per_day, duration = seconds_per_year)

solar |> plot_orbits(three_d = FALSE)

## ----inner-solar-system-------------------------------------------------------
create_system() |>
  add_sun() |>
  add_planet("Mercury", parent = "Sun") |>
  add_planet("Venus",   parent = "Sun") |>
  add_planet("Earth",   parent = "Sun") |>
  add_planet("Mars",    parent = "Sun") |>
  simulate_system(time_step = seconds_per_day, duration = seconds_per_year * 2) |>
  plot_orbits(three_d = FALSE)

## ----earth-moon-sim-----------------------------------------------------------
earth_moon <- create_system() |>
  add_body("Earth", mass = mass_earth) |>
  add_planet("Moon", parent = "Earth") |>
  simulate_system(time_step = seconds_per_hour, duration = seconds_per_day * 28)

earth_moon |> plot_orbits()

## ----eval = FALSE-------------------------------------------------------------
# animate_system(earth_moon, fps = 15, duration = 5)

## -----------------------------------------------------------------------------
create_system() |>
  add_sun() |>
  add_body("Earth", mass = mass_earth, x = distance_earth_sun, vy = speed_earth) |>
  simulate_system(time_step = seconds_per_day, duration = seconds_per_year) |>
  plot_orbits()

## -----------------------------------------------------------------------------
create_system() |>
  add_sun() |>
  add_body("Earth", mass = mass_earth, x = distance_earth_sun, vy = speed_earth) |>
  add_body("Moon",  mass = mass_moon,
           x = distance_earth_sun + distance_earth_moon,
           vy = speed_earth + speed_moon) |>
  simulate_system(time_step = seconds_per_hour, duration = seconds_per_year) |>
  plot_orbits()

## ----sun-earth-moon-sim-------------------------------------------------------
sun_earth_moon <- create_system() |>
  add_sun() |>
  add_body("Earth", mass = mass_earth, x = distance_earth_sun, vy = speed_earth) |>
  add_body("Moon",  mass = mass_moon,
           x = distance_earth_sun + distance_earth_moon,
           vy = speed_earth + speed_moon) |>
  simulate_system(time_step = seconds_per_hour, duration = seconds_per_year) |>
  shift_reference_frame("Earth")

sun_earth_moon |> plot_orbits()

## ----eval = FALSE-------------------------------------------------------------
# animate_system(sun_earth_moon, fps = 15, duration = 6)

## -----------------------------------------------------------------------------
AU <- distance_earth_sun

# Star masses
m_A <- 0.68 * mass_sun
m_B <- 0.20 * mass_sun
m_planet <- 0.333 * mass_jupiter

# Binary star orbit (~0.22 AU separation)
a_bin <- 0.22 * AU
r_A <- a_bin * m_B / (m_A + m_B)
r_B <- a_bin * m_A / (m_A + m_B)
v_A <- sqrt(gravitational_constant * m_B^2 / ((m_A + m_B) * a_bin))
v_B <- sqrt(gravitational_constant * m_A^2 / ((m_A + m_B) * a_bin))

# Planet orbit (0.7048 AU from barycenter)
r_planet <- 0.7048 * AU
v_planet <- sqrt(gravitational_constant * (m_A + m_B) / r_planet)

kepler16 <- create_system() |>
  add_body("Star A", mass = m_A, x = r_A, vy = v_A) |>
  add_body("Star B", mass = m_B, x = -r_B, vy = -v_B) |>
  add_body("Kepler-16b", mass = m_planet, x = r_planet, vy = v_planet) |>
  simulate_system(time_step = seconds_per_hour, duration = seconds_per_day * 228.8 * 3)

kepler16 |> plot_orbits()

## ----eval = FALSE-------------------------------------------------------------
# animate_system(kepler16, fps = 15, duration = 6)

## ----comet--------------------------------------------------------------------
create_system() |>
  add_sun() |>
  add_planet("Earth", parent = "Sun") |>
  add_body_keplerian(
    "Comet", mass = 2.2e14, parent = "Sun",
    a = 2.5 * distance_earth_sun, e = 0.85,
    i = 50, lan = 60, arg_pe = 120, nu = 150
  ) |>
  simulate_system(time_step = seconds_per_hour * 6, duration = seconds_per_year * 3) |>
  plot_orbits()

## ----circular-mars------------------------------------------------------------
bind_rows(
  create_system() |>
    add_sun() |>
    add_planet("Mars", parent = "Sun") |>
    simulate_system(time_step = seconds_per_day,
                    duration = seconds_per_day * 687) |>
    filter(id == "Mars") |>
    mutate(case = "Real Mars (e = 0.093)"),
  create_system() |>
    add_sun() |>
    add_planet("Mars", parent = "Sun", e = 0) |>
    simulate_system(time_step = seconds_per_day,
                    duration = seconds_per_day * 687) |>
    filter(id == "Mars") |>
    mutate(case = "Circular Mars (e = 0)")
) |>
  ggplot2::ggplot(ggplot2::aes(x = x, y = y, color = case)) +
  ggplot2::geom_path(linewidth = 0.8) +
  ggplot2::coord_equal() +
  ggplot2::theme_minimal() +
  ggplot2::labs(color = NULL)

