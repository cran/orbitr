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
sim <- create_system() |>
  add_sun() |>
  add_body("Earth", mass = mass_earth, x = distance_earth_sun, vy = speed_earth) |>
  add_body("Moon",  mass = mass_moon,
           x = distance_earth_sun + distance_earth_moon,
           vy = speed_earth + speed_moon) |>
  simulate_system(time_step = seconds_per_hour, duration = seconds_per_year)

sim |> plot_orbits()

## -----------------------------------------------------------------------------
sim |>
  shift_reference_frame("Earth") |>
  plot_orbits()

## -----------------------------------------------------------------------------
library(ggplot2)

sim |>
  shift_reference_frame("Earth", keep_center = FALSE) |>
  dplyr::filter(id == "Moon") |>
  ggplot(aes(x = x, y = y, color = time / seconds_per_day)) +
  geom_path(linewidth = 1.2) +
  scale_color_viridis_c(name = "Day") +
  coord_equal() +
  labs(title = "Lunar Orbit (Earth-Centered)", x = "X (m)", y = "Y (m)") +
  theme_minimal()

## -----------------------------------------------------------------------------
# Same simulation, three different perspectives

# 1. From the Sun (original frame, but explicit)
sim |>
  shift_reference_frame("Sun") |>
  plot_orbits()

## -----------------------------------------------------------------------------
# 2. From the Earth
sim |>
  shift_reference_frame("Earth") |>
  plot_orbits()

## -----------------------------------------------------------------------------
# 3. From the Moon
sim |>
  shift_reference_frame("Moon") |>
  plot_orbits()

## -----------------------------------------------------------------------------
library(ggplot2)

sim |>
  shift_reference_frame("Earth", keep_center = FALSE) |>
  dplyr::filter(id == "Moon") |>
  dplyr::mutate(speed = sqrt(vx^2 + vy^2 + vz^2)) |>
  ggplot(aes(x = time / seconds_per_day, y = speed)) +
  geom_line(color = "#2563eb", linewidth = 0.8) +
  labs(title = "Moon's Orbital Speed Relative to Earth",
       x = "Time (days)", y = "Speed (m/s)") +
  theme_minimal()

## -----------------------------------------------------------------------------
AU <- distance_earth_sun

m_A <- 0.68 * mass_sun
m_B <- 0.20 * mass_sun
m_planet <- 0.333 * mass_jupiter

a_bin <- 0.22 * AU
r_A <- a_bin * m_B / (m_A + m_B)
r_B <- a_bin * m_A / (m_A + m_B)
v_A <- sqrt(gravitational_constant * m_B^2 / ((m_A + m_B) * a_bin))
v_B <- sqrt(gravitational_constant * m_A^2 / ((m_A + m_B) * a_bin))

r_planet <- 0.7048 * AU
v_planet <- sqrt(gravitational_constant * (m_A + m_B) / r_planet)

kepler16 <- create_system() |>
  add_body("Star A", mass = m_A, x = r_A, vy = v_A) |>
  add_body("Star B", mass = m_B, x = -r_B, vy = -v_B) |>
  add_body("Kepler-16b", mass = m_planet, x = r_planet, vy = v_planet) |>
  simulate_system(time_step = seconds_per_hour, duration = seconds_per_day * 228.8 * 3)

## -----------------------------------------------------------------------------
kepler16 |> plot_orbits()

## -----------------------------------------------------------------------------
kepler16 |>
  shift_reference_frame("Kepler-16b", keep_center = FALSE) |>
  plot_orbits()

## -----------------------------------------------------------------------------
# Distance between Earth and Moon over time
sim |>
  shift_reference_frame("Earth", keep_center = FALSE) |>
  dplyr::filter(id == "Moon") |>
  dplyr::mutate(distance_km = sqrt(x^2 + y^2 + z^2) / 1000) |>
  ggplot(aes(x = time / seconds_per_day, y = distance_km)) +
  geom_line(color = "#dc2626", linewidth = 0.8) +
  labs(title = "Earth-Moon Distance Over One Year",
       x = "Time (days)", y = "Distance (km)") +
  theme_minimal()

