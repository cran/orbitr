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
  add_body("Earth", mass = mass_earth) |>
  add_body("Moon",  mass = mass_moon, x = distance_earth_moon, vy = speed_moon) |>
  simulate_system(time_step = seconds_per_hour, duration = seconds_per_day * 28)

sim

## -----------------------------------------------------------------------------
library(ggplot2)

sim |>
  dplyr::mutate(r = sqrt(x^2 + y^2)) |>
  ggplot(aes(x = time / seconds_per_day, y = r, color = id)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Distance from Barycenter Over Time",
    x = "Time (days)",
    y = "Distance (m)",
    color = "Body"
  ) +
  theme_minimal()

## -----------------------------------------------------------------------------
sim |>
  shift_reference_frame("Earth", keep_center = FALSE) |>
  ggplot(aes(x = x, y = y, color = time / seconds_per_day)) +
  geom_path(linewidth = 1.2) +
  scale_color_viridis_c(name = "Day") +
  coord_equal() +
  labs(title = "Lunar Orbit (Earth-Centered)", x = "X (m)", y = "Y (m)") +
  theme_minimal()

## -----------------------------------------------------------------------------
library(plotly)

sim <- create_system() |>
  add_body("Earth", mass = mass_earth) |>
  add_body("Moon",  mass = mass_moon,
           x = distance_earth_moon,
           vy = speed_moon * cos(5 * pi / 180),
           vz = speed_moon * sin(5 * pi / 180)) |>
  simulate_system(time_step = seconds_per_hour, duration = seconds_per_day * 28)

sim <- sim |>
  dplyr::mutate(speed = sqrt(vx^2 + vy^2 + vz^2))

plot_ly() |>
  add_trace(
    data = dplyr::filter(sim, id == "Moon"),
    x = ~x, y = ~y, z = ~z,
    type = 'scatter3d', mode = 'lines',
    line = list(
      width = 5,
      color = ~speed,
      colorscale = 'Viridis',
      showscale = TRUE,
      colorbar = list(title = "Speed (m/s)")
    ),
    name = "Moon"
  ) |>
  add_trace(
    data = dplyr::filter(sim, id == "Earth"),
    x = ~x, y = ~y, z = ~z,
    type = 'scatter3d', mode = 'lines',
    line = list(width = 3, color = 'gray'),
    name = "Earth"
  ) |>
  layout(
    title = "Lunar Orbit Around Earth",
    showlegend = FALSE,
    scene = list(
      xaxis = list(title = 'X (m)'),
      yaxis = list(title = 'Y (m)'),
      zaxis = list(title = 'Z (m)'),
      aspectmode = "data"
    )
  )

