library(tidyverse)
View(Pitcher_data)
Pitcher_data <- Pitcher_data %>%
  mutate(
    z_norm = (pitch_plate_location_height - strikezone_bottom) /
      (strikezone_top - strikezone_bottom)
  )
ggplot(Pitcher_data,
       aes(x = pitch_plate_location_side,
           y = z_norm)) +
  stat_density_2d(aes(fill = after_stat(level)),
                  geom = "polygon",
                  alpha = 0.7) +
  facet_wrap(~ pitch_type) +
  geom_hline(yintercept = c(0, 1), linetype = "dashed") +
  geom_vline(xintercept = c(-0.708, 0.708), linetype = "dashed") +
  coord_fixed() +
  theme_minimal()
"z_norm" %in% colnames(Pitcher_data)
Zone_summary <- Pitcher_data %>%
  mutate(
    zone_band = case_when(
      z_norm < 0 ~ "Below",
      z_norm <= 0.33 ~ "Low",
      z_norm <= 0.66 ~ "Middle",
      z_norm <= 1 ~ "High",
      TRUE ~ "Above"
    )
  ) %>%
  group_by(pitch_type, zone_band) %>%
  summarize(
    avg_ev = mean(hit_exit_velocity, na.rm = TRUE),
    hh_rate = mean(hit_exit_velocity >= 95, na.rm = TRUE),
    n = n()
  ) %>%
  filter(n > 50)
View(Zone_summary)
ggplot(Zone_summary,
       aes(x = zone_band,
           y = avg_ev,
           fill = pitch_type)) +
  geom_bar(stat = "identity",
           position = "dodge") +
  labs(
    title = "Average Exit Velocity by Pitch Type and Zone Band",
    x = "Zone Band",
    y = "Average Exit Velocity (mph)"
  ) +
  theme_minimal()
Pitcher_data <- Pitcher_data %>%
  mutate(hard_hit = !is.na(hit_exit_velocity) & hit_exit_velocity >= 95)

ggplot(Pitcher_data, aes(x = pitch_plate_location_side, y = z_norm)) +
  stat_density_2d(
    aes(fill = after_stat(level)),
    geom = "polygon",
    alpha = 0.7
  ) +
  geom_point(
    data = Pitcher_data %>%
      filter(
        event_result %in% c("home_run", "double", "triple"),
        !is.na(hit_exit_velocity)
      ),
    aes(x = pitch_plate_location_side,
        y = z_norm,
        color = event_result),
    inherit.aes = FALSE,
    alpha = 0.9,
    size = 2
  ) +
  scale_color_manual(
    values = c(
      "home_run" = "red",
      "double"   = "orange",
      "triple"   = "purple"
    )
  ) + facet_wrap(~ pitch_type) +
  geom_hline(yintercept = c(0, 1), linetype = "dashed") +
  geom_vline(xintercept = c(-0.708, 0.708), linetype = "dashed") +
  coord_fixed() +
  theme_minimal()
pitch_mix_yoy <- Pitcher_data %>%
  filter(is_pitch == TRUE) %>%
  group_by(year, pitch_type) %>%
  summarize(pitches = n(), .groups = "drop") %>%
  group_by(year) %>%
  mutate(
    total_pitches = sum(pitches),
    pitch_mix_pct = pitches / total_pitches
  ) %>%
  ungroup()
ggplot(pitch_mix_yoy,
       aes(x = pitch_type, y = pitch_mix_pct, fill = factor(year))) +
  geom_col(position = "dodge") +
  scale_fill_manual(
    values = c(
      "2024" = "orange",
      "2025" = "black"
    )
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Pitch Mix Percentage by Pitch Type and Year",
    x = "Pitch Type",
    y = "Usage (%)",
    fill = "Year"
  ) +
  theme_minimal()

velo_by_inning <- Pitcher_data %>%
  filter(is_pitch == TRUE, !is.na(pitch_release_velocity)) %>%
  group_by(year, inning) %>%
  summarize(
    avg_velo = mean(pitch_release_velocity, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(n >= 30)

ggplot(velo_by_inning,
       aes(x = inning, y = avg_velo, color = factor(year))) +
  geom_line() +
  geom_point() +
  scale_color_manual(
    values = c("2024" = "orange", "2025" = "black")
  ) +
  labs(
    title = "Average Pitch Velocity by Inning",
    x = "Inning",
    y = "Average Velocity (mph)",
    color = "Year"
  ) +
  theme_minimal()

velo_by_pitch_num <- Pitcher_data %>%
  filter(is_pitch == TRUE, !is.na(pitch_release_velocity)) %>%
  mutate(
    pitch_num_bin = floor((pitch_number - 1) / 5) * 5 + 1
  ) %>%
  group_by(year, pitch_num_bin) %>%
  summarize(
    avg_velo = mean(pitch_release_velocity, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(n >= 20)

ggplot(velo_by_pitch_num,
       aes(x = pitch_num_bin, y = factor(year), fill = avg_velo)) +
  geom_tile() +
  labs(
    title = "Average Pitch Velocity by Pitch Number",
    x = "Pitch Number (binned)",
    y = "year",
    fill = "Avg Velo"
  ) +
  theme_minimal()

zone_damage_yoy <- Pitcher_data %>%
  filter(is_ball_in_play == TRUE) %>%   # excludes fouls
  mutate(
    hard_hit = hit_exit_velocity >= 95,
    zone_band = case_when(
      z_norm < 0 ~ "Below",
      z_norm <= 0.33 ~ "Low",
      z_norm <= 0.66 ~ "Middle",
      z_norm <= 1 ~ "High",
      TRUE ~ "Above"
    )
  ) %>%
  group_by(year, zone_band) %>%
  summarize(
    hh_rate = mean(hard_hit),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(n >= 25)
ggplot(zone_damage_yoy,
       aes(x = zone_band, y = hh_rate, fill = factor(year))) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(
    values = c("2024" = "orange", "2025" = "black")
  ) +
  labs(
    title = "Hard-Hit Rate by Zone Band (Year over Year)",
    subtitle = "Balls in play only",
    x = "Zone Band",
    y = "Hard-Hit Rate",
    fill = "Year"
  ) +
  theme_minimal()

whiff_chase_yoy <- Pitcher_data %>%
  filter(is_pitch == TRUE) %>%
  group_by(year, pitch_type) %>%
  summarize(
    whiff_rate = mean(is_swing_and_miss, na.rm = TRUE),
    chase_rate = mean(is_chase, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(n >= 50)
ggplot(whiff_chase_yoy,
       aes(x = pitch_type, y = whiff_rate, fill = factor(year))) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(
    values = c("2024" = "orange", "2025" = "black")
  ) +
  labs(
    title = "Whiff Rate by Pitch Type (Year over Year)",
    x = "Pitch Type",
    y = "Whiff Rate",
    fill = "Year"
  ) +
  theme_minimal()
ggplot(whiff_chase_yoy,
       aes(x = pitch_type, y = chase_rate, fill = factor(year))) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(
    values = c("2024" = "orange", "2025" = "black")
  ) +
  labs(
    title = "Chase Rate by Pitch Type (Year over Year)",
    x = "Pitch Type",
    y = "Chase Rate",
    fill = "Year"
  ) +
  theme_minimal()

count_whiff <- Pitcher_data %>%
  filter(is_pitch == TRUE) %>%
  mutate(
    count_state = case_when(
      is_pitcher_ahead ~ "Pitcher Ahead",
      is_hitter_ahead ~ "Pitcher Behind",
      TRUE ~ "Even"
    )
  ) %>%
  group_by(year, count_state) %>%
  summarize(
    whiff_rate = mean(is_swing_and_miss, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(count_state != "Even", n >= 50)
ggplot(count_whiff,
       aes(x = count_state, y = whiff_rate, fill = factor(year))) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(
    values = c("2024" = "orange", "2025" = "black")
  ) +
  labs(
    title = "Whiff Rate by Count Leverage (Year over Year)",
    x = "Count State",
    y = "Whiff Rate",
    fill = "Year"
  ) +
  theme_minimal()

csw_yoy <- Pitcher_data %>%
  filter(is_pitch == TRUE) %>%
  group_by(year, pitch_type) %>%
  summarize(
    csw = mean(is_strike | is_swing_and_miss, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(n >= 50)
ggplot(csw_yoy,
       aes(x = pitch_type, y = csw, fill = factor(year))) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(
    values = c("2024" = "orange", "2025" = "black")
  ) +
  labs(
    title = "Called Strike + Whiff Rate by Pitch Type (YoY)",
    x = "Pitch Type",
    y = "CSW%",
    fill = "Year"
  ) +
  theme_minimal()
