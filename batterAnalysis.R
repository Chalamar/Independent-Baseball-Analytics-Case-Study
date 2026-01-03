library(DBI)
library(RSQLite)
#connection to SQL
con <- dbConnect(SQLite(), ":memory:")
dbWriteTable(
  con,
  "batter_data",
  Batter_Data,
  overwrite = TRUE
)
  # confirm column names
ls()                 # confirm object name
head(Batter_Data)    # confirm data loaded
colnames(Batter_Data)

#batter xbh (balls in play only)
xbh_data <- dbGetQuery(con, "
SELECT
  pitch_type,
  COUNT(*) AS bip,
  SUM(CASE
        WHEN event_result IN ('double','triple','home_run')
        THEN 1 ELSE 0
      END) AS xbh,
  ROUND(
    1.0 * SUM(CASE
                WHEN event_result IN ('double','triple','home_run')
                THEN 1 ELSE 0
              END) / COUNT(*),
    3
  ) AS xbh_rate,
  ROUND(AVG(hit_exit_velocity), 1) AS avg_ev
FROM batter_data
WHERE
  is_ball_in_play = 1
  AND pitch_type IS NOT NULL
GROUP BY pitch_type
HAVING COUNT(*) >= 25
ORDER BY xbh_rate DESC, avg_ev DESC
")

#chase whiff rate
profile_by_pitch <- dbGetQuery(con, "SELECT
  pitch_type,
  COUNT(*) AS pitches,
  SUM(CASE WHEN is_swing = 1 THEN 1 ELSE 0 END) AS swings,
  ROUND(
    1.0 * SUM(CASE WHEN is_swing_and_miss = 1 THEN 1 ELSE 0 END) /
    NULLIF(SUM(CASE WHEN is_swing = 1 THEN 1 ELSE 0 END), 0),
    3
  ) AS whiff_rate,
  ROUND(
    1.0 * SUM(CASE WHEN is_chase = 1 THEN 1 ELSE 0 END) /
    NULLIF(SUM(CASE WHEN is_swing = 1 THEN 1 ELSE 0 END), 0),
    3
  ) AS chase_rate
FROM batter_data
WHERE
  is_pitch = 1
  AND pitch_type IS NOT NULL
GROUP BY pitch_type
HAVING COUNT(*) >= 50
ORDER BY whiff_rate DESC;")

dbDisconnect(con)

#R visuals of SQL results 

library(tidyverse)
library(scales)

xbh_plot_df <- xbh_data %>%
  mutate(
    pitch_type = fct_reorder(pitch_type, xbh_rate),
    xbh_lbl = percent(xbh_rate, accuracy = 0.1)  # change to 1 for whole %
  )

ggplot(xbh_plot_df, aes(x = xbh_rate, y = pitch_type)) +
  geom_segment(aes(x = 0, xend = xbh_rate, yend = pitch_type),
               linewidth = 1, alpha = 0.6) +
  geom_point(aes(size = bip), alpha = 0.9) +
  geom_text(aes(label = xbh_lbl),
            hjust = -0.15, size = 5) +
  scale_x_continuous(
    labels = percent_format(accuracy = 1),
    breaks = seq(0, 0.20, by = 0.05),
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title = "Player A Extra-Base Hit Rate by Pitch Type",
    subtitle = "Dot size = balls in play (BIP)",
    x = "XBH Rate (per BIP)",
    y = NULL,
    size = "BIP"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 12)
  )

wc_df <- profile_by_pitch %>%
  mutate(
    label = paste0(pitch_type, "\nSwings: ", swings)
  )

chase_whiff<-ggplot(wc_df, aes(x = chase_rate, y = whiff_rate)) +
  geom_point(aes(size = swings), alpha = 0.9) +
  geom_text(aes(label = pitch_type), vjust = -0.8, size = 4) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Player A Swing Profile by Pitch Type",
    subtitle = "Higher-right = best put-away pitches (more chase + more whiffs)",
    x = "Chase Rate (per swing)",
    y = "Whiff Rate (per swing)",
    size = "Swings"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

# --- 1) Prep: normalize vertical location + define XBH flag ---
batter_plot_df <- Batter_Data %>%
  filter(
    is_pitch == TRUE,
    !is.na(pitch_plate_location_side),
    !is.na(pitch_plate_location_height),
    !is.na(strikezone_top),
    !is.na(strikezone_bottom)
  ) %>%
  mutate(
    z_norm = (pitch_plate_location_height - strikezone_bottom) /
      (strikezone_top - strikezone_bottom),
    is_xbh = event_result %in% c("double", "triple", "home_run")
  )

# --- 2) Choose pitch types to show (keep it tight for a slide) ---
pitch_types_to_plot <- c("four_seam", "curveball", "sweeper")

batter_plot_df <- batter_plot_df %>%
  filter(pitch_type %in% pitch_types_to_plot) %>%
  mutate(pitch_type = factor(pitch_type, levels = pitch_types_to_plot))

# --- 3) Plot: density of all pitches + XBH overlay ---
ggplot(batter_plot_df, aes(x = pitch_plate_location_side, y = z_norm)) +
  stat_density_2d(
    aes(fill = after_stat(level)),
    geom = "polygon",
    alpha = 0.55
  ) +
  geom_point(
    data = batter_plot_df %>% filter(is_xbh),
    aes(color = event_result),
    size = 2.2,
    alpha = 0.9
  ) +
  scale_color_manual(
    values = c(
      "home_run" = "red",
      "double"   = "orange",
      "triple"   = "purple"
    )
  ) +
  scale_y_continuous(
    breaks = seq(-0.5, 2.0, by = 0.5),
    labels = function(x) paste0(x)
  ) +
  geom_hline(yintercept = c(0, 1), linetype = "dashed") +
  geom_vline(xintercept = c(-0.708, 0.708), linetype = "dashed") +
  coord_fixed() +
  facet_wrap(~ pitch_type, nrow = 1) +
  labs(
    title = "Player A Damage Map by Pitch Type",
    subtitle = "Density = pitch locations seen; colored points = extra-base hits allowed",
    x = "Plate Location (Side)",
    y = "Normalized Height (z_norm)",
    color = "XBH Result"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold")
  )