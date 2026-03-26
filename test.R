library(tidyverse)
library(cowplot)

ex_start <- as.Date("2001-07-18")
seeds_nuts <- read_csv("data/Nuts.csv") |>
  mutate(date = as.Date(as.character(date), format = "%Y%m%d"),
         day_of_ex = date - ex_start)

head(seeds_nuts)

seeds_underway <- read_csv("data/Underway.csv",
                           na = c("nd", "lt 0.20")) |>
  mutate(date = as.Date(as.character(date), format = "%Y%m%d"),
         datetime = as.POSIXct(paste(date, time_local_JST),
                               format = "%Y-%m-%d %H%M"),
         day_of_ex = date - ex_start) |>
  group_by(day_of_ex) |>
  mutate(sf6_interp = approx(x = datetime[!is.na(SF6)],
                             y = SF6[!is.na(SF6)],
                             xout = datetime,
                             method = "linear")$y,
         patch = case_when(sf6_interp < 3 ~ "Out",
                           sf6_interp > max(SF6, na.rm = TRUE) / 2 ~ "In",
                           .default = "Edge")) |>
  ungroup()
# Remove one outlier Fe measurement
seeds_underway$Diss_Fe[seeds_underway$Diss_Fe > 15] <- NA

seeds_underway_summary <- seeds_underway |>
  drop_na(patch) |>
  filter(patch != "Edge") |>
  group_by(day_of_ex, patch) |>
  summarize(across(c(SF6, Diss_Fe, NO3, Chl_a),
                   list(mean = \(x) mean(x, na.rm = TRUE),
                        sd = \(x) sd(x, na.rm = TRUE),
                        n = \(x) sum(!is.na(x)))),
            .groups = "drop")

sf6_plot <- ggplot(seeds_underway_summary,
                   aes(day_of_ex, SF6_mean, color = patch)) +
  geom_pointrange(aes(ymin = SF6_mean - SF6_sd,
                      ymax = SF6_mean + SF6_sd)) +
  scale_x_continuous(breaks = seq(0, 14, by = 2)) +
  scale_color_manual(values = c("cornflowerblue", "firebrick")) +
  labs(x = "Day of Experiment",
       y = "SF6 (fM)") +
  theme_classic(14) +
  theme(legend.position = "inside",
        legend.position.inside = c(0.99, 0.99),
        legend.justification = c(1, 1),
        legend.title = element_blank())

fe_plot <- ggplot(seeds_underway_summary,
                  aes(day_of_ex, Diss_Fe_mean, color = patch)) +
  geom_pointrange(aes(ymin = Diss_Fe_mean - Diss_Fe_sd,
                      ymax = Diss_Fe_mean + Diss_Fe_sd)) +
  scale_x_continuous(breaks = seq(0, 14, by = 2)) +
  scale_color_manual(values = c("cornflowerblue", "firebrick")) +
  labs(x = "Day of Experiment",
       y = "Fe (nM)") +
  theme_classic(14) +
  theme(legend.position = "inside",
        legend.position.inside = c(0.99, 0.99),
        legend.justification = c(1, 1),
        legend.title = element_blank())

no3_plot <- ggplot(seeds_underway_summary,
                   aes(day_of_ex, NO3_mean, color = patch)) +
  geom_pointrange(aes(ymin = NO3_mean - NO3_sd,
                      ymax = NO3_mean + NO3_sd)) +
  scale_x_continuous(breaks = seq(0, 14, by = 2)) +
  scale_color_manual(values = c("cornflowerblue", "firebrick")) +
  labs(x = "Day of Experiment",
       y = expression(NO[3] ~ (mu * M))) +
  theme_classic(14) +
  theme(legend.position = "inside",
        legend.position.inside = c(0.01, 0.01),
        legend.justification = c(0, 0),
        legend.title = element_blank())

chla_plot <- ggplot(seeds_underway_summary,
                    aes(day_of_ex, Chl_a_mean, color = patch)) +
  geom_pointrange(aes(ymin = Chl_a_mean - Chl_a_sd,
                      ymax = Chl_a_mean + Chl_a_sd)) +
  scale_x_continuous(breaks = seq(0, 14, by = 2)) +
  scale_color_manual(values = c("cornflowerblue", "firebrick")) +
  labs(x = "Day of Experiment",
       y = expression(Chl ~ italic(a) ~ (mu * g ~ l^-1))) +
  theme_classic(14) +
  theme(legend.position = "inside",
        legend.position.inside = c(0.01, 0.99),
        legend.justification = c(0, 1),
        legend.title = element_blank())

plot_grid(sf6_plot, fe_plot, no3_plot, chla_plot,
          byrow = FALSE,
          nrow = 2)
