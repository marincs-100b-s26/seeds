library(tidyverse)
library(cowplot)

ex_start <- as_date("2001-07-18")
# seeds_nuts <- read_csv("data/Nuts.csv") |>
#   mutate(date = as.Date(as.character(date), format = "%Y%m%d"),
#          day_of_ex = date - ex_start)
#
# head(seeds_nuts)

linear_interpolate <- function(x, y) {
  mask <- !is.na(x) & !is.na(y)
  x2 <- x[mask]
  y2 <- y[mask]
  approx(x2, y2, xout = x)$y
}

#

seeds_underway <- read_csv("data/Underway.csv", na = "nd") |>
  mutate(datetime = as_datetime(paste(date, time_local_JST),
                                format = "%Y%m%d %H%M",
                                tz = "Asia/Tokyo"),
         date = as_date(datetime),
         day_of_ex = date - ex_start,
         Diss_Fe = parse_number(ifelse(Diss_Fe == "lt 0.20", 0, Diss_Fe)))

# One of these columns (SF6, Diss_Fe, NO3, Chl_a) has an outlier that's probably
# due to an instrument malfunction. Create histograms to identify it, then use
# filter() to remove it.
ggplot(seeds_underway, aes(SF6)) +
  geom_histogram()
ggplot(seeds_underway, aes(Diss_Fe)) +
  geom_histogram()
ggplot(seeds_underway, aes(NO3)) +
  geom_histogram()
ggplot(seeds_underway, aes(Chl_a)) +
  geom_histogram()

# Remove one outlier Fe measurement
seeds_underway <- filter(seeds_underway, Diss_Fe < 15)

# Inside the patch is "defined as >50% of the peak SF6 levels on that day"
# Outside the patch is "defined as SF6 <3 fM"
#
seeds_underway <- seeds_underway |>
  group_by(day_of_ex) |>
  mutate(sf6_interp = linear_interpolate(x = datetime, y = SF6),
         patch = case_when(sf6_interp < 3 ~ "Out",
                           sf6_interp > max(SF6, na.rm = TRUE) / 2 ~ "In",
                           .default = "Edge")) |>
  ungroup()

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
          nrow = 2,
          align = "v")
ggsave("figs/tsuda2003fig2.png",
       height = 4, width = 6, units = "in")
