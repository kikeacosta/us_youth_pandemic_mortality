# Figure 2 (state maps), Figure 3 (region bars), Table A1 (states by region) and
# Table A2 (rates by state): COVID-19 death rate, excess death rate and COVID-19
# deaths as a percentage of excess deaths, ages 0-24, 2020-2023.
#
# Input : data_inter/c19_deaths_by_state_and_age.rds (script 01)
#         data_inter/excess_states_age_u25.rds, excess_regions_u25.rds (script 04)
#         data_input/population/pop_state_age_0_24_2020_2023.txt
#         US state boundaries (tigris cartographic 1:5m, 2022), cached in
#         data_inter/us_states_cb_2022.rds after the first download
# Output: figures/fig2_maps_by_state.png, figures/fig3_bars_by_region.png (+ .csv)
#         tables/tableA1_regions_by_state.docx, tables/tableA2_rates_by_state.docx (+ .csv)
#         data_inter/fig2_fig3_state_region_data.rds (used by script 09)

source("code/00_functions.R")
suppressPackageStartupMessages({
  library(sf)
  library(tigris)
  library(patchwork)
  library(officer)
  library(flextable)
})

# Regions (bea, west_to_east, bea_lookup) are defined in code/00_functions.R

# COVID-19 deaths, ages 0-24 ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cov <- read_rds("data_inter/c19_deaths_by_state_and_age.rds")

cov2 <-
  cov %>%
  filter(age <= 24) %>%
  reframe(dx = sum(dx),
          .by = c(state, year)) %>%
  mutate(cause = "c19")

cov3 <-
  cov2 %>%
  bind_rows(cov2 %>%
              reframe(dx = sum(dx),
                      .by = c(year, cause)) %>%
              mutate(state = "us"))

cov3region <-
  cov3 %>%
  filter(state != "us") %>%
  left_join(bea_lookup, by = "state") %>%
  reframe(dx = sum(dx),
          .by = c(bea_region, year, cause))

# Excess deaths, ages 0-24 ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
exc <- read_rds("data_inter/excess_states_age_u25.rds")

# intervals: 95% CI of the baseline (lc, uc) carried to the excess
exc2 <-
  exc %>%
  mutate(dx = exc,
         dx_unc1 = dts - lc,
         dx_unc2 = dts - uc,
         dx_lb = pmin(dx_unc1, dx_unc2),
         dx_ub = pmax(dx_unc1, dx_unc2)) %>%
  select(state, year, dx_lb, dx, dx_ub) %>%
  mutate(cause = "excess")

# Regional excess: sum of state excess; interval = sum of the state bounds.
# A baseline fitted directly on regional totals is in
# data_inter/excess_regions_u25.rds (script 04) and compared in script 10.
exc2region <-
  exc %>%
  filter(state != "us") %>%
  left_join(bea_lookup, by = "state") %>%
  group_by(bea_region, year) %>%
  summarise(dx = sum(exc),
            dx_unc1 = sum(dts - lc),
            dx_unc2 = sum(dts - uc),
            .groups = "drop") %>%
  mutate(dx_lb = pmin(dx_unc1, dx_unc2),
         dx_ub = pmax(dx_unc1, dx_unc2)) %>%
  select(bea_region, year, dx_lb, dx, dx_ub) %>%
  mutate(cause = "excess")

stopifnot(all(exc2region$dx_lb <= exc2region$dx),
          all(exc2region$dx_ub >= exc2region$dx))

bsns <-
  exc %>%
  select(state, year, bsn)

# Population, ages 0-24 ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~
pop <- read_tsv("data_input/population/pop_state_age_0_24_2020_2023.txt", show_col_types = FALSE)

pop2 <-
  pop %>%
  select(state = 2,
         year = 5,
         pop = 6) %>%
  drop_na(state) %>%
  mutate(year = as.double(year))

pop3 <-
  pop2 %>%
  bind_rows(pop2 %>%
              reframe(pop = sum(pop),
                      .by = c(year)) %>%
              mutate(state = "us"))

pop2region <-
  pop2 %>%
  left_join(bea_lookup, by = "state") %>%
  reframe(pop = sum(pop),
          .by = c(bea_region, year))

# Combined state and region data ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
lvs <- c("US", exc2 %>% filter(state != "us") %>% pull(state) %>% unique() %>% sort())

dt <-
  bind_rows(cov3, exc2) %>%
  left_join(pop3, by = c("state", "year")) %>%
  left_join(bsns, by = c("state", "year")) %>%
  mutate(mx = 1e5 * dx / pop,
         mx_lb = 1e5 * dx_lb / pop,
         mx_ub = 1e5 * dx_ub / pop,
         psc = (bsn + dx) / bsn,
         state = ifelse(state == "us", "US", state),
         state = factor(state, levels = lvs),
         type = ifelse(state == "US", "US", "State"))

stopifnot(!anyNA(dt$pop), !anyNA(dt$mx))

dtregion <-
  bind_rows(cov3region, exc2region) %>%
  left_join(pop2region, by = c("bea_region", "year")) %>%
  mutate(mx = 1e5 * dx / pop,
         mx_lb = 1e5 * dx_lb / pop,
         mx_ub = 1e5 * dx_ub / pop,
         bea_region = factor(bea_region, levels = west_to_east))

# wide: c19 and excess rates side by side, plus their ratio (%)
dtWide <-
  dt %>%
  select(state, year, cause, mx) %>%
  pivot_wider(names_from = cause, values_from = mx) %>%
  mutate(prop = c19 / excess * 100)

# Figure 2: maps ====
# ~~~~~~~~~~~~~~~~~~~
# state boundaries, cached after the first download
shp_file <- "data_inter/us_states_cb_2022.rds"
if (file.exists(shp_file)) {
  states_sf <- read_rds(shp_file)
} else {
  options(tigris_use_cache = TRUE)
  states_sf <-
    states(cb = TRUE, year = 2022) %>%
    filter(!STUSPS %in% c("PR", "VI", "GU", "MP", "AS")) %>%
    shift_geometry() %>%          # Alaska and Hawaii moved below the mainland
    select(STUSPS, NAME, geometry)
  write_rds(states_sf, shp_file)
}

mymapdata <-
  states_sf %>%
  left_join(dtWide, by = c("NAME" = "state"))

stopifnot(nrow(mymapdata) == 51 * 4)

# region outlines
bea_sf <-
  states_sf %>%
  inner_join(bea_lookup %>% select(STUSPS = abb, bea_region), by = "STUSPS") %>%
  group_by(bea_region) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

pals <- list(
  c(low = "white", high = "#B2182B"),                   # white / red for c19
  c(low = "#1B7837", high = "#762A83"),                 # green / purple for excess
  c(neg = "#2166AC", low = "#FFFFCC", high = "#B2182B") # blue / yellow / red for ratio
)

vars <- c("c19", "excess", "prop")
var_labs <- c(c19    = "COVID-19 Death Rate (per 100,000)",
              excess = "Excess Death Rate (per 100,000)",
              prop   = "COVID-19 / Excess Deaths (%)")

map_theme <- theme_bw() +
  theme(
    axis.text          = element_blank(),
    axis.ticks         = element_blank(),
    panel.grid         = element_blank(),
    panel.background   = element_blank(),
    panel.spacing      = unit(2, "pt"),
    strip.placement    = "outside",
    strip.background.y = element_blank(),
    strip.text.y.left  = element_text(angle = 0, vjust = 1, size = 9, face = "bold", margin = margin(r = 3)),
    legend.position    = "bottom",
    legend.direction   = "horizontal",
    legend.box.spacing = unit(2, "pt"),
    legend.margin      = margin(0, 0, 0, 0),
    legend.text        = element_text(size = 7, margin = margin(t = 1)),
    plot.margin        = margin(2, 2, 2, 2),
    plot.title         = element_text(size = 10, margin = margin(b = 2))
  )

draw_map <- function(d, pal, var, fill_scale = NULL) {

  if (is.null(fill_scale)) {
    fill_scale <- scale_fill_gradient2(
      low = pal["low"], mid = "white", high = pal["high"],
      midpoint = 0, name = NULL, n.breaks = 4
    )
  }

  ggplot(d) +
    geom_sf(aes(fill = .data[[var]]), color = "grey40", linewidth = 0.2) +
    geom_sf(data = bea_sf, fill = NA, color = "black",
            linewidth = 0.5, inherit.aes = FALSE) +
    facet_grid(year ~ ., switch = "y") +
    fill_scale +
    guides(fill = guide_colourbar(
      theme = theme(legend.key.width  = unit(3.5, "cm"),
                    legend.key.height = unit(0.25, "cm"))
    )) +
    labs(title = unname(var_labs[var])) +
    coord_sf(expand = FALSE) +
    map_theme
}

# ratio map: signed pseudo-log scale so negative and very large ratios stay legible
make_prop_col <- function(d, pal, var) {

  cap  <- 1000                                   # values beyond +/-1000 are squished
  rng  <- range(d[[var]], na.rm = TRUE)
  lims <- c(max(min(rng[1], 0), -cap), cap)

  tr     <- scales::transform_pseudo_log(sigma = 1, base = 10)
  lims_t <- tr$transform(lims)
  zero   <- (0 - lims_t[1]) / diff(lims_t)      # where 0 falls on the legend bar

  brks <- c(-1000, -100, -10, 0, 10, 100, 1000)
  brks <- brks[brks >= lims[1] & brks <= lims[2]]

  fill_scale <- scale_fill_gradientn(
    colours   = c(pal["neg"], pal["low"], pal["high"]),
    values    = c(0, zero, 1),
    limits    = lims, oob = scales::squish,
    transform = tr,
    breaks    = brks,
    labels    = function(x){
      x <- round(x, 6)
      dplyr::case_when(
        x >=  cap ~ "≥1000",
        x <= -cap ~ "≤−1000",
        TRUE      ~ as.character(signif(x, 2))
      )},
    na.value  = "grey85",
    name      = NULL
  )

  draw_map(d, pal, var, fill_scale) +
    theme(plot.caption = element_text(size = 7, hjust = 0))
}

plots_v <- list(
  draw_map(mymapdata, pals[[1]], vars[1]),
  draw_map(mymapdata, pals[[2]], vars[2]),
  make_prop_col(mymapdata, pals[[3]], vars[3])
)

# year labels only on the first column
plots_v[-1] <- lapply(plots_v[-1], \(p) p + theme(strip.text.y.left = element_blank(),
                                                  strip.background.y = element_blank()))

p_v <- wrap_plots(plots_v, nrow = 1) & theme(plot.margin = margin(1, 2, 1, 2))

# figure size from the map aspect ratio
bb      <- st_bbox(mymapdata)
asp     <- unname((bb["ymax"] - bb["ymin"]) / (bb["xmax"] - bb["xmin"]))
n_cols  <- length(plots_v)
fig_w   <- 8
panel_w <- (fig_w - 0.6) / n_cols
n_years <- n_distinct(mymapdata$year)
fig_h   <- n_years * panel_w * asp + 0.8

ggsave("figures/fig2_maps_by_state.png", p_v, width = fig_w, height = fig_h, dpi = 300)

# Figure 3: bars by region ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cause_labs <- c(c19    = "COVID-19 Death Rate\n(per 100,000)",
                excess = "Excess Death Rate\n(per 100,000)",
                prop   = "COVID-19 / Excess Deaths (%)")
cause_cols <- c(c19 = "#B2182B", excess = "#762A83", prop = "#FD8D3C")

bar_data <-
  dtregion %>%
  bind_rows(dtregion %>%
              select(bea_region, year, cause, dx) %>%
              pivot_wider(values_from = dx, names_from = cause) %>%
              mutate(mx = c19 / excess * 100,    # ratio of deaths = ratio of rates
                     cause = "prop") %>%
              select(bea_region, year, cause, mx)) %>%
  mutate(bea_region = factor(bea_region, levels = rev(west_to_east)))  # Far West on top

p_bar <-
  ggplot(bar_data, aes(x = mx, y = bea_region, fill = cause)) +
  geom_col(width = 0.75) +
  geom_vline(xintercept = 0, linewidth = 0.3, colour = "grey40") +
  geom_errorbar(aes(xmin = mx_lb, xmax = mx_ub), width = 0.4, na.rm = TRUE) +
  geom_text(aes(label = case_when(
                  cause == "prop"   ~ sprintf("%.0f", mx),
                  cause == "c19"    ~ sprintf("%.2f", mx),
                  cause == "excess" ~ sprintf("%.1f", mx)),
                vjust = 1.1,
                hjust = ifelse(mx < 0, 1.1, -0.1)),
            size = 2.5) +
  scale_fill_manual(values = cause_cols, guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0.18, 0.2))) +
  facet_grid(year ~ cause, switch = "y", scales = "free_x",
             labeller = labeller(cause = cause_labs)) +
  labs(x = NULL, y = NULL) +
  theme_bars()

ggsave("figures/fig3_bars_by_region.png", p_bar,
       width = 7, height = 1.6 * n_distinct(bar_data$year) + 0.4, dpi = 300)

bar_data %>%
  select(year, bea_region, cause, value = mx, lower = mx_lb, upper = mx_ub) %>%
  arrange(year, cause, bea_region) %>%
  write_csv("figures/fig3_bars_by_region.csv")

# Table A2: c19, excess (95% CI), ratio by state ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dtWide_ci <-
  dt %>%
  filter(cause == "excess") %>%
  select(state, year, cause, mx_lb, mx_ub) %>%
  pivot_wider(names_from = cause, values_from = c(mx_lb, mx_ub))

ft_long <-
  dtWide %>%
  left_join(dtWide_ci, by = c("state", "year")) %>%
  mutate(across(c(c19, excess, mx_lb_excess, mx_ub_excess, prop), ~ round(.x, 2) + 0))

ft_long %>%
  select(state, year, c19_rate = c19, excess_rate = excess,
         excess_lower = mx_lb_excess, excess_upper = mx_ub_excess, c19_pct_of_excess = prop) %>%
  arrange(state != "US", state, year) %>%
  write_csv("tables/tableA2_rates_by_state.csv")

ft <-
  ft_long %>%
  mutate(excessci = sprintf("%.2f\n(%.2f, %.2f)", excess, mx_lb_excess, mx_ub_excess)) %>%
  select(state, year, c19, excessci, prop) %>%
  pivot_wider(names_from = year, values_from = c(c19, excessci, prop))

ft_df <-
  ft %>%
  mutate(is_us = state %in% c("United States", "US", "USA")) %>%
  arrange(desc(is_us), state) %>%
  select(-is_us)

# bottom header row: year for each value column
yr_labs <- setNames(as.list(sub(".*_", "", names(ft_df)[-1])), names(ft_df)[-1])

tab <- flextable(ft_df) %>%
  set_header_labels(state = "State", values = yr_labs) %>%
  add_header_row(
    values    = c("State", "COVID-19 death rate", "Excess death rate\n(95% CI)",
                  "COVID-19 as a percentage of excess death rate"),
    colwidths = c(1, 4, 4, 4)
  ) %>%
  merge_v(j = "state", part = "header") %>%
  colformat_double(digits = 2, na_str = "—") %>%
  theme_booktabs() %>%
  bold(part = "header") %>%
  bold(i = 1, part = "body") %>%                       # US row
  hline(i = 1, border = fp_border(width = 0.75), part = "body") %>%
  vline(j = c(1, 5, 9), border = fp_border(width = 0.5), part = "all") %>%
  align(j = -1, align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "all") %>%
  valign(valign = "bottom", part = "header") %>%
  fontsize(size = 8, part = "all") %>%
  padding(padding = 2, part = "all") %>%
  autofit()

# smaller font for the intervals
for (yr in 2020:2023) {
  col <- paste0("excessci_", yr)
  parts <- strsplit(ft_df[[col]], "\n", fixed = TRUE)
  est <- vapply(parts, `[`, "", 1)
  ci  <- vapply(parts, `[`, "", 2)
  tab <- flextable::compose(tab, j = col, part = "body",
                            value = as_paragraph(est, "\n", as_chunk(ci, props = fp_text_default(font.size = 7))))
}

# red text for negative ratios (negative excess)
for (col in grep("^prop_", names(ft_df), value = TRUE)) {
  tab <- color(tab, i = as.formula(paste0("~ ", col, " < 0")),
               j = col, color = "red", part = "body")
}

save_as_docx(
  tab,
  path = "tables/tableA2_rates_by_state.docx",
  pr_section = prop_section(page_size = page_size(orient = "landscape"))
)

# Table A1: states in each region ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ft_reg <-
  bea_lookup %>%
  mutate(bea_region = factor(bea_region, levels = west_to_east)) %>%
  select(bea_region, state) %>%
  arrange(bea_region, state)

write_csv(ft_reg %>% rename(region = bea_region), "tables/tableA1_regions_by_state.csv")

tab_reg <- flextable(ft_reg) %>%
  set_header_labels(state = "State", bea_region = "Region") %>%
  merge_v(j = "bea_region") %>%
  theme_booktabs() %>%
  bold(part = "header") %>%
  fontsize(size = 8, part = "all") %>%
  padding(padding = 2, part = "all") %>%
  autofit()

save_as_docx(tab_reg, path = "tables/tableA1_regions_by_state.docx")

write_rds(list(dt = dt, dtregion = dtregion, dtWide = dtWide),
          "data_inter/fig2_fig3_state_region_data.rds")
