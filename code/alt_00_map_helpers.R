# Helpers shared by the alternative state maps (alt_fig2_*.R): state and BEA
# region boundaries, a common map theme, the figure height for a given number
# of map columns, and one drawing function per kind of colour scale. Each
# drawing function takes the data (an sf object with one row per state x year
# and a `year` column) as its first argument.
#
# Sourced after code/00_functions.R; needs data_inter/us_states_cb_2022.rds
# (cached by script 06).

suppressPackageStartupMessages({
  library(sf)
  library(patchwork)
})

states_sf <- read_rds("data_inter/us_states_cb_2022.rds")

bea_sf <-
  states_sf %>%
  inner_join(bea_lookup %>% select(STUSPS = abb, bea_region), by = "STUSPS") %>%
  group_by(bea_region) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

map_theme <-
  theme_bw() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), panel.spacing = unit(2, "pt"),
        strip.placement = "outside", strip.background.y = element_blank(),
        strip.text.y.left = element_text(angle = 0, vjust = 1, size = 9, face = "bold",
                                         margin = margin(r = 3)),
        legend.position = "bottom", legend.direction = "horizontal",
        legend.box.spacing = unit(2, "pt"), legend.margin = margin(0, 0, 0, 0),
        legend.text = element_text(size = 7, margin = margin(t = 1)),
        plot.margin = margin(2, 2, 2, 2),
        plot.title = element_text(size = 9, margin = margin(b = 2)))

# figure height (inches) for four years of maps in n_cols columns at width fig_w
bb    <- st_bbox(states_sf)
asp   <- unname((bb["ymax"] - bb["ymin"]) / (bb["xmax"] - bb["xmin"]))
fig_h <- function(n_cols, fig_w) 4 * ((fig_w - 0.6) / n_cols) * asp + 0.9

# common skeleton: state fills, BEA outlines, one row per year
map_base <- function(dat, var, lims, title, show_years){
  p <-
    ggplot(dat) +
    geom_sf(aes(fill = pmax(pmin(.data[[var]], lims[2]), lims[1])),
            colour = "grey40", linewidth = 0.2) +
    geom_sf(data = bea_sf, fill = NA, colour = "black", linewidth = 0.5, inherit.aes = FALSE) +
    facet_grid(year ~ ., switch = "y") +
    labs(title = title) +
    coord_sf(expand = FALSE) +
    map_theme
  if (!show_years) p <- p + theme(strip.text.y.left = element_blank())
  p
}

# diverging blue-white-red scale centred on 0, values beyond +-lim clipped
draw_pscore_map <- function(dat, var, title, lim, legend_title, show_years = TRUE){
  map_base(dat, var, c(-lim, lim), title, show_years) +
    scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0,
                         limits = c(-lim, lim), name = legend_title,
                         labels = function(x) sprintf("%+.0f", x)) +
    guides(fill = guide_colourbar(title.position = "left", title.vjust = 0.85,
                                  theme = theme(legend.key.width = unit(4, "cm"),
                                                legend.key.height = unit(0.25, "cm"),
                                                legend.title = element_text(size = 8, margin = margin(r = 8)))))
}

# sequential white-red scale between lims, NA in grey
draw_share_map <- function(dat, var, title, lims, legend_title, show_years = TRUE, mid = 0){
  map_base(dat, var, lims, title, show_years) +
    scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", midpoint = mid,
                         limits = lims, name = legend_title, na.value = "grey80",
                         labels = function(x) sprintf("%.1f", x)) +
    guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5,
                                  theme = theme(legend.key.width = unit(3, "cm"),
                                                legend.key.height = unit(0.25, "cm"),
                                                legend.title = element_text(size = 8, margin = margin(r = 8)))))
}

# arbitrary colour ramp between lims with explicit breaks and labels
draw_component_map <- function(dat, var, title, lims, cols, breaks, labels, legend_title,
                               show_years = TRUE, na_col = "grey80"){
  map_base(dat, var, lims, title, show_years) +
    scale_fill_gradientn(colours = cols, values = scales::rescale(seq(lims[1], lims[2], length.out = length(cols))),
                         limits = lims, breaks = breaks, labels = labels, name = legend_title,
                         na.value = na_col) +
    guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5,
                                  theme = theme(legend.key.width = unit(3.6, "cm"),
                                                legend.key.height = unit(0.25, "cm"),
                                                legend.title = element_text(size = 8))))
}
