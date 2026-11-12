################################################################################
# Map of the EXPECTED number of births in 2025 by municipality (counterfactual
# from the model, median over posterior draws). No lake, no frame, no legend.
# Output: figure/fig_expected_births_map.png (same 10 x 6 in format as fig3.png),
# which is picked up by manuscript/method_schematic2.tex (Municipality-level box).
#
# Run from the project root:  source("reports/fig_expected_births_map.R")
################################################################################

source("R/000_setup.R")      # provides new_mun_sf, packages (dplyr, ggplot2, sf), code_root_path

## ---- config (same conventions as reports/mun_correlations.R) ----------------
map_year  <- 2025
save.date <- "20260625"
mod_name  <- "mod8_2025"     # main model: all citizenships, all parities
seed_id   <- 1
res_path  <- paste0(code_root_path, "results/2025/")

## ---- expected births per municipality ---------------------------------------
draws <- readRDS(paste0(res_path, save.date, "_", mod_name, "_seedid", seed_id,
                        "_excess_birth_year_mun_draw_df.RDS"))

exp_mun_df <- draws %>%
  filter(year == map_year) %>%
  group_by(mun_id, draw) %>%
  dplyr::summarise(n_pred = sum(n_pred), .groups = "drop") %>%   # expected births, all ages, per draw
  group_by(mun_id) %>%
  dplyr::summarise(n_exp = median(n_pred), .groups = "drop") %>% # median over draws
  dplyr::mutate(mun_id = as.numeric(mun_id))

## ---- map (lake removed: it is simply not drawn) ------------------------------
map_sf <- exp_mun_df %>%
  left_join(new_mun_sf %>% dplyr::mutate(mun_id = as.numeric(mun_id)), by = "mun_id") %>%
  st_as_sf()

## ---- the three biggest cities: dot + leader line + name (same logic/offsets as fig3) ----
mpc <- 15000                 # metres of map per cm of the figure (tune like in figures.qmd)
city_offsets <- tibble::tribble(
  ~mun_name2, ~label,   ~dx_cm, ~dy_cm, ~hjust,
  "Genève",   "Geneva",    1.0,   -0.4,      0,
  "Basel",    "Basel",    -0.5,    0.5,      1,
  "Zürich",   "Zurich",   -2.0,    2.2,      1
)
city_xy <- new_mun_sf %>%
  filter(mun_name2 %in% city_offsets$mun_name2) %>%
  group_by(mun_name2) %>% dplyr::summarise(.groups = "drop") %>%
  sf::st_point_on_surface() %>%
  dplyr::mutate(cx = sf::st_coordinates(.)[, 1], cy = sf::st_coordinates(.)[, 2]) %>%
  sf::st_drop_geometry() %>%
  left_join(city_offsets, by = "mun_name2") %>%
  dplyr::mutate(lx = cx + dx_cm * mpc, ly = cy + dy_cm * mpc)

# light orange -> dark red (same colours as the age-15 / age-50 curves of the schematic);
# log scale because municipality sizes are very skewed
fig_exp <- ggplot(map_sf) +
  geom_sf(aes(fill = n_exp), color = NA) +
  scale_fill_gradient(low = "#FBE3C2", high = "#9E2B25", trans = "log10",
                      guide = "none") +                          # no legend
  geom_segment(data = city_xy, aes(x = cx, y = cy, xend = lx, yend = ly),
               inherit.aes = FALSE, linewidth = 0.3, colour = "grey20") +
  geom_point(data = city_xy, aes(x = cx, y = cy),
             inherit.aes = FALSE, size = 0.7, colour = "grey20") +
  geom_text(data = city_xy, aes(x = lx, y = ly, label = label, hjust = hjust),
            inherit.aes = FALSE, vjust = 0.5, size = 3) +
  coord_sf(clip = "off") +                                       # labels may sit outside the map
  theme_void() +
  theme(legend.position = "none")

ggsave(paste0(code_root_path, "figure/fig_expected_births_map.png"),
       plot = fig_exp, width = 10, height = 6, units = "in", dpi = 600)
