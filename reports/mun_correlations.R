################################################################################
# Municipality-level correlations between relative excess/deficit of births and
# municipality characteristics, population-weighted (female 15-50).
#
# - relative excess per municipality = median over posterior draws of
#   sum(n_exc)/sum(n_pred) over the period 2017-2025 (unsmoothed draws,
#   same source as the decile analysis).
# - weighted Pearson correlation (unadjusted) and weighted PARTIAL correlation
#   adjusting for population density (pop_dens, the first density indicator used
#   in the main analysis).
#
# Run alone from the project root:  source("reports/mun_correlations.R")
################################################################################

library(dplyr)
library(tidyr)

## ---- paths & config ---------------------------------------------------------
# project root (edit if you run from elsewhere)
if (!exists("code_root_path")) code_root_path <- ""   # "" = current working dir is project root

last_year  <- 2025
year_range <- 2017:last_year
save.date  <- "20260625"
mod_name   <- "mod8_2025"          # main model (all citizenships, all parities)
seed_id    <- 1
res_path   <- paste0(code_root_path, "results/", ifelse(last_year == 2025, "2025/", ""))
dens       <- "pop_dens"           # first density indicator used in the main analysis

## ---- load covariates (savepoint) --------------------------------------------
# provides: new_pop_mun_df, rural_urban_df, pop_dens_df, sep_df3,
#           childcare_institutions_df, vote_mun_df
load(paste0(code_root_path,
            ifelse(last_year == 2024, "savepoint/cleaned_df.RData",
                                      "savepoint/cleaned2025_df.RData")))
pop_mun_df <- new_pop_mun_df        # weight source (female 15-50), 2025 municipal boundaries

## ---- load municipality excess draws -----------------------------------------
# all-parity main model -> no "_childless" suffix
excess_birth_year_mun_draw_df <- readRDS(paste0(
  res_path, save.date, "_", mod_name, "_", "seedid", seed_id, "_",
  "excess_birth_year_mun_draw_df", ".RDS"))

## ---- municipality covariates (identical to df_var0 in R/230) -----------------
df_var0 <- excess_birth_year_mun_draw_df %>%
  dplyr::select(mun_id, mun_name) %>% distinct() %>%
  # population of the latest analysis year (female 15-50, month 1) -> weight
  left_join(pop_mun_df %>% filter(year == max(year_range), month == 1) %>%
              group_by(mun_id) %>% dplyr::summarise(n_pop = sum(n), .groups = "drop"),
            by = "mun_id") %>%
  # rural / urban (9 categories, categorical -> excluded below)
  left_join(rural_urban_df %>% dplyr::select(mun_id, urban_rural_type2), by = "mun_id") %>%
  # population density indicators
  left_join(pop_dens_df %>% pivot_wider(id_cols = mun_id, names_from = "variable",
                                        values_from = "value"), by = "mun_id") %>%
  # socio-economic position
  left_join(sep_df3 %>% dplyr::select(mun_id, sep = ssep3_d_mean), by = "mun_id") %>%
  # childcare
  left_join(childcare_institutions_df %>% dplyr::select(mun_id, rel_empl_mean), by = "mun_id") %>%
  # votation (population-weighted yes proportion per object)
  left_join(vote_mun_df %>%
              dplyr::mutate(vote_object = paste0("votation_", vote_object)) %>%
              group_by(mun_id, vote_object) %>%
              dplyr::summarise(yes_prop = sum(yes_prop * n_pop) / sum(n_pop), .groups = "drop") %>%
              pivot_wider(id_cols = mun_id, names_from = vote_object, values_from = yes_prop),
            by = "mun_id")

## ---- relative excess per municipality: median over draws, 2017-2025 ----------
mun_rel <- excess_birth_year_mun_draw_df %>%
  filter(year %in% year_range) %>%
  group_by(mun_id, draw) %>%
  dplyr::summarise(rel_exc = sum(n_exc) / sum(n_pred), .groups = "drop") %>%  # per draw
  group_by(mun_id) %>%
  dplyr::summarise(rel_exc = median(rel_exc), .groups = "drop")               # median over draws

d <- df_var0 %>% left_join(mun_rel, by = "mun_id")

## ---- weighted correlation helpers -------------------------------------------
# weighted Pearson correlation
wcor <- function(x, y, w){
  ok <- is.finite(x) & is.finite(y) & is.finite(w)
  x <- x[ok]; y <- y[ok]; w <- w[ok]
  mx <- weighted.mean(x, w); my <- weighted.mean(y, w)
  sum(w * (x - mx) * (y - my)) / sqrt(sum(w * (x - mx)^2) * sum(w * (y - my)^2))
}
# weighted partial correlation of x and y given z (residualize both on z, weighted)
wpcor <- function(x, y, z, w){
  ok <- is.finite(x) & is.finite(y) & is.finite(z) & is.finite(w)
  x <- x[ok]; y <- y[ok]; z <- z[ok]; w <- w[ok]
  rx <- residuals(lm(x ~ z, weights = w))
  ry <- residuals(lm(y ~ z, weights = w))
  wcor(rx, ry, w)
}
# number of municipalities with complete data for a pair (for reference)
n_ok <- function(x, y, w) sum(is.finite(x) & is.finite(y) & is.finite(w))

## ---- variables to correlate -------------------------------------------------
vars <- setdiff(names(df_var0), c("mun_id", "mun_name", "urban_rural_type2", "n_pop"))

res <- bind_rows(lapply(vars, function(v){
  x <- d[[v]]; y <- d$rel_exc; w <- d$n_pop
  data.frame(
    variable              = v,
    n_mun                 = n_ok(x, y, w),
    r_unadjusted          = wcor(x, y, w),
    r_partial_adj_density = if (v == dens) NA_real_ else wpcor(x, y, d[[dens]], w)
  )
})) %>%
  dplyr::mutate(across(c(r_unadjusted, r_partial_adj_density), ~ round(.x, 3))) %>%
  arrange(desc(abs(r_unadjusted)))

print(res, row.names = FALSE)

## ---- save -------------------------------------------------------------------
out_file <- paste0(res_path, save.date, "_", mod_name, "_seedid", seed_id,
                   "_mun_correlations_", min(year_range), "_", max(year_range), ".csv")
write.csv(res, out_file, row.names = FALSE)
cat("\nSaved:", out_file, "\n")



library(ggplot2)

plot_vars <- function(xvar, yvar = "rel_exc", data = d){
  dd <- data[is.finite(data[[xvar]]) & is.finite(data[[yvar]]) & is.finite(data$n_pop), ]
  r  <- wcor(dd[[xvar]], dd[[yvar]], dd$n_pop)          # weighted Pearson (from the script)
  ggplot(dd, aes(.data[[xvar]], .data[[yvar]])) +
    geom_point(aes(size = n_pop), alpha = 0.35, colour = "steelblue") +
    geom_smooth(aes(weight = n_pop), method = "lm", se = TRUE,
                colour = "darkred", linewidth = 0.8) +          # pop-weighted fit
    scale_size_area(max_size = 10, name = "Population\n(female 15-50)",
                    labels = scales::comma) +
    labs(x = xvar, y = yvar,
         title = paste0(yvar, " vs ", xvar),
         subtitle = paste0("Weighted Pearson r = ", round(r, 3),
                           " (n = ", nrow(dd), " municipalities)")) +
    theme_minimal()
}

# examples:
plot_vars("pop_dens")            + scale_x_continuous(limits = c(0,5400))           # relative excess vs population density
plot_vars("rel_empl_mean") + scale_x_continuous(limits = c(0,0.1))             # vs childcare provision
plot_vars("votation_paternity_leave")       # vs paternity-leave vote
