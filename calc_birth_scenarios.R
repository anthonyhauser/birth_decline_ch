calc_births_scenarios <- function(birth_prob_by_age_df,
                                  n_women = 1000,
                                  young_multiplier = 0.5) {
  
  age_structure <- tibble(
    mother_age = 15:50,
    women = if_else(mother_age <= 30,n_women * young_multiplier,n_women))
  
  fert_2000 <- birth_prob_by_age_df %>%
    filter(year == 2000) %>%
    dplyr::select(mother_age, est_2000 = est)
  
  births_fixed <- birth_prob_by_age_df %>%
    distinct(year, mother_age) %>%
    left_join(age_structure, by = "mother_age") %>%
    left_join(fert_2000, by = "mother_age") %>%
    group_by(year) %>%
    summarise(
      births = sum(women * est_2000),
      .groups = "drop"
    ) %>%
    mutate(scenario = "fixed_2000")
  
  births_changing <- birth_prob_by_age_df %>%
    left_join(age_structure, by = "mother_age") %>%
    group_by(year) %>%
    summarise(
      births = sum(women * est),
      .groups = "drop"
    ) %>%
    mutate(scenario = "changing")
  
  bind_rows(births_fixed, births_changing)
}
exp_births_df <- calc_births_scenarios(birth_prob_by_age_df)

exp_births_df %>%
  ggplot(aes(x = year, y = births, color = scenario)) +
  geom_line(linewidth = 1) +
  labs(
    x = "Year",
    y = "Expected births",
    color = "Scenario"
  ) +
  theme_minimal()
