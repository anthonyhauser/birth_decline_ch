df %>%
  group_by(
    Ereignismonat,
    Mutter..Alter.in.erfüllten.Jahren,
    Vater..Alter.in.erfüllten.Jahren,
    Mutter..Wohngemeinde...Wohnstaat,
    Vater..Staatsangehörigkeit,
    Flag..Zivilstand.der.Mutter,
    Vater..Alter.in.erreichten.Jahren,
    Mutter..Alter.in.erreichten.Jahren,
    Mutter..Staatsangehörigkeit
  ) %>%
  mutate(
    group_id = cur_group_id(),
    n = n()
  ) %>%
  filter(n > 1) %>%
  arrange(-n,group_id) %>% View()
