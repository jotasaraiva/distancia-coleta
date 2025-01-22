make_beautiful_gt <- function(gt_table) {
  gt_table |> 
    tab_options(
      table.font.size = 14,
      column_labels.font.size = 16
    ) %>%
    tab_style(
      style = list(cell_borders(
        sides = "all",
        weight = px(1),
        color = "gray"
      )),
      locations = cells_body()
    ) %>%
    tab_style(
      style = cell_text(
        weight = "bold",
        size = px(18),
        color = "white"
      ),
      locations = cells_column_labels()  # Customize the column labels (bold and white text)
    ) %>%
    tab_style(
      style = cell_fill(color = "#006400"),  # Green color for header
      locations = cells_column_labels()
    ) %>%
    tab_style(
      style = cell_text(color = "#333333"),  # Text color for body
      locations = cells_body()
    )
}

coletas_path <- here::here('data/tbg_amco_amostra_coletada_202501211400.csv')
fazendas_path <- here::here('data/_select_taa_id_as_id_amostra_tff_fzda_md_poligono_as_fazenda_geo_202501211405.csv')

coletas_data <- read_csv(coletas_path)
fazendas_data <- read_csv(fazendas_path)

data <-
  left_join(coletas_data,
            fazendas_data,
            by = c('fk_amst_amostra' = 'id_amostra')) |>
  filter(!is.na(fazenda)) |>
  select(-amostra_coord) |>
  st_as_sf(wkt = "fazenda_geom") |>
  filter(amco_nr_distancia < 10000) |>
  filter(str_detect(fazenda, '2024'))

plot_absoluto <-
  data |>
  ggplot(aes(amco_dt_data_coleta, amco_nr_distancia)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap('fazenda', scales = 'free_y') +
  theme_minimal() +
  labs(x = 'Data', y = 'Distância', color = 'Fazenda') +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))

plot_relativo <-
  data |>
  ggplot(aes(amco_dt_data_coleta, amco_nr_distancia)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap('fazenda') +
  theme_minimal() +
  labs(x = 'Data', y = 'Distância', color = 'Fazenda') +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))

tabela_cresc <-
  data |>
  as_tibble() |>
  filter(amco_nr_distancia < 600) |> 
  mutate(date = as.Date(amco_dt_data_coleta)) |>
  select(date, fazenda, amco_nr_distancia) |>
  summarise(dist_med = mean(amco_nr_distancia),
            .by = c(fazenda, date)) |>
  arrange(fazenda, date) |>
  summarise(.by = fazenda,
            dist_med_dif = last(dist_med) - first(dist_med)) |>
  arrange(dist_med_dif) |>
  gt() |>
  cols_label(fazenda = 'Fazenda', dist_med_dif = 'Crescimento Médio de Distância') |>
  make_beautiful_gt() |>
  tab_style(
    style = cell_fill(color = "lightgreen"),
    locations = cells_body(columns = dist_med_dif, rows = dist_med_dif > 0)
  ) |>
  tab_style(
    style = cell_fill(color = "salmon"),
    locations = cells_body(columns = dist_med_dif, rows = dist_med_dif < 0)
  )

ultimas_coletas <-
  data |>
  filter(amco_nr_distancia < 600) |>
  group_by(fazenda, fk_amst_amostra) |>
  filter(amco_dt_data_coleta == max(amco_dt_data_coleta)) |>
  ungroup() |> 
  as_tibble() |>
  mutate(date = as.Date(amco_dt_data_coleta)) |>
  summarise(dist_med = mean(amco_nr_distancia), .by = fazenda) |> 
  select(fazenda, dist_med) |>
  arrange(dist_med)

tabela_distancia <-
  ultimas_coletas |>
  gt() |>
  make_beautiful_gt() |> 
  cols_label(
    fazenda = 'Fazenda',
    dist_med = 'Distância Média por Fazenda'
  ) |> 
  data_color(
    dist_med,
    palette = 'RdYlGn',
    method = 'bin',
    bins = c(0, 25, 50, 75, 100, 125, 150, 600)
  )

dist_med_total <- mean(ultimas_coletas$dist_med)
