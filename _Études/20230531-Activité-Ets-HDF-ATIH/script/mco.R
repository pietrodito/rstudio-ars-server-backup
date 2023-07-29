require(hdfmaps)

(`ex-DG`  <- read_excel("data/MCO_HDF.xlsx", skip = 4, sheet = 1))
(`ex-OQN` <- read_excel("data/MCO_HDF.xlsx", skip = 4, sheet = 2))


prepare_data <- function(df, prefixe_colonne) {
 df_name <- as.character(substitute(df))
 (
  df
  %>% select(num_range(str_c(prefixe_colonne, " "), 2019:2022))
  %>% rename_with(function(col) str_remove(col, prefixe_colonne) %>% str_trim())
  %>% summarise(across(1:4, sum))
  %>% mutate(across(1:4, ~ . / `2019` * 100))
  %>% pivot_longer(cols = 1:4, names_to = "Année", values_to = prefixe_colonne)
  %>% mutate(Année = as.integer(Année))
  %>% mutate(`Type établissement` = df_name)
 )
}

draw_evolution <- function(prefixe_colonne, complément_titre = "") {
 (
  bind_rows(
  prepare_data(`ex-DG`,  prefixe_colonne),
  prepare_data(`ex-OQN`, prefixe_colonne))
  %>% mutate(value = percent(!! rlang::sym(prefixe_colonne) / 100,
                             accuracy = 0.1))
  %>% ggplot(aes(x = Année,
                 y = !! rlang::sym(prefixe_colonne),
                 label = value,
                 color = `Type établissement`))
   +  geom_line(size = 2)
   +  geom_label(show.legend = FALSE)
   +  ggtitle(str_c("Évolution du", prefixe_colonne,
                    "entre 2019 et 2022", complément_titre,
                    sep = " "))
   + theme_minimal()
 )
}
draw_evolution("nombre de séjours/séances", "hors dialyse HDF")
draw_evolution("nombre de séjours hors séances", "HDF")
draw_evolution("volume économique hors séances", "HDF")
draw_evolution("volume économique", "HDF")


