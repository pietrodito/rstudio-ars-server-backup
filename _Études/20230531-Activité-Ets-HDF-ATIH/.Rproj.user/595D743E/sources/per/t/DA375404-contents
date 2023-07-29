require(hdfmaps)

file <- "data/SSR_HDF.xlsx"

(ssr <- read_excel(file, range = "B2:M3"))

names(ssr) <- c("nb journées HC 2019",
                "nb journées HP 2019",
                "Total journées 2019",
                "Nombre séjours HC 2019",
                "nb journées HC 2021",
                "nb journées HP 2021",
                "Total journées 2021",
                "Nombre séjours HC 2021",
                "nb journées HC 2022",
                "nb journées HP 2022",
                "Total journées 2022",
                "Nombre séjours HC 2022")


prepare_data <- function(prefixe_colonne) {
 (
  ssr
  %>% select(starts_with(prefixe_colonne))
  %>% rename_with(function(col) str_remove(col, prefixe_colonne) %>% str_trim())
  %>% mutate(across(everything(), ~ . / `2019` * 100))
  %>% pivot_longer(cols = 1:3, names_to = "Année", values_to = prefixe_colonne)
  %>% mutate(Année = as.integer(Année))
 )
}

draw_evolution <- function(prefixe_colonne, complément_titre = "") {
 (
  prepare_data(prefixe_colonne)
  %>% mutate(value = percent(!! rlang::sym(prefixe_colonne) / 100,
                             accuracy = 0.1))
  %>% ggplot(aes(x = Année,
                 y = !! rlang::sym(prefixe_colonne),
                 label = value))
   +  geom_line(size = 2)
   +  geom_label(show.legend = FALSE)
   +  ggtitle(str_c("Évolution du", prefixe_colonne,
                    "entre 2019 et 2022", complément_titre,
                    sep = " "))
   + theme_minimal()
 )
}


draw_evolution("nb journées HC")
draw_evolution("nb journées HP")
draw_evolution("Total journées")
draw_evolution("Nombre séjours HC")



(prefixe_col <- "nb journées HC")
prepare_data_t <- function(prefixe_col) {
 (
  prepare_data(prefixe_col)
  %>% pivot_wider(names_from = 1, values_from = 2)
  %>% mutate(type = prefixe_col)
 )
}
(
 prepare_data_t("nb journées HC")
 %>% bind_rows(prepare_data_t("nb journées HP"))
 %>% bind_rows(prepare_data_t("Total journées"))
 %>% bind_rows(prepare_data_t("Nombre séjours HC"))
 %>% pivot_longer(1:3, names_to = "Année", values_to = "Ratio 2019")
 %>% mutate(across(all_of("Année"), as.integer))
 %>% mutate(p =  percent(`Ratio 2019` / 100))
 %>% ggplot(aes(x = Année, y = `Ratio 2019`, color = type, label = p))
  +  geom_line(size = 2)
  +  geom_label(show.legend = F)
)

