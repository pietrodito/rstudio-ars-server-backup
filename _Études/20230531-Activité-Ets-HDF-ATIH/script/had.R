require(hdfmaps)

(file <- "data/HAD_HDF.xlsx")

(journées            <- read_excel(file, range = "A15:E27", sheet = 1))
(valo                <- read_excel(file, range = "A31:E32", sheet = 1))
(journées_hors_covid <- read_excel(file, range = "A15:E27", sheet = 2))
(valo_hors_covid     <- read_excel(file, range = "A31:E32", sheet = 2))

(journées            %<>% mutate(Type = "Total"))
(journées_hors_covid %<>% mutate(Type = "Hors COVID"))
(valo                %<>% mutate(Type = "Total"))
(valo_hors_covid     %<>% mutate(Type = "Hors COVID"))

(
 bind_rows(valo, valo_hors_covid)
 %>% mutate(across(2:5, ~ . / `2019` * 100))
 %>% pivot_longer(cols = 2:5, names_to = "Année",
                  values_to = "Valorisation économique")
 %>% mutate(across(Année, as.integer))
 %>% mutate(value = percent(`Valorisation économique` / 100, accuracy = 0.1))
 %>% ggplot(aes(x = Année, y = `Valorisation économique`, color = Type,
                label = value))
  +  geom_line(size = 2)
  +  geom_label(show.legend = FALSE)
  +  theme_minimal()
)

