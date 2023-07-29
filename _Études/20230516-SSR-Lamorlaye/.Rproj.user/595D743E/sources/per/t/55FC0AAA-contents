source("R/01-load-data.R")

((
 df
 %>% filter(type_hosp == "HC")
 %>% group_by(Année)
 %>% summarise(`Nb journées effectives` =
                sum(`Nb journées`) - sum(`Nb séjours`))
 %>% ungroup()
 %>% mutate(`Nb lits.journées` = c(rep(50 * 52 * 5, 4), 50*8*5),
            `Taux occupation HC` =
             100 *`Nb journées effectives` / `Nb lits.journées`)
 %>% select(1, 4)
) -> hc)

((
 df
 %>% filter(type_hosp == "HP")
 %>% group_by(Année)
 %>% summarise(`Nb journées` = sum(`Nb journées`))
 %>% ungroup()
 %>% mutate(`Nb lits.journées` = c(rep(52 * 52 * 5, 4), 50*8*5),
            `Taux occupation HP` =
             100 *`Nb journées` / `Nb lits.journées`)
 %>% select(1, 4)
) -> hp)

((
 hc
 %>% bind_cols(hp %>% select(2))
 %>% mutate(`Année` = 2019:2023)
) -> taux_occupation)

(
 taux_occupation
 %>% ggplot(aes(x = Année))
  +  geom_line(aes(y = `Taux occupation HC`, color = 'complète'), lwd = 2)
  +  geom_line(aes(y = `Taux occupation HP`, color = 'partielle'), lwd = 2)
  +  scale_color_manual('Hospitalisation', values=c('red', 'steelblue'))
  +  ylim(c(0, 105))
  +  theme_minimal()
  +  labs(title = "Taux d'occupation de 2019 à février 2023",
          x = 'Année',
          y = "Taux d'occupation")
)

