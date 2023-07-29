((
 df
 %>% group_by(Année, codeGeo)
 %>% summarise(`Nb. journées` = sum(`Nb journées`))
 %>% ungroup()
) -> nb_journées_par_zone_année)

((
 communes
 %>% group_by(`Code géographique PMSI 2022`)
 %>% filter(row_number() == 1)
 %>% ungroup()
) -> code_geo_points)

((
 df
 %>% filter(Année == 2022)
 %>% filter(`GDE REG` %in% c("HDF", "IDF"),
            type_hosp == "HC")
 %>% left_join(code_geo_points,
               by = c("codeGeo" = "Code géographique PMSI 2022"))
 %>% st_sf()
) -> patient_locs)


(
 communes
 %>% filter(str_detect(nom_commune_postal, "LAMORLAYE"))
) -> lamorlaye

((
 communes
 %>% filter(str_detect(nom_commune_postal, "SENLIS"),
            code_postal == 60300)
) -> senlis)
((
 communes
 %>% filter(str_detect(nom_commune_postal, "GOUVIEUX"))
) -> gouvieux)
((
 communes
 %>% filter(str_detect(nom_commune_postal, "PARIS"),
            code_postal == "75001")
) -> paris)
((
 communes
 %>% filter(str_detect(nom_commune_postal, "AMIENS"),
            code_postal == 80000)
) -> amiens)

(b = barycentre(patient_locs, patient_locs$`Nb journées`))
st_crs(b) <- 4236

(
 ggplot()
  + geom_sf(data = france)
  + geom_sf(data = autres_regions)
  + geom_sf(data = patient_locs, aes(alpha = `Nb journées`))
  + geom_sf(data = b, color = 'red')
  + geom_sf(data = lamorlaye, color = "green")
  + geom_sf(data = senlis, color = "yellow")
  + geom_sf(data = gouvieux, color = "blue")
  + geom_sf_text(data = paris, label = "PARIS")
  + geom_sf_text(data = amiens, label = "AMIENS")
  + ggplot2::coord_sf(xlim = c(1.4, 4.2), ylim = c(48.3, 50.05))
  + theme_alice()
)



