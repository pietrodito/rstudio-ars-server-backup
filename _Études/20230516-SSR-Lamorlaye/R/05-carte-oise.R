source("R/01-load-data.R")

((
 "./data/Carte code postaux 2018/codes_postaux_region.shp"
 %>% st_read()
 %>% mutate(DEP = str_c("0", DEP))
 %>% filter(DEP == "060")
) -> france)

((
 "./data/Code_Département_Région.csv"
 %>% read_csv()
 %>% bind_rows(tibble(Code = "020",
                      Département = "Corse",
                      Région = "Corse"))
) -> map_dpt_région)


((
 "./data/Carte code postaux 2018/codes_postaux_region.shp"
 %>% st_read()
 %>% mutate(DEP = str_c("0", DEP))
 %>% left_join(map_dpt_région, by = c("DEP" = "Code"))
) -> france)


((
 france
 %>% filter(ID == 60260)
) -> lamorlaye)

((
 "./data/HDF_zonages_proposés_201804262avec3codesenC.xlsx"
 %>% read_excel()
 %>% rename(Zone = 6)
 %>% select(3, 6)
 %>% unique()
) -> map_codeGeo_zone)

((
 df
 %>% filter(Année == year)
 %>% filter(Dépt == 60)
 %>% left_join(map_codeGeo_zone, by = c("codeGeo" = "Code géographique"))
 %>% group_by(Année, Zone)
 %>% summarise(`Nb journées` = sum(`Nb journées`))
) -> nb_journées_par_zone_année)

(GEOLOC <- read_csv("data/new_FINESS_GEOLOC_2022_plus_archives_HDF.csv"))

((
 nb_journées_par_zone_année
 %>% mutate(nofinesset =
             ifelse(`Zone` == "Beauvais",          "600008197",
             ifelse(`Zone` == "Compiègne - Noyon", "600002919",
             ifelse(`Zone` == "Creil - Senlis",    "600000467",
                    "590037768"))))
 %>% inner_join(GEOLOC)
) -> points_on_map)

(
 GEOLOC
 %>% filter(str_detect(rslongue, str_to_upper("BEAUVAIS")))
 # %>% filter(str_detect(rslongue, "LOUIS"))
 %>% filter(str_starts(nofinesset, "60"))
)


((
 france
 %>% filter(Région == "Hauts-de-France")
 %>% pull(Département)
 %>% unique()
 %>% sort()
 %>% as.list()
) -> nom_dpts)

(names(nom_dpts) <- nom_dpts)

((
 nom_dpts
 %>% map(function(dpt) {
  (
   france
   %>% filter(Département == dpt)
  )
 })
) -> dpts_CP)

((
 dpts_CP
 %>% map(function(dpt) {
   st_union(dpt)
 })
) -> dpts)


land_color <- c('antiquewhite1')
((
 nom_dpts
 %>% map_chr(function(dpt) {
  str_c("geom_sf(data = dpts[[\"", dpt, "\"]], fill = land_color)")
  })
 %>% str_c(collapse = " + \n")
 %>% str_c("ggplot() + ", .)
) -> geom_sf_calls)

land_color <- c('antiquewhite1')
year <- "2022"
points_on_map_this_year <- points_on_map %>% filter(Année == year)
(
 eval(str2expression(geom_sf_calls))
  + geom_sf(data = lamorlaye, fill = "green", color = "black")
  + geom_circle(data = points_on_map_this_year,
               aes(x0 = X, y0 = Y, r = 300*sqrt(`Nb journées`)),
               fill = 'red', colour = 'red', alpha = 0.2)
  + geom_text(data = points_on_map_this_year,
              aes(X, Y,
                  label = str_c(`Zone`, "\n",
                                `Nb journées`) ))
  + theme_void()
  + theme(panel.background = element_rect(fill = 'aliceblue'))
  + coord_sf(xlim = c(612000, 705000), ylim = c(6890000, 6950000))
)

