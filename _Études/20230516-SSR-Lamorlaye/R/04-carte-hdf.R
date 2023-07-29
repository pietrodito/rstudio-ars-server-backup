source("R/01-load-data.R")

# ============
# --- DATA ---
# ============
((
 df
 %>% filter(`GDE REG` == "HDF")
 %>% group_by(Année, `DéptNom`)
 %>% summarise(`Nb. journées` = sum(`Nb journées`))
 %>% ungroup()
) -> nb_journées_par_zone_année)

(GEOLOC <- read_csv("data/new_FINESS_GEOLOC_2022_plus_archives_HDF.csv"))

((
 nb_journées_par_zone_année
 %>% mutate(nofinesset =
             ifelse(`DéptNom` == "OISE",          "600011571",
             ifelse(`DéptNom` == "SOMME",         "800000614",
             ifelse(`DéptNom` == "NORD",          "590000782",
             ifelse(`DéptNom` == "PAS DE CALAIS", "620030874",
             ifelse(`DéptNom` == "AISNE",         "020003794",
                    "590037768"))))))
 %>% inner_join(GEOLOC)
) -> points_on_map)

(
 GEOLOC
 %>% filter(str_detect(rslongue, str_to_upper("CLERMONT")))
 # %>% filter(str_detect(rslongue, "LOUIS"))
 %>% filter(str_starts(nofinesset, "60"))
)


# ===========
# --- MAP ---
# ===========
world_map_data <- ne_countries(scale = "medium", returnclass = "sf")

(
 world_map_data
 %>% filter(geounit == "Belgium" | geounit == "Switzerland" |
             geounit == "Germany" | geounit == "Luxembourg" )
) -> pays_limitrophes

mytheme <- theme(text = element_text(family = 'Avenir')
                 ,panel.grid.major = element_line(color = '#cccccc'
                                                   ,linetype = 'dashed'
                                                  ,size = .3
                 )
                 ,panel.background = element_rect(fill = 'aliceblue')
                 ,plot.title = element_text(size = 32)
                 ,plot.subtitle = element_text(size = 14)
                 ,axis.title = element_blank()
                 ,axis.text = element_text(size = 10)
)

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
 france
 %>% pull(Région)
 %>% unique()
 %>% sort()
 %>% as.list()
) -> nom_regions)

(names(nom_regions) <- nom_regions)

((
 nom_regions
 %>% map(function(region) {
  (
   france
   %>% filter(Région == region)
  )
 })
) -> regions_CP)

((
 regions_CP
 %>% map(function(region) {
   st_union(region)
 })
) -> regions)

(regions_proches_hdf <- c("Grand Est",
                          "Île-de-France", "Normandie",
                          "Centre-Val de Loire",
                          "Bourgogne-Franche-Comté"))

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

) -> geom_sf_calls_dpts)

((
 regions_proches_hdf
 %>% map_chr(function(region) {
  str_c("geom_sf(data = regions[[\"", region, "\"]], fill = \"white\")")
  })
 %>% str_c(collapse = " + \n")
 %>% str_c("ggplot() + ", .)
) -> geom_sf_calls_region)

(geom_sf_calls <- str_c(geom_sf_calls_region, " + ", geom_sf_calls_dpts))

land_color <- c('antiquewhite1')
year <- "2022"
points_on_map_this_year <- points_on_map %>% filter(Année == year)
(
 eval(str2expression(geom_sf_calls))
  # + geom_sf(data = regions[["Hauts-de-France"]], fill = land_color)
  + geom_sf(data = pays_limitrophes, fill = "white")
  + geom_sf(data = lamorlaye, fill = "green", color = "black")
  + geom_circle(data = points_on_map_this_year,
               aes(x0 = X, y0 = Y, r = 300*sqrt(`Nb. journées`)),
               fill = 'red', colour = 'red', alpha = 0.2)
  + geom_text(data = points_on_map_this_year,
              aes(X, Y,
                  label = str_c(`DéptNom`, "\n",
                                `Nb. journées`) ))
  + theme_void()
  + theme(panel.background = element_rect(fill = 'aliceblue'))
  + coord_sf(xlim = c(592000, 765000), ylim = c(6890000, 7100000))
)

