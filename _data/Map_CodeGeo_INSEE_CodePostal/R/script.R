require(tidyverse)
require(readxl)

((
 "./data/codepost2022.xlsx"
 %>% read_excel()
 %>% select(all_of(c("Code postal 2022",
                     "Code géographique PMSI 2022",
                     "Libellé poste")))
 %>% rename(`Code géographique` = `Code géographique PMSI 2022`,
            `Code postal` = `Code postal 2022`)
 %>% group_by(`Code géographique`)
 %>% mutate(N = n())
 %>% filter(N > 1)
) -> mapping_code_postaux_code_geo)

((
 "./data/HDF_zonages_proposés_201804262avec3codesenC.xlsx"
 %>% read_excel()
 %>% rename(Zone = 6)
 %>% select(all_of(c("Code géographique", "Zone")))
 # %>% rename(`Code INSEE` = `Code commune`)
) -> mapping_code_insee_code_geo)



((
 "./data/codes_communes_insee.xlsx"
 %>% read_excel()
 # %>% filter(DEP == 2, ARR == 21)
 %>% select(COM)
 %>% mutate(COM = str_c("0", COM))
 %>% rename(`Code INSEE` = COM)
) -> code_insee_communes_arrondissement_chateau_thierry)

(
 code_insee_communes_arrondissement_chateau_thierry
 %>% inner_join(mapping_code_insee_code_geo_aisne)
 %>% inner_join(mapping_code_postaux_code_geo_aisne)
 # %>% write_csv("./data/codes_insee_geo_postaux_arrondissement_CT")
 # %>% filter(str_starts(`Code INSEE`, "02|59|60|62|80"))
 %>% sample_n(10)
)
