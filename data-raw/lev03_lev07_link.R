## code to prepare `lev03_lev07_link` dataset goes here

data <- readr::read_csv("hybas_lev07_03_HYBAS_ID_v1c_fixed.csv")

lev03_lev07_link <- dplyr::select(data,
                                  "HYBAS_ID_lev7" = HYBAS_ID,
                                  "HYBAS_ID_lev3" = lev3_HYBAS_ID,
                                  MAIN_BAS)

usethis::use_data(lev03_lev07_link, overwrite = TRUE)
