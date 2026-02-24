## code to prepare `socio` dataset goes here

#########################################################################
############ Variables (a.k.a. colors of the shapes) ####################
#########################################################################

####### nutrients
#data_socio    <- sf::read_sf("gbf7_nations.gpkg")
data_socio <- sf::st_drop_geometry(data_socio)

# remove columns starting 'HYBAS'
data_socio <- data_socio[, !grepl("HYBAS", colnames(data_socio))]

# add 'gbf7_' to the name of the columns of data_gbf7 apart from HYBAS_ID and geom
colnames(data_socio)[2:ncol(data_socio)] <- paste0("gbf7_", colnames(data_socio)[2:ncol(data_socio)])

# remove cols 2:19
#data_socio <- data_socio[, -c(2:19)]

# attached to the simplified geometries

#1: simplify geoms
sf::sf_use_s2(FALSE)
#shapes_3 <- sf::st_read("BNDA_simplified_wgs84_fixed_crop.gpkg")
shapes_3 <- shapes_3 %>% group_by(isoclr, nam_en) %>% summarise(geom = sf::st_union(geom)) %>% sf::st_make_valid()
shapes_lev3_precise <- sf::st_cast(shapes_3, "MULTIPOLYGON")


shapes_lev3_precise_polygons <- sf::st_cast(shapes_lev3_precise, "POLYGON")

#simplify
shapes_lev3_polygons_sim <- rmapshaper::ms_simplify(shapes_lev3_precise_polygons, keep = 0.1)

shapes_dissolved <- shapes_lev3_polygons_sim %>%
  dplyr::group_by(nam_en) %>%
  dplyr::summarise(geom = sf::st_union(geom), .groups = "drop") %>%
  sf::st_make_valid()

data_socio_unique <- data_socio %>%
  group_by(nam_en) %>%
  summarise(across(everything(), ~ first(na.omit(.x))), .groups = "drop")



data_socio_data_only <- data_socio_unique %>%
  sf::st_drop_geometry()
data_socio <- shapes_dissolved %>%
  left_join(data_socio_data_only, by = "nam_en")

sum(duplicated(data_socio$nam_en))





# turn all NA into 0
data_socio[is.na(data_socio)] <- 0


# keep columns 1, 10 and everything 20 onwards
data_socio <- data_socio[, c(1, 4:ncol(data_socio),2)]
data_socio <- dplyr::select(data_socio, -HYBAS_ID.x)

# remove columns after 152
#data_socio <- data_socio[, 1:152]

# remove any column beginning HYBAS
data_socio <- data_socio[, !grepl("HYBAS", colnames(data_socio))]

# rename gbf7_geom to geom
colnames(data_socio)[colnames(data_socio) == "gbf7_geom"] <- "geom"

# remove all rows with no nam_en
data_socio <- data_socio[!is.na(data_socio$nam_en), ]
# removwe all duplicates
data_socio <- data_socio[!duplicated(data_socio$nam_en), ]

class(data_socio)
usethis::use_data(data_socio, overwrite = TRUE)

colnames(data_socio)



#########################################################################
############ Outlines (borders of the shapes) ###########################
#########################################################################
#open data
shapes_lev3_polygons_sim <- rmapshaper::ms_simplify(shapes_lev3_precise_polygons, keep = 0.1)
socio_lines <- sf::st_cast(shapes_lev3_polygons_sim, to = "MULTILINESTRING")
socio_lines  <- sf::st_cast(socio_lines, to = "LINESTRING")

usethis::use_data(socio_lines, overwrite = TRUE)

#########################################################################
############ Shapes precise (to check what was clicked) #################
#########################################################################
lev3_shapes_precise <- shapes_3

