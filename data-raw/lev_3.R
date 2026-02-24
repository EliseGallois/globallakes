## code to prepare `lev_3` dataset goes here

#########################################################################
############ Variables (a.k.a. colors of the shapes) ####################
#########################################################################

####### nutrients
#data_nutrients    <- sf::read_sf("golem_app/merged_hydro_gnm_zone03_round.gpkg")
# instead, read in this "merged_hydro_gnm_zone03_round.csv"
#data_nutrients <- read.csv("merged_hydro_gnm_zone03_round.csv")
data_nutrients <- sf::st_drop_geometry(data_nutrients)

####### gbf 2 & gbf 7
#data_gbf <- sf::read_sf("golem_app/BasinATLAS_shiny_lev3_filt 1.gpkg")
data_gbf <- sf::st_drop_geometry(data_gbf)

#data_gbf7 <- sf::read_sf("golem_app/GBF_7/gbf7_lev3.gpkg")
data_gbf7 <- sf::st_drop_geometry(data_gbf7)

# add 'gbf7_' to the name of the columns of data_gbf7 apart from HYBAS_ID and geom
colnames(data_gbf7)[2:ncol(data_gbf7)] <- paste0("gbf7_", colnames(data_gbf7)[2:ncol(data_gbf7)])


###### all together
isTruthy(data_gbf$HYBAS_ID == data_nutrients$HYBAS_ID)

lev3_data <- cbind(data_gbf [, c(-15)], data_nutrients[,c(-1)])

# join data_gbf7 to lev3_data by HYBAS_ID
lev3_data <- merge(lev3_data, data_gbf7, by = "HYBAS_ID", all.x = T)

######## resolve factor classes

glc_lookup <- data.frame(
  GLC_ID = 1:23,
  GLC_Name = c(
    "Tree cover, broadleaved, evergreen",
    "Tree cover, broadleaved, deciduous, closed",
    "Tree cover, broadleaved, deciduous, open",
    "Tree cover, needle-leaved, evergreen",
    "Tree cover, needle-leaved, deciduous",
    "Tree cover, mixed leaf type",
    "Tree cover, regularly flooded, fresh",
    "Tree cover, regularly flooded, saline",
    "Mosaic: tree cover/other natural vegetation",
    "Tree cover, burnt",
    "Shrub cover, closed-open, evergreen",
    "Shrub cover, closed-open, deciduous",
    "Herbaceous cover, closed-open",
    "Sparse herbaceous or sparse shrub cover",
    "Regularly flooded shrub and/or herbaceous cover",
    "Cultivated and managed areas",
    "Mosaic: cropland/tree cover/other natural vegetation",
    "Mosaic: cropland/shrub and/or herbaceous cover",
    "Bare areas",
    "Water bodies",
    "Snow and ice",
    "Artificial surfaces",
    "No data"
  )
)



lev3_data <- lev3_data %>%
  left_join(glc_lookup, by = c("glc_cl_smj" = "GLC_ID")) %>%
  mutate(glc_cl_smj = factor(GLC_Name)) %>%
  select(-GLC_Name)


wetland_lookup <- data.frame(
  WET_ID = 1:13,
  WET_Name = c(
    "Lake",
    "Reservoir",
    "River",
    "Freshwater marsh / floodplain",
    "Swamp forest / flooded forest",
    "Coastal wetland",
    "Pan, brackish / saline wetland",
    "Bog, fen, mire (peatland)",
    "Intermittent Wetland / Lake",
    "50-100% wetland",
    "25-50% wetland",
    "0-25% wetland",
    "No data"
  )
)

lev3_data <- lev3_data %>%
  left_join(wetland_lookup, by = c("wet_cl_smj" = "WET_ID")) %>%
  mutate(wet_cl_smj = factor(WET_Name)) %>%
  select(-WET_Name)

lev3_data$wet_cl_smj <- addNA(lev3_data$wet_cl_smj)
levels(lev3_data$wet_cl_smj) <- c(levels(lev3_data$wet_cl_smj), "No data")
lev3_data$wet_cl_smj[is.na(lev3_data$wet_cl_smj)] <- "No data"

# attached to the simplified geometries

#1: simplify geoms
sf::sf_use_s2(FALSE)
#shapes_3 <- sf::st_as_sf(as.data.frame(dplyr::select(sf::read_sf("C:/Users/eligal/Desktop/Global_P_Dashboard/1_Data/golem_app/BasinATLAS_shiny_lev3_filt 1.gpkg"),
                #                                  HYBAS_ID)))
shapes_lev3_precise <- sf::st_cast(shapes_3, "MULTIPOLYGON")


shapes_lev3_precise_polygons <- sf::st_cast(shapes_lev3_precise, "POLYGON")

#simlify
shapes_lev3_polygons_sim <- rmapshaper::ms_simplify(shapes_lev3_precise_polygons, keep = 0.05)
#attach
lev3_vars <- merge(shapes_lev3_polygons_sim,
                   lev3_data,
                   by = "HYBAS_ID",
                   all.x = T)

# turn all NA into 0
lev3_vars[is.na(lev3_vars)] <- 0

class(lev3_vars)
usethis::use_data(lev3_vars, overwrite = TRUE)

#########################################################################
############ Outlines (borders of the shapes) ###########################
#########################################################################
#open data
shapes_lev3_polygons_sim <- rmapshaper::ms_simplify(shapes_lev3_precise_polygons, keep = 0.1)
lev3_lines <- sf::st_cast(shapes_lev3_polygons_sim, to = "MULTILINESTRING")
lev3_lines  <- sf::st_cast(lev3_lines, to = "LINESTRING")

usethis::use_data(lev3_lines, overwrite = TRUE)

#########################################################################
############ Shapes precise (to check what was clicked) #################
#########################################################################
lev3_shapes_precise <- shapes_3

