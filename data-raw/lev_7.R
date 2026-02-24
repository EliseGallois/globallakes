## code to prepare `lev_7` datasets goes here

#########################################################################
############ Variables (a.k.a. colors of the shapes) ####################
#########################################################################

####### nutrients
#data_nutrients    <- sf::read_sf("merged_hydro_gnm_zone07_round.gpkg")
data_nutrients    <- sf::st_drop_geometry(data_nutrients)
####### gbf 2 & gbf 7
#data_gbf <- sf::read_sf("BasinATLAS_shiny_lev7_filt.gpkg")
data_gbf <- sf::st_drop_geometry(data_gbf)

###### all together
isTruthy(data_gbf$HYBAS_ID == data_nutrients$HYBAS_ID)
lev7_data <- cbind(data_gbf [, c(-15)], data_nutrients[,c(-1)])

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



lev7_data <- lev7_data %>%
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

lev7_data <- lev7_data %>%
  left_join(wetland_lookup, by = c("wet_cl_smj" = "WET_ID")) %>%
  mutate(wet_cl_smj = factor(WET_Name)) %>%
  select(-WET_Name)

lev7_data$wet_cl_smj <- addNA(lev7_data$wet_cl_smj)
levels(lev7_data$wet_cl_smj) <- c(levels(lev7_data$wet_cl_smj), "No data")
lev7_data$wet_cl_smj[is.na(lev7_data$wet_cl_smj)] <- "No data"



# attached to the simplified geometries

#1: simplify geoms
sf::sf_use_s2(FALSE)
#shapes_7 <- sf::st_as_sf(as.data.frame(dplyr::select(sf::read_sf("BasinATLAS_shiny_lev7_filt.gpkg"),
                                                    # HYBAS_ID)))

shapes_lev7_precise <- sf::st_cast(shapes_7, "MULTIPOLYGON")

shapes_lev7_precise_polygons <- dplyr::select(sf::st_cast(shapes_lev7_precise, "POLYGON"),
                                              HYBAS_ID)
#simplify
shapes_lev7_polygons_sim <- rmapshaper::ms_simplify(shapes_lev7_precise_polygons, keep = 0.2)

lev7_vars <- merge(shapes_lev7_polygons_sim,
                   lev7_data,
                   by = "HYBAS_ID",
                   all.x = T)


# Load or prepare `lev3_vars` (example: loading from file)
#load("C:/Users/eligal/Desktop/Global_P_Dashboard/BasinATLASgol/data/lev3_vars.rda")

# Align columns between `lev7_vars` and `lev3_vars`
missing_cols_lev7 <- setdiff(colnames(lev3_vars), colnames(lev7_vars))
lev7_vars[missing_cols_lev7] <-  0

missing_cols_lev3 <- setdiff(colnames(lev7_vars), colnames(lev3_vars))
lev3_vars[missing_cols_lev3] <- 0


class(lev7_vars)
usethis::use_data(lev7_vars, overwrite = TRUE)

#########################################################################
############ Outlines (borders of the shapes) ###########################
#########################################################################
shapes_lev7_polygons_sim <- rmapshaper::ms_simplify(shapes_lev7_precise_polygons, keep = 0.3)
lev7_lines <- sf::st_cast(shapes_lev7_polygons_sim, to = "MULTILINESTRING")
lev7_lines  <- sf::st_cast(lev7_lines, to = "LINESTRING")

usethis::use_data(lev7_lines, overwrite = TRUE)

#########################################################################
############ Shapes precise (to check what was clicked) #################
#########################################################################
lev7_shapes_precise <- shapes_7

usethis::use_data(lev7_shapes_precise, overwrite = TRUE)



# rbind lev7 and lev3 vars
test_vars <- rbind(lev7_vars, lev3_vars)

# alt version -----
## code to prepare `lev_7` datasets goes here

#########################################################################
############ Variables (a.k.a. colors of the shapes) ####################
#########################################################################

####### nutrients
#data_nutrients    <- sf::read_sf("smaller_topocat.gpkg")
#data_nutrients    <- sf::read_sf("topo_250ha_gnm_rivers.gpkg")

# keep only columns beginning Hylak and SSP
data_nutrients <- data_nutrients[, grep("^Hylak|^SSP", colnames(data_nutrients), value = TRUE)]

#load("data/globo_topo_poly.rda")

# keep only rows where data_nutrients Hylak_id match globo_topo_poly Hylak_id
data_nutrients <- data_nutrients[data_nutrients$Hylak_id %in% globo_topo_poly$Hylak_id,]


(p2 <- ggplot() +
    geom_sf(data = world, fill = "white", color = "gray40", linewidth = 0.1) +
    geom_sf(data = data_nutrients, aes(fill = Hylak_id)) +
    #scale_col_viridis_d(name = "Transnational Catchment", option = "viridis") +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
    ))

data_nutrients    <- sf::st_drop_geometry(data_nutrients)

######## gbf 2 & gbf 7
#data_gbf <- sf::read_sf("C:/Users/eligal/Desktop/Global_P_Dashboard/1_Data/golem_app/BasinATLAS_shiny_lev7_filt.gpkg")
data_gbf <- sf::st_drop_geometry(data_gbf)

###### all together
isTruthy(data_gbf$HYBAS_ID == data_nutrients$HYBAS_ID)
lev7_data <- cbind(data_gbf [, c(-15)], data_nutrients[,c(-1)])

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



lev7_data <- lev7_data %>%
  left_join(glc_lookup, by = c("glc_cl_smj" = "GLC_ID")) %>%
  mutate(glc_cl_smj = factor(GLC_Name)) %>%
  select(-GLC_Name)

lev7_vars <- lev7_vars %>%
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

lev7_data <- lev7_data %>%
  left_join(wetland_lookup, by = c("wet_cl_smj" = "WET_ID")) %>%
  mutate(wet_cl_smj = factor(WET_Name)) %>%
  select(-WET_Name)

lev7_vars <- lev7_vars %>%
  left_join(wetland_lookup, by = c("wet_cl_smj" = "WET_ID")) %>%
  mutate(wet_cl_smj = factor(WET_Name)) %>%
  select(-WET_Name)

lev7_data$wet_cl_smj <- addNA(lev7_data$wet_cl_smj)
levels(lev7_data$wet_cl_smj) <- c(levels(lev7_data$wet_cl_smj), "No data")
lev7_data$wet_cl_smj[is.na(lev7_data$wet_cl_smj)] <- "No data"

lev7_vars$glc_cl_smj <- addNA(lev7_vars$glc_cl_smj)
levels(lev7_vars$glc_cl_smj) <- c(levels(lev7_vars$glc_cl_smj), "No data")
lev7_vars$glc_cl_smj[is.na(lev7_vars$glc_cl_smj)] <- "No data"

# attached to the simplified geometries

#1: simplify geoms
## code to prepare `test_leafgl` dataset goes here
sf::sf_use_s2(FALSE)
#shapes_7 <- sf::st_as_sf(as.data.frame(dplyr::select(sf::read_sf("topocat_lake_catch_Hylak_id.gpkg"),
                                                                      # Hylak_id)))
#load("globo_topo_poly.rda")

shapes_7<- shapes_7[shapes_7$Hylak_id %in% globo_topo_poly$Hylak_id,]
shapes_7<- shapes_7[shapes_7$Hylak_id %in% data_nutrients$Hylak_id,]

shapes_lev7_precise <- sf::st_cast(shapes_7, "MULTIPOLYGON")

shapes_lev7_precise_polygons <- dplyr::select(sf::st_cast(shapes_lev7_precise, "POLYGON"),
                                              Hylak_id)

shapes_lev7_polygons_sim <- sf::st_simplify(shapes_lev7_precise_polygons, dTolerance = 0.02)

#simplify
shapes_lev7_polygons_sim <- rmapshaper::ms_simplify(shapes_lev7_precise_polygons, keep = 0.2)


n <- nrow(shapes_lev7_precise_polygons) / 4
split_data <- split(shapes_lev7_precise_polygons, ceiling(seq_len(nrow(shapes_lev7_precise_polygons)) / n))

#  ms_simplify to each chunk
simplified_chunks <- lapply(split_data, function(chunk) {
  rmapshaper::ms_simplify(chunk, keep = 0.3)
})

shapes_lev7_polygons_sim <- do.call(rbind, simplified_chunks)

lev7_vars <- data_nutrients

# lev7_vars drop geometry
lev7_vars <- sf::st_drop_geometry(lev7_vars)

lev7_vars <- merge(shapes_lev7_polygons_sim,
                   lev7_vars,
                   by = "Hylak_id",
                   all.x = T)

lev7_vars <- lev7_vars[!sf::st_is_empty(lev7_vars), ]





#load("lev3_vars.rda")

missing_cols_lev7 <- setdiff(colnames(lev3_vars), colnames(lev7_vars))
lev7_vars[missing_cols_lev7] <-  0

missing_cols_lev3 <- setdiff(colnames(lev7_vars), colnames(lev3_vars))
lev3_vars[missing_cols_lev3] <- 0


class(lev7_vars)
usethis::use_data(lev7_vars, overwrite = TRUE)

#  remove columns 853, 854 and 855
lev7_vars <- lev7_vars %>%
  select(-c(853, 854, 855))

# population KPIs
#popn <- raster::raster("gpw_v4_population_density_rev11_2020_30_sec.tif")

# get a popn average for each polygon and add column values to lev7_vars
popn_lev7 <- sf::st_as_sf(lev7_vars)
popn_lev7 <- sf::st_transform(popn_lev7, crs = sf::st_crs(popn))
popn_lev7$popn <- raster::extract(popn, popn_lev7, fun = mean, na.rm = TRUE)

library(exactextractr)
popn_lev7$popn <- exact_extract(popn, popn_lev7, 'mean')
lev7_vars$popn <- popn_lev7$popn
# population KPIs
#depr <- raster::raster("povmap-grdi-v1-Sep2022.tif")

# get a popn average for each polygon and add column values to lev7_vars
depr_lev7 <- sf::st_as_sf(lev7_vars)
depr_lev7 <- sf::st_transform(depr_lev7, crs = sf::st_crs(depr))
depr_lev7$depr <- raster::extract(depr, depr_lev7, fun = mean, na.rm = TRUE)

library(exactextractr)
depr_lev7$depr <- exact_extract(depr, depr_lev7, 'mean')



# add popn to lev7_vars
lev7_vars$gbf7_mean_deprivation <- popn_lev7$depr


# add GBF data
#file_path <- "topocat_Hylak_id_lev03_HYBAS_ID_v1c_filt2_catchments_hybas_agg.gpkg"
catchments <- sf::st_read(file_path, layer = "topocat_Hylak_id_lev03_HYBAS_ID_v1c_filt2_catchments_hybas_agg")

# get data from cols 10:19 from catchments and join to lev7_vars based on Hylak_id

gbf_data <- catchments %>%
  st_drop_geometry() %>%
  select(Hylak_id, all_of(names(catchments)[10:19]))

# Join GBF data to lev7_vars by Hylak_id
lev7_vars <- lev7_vars %>%
  left_join(gbf_data, by = "Hylak_id")

# remove "_hybas_12_agg" from any column name
lev7_vars <- lev7_vars %>%
  rename_with(~ gsub("_hybas_12_agg$", "", .x))



# FROM HERE ----



# add in data_socio cols of interest
#load("data_socio.rda")
#data_socio <- shapes_3 %>% group_by(isoclr, nam_en) %>% summarise(geom = sf::st_union(geom)) %>% sf::st_make_valid()
data_socio <- sf::st_cast(data_socio, "MULTIPOLYGON")
lev7_vars <- sf::st_cast(lev7_vars, "MULTIPOLYGON")

# remove nam_en from lev7_vars
lev7_vars <- lev7_vars %>%
  select(-nam_en)


sf::st_geometry_type(lev7_vars)
sf::st_geometry_type(data_socio)


#data_usa_fix <-   sf::st_read("BNDA_simplified_wgs84_fixed_crop.gpkg")


usa_geom <- data_usa_fix[data_usa_fix$nam_en == "United States of America", ]
usa_geom <- sf::st_cast(usa_geom, "MULTIPOLYGON")
# 1. Merge all USA geometries into one MULTIPOLYGON
usa_union_geom <- sf::st_union(usa_geom)

# Check the class of usa_union_geom
class(usa_union_geom)
# Likely "sfc_MULTIPOLYGON"

# Extract single geometry (sfg) from sfc:
usa_union_geom_sfg <- usa_union_geom[[1]]

# Then use this inside map2()
new_geoms <- purrr::map2(
  data_socio$nam_en,
  sf::st_geometry(data_socio),
  function(name, geom) {
    if (name == "United States of America") {
      usa_union_geom_sfg
    } else {
      geom
    }
  }
)

data_socio <- sf::st_set_geometry(data_socio, sf::st_sfc(new_geoms, crs = sf::st_crs(data_socio)))
plot(sf::st_geometry(data_socio), col = "lightblue", border = "black")
plot(usa_union_geom, col = "red", add = TRUE, lwd = 2)


#  validity
invalid_lev7 <- !sf::st_is_valid(lev7_vars)
invalid_socio <- !sf::st_is_valid(data_socio)

if (any(invalid_lev7)) {
  lev7_vars <- sf::st_make_valid(lev7_vars)
}
if (any(invalid_socio)) {
  data_socio <- sf::st_make_valid(data_socio)
}







intersections <- sf::st_intersects(lev7_vars, data_socio)

summary_df <- map_dfr(intersections, function(idxs) {
  if (length(idxs) == 0) {
    tibble::tibble(
      nam_en = NA_character_,
      gbf7_RLI_2024 = NA_real_,
      gbf7_mean_deprivation = NA_real_
    )
  } else {
    matched <- data_socio[idxs, ]

    # Clean and collapse country names uniquely
    country_list <- unique(na.omit(matched$nam_en))
    country_string <- dplyr::case_when(
      length(country_list) == 0 ~ NA_character_,
      length(country_list) == 1 ~ country_list[1],
      length(country_list) == 2 ~ paste(country_list, collapse = " and "),
      length(country_list) > 2  ~ paste(
        paste(country_list[-length(country_list)], collapse = ", "),
        "and", country_list[length(country_list)]
      )
    )

    tibble::tibble(
      nam_en = country_string,
      gbf7_RLI_2024 = mean(matched$gbf7_RLI_2024, na.rm = TRUE),
      gbf7_mean_deprivation = mean(matched$gbf7_mean_deprivation, na.rm = TRUE)
    )
  }
})

lev7_vars_cleaned <- lev7_vars %>%
  dplyr::select(-matches("^nam_en$|^nam_en\\.\\.\\.[0-9]+$",
                  ignore.case = FALSE)) %>%
  dplyr::select(-matches("^gbf7_RLI_2024$|^gbf7_RLI_2024\\.\\.\\.[0-9]+$",
                  ignore.case = FALSE)) %>%
  dplyr::select(-matches("^gbf7_mean_deprivation$|^gbf7_mean_deprivation\\.\\.\\.[0-9]+$",
                  ignore.case = FALSE))



lev7_vars <- dplyr::bind_cols(lev7_vars_cleaned, summary_df)




# alt ----
intersections <- sf::st_intersects(lev7_vars, data_socio)
#  data frame mapping each Hylak_id to its intersecting data_socio rows
hylak_ids <- lev7_vars$Hylak_id

intersection_df <- map2_dfr(
  .x = intersections,
  .y = hylak_ids,
  .f = function(idxs, hylak_id) {
    if (length(idxs) == 0) return(NULL)
    data_socio[idxs, ] %>%
      sf::st_drop_geometry() %>%
      dplyr::mutate(Hylak_id = hylak_id)
  }
)

summary_df <- intersection_df %>%
  dplyr::group_by(Hylak_id) %>%
  dplyr::summarise(
    nam_en = dplyr::case_when(
      n_distinct(na.omit(nam_en)) == 0 ~ NA_character_,
      n_distinct(na.omit(nam_en)) == 1 ~ first(na.omit(nam_en)),
      n_distinct(na.omit(nam_en)) == 2 ~ paste(unique(na.omit(nam_en)), collapse = " and "),
      n_distinct(na.omit(nam_en)) > 2  ~ paste(
        paste(head(unique(na.omit(nam_en)), -1), collapse = ", "),
        "and", tail(unique(na.omit(nam_en)), 1)
      )
    ),
    gbf7_RLI_2024 = mean(gbf7_RLI_2024, na.rm = TRUE),
    gbf7_mean_deprivation = mean(gbf7_mean_deprivation, na.rm = TRUE),
    .groups = "drop"
  )



lev7_vars <- lev7_vars %>%
  left_join(summary_df, by = "Hylak_id")



class(lev7_vars)
usethis::use_data(lev7_vars, overwrite = TRUE)






lev7_vars <- lev7_vars %>%
  select(-starts_with("SSP"))


gnm <- gnm %>%
  rename_with(~ gsub("\\.x$", "", .x))

lev7_data <- lev7_vars %>%
  st_drop_geometry() %>%
  distinct(Hylak_id, .keep_all = TRUE)

shapes_7_ssp <- gnm %>%
  st_drop_geometry() %>%
  select(Hylak_id, starts_with("SSP")) %>%
  distinct(Hylak_id, .keep_all = TRUE)

lev7_joined <- lev7_data %>%
  left_join(shapes_7_ssp, by = "Hylak_id")

lev7_vars1 <- lev7_vars %>%
  select(Hylak_id, geom) %>%
  right_join(lev7_joined, by = "Hylak_id") %>%
  st_as_sf()

lev7_vars <- lev7_vars1


#########################################################################
############ Outlines (borders of the shapes) ###########################
#########################################################################
shapes_lev7_polygons_sim <- rmapshaper::ms_simplify(shapes_lev7_precise_polygons, keep = 0.3)
lev7_lines <- sf::st_cast(shapes_lev7_polygons_sim, to = "MULTILINESTRING")
lev7_lines  <- sf::st_cast(lev7_lines, to = "LINESTRING")
lev7_lines <- lev7_lines[!sf::st_is_empty(lev7_lines), ]

usethis::use_data(lev7_lines, overwrite = TRUE)

#########################################################################
############ Shapes precise (to check what was clicked) #################
#########################################################################
lev7_shapes_precise <- shapes_7

usethis::use_data(lev7_shapes_precise, overwrite = TRUE)



# rbind lev7 and lev3 vars
test_vars <- rbind(lev7_vars, lev3_vars)
