#' Base leaflet map for level 3
#' @name leaf_base_lev3
#' @author Maddalena Tigli
#' @description make the base map throughout the whole app
#' @param max_zoom numeric max zoom allowed
#' @return leaflet object
#' @export
#' @examples
#' \dontrun{
#' leaf_base_lev3 ()
#' }
#' base leaflet map for level 3
leaf_base_lev3 <- function(max_zoom = 15) {
  leaflet::leaflet(
    options = leaflet::leafletOptions(
      worldCopyJump = FALSE,
      maxBoundsViscosity = 1.0,
      maxZoom = max_zoom,
      preferCanvas = TRUE
    )
  ) %>%
    leaflet::addProviderTiles(
      "OpenStreetMap.Mapnik",
      options = leaflet::providerTileOptions(
        minZoom = 2,
        maxZoom = max_zoom,
        noWrap = TRUE,
        tileSize = 256
      )
    ) %>%
    leaflet::setMaxBounds(-180, -90, 180, 90) %>%
    htmlwidgets::onRender(
      "function(el,x){ this.zoomControl.setPosition('bottomright'); }"
    )
}

#' base leaflet map for level 7
leaf_base_lev7 <- function(max_zoom = 15) {
  leaflet::leaflet(
    options = leaflet::leafletOptions(
      worldCopyJump = FALSE,
      maxBoundsViscosity = 1.0,
      maxZoom = max_zoom,
      preferCanvas = TRUE
    )
  ) %>%
    leaflet::addProviderTiles(
      "OpenStreetMap.Mapnik",
      options = leaflet::providerTileOptions(
        minZoom = 2,
        maxZoom = max_zoom,
        noWrap = TRUE,
        tileSize = 256
      )
    ) %>%
    leaflet::setMaxBounds(-180, -90, 180, 90) %>%
    htmlwidgets::onRender(
      "function(el,x){ this.zoomControl.setPosition('bottomright'); }"
    )
}


#' name legend
#' @name legend_names
#' @author Maddalena Tigli
#' @description conversion from name displayed to "human" name
#' @param layer name of layer displayed
#' @return string with title for the legend
#' @export
#' @examples
#' \dontrun{
#' legend_names ("SSP1_LandCover_1970")
#' }
legend_names <- function(layer){
name <- gsub("^SSP\\d+_|_\\d{4}$", "", layer)
new_name <-  switch(name,
                    "Phosphorus_Rivers_Psurface_runoff_nat" = "Phosphorus load from surface runoff from natural land (kg P/km² yr-1)",
                    "Phosphorus_Rivers_Psurface_runoff_agri" = "Phosphorus load from surface runoff from agricultural land (kg P/km² yr-1)",
                    "Phosphorus_Rivers_Pweathering" = "Phosphorus load from weathering reaching surface water (kg P/km² yr-1)",
                    "Phosphorus_Rivers_Pvegetation" = "Phosphorus load from allochtonous organic matter input to rivers (kg P/km² yr-1)",
                    "Phosphorus_Rivers_Paquaculture" = "Phosphorus load from aquaculture to surface water (kg P/km² yr-1)",
                    "Phosphorus_Rivers_Psewage" = "Phosphorus load from waste water (human and industry) to surface water (kg P/km² yr-1)",
                    "Nitrogen_Rivers_Nsurface_runoff_nat" = "Nitrogen load from surface runoff from natural land (kg N/km² yr-1)",
                    "Nitrogen_Rivers_Nsurface_runoff_agri" = "Nitrogen load from surface runoff from agricultural land (kg N/km² yr-1)",
                    "Nitrogen_Rivers_Ngroundwater_nat" = "Nitrogen load from groundwater from natural land (kg N/km² yr-1)",
                    "Nitrogen_Rivers_Ngroundwater_agri" = "Nitrogen load from groundwater from agricultural land (kg N/km² yr-1)",
                    "Nitrogen_Rivers_Nvegetation" = "Nitrogen load from allochtonous organic matter input to rivers (kg N/km² yr-1)",
                    "Nitrogen_Rivers_Ndeposition_water" = "Direct nitrogen deposition on water (kg N/km² yr-1)",
                    "Nitrogen_Rivers_Naquaculture" = "Nitrogen load from aquaculture to surface water (kg N/km² yr-1)",
                    "Nitrogen_Rivers_Nsewage" = "Nitrogen load from aquaculture to surface water (kg N/km² yr-1)",
                 "legacy" = "legacy P",
                 "dis_m3_pyr" = "Nat. discharge (m3/y)",
                 "dis_m3_pmn" = "Min Nat. discharge (m3/y)",
                 "dis_m3_pmx" = "Max Nat. discharge (m3/y)",
                 "run_mm_syr" = "Land surface runoff (mm/y)",
                 "ari_ix_sav" = "Global aridity index (sub-basin)",
                 "ari_ix_uav" = "Global aridity index (watershed upstream)",
                 "glc_cl_smj" = "Dominant land cover type",
                 "wet_cl_smj" = "Dominant wetland type",
                 "pac_pc_sse" = "Protected area extent (sub-basin) %",
                 "pac_pc_use" = "Protected area extent (watershed upstream) %",
                 "gbf7_EN_MAR_CHLANM_Extreme" = "Chlorophyll-a anomaly %",
                 "gbf7_EN_MAR_CHLANM_Moderate" = "Chlorophyll-a anomaly %" ,
                 "gbf7_EN_MAR_CHLANM_High" = "Chlorophyll-a anomaly %",
                 "gbf7_P_import_weight_1000kg" = "Phosphorus Import Weights (1000kg)",
                 "gbf7_P_export_weight_1000kg" = "Phosphorus Export Weights (1000kg)",
                 "gbf7_weighted_P_efficiency" = "Phosphorus use efficiency",
                 "gbf7_weighted_N_efficiency" = "Nitrogen use efficiency",
                 )


 return(new_name)
}

