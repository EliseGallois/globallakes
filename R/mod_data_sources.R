# mod_data_sources.R
mod_data_sources_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Roboto:wght@400;700&display=swap"),
      tags$style(HTML("
        .content {
          font-family: 'Roboto', sans-serif;
          color: #333;
          line-height: 1.6;
          padding: 20px;
        }
        .content h3 {
          font-family: 'Roboto', sans-serif;
          color: #37a635;
          margin-top: 20px;
        }
        .content h1 {
          font-family: 'Roboto', sans-serif;
          color: #37a635;
          margin-top: 20px;
        }
         .content h4 {
          font-family: 'Roboto', sans-serif;
          color: #37a635;
          margin-top: 15px;
        }
        .content h5 {
          font-family: 'Roboto', sans-serif;
          color: #37a635;
          margin-top: 15px;
        }
        .content p {
          font-family: 'Roboto', sans-serif;
          text-align: justify;
          margin-bottom: 10px;
        }
        .content p.gray {
          font-family: 'Roboto', sans-serif;
          color: gray;
        }
        .img-container {
          width: 100%;
          height: 150px;
          object-fit: cover;
          display: block;
          padding: 10px;
          margin-top: 20px;
          border-radius: 10px;
        }
      "))
    ),
    div(class = "content",
        h1("Data Sources"),
        p("Click the drop-down menu options to brose the datasets used to construct this dashboard. Metadata includes spatial and temporal resolution, download links, references, and licenses.", style = "color:gray"),
        tags$img(
          src = "www/logos/AdobeStock_373669943.jpeg",
          style = "width: 100%; height: 160px; object-fit: cover; display: block; margin-top: 20px; border-radius: 10px;"
        ),

    bslib::accordion(
      id = ns("data_accordion"),
      open = FALSE,

      # uP4 Nitrogen and Phosphorus Delivery
      bslib::accordion_panel(
        title = HTML("<strong>Nitrogen and Phosphorus Emissions </strong>"),
        div(
          style = "padding: 10px; background-color: #f9f9f9; border-radius: 5px;",
          HTML("&#x2B06; <strong> <a href='https://dataportaal.pbl.nl/downloads/IMAGE/GNM/' target='_blank'>Data Portal</a> </strong>"),
          br(),
          p("Description: N and P delivery to surface water and in-stream transport and retention processes. Data globally aggregated via the Global Nutrient Model. Variables available to download at HYDROBASIN Level 3 and catchment-level resolution."),
          p(
            "This modelled data incorporates shared socio-economic pathway projections. Read more about SSPs ",
            tags$a(
              "here",
              href = "https://ore.exeter.ac.uk/articles/online_resource/The_Scenario_Model_Intercomparison_Project_ScenarioMIP_for_CMIP6/29724578?file=56741828",
              target = "_blank"   # <-- opens in a new tab
            ),
            "."
          ),
          h4("Scope"), p("Global"),
          h4("Category"), p("Nutrients"),
          h4("Timepoints"), p("1970 - 2070 (every 5 years)"),
          h4("License"), HTML("<a href='https://creativecommons.org/licenses/by/4.0/' target='_blank'>CC-BY-4.0 DEED</a>"),
          br(),
          h4("Variables in Dashboard"),
          tags$ul(
            style = "text-align: left; padding-left: 20px;",
            tags$li("SSPx_Phosphorus_Rivers_Psurface_runoff_nat_date"),
            tags$li("SSPx_Phosphorus_Rivers_Psurface_runoff_agri_date"),
            tags$li("SSPx_Phosphorus_Rivers_Pweathering_date"),
            tags$li("SSPx_Phosphorus_Rivers_Pvegetation_date"),
            tags$li("SSPx_Phosphorus_Rivers_Paquaculture_date" ),
            tags$li("SSPx_Phosphorus_Rivers_Psewage_date"),
            tags$li("SSPx_Nitrogen_Rivers_Nsurface_runoff_nat_date"),
            tags$li("SSPx_Nitrogen_Rivers_Nsurface_runoff_agri_date"),
            tags$li("SSPx_Nitrogen_Rivers_Ngroundwater_nat_ date "),
            tags$li("SSPx_Nitrogen_Rivers_Ngroundwater_agri_date"),
            tags$li("SSPx_Nitrogen_Rivers_Nvegetation_date"),
            tags$li("SSPx_Nitrogen_Rivers_Naquaculture_date"),
            tags$li("SSPx_Nitrogen_Rivers_Nsewage_date")

        )
        )
      ),

      # uP28 BasinATLAS
      bslib::accordion_panel(
        title = HTML("<strong>BasinATLAS (HydroATLAS)</strong>"),
        div(
          style = "padding: 10px; background-color: #f9f9f9; border-radius: 5px;",
          HTML("&#x2B06; <strong> <a href='https://www.hydrosheds.org/hydroatlas' target='_blank'>Data Portal</a> </strong>"),
          br(),
          p("Description: Sub-basin characteristics for hierarchically nested watersheds."),
          h4("Scope"), p("Global"),
          h4("Category"), p("Catchments"),
          h4("Timepoints"), p("Varies"),
          h4("License"), HTML("<a href='http://creativecommons.org/licenses/by/4.0/' target='_blank'>CC-BY-4.0</a>"),
          br(),
          h4("Variables in Dashboard"),
          tags$ul(
            style = "text-align: left; padding-left: 20px;",
            tags$li("HYBAS_ID"),
            tags$li("SUB_AREA"),
            tags$li("UP_AREA"),
            tags$li("SORT"),
            tags$li("dis_m3_pyr"),
            tags$li("dis_m3_pmn"),
            tags$li("dis_m3_pmx"),
            tags$li("run_mm_syr"),
            tags$li("ari_ix_sav"),
            tags$li("glc_cl_smj"),
            tags$li("wet_cl_smj"),
            tags$li("pac_pc_use"),
            tags$li("pac_pc_sse")



          )
        )
      ),

      # uP92 Cropland Nutrient Balance
      bslib::accordion_panel(
        title = HTML("<strong>Cropland Nutrient Efficiency</strong>"),
        div(
          style = "padding: 10px; background-color: #f9f9f9; border-radius: 5px;",
          HTML("&#x2B06; <strong> <a href='https://www.fao.org/faostat/en/#data/ESB' target='_blank'>Data Portal</a> </strong>"),
          br(),
          p("Description: Flows of nitrogen, phosphorus, and potassium on cropland."),
          h4("Scope"), p("Global"),
          h4("Category"), p("Nutrients"),
          h4("Timepoints"), p("1961 - 2020 (annual)"),
          h4("License"), HTML("<a href='https://creativecommons.org/licenses/by-nc-sa/3.0/igo/' target='_blank'>CC BY-NC-SA 3.0 IGO</a>"),
          br(),
          h4("Variables in Dashboard"),
          tags$ul(
            style = "text-align: left; padding-left: 20px;",
            tags$li("P_efficiency_year"),
            tags$li("N_efficiency_year"),



          )
        )
      ),


      # uP95 Index of Coastal Eutrophication Potential
      bslib::accordion_panel(
        title = HTML("<strong>Index of Coastal Eutrophication Potential</strong>"),
        div(
          style = "padding: 10px; background-color: #f9f9f9; border-radius: 5px;",
          HTML("&#x2B06; <strong> <a href='https://www.un.org/en/about-us/copyright/' target='_blank'>Data Portal</a> </strong>"),
          br(),
          p("Description: Measures contribution to coastal eutrophication."),
          h4("Scope"), p("Global"),
          h4("Category"), p("Environment"),
          h4("Timepoints"), p("2000 - 2023 (annual)"),
          h4("License"), HTML("<a href='https://www.un.org/en/about-us/terms-of-use' target='_blank'>UN Terms of Use</a>"),
          br(),
          h4("Variables in Dashboard"),
          tags$ul(
            style = "text-align: left; padding-left: 20px;",
            tags$li("EN_MAR_CHLANM_Extreme_year"),
            tags$li("EN_MAR_CHLANM_High_year"  ),
            tags$li("EN_MAR_CHLANM_Moderate_year")


          )
        )
      ),

      # uP85 Resource Trade Phosphorus
      bslib::accordion_panel(
        title = HTML("<strong>Resource Trade Phosphorus</strong>"),
        div(
          style = "padding: 10px; background-color: #f9f9f9; border-radius: 5px;",
          HTML("&#x2B06; <strong> <a href='https://resourcetrade.earth/?category=122&units=value&autozoom=1' target='_blank'>Data Portal</a> </strong>"),
          br(),
          p("Description: Trade data from the Chatham House Resource Trade Database."),
          h4("Scope"), p("Global"),
          h4("Category"), p("Nutrients"),
          h4("Timepoints"), p("2000 - 2022 (annual)"),
          h4("License"), p("Chatham House (2021)"),
          HTML("<a href='https://resourcetrade.earth/' target='_blank'>Read the dataset</a>"),
          br(),
          h4("Variables in Dashboard"),
          tags$ul(
            style = "text-align: left; padding-left: 20px;",
            tags$li("P_import_year"),
            tags$li("P_export_year"),
            tags$li("N_import_value"),
            tags$li("N_export_value")
          )
        )
        ),


      # uP38 GloboLakes
      bslib::accordion_panel(
        title = HTML("<strong>GloboLakes</strong>"),
        div(
          style = "padding: 10px; background-color: #f9f9f9; border-radius: 5px;",
          HTML("&#x2B06; <strong> <a href='https://catalogue.ceda.ac.uk/uuid/84d4f66b668241328df0c43f8f3b3e16' target='_blank'>Data Portal</a> </strong>"),
          br(),
          p("Description: High-resolution limnology dataset for inland waters."),
          h4("Scope"), p("Global"),
          h4("Category"), p("Catchments"),
          h4("Timepoints"), p("2005 - 2010 (annual)"),
          h4("License"), HTML("<a href='https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/' target='_blank'>Open Government License</a>"),
          h4("Reference"), HTML("<a href='https://dx.doi.org/10.5285/6be871bc-9572-4345-bb9a-2c42d9d85ceb' target='_blank'>Read the dataset</a>"),
          h4("Variables in Dashboard"),
          tags$ul(
            style = "text-align: left; padding-left: 20px;",
            tags$li("Lake_name"  ),
            tags$li("Hylak_id")


          )
        )
      ),

      # uP73 Lake-TopoCat
      bslib::accordion_panel(
        title = HTML("<strong>Lake-TopoCat</strong>"),
        div(
          style = "padding: 10px; background-color: #f9f9f9; border-radius: 5px;",
          HTML("&#x2B06; <strong> <a href='https://zenodo.org/records/7916729' target='_blank'>Data Portal</a> </strong>"),
          br(),
          p("Description: Global database of lake drainage topology and catchments."),
          h4("Scope"), p("Global"),
          h4("Category"), p("Catchments"),
          h4("Timepoints"), p("Varies"),
          h4("License"), HTML("<a href='https://creativecommons.org/licenses/by/4.0/' target='_blank'>CC BY 4.0</a>"),
          h4("Reference"), HTML("<a href='https://doi.org/10.5194/essd-15-3483-2023' target='_blank'>Read the dataset</a>"),

          h4("Variables in Dashboard"),
          tags$ul(
            style = "text-align: left; padding-left: 20px;",
            tags$li("Hylak_id"),
            tags$li("Lake_area"  ),
            tags$li("Lake_type")


          )
        )
      ),

      # uP104 UN Countries
      bslib::accordion_panel(
        title = HTML("<strong>UN Countries Boundaries </strong>"),
        div(
          style = "padding: 10px; background-color: #f9f9f9; border-radius: 5px;",
          HTML("&#x2B06; <strong> <a href='https://geoportal.un.org/arcgis/home/item.html?id=fa74ef8499094e41bf0d025006e37fc9#overview' target='_blank'>Data Portal</a> </strong>"),
          br(),
          p("Description: The United Nations Geospatial Data, or Geodata, is a worldwide geospatial dataset of the United Nations.The United Nations Geodata is provided to facilitate the preparation of cartographic materials in the United Nations includes geometry, attributes and labels to facilitate the adequate depiction and naming of geographic features for the preparation of maps in accordance with United Nations policies and practices. The geospatial datasets here included are referred to as UN Geodata simplified and are generalized based on UNGeodata 25 million scale. The feature layers include polygons/areas of countries (BNDA_simplified), lines for international boundaries and limits (BNDL_simplified), and major water body (WBYA_simplified). In addition, aggregated regional areas are available following M49 methodology (GEOA_simplified, SUBA_simplified, INTA_simplified) and SDG regional grouping (SDGA_simplified)."),
          h4("Scope"), p("Global"),
          h4("Category"), p("Demographic"),
          h4("Timepoints"), p("NA"),
          h4("License"), p("The boundaries and names shown and the designations used on this map do not imply official endorsement or acceptance by the United Nations. (short form)The designations employed and the presentation of material on this map do not imply the expression of any opinion whatsoever on the part of the Secretariat of the United Nations concerning the legal status of any country, territory, city or area or of its authorities, or concerning the delimitation of its frontiers or boundaries. (long form)Final boundary between the Republic of Sudan and the Republic of South Sudan has not yet been determined.Dotted line represents approximately the Line of Control in Jammu and Kashmir agreed upon by India and Pakistan. The final status of Jammu and Kashmir has not yet been agreed upon by the parties.A dispute exists between the Governments of Argentina and the United Kingdom of Great Britain and Northern Ireland concerning sovereignty over the Falkland Islands (Malvinas). CC BY 4.0"),
          h4("Variables in Dashboard"),
          tags$ul(
            style = "text-align: left; padding-left: 20px;",
            tags$li("nam_en"),
            tags$li("sub_reg" )


          )
        )
      ),
      # uP1 Population density
      bslib::accordion_panel(
        title = HTML("<strong>Population Density </strong>"),
        div(
          style = "padding: 10px; background-color: #f9f9f9; border-radius: 5px;",
          HTML("&#x2B06; <strong> <a href='https://sedac.ciesin.columbia.edu/data/set/gpw-v4-population-density-rev11/data-download' target='_blank'>Data Portal</a> </strong>"),
          br(),
          p("Description: Gridded Population of the World, Version 4 (GPWv4)"),
          h4("Scope"), p("Global"),
          h4("Category"), p("Demographic"),
          h4("Timepoints"), p("2000, 2005, 2010, 2015, and 2020."),
          h4("License"), p("When authors make use of data they should cite both the data set and the scientific publication, if available. Such a practice gives credit to data set producers and advances principles of transparency and reproducibility. Please visit the data citations page (https://sedac.ciesin.columbia.edu/citations) for details. Users who would like to choose to format the citation(s) for this dataset using a myriad of alternate styles can copy the DOI number and paste it into Crosscite's website."),
          h4("Reference"), p("Center for International Earth Science Information Network - CIESIN - Columbia University. 2018. Gridded Population of the World, Version 4 (GPWv4): Population Density, Revision 11. Palisades, New York: NASA Socioeconomic Data and Applications Center (SEDAC). https://doi.org/10.7927/H49C6VHW. Accessed 15th July 2025."),
          h4("Variables in Dashboard"),
          tags$ul(
            style = "text-align: left; padding-left: 20px;",
            tags$li("pop_dens")


          )
        )
      ),

      bslib::accordion_panel(
        title = HTML("<strong>IUCN Red List Index </strong>"),
        div(
          style = "padding: 10px; background-color: #f9f9f9; border-radius: 5px;",
          HTML("&#x2B06; <strong> <a href='https://www.iucnredlist.org/assessment/red-list-index' target='_blank'>Data Portal</a> </strong>"),
          br(),
          p("IUCN Red List Index"),
          h4("Scope"), p("Global"),
          h4("Category"), p("Biodiversity"),
          h4("Timepoints"), p("2024"),
          h4("License"), p("The data on The IUCN Red List are freely available for non-commercial use."),
          h4("Variables in Dashboard"),
          tags$ul(
            style = "text-align: left; padding-left: 20px;",
            tags$li("gbf7_RLI_2024")


          )
        )
      ),

      bslib::accordion_panel(
        title = HTML("<strong>Global Mean Deprivation Index </strong>"),
        div(
          style = "padding: 10px; background-color: #f9f9f9; border-radius: 5px;",
          HTML("&#x2B06; <strong> <a href='https://sedac.ciesin.columbia.edu/data/set/povmap-grdi-v1/data-download' target='_blank'>Data Portal</a> </strong>"),
          br(),
          p("Global gridded relative levels of multidimensional deprivation and poverty per pixel. Value of 100 represents the highest level of deprivation and a value of 0 the lowest. Model / methods: GRDIv1 is built from sociodemographic and satellite data inputs that were spatially harmonized, indexed, and weighted into six main components to produce the final index raster. Inputs were selected from the best-available data that either continuously vary across space or have at least administrative level 1 (provincial/state) resolution, and which have global spatial coverage."),
          h4("Scope"), p("Global"),
          h4("Category"), p("Demographic"),
          h4("Timepoints"), p("2024"),
          h4("Reference"), p("Center for International Earth Science Information Network - CIESIN - Columbia University. 2022. Global Gridded Relative Deprivation Index (GRDI), Version 1. Palisades, New York: NASA Socioeconomic Data and Applications Center (SEDAC). https://doi.org/10.7927/3xxe-ap97. Accessed 15th July 2025"),
          h4("License"), p("When authors make use of data they should cite both the data set and the scientific publication, if available. Such a practice gives credit to data set producers and advances principles of transparency and reproducibility. Please visit the data citations page (https://sedac.ciesin.columbia.edu/citations) for details. Users who would like to choose to format the citation(s) for this dataset using a myriad of alternate styles can copy the DOI number and paste it into Crosscite's website."),
          h4("Variables in Dashboard"),
          tags$ul(
            style = "text-align: left; padding-left: 20px;",
            tags$li("gbf7_mean_deprivation_2024")


          )
        )
      ),
    )
    )
  )
}


mod_data_sources_server <- function(id, rv, x) {
  moduleServer(id, function(input, output, session) {
    # no server logic needed for this tab
  })
}
