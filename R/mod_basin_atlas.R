#' basin_atlas UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_basin_atlas_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),

    div(
      class = "full-stretch-container",
      tags$head(
        tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Roboto:wght@400;700&display=swap"),
        tags$style(HTML("
          /* GENERAL STYLES */
.content {
  font-family: 'Roboto', sans-serif;
  color: #333;
  line-height: 1.6;
  padding: 10px;
}
.content h2 {
  color: #37a635;
  margin-top: 20px;
}
.content h5 {
  color: #37a635;
  margin-top: 15px;
}
.content p {
  text-align: justify;
  margin-bottom: 10px;
}
.content p.gray {
  color: gray;
}

.img-container {
  width: 100%;
  height: 100px;
  object-fit: cover;
  display: block;
  padding: 10px;
  margin-top: 20px;
  border-radius: 10px;
}

.leaflet-container {
  cursor: default !important;
  margin-bottom: 0px;
}

/* LEGEND */
.leaflet-control-container .info.legend h5 {
  white-space: normal !important;
  word-wrap: break-word;
  word-break: break-all;
  margin: 0;
  padding: 0;
}
.leaflet-control-container .info.legend {
  max-width: 200px;
  white-space: normal;
  word-wrap: break-word;
  padding: 6px 8px;
  line-height: 1.4;
  background-color: rgba(255, 255, 255, 0.98) !important;
}

.multi-col-legend {
  max-width: 350px !important;
  max-height: 50vh !important;
  overflow-y: auto !important;
  height: auto !important;
  display: flex !important;
  flex-direction: column !important;
  padding: 4px 6px !important;
  background-color: rgba(255, 255, 255, 0.98) !important;
}
.multi-col-legend h5 {
  text-align: center;
  margin-bottom: 3px;
}
.multi-col-legend div:not(.leaflet-control-container) {
  display: grid !important;
  grid-template-columns: repeat(2, minmax(100px, 1fr)) !important;
  gap: 2px 5px !important;
  line-height: 1.2;
}
.multi-col-legend i,
.multi-col-legend span {
  display: inline-flex !important;
  align-items: center;
  white-space: nowrap !important;
  overflow: hidden;
  text-overflow: ellipsis;
}
.multi-col-legend i {
  width: 13px !important;
  height: 13px !important;
  margin-right: 2px !important;
  flex-shrink: 0;
}

/* INFO PANEL */
#info_box {
  max-width: 300px;
  width: auto;
  transform: none !important;
}
#info_box h5 {
  white-space: nowrap;
}
#info_box .form-group.shiny-input-container {
  margin-bottom: 0;
}

/* DROPDOWN INFO */
.dropdown-info {
  display: flex;
  align-items: center;
  margin-bottom: 5px;
}
.dropdown-info label {
  margin-right: 10px;
}

/* MODAL FIXES */
.modal-content {
  padding: 0 !important;
  height: calc(100vh - 100px);
}
.modal-body {
  height: 100%;
  overflow-y: auto;
  padding: 0 !important;
}
.modal-footer {
  display: none;
}
.modal-dialog {
  max-width: fit-content !important;
}

/* GLOBAL BODY */
body {
  margin: 0;
  padding: 0;
  overflow-x: hidden;
}

/* FULL STRETCH LAYOUT */
:root {
  --navbar-h: 55px;
  --footer-h: 75px;
}

.full-stretch-container {
    width: 100vw;
    height: calc(100vh - var(--navbar-h) - var(--footer-h));
    min-height: calc(100vh - var(--navbar-h) - var(--footer-h));
    margin-top: var(--navbar-h);
   margin-bottom: var(--footer-h) !important;
    padding: 0;
}


.container-fluid {
  padding: 0;
}

/* SIDEBAR LAYOUT */
.bslib-sidebar-layout {
  height: calc(100vh - 50px - 75px) !important;
  padding: 0 !important;
  margin: 0 !important;
}
.bslib-sidebar-layout > .sidebar .form-group.shiny-input-container {
  margin-bottom: 5px;
}
.bslib-sidebar-layout > .sidebar h4 {
  margin-top: 10px;
  margin-bottom: 5px;
}
.action-button.btn-warning {
  margin-top: 5px;
  margin-bottom: 5px;
}
.sidebar > img:first-child {
  margin-top: 0 !important;
  padding-top: 0 !important;
}

/* WELCOME BOX */
.welcome-box {
  position: absolute;
  z-index: 9999;
  background-color: rgba(255, 255, 255, 0.95);
  padding: 15px;
  border-radius: 8px;
  box-shadow: 2px 2px 10px rgba(0,0,0,0.3);
  max-width: 300px;
  width: auto;
  overflow: hidden !important;
}
.welcome-box .close-btn {
  position: absolute;
  top: 5px;
  right: 10px;
  background: none;
  border: none;
  font-size: 18px;
  font-weight: bold;
  color: #888;
  cursor: pointer;
}
.welcome-box .close-btn:hover {
  color: #000;
}

        "))
      ),

      bslib::layout_sidebar(
        class = "p-0",
        sidebar_collapsible = FALSE,
        sidebar = bslib::sidebar(
          open = c("open"),
          width = "28%",
          bg = "white",

          div(style = "padding: 1px;",
              div(style = "text-align: center; margin: 2px 0; font-weight: bold; font-size: 14px; color: #888;", "Select a variable to view globally:"),
              h4(bsicons::bs_icon("moisture"), " Cross-sector Nutrient Budgets", style = "color:black; font-size: 12px;"),
              div(class = "dropdown-info",
                  selectInput(ns("nutrient_dataset"), label = NULL,
                              choices = c("Select..." = "",
                                          "Phosphorus load from surface runoff from natural land" = "Phosphorus_Rivers_Psurface_runoff_nat",
                                          "Phosphorus load from surface runoff from agricultural land" = "Phosphorus_Rivers_Psurface_runoff_agri",
                                          "Phosphorus load from weathering reaching surface water" = "Phosphorus_Rivers_Pweathering",
                                          "Phosphorus load from allochtonous organic matter input to rivers" = "Phosphorus_Rivers_Pvegetation",
                                          "Phosphorus load from aquaculture to surface water" = "Phosphorus_Rivers_Paquaculture",
                                          "Phosphorus load from waste water and sewage (human and industry) to surface water" = "Phosphorus_Rivers_Psewage",
                                          "Nitrogen load from surface runoff from natural land" = "Nitrogen_Rivers_Nsurface_runoff_nat",
                                          "Nitrogen load from surface runoff from agricultural land)" = "Nitrogen_Rivers_Nsurface_runoff_agri",
                                          "Nitrogen load from groundwater from natural land" = "Nitrogen_Rivers_Ngroundwater_nat",
                                          "Nitrogen load from groundwater from agricultural land" = "Nitrogen_Rivers_Ngroundwater_agri",
                                          "Nitrogen load from allochtonous organic matter input to rivers" = "Nitrogen_Rivers_Nvegetation",
                                          "Direct nitrogen deposition on water from the atmosphere" = "Nitrogen_Rivers__Ndeposition_water",
                                          "Nitrogen load from waste water and sewage (human and industry) to surface water" = "Nitrogen_Rivers_Nsewage"),
                              selected = "", width = "85%"),
                  div(class = "info-icon", bslib::tooltip(bsicons::bs_icon("info-circle"), p("Phosphorus and nitrogen budgets across sectors")))
              ),

              div(style = "text-align: center; margin: 5px 0; font-weight: bold; font-size: 14px; color: #888;", "or"),


              h4(bsicons::bs_icon("flower1"), " Ecosystem Data", style = "color:black; font-size: 12px;"),
              div(class = "dropdown-info",
                  selectInput(ns("gbf_2"), label = NULL,
                              choices = c("Select..." = "",
                                          "Annual natural river discharge per year" = "dis_m3_pyr",
                                          "Annual minimum natural river discharge per year" = "dis_m3_pmn",
                                          "Annual maximum natural river discharge per year" = "dis_m3_pmx",
                                          "Annual land surface runoff per year" = "run_mm_syr",
                                          "Global aridity index in sub-basin" = "ari_ix_sav",
                                          "Global aridity index in tot. watershed upstream" = "ari_ix_uav",
                                          "Land cover classes (spatial majority)" ="glc_cl_smj",
                                          "Wetland classes (spatial majority)" = "wet_cl_smj",
                                          "Protected area extent in sub-basin" = "pac_pc_sse",
                                          "Protected area extent in tot. watershed upstream" = "pac_pc_use"),
                              selected = "", width = "85%"),
                  div(class = "info-icon", bslib::tooltip(bsicons::bs_icon("info-circle"), p("Discharge, aridity and landscape variables")))
              ),

              #div(style = "text-align: center; margin-top: 5px; margin-bottom: 5px;",
              #   actionButton(ns("reset_button"), "Reset Selections",
              #              class = "btn btn-warning", style = "width: 70%; font-weight: bold;"))
          ),

          tags$img(src = "www/logos/diagram.png", alt = "Diagram", style = "max-width: 100%; display: block;")
        ),



        leaflet::leafletOutput(ns("map"), height = "100%", width = "100%")
      ),

      conditionalPanel(
        condition = "input.nutrient_dataset !== 'legacy' && input.nutrient_dataset !== ''",
        ns = ns,
        shinyjqui::jqui_draggable(
          absolutePanel(
            id = "slideryear",
            style = "z-index: 1000; background-color: rgba(255, 255, 255, 0.98); padding: 15px; border-radius: 8px; box-shadow: 2px 2px 5px rgba(0,0,0,0.3);",
            draggable = TRUE,
            top = "auto", left = "30%", right = "auto", bottom = "10%",
            width = 380, height = "auto",

            shinyWidgets::sliderTextInput(
              ns("year"), label = NULL,
              choices = seq(1970, 2070, 5),
              selected = 2025,
              width = "100%"
            ),

            div(style = "font-size: 12px; color: gray; margin-top: 10px; margin-bottom: 8px;",
                "SSP projections available 2015-2070"),

            radioButtons(ns("climate_scenario"),
                         label = "Select a shared socio-economic scenario",
                         choices = c(
                           "Sustainability (SSP1)" = "SSP1",
                           "Middle of the road (SSP2)" = "SSP2",
                           "Regional rivalry (SSP3)" = "SSP3",
                           "Global inequality (SSP4)" = "SSP4",
                           "Fossil-fueled development (SSP5)" = "SSP5"
                         ),
                         selected = "SSP1",
                         inline = FALSE,
                         width = "100%"
            )
          )
        )
      ),

      # Info panel
      shinyjqui::jqui_draggable(
        absolutePanel(
          id = ns("info_box"),
          class = "info-panel",
          top = "100px",
          left = "30%",
          right = "auto",
          bottom = "auto",
          draggable = FALSE,
          style = "z-index: 800; background-color: rgba(255, 255, 255, 0.98); padding: 10px; border-radius: 8px; box-shadow: 2px 2px 5px rgba(0,0,0,0.3);",

          h5("Search for a specific location", style = "margin-top: 0; margin-bottom: 5px; font-weight: bold; font-size: 14px; color: #333;"),
          # search bar
          div(style = "text-align: center; margin-bottom: 5px; font-weight: bold; font-size: 14px; color: #333;"),
          div(
            style = "display: flex; align-items: center; gap: 5px; margin-bottom: 10px;",
            textInput(ns("search_query"), label = NULL, placeholder = "Type and click...", width = "170px"),
            actionButton(ns("search_btn"), "🔍",
                         style = "padding: 2px 8px; min-width: 0; font-size: 16px; background-color: #eaeaea; border: 1px solid #ccc; color: #333;")
          ),


          uiOutput(ns("variable_info"))
        )
      ),

      # dismissable logo box
      shinyjqui::jqui_draggable(
        uiOutput(ns("logo_box_ui"))
      )
    )
  ) }


#' basin_atlas Server Functions
#'
#' @noRd
mod_basin_atlas_server <- function(id, rv, x, lev3_vars, lev3_lines, lev3_shapes_precise, globo_topo_poly, globo_topo_lines, coast_sf) {
  moduleServer(id, session = x, function(input, output, session) {
    ns <- session$ns
    rv_local <- reactiveValues(
      showWelcome = TRUE,
      hovered_id = NULL,
      current_view = list(lat = -10.0, lng = -55.0, zoom = 4),
      current_variable = NULL,
      category_display = NULL,
      done = 0
    )

    coastal_lake_ids <- unique(coast_sf$Hylak_id)


    session$onFlush(once = TRUE, function() {

      ssp_url <- "https://ore.exeter.ac.uk/articles/online_resource/The_Scenario_Model_Intercomparison_Project_ScenarioMIP_for_CMIP6/29724578?file=56741828"

      ssp_link_html <- paste0(
        'Select a <a href="', ssp_url,
        '" target="_blank">shared socio-economic scenario</a>'
      )

      shinyjs::runjs(
        sprintf(
          '
      setTimeout(function(){
        // Targets the module element ID, then finds the label
        var radioId = "#%s-climate_scenario";
        var labelElement = $(radioId).find("label[for]");

        if (labelElement.length) {
            labelElement.html(\'%s\');
        }
      }, 0);
      ',
          id,
          ssp_link_html
        )
      )
    })

    #  function to show a warning modal
    show_warning_modal <- function(message) {
      showModal(modalDialog(
        div(
          style = "width: auto; max-width: 400px; padding: 10px; text-align: center;",
          h4("Warning", style = "margin-top: 0;"),
          p(message, style = "margin-bottom: 0;")
        ),
        easyClose = TRUE,
        footer = NULL
      ))
      shinyjs::runjs("
        $('.modal-dialog').css({ 'max-width': '400px', 'width': 'auto' });
        $('.modal-content').css({ 'max-height': '150px', 'height': 'auto', 'display': 'block' });
        $('.modal').modal('handleUpdate');
      ")
    }

    #  function to check for valid polygons
    is_valid_polygon <- function(data) {
      return(inherits(sf::st_geometry(data), c("sfc_POLYGON", "sfc_MULTIPOLYGON")))
    }

    #  variable lookups and ranges
    units_lookup <- c(
      Phosphorus_Rivers_Psurface_runoff_nat = "kg P/km²/yr",
      Phosphorus_Rivers_Psurface_runoff_agri = "kg P/km²/yr",
      Phosphorus_Rivers_Pweathering = "kg P/km²/yr",
      Phosphorus_Rivers_Pvegetation = "kg P/km²/yr",
      Phosphorus_Rivers_Paquaculture = "kg P/km²/yr",
      Phosphorus_Rivers_Psewage = "kg P/km²/yr",
      Nitrogen_Rivers_Nsurface_runoff_nat = "kg N/km²/yr",
      Nitrogen_Rivers_Nsurface_runoff_agri = "kg N/km²/yr",
      Nitrogen_Rivers_Ngroundwater_nat = "kg N/km²/yr",
      Nitrogen_Rivers_Ngroundwater_agri = "kg N/km²/yr",
      Nitrogen_Rivers_Nvegetation = "kg N/km²/yr",
      Nitrogen_Rivers_Naquaculture = "kg N/km²/yr",
      Nitrogen_Rivers_Ndeposition_water = "kg N/km²/yr",
      Nitrogen_Rivers_Nsewage = "kg N/km²/yr",
      dis_m3_pyr = "m³/yr",
      dis_m3_pmn = "m³/yr",
      dis_m3_pmx = "m³/yr",
      run_mm_syr = "mm/yr",
      ari_ix_sav = "index",
      ari_ix_uav = "index",
      glc_cl_smj = "class",
      wet_cl_smj = "class",
      pac_pc_sse = "%",
      pac_pc_use = "%"
    )
    nutrient_vars <- c("Phosphorus_Rivers_Psurface_runoff_nat", "Phosphorus_Rivers_Psurface_runoff_agri", "Phosphorus_Rivers_Pweathering", "Phosphorus_Rivers_Pvegetation", "Phosphorus_Rivers_Paquaculture", "Phosphorus_Rivers_Psewage", "Nitrogen_Rivers_Nsurface_runoff_nat", "Nitrogen_Rivers_Nsurface_runoff_agri", "Nitrogen_Rivers_Ngroundwater_nat", "Nitrogen_Rivers_Ngroundwater_agri", "Nitrogen_Rivers_Nvegetation", "Nitrogen_Rivers_Naquaculture", "Nitrogen_Rivers_Ndeposition_water", "Nitrogen_Rivers_Nsewage")
    years <- seq(1970, 2070, 5)
    ssps <- c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5")
    global_variable_ranges <- list()
    for (var in nutrient_vars) {
      columns_to_check <- paste(rep(ssps, each = length(years)), var, rep(years, times = length(ssps)), sep = "_")
      existing_columns <- columns_to_check[columns_to_check %in% colnames(lev3_vars)]
      all_values <- unlist(lev3_vars[, existing_columns, drop = FALSE])
      global_variable_ranges[[var]] <- list(min = min(all_values, na.rm = TRUE), max = max(all_values, na.rm = TRUE))
    }
    static_vars <- c("dis_m3_pyr", "dis_m3_pmn", "dis_m3_pmx", "run_mm_syr", "ari_ix_sav", "ari_ix_uav", "pac_pc_sse", "pac_pc_use")
    for (var in static_vars) {
      values <- lev3_vars[[var]]
      global_variable_ranges[[var]] <- list(min = min(values, na.rm = TRUE), max = max(values, na.rm = TRUE))
    }

    # reactive expression for data conversion
    lev3_vars_converted <- reactive({
      data_copy <- lev3_vars


      # conversion factor: 1 kg / km^2-yr * 2500 km^2/basin / 1,000,000 kg/tonne

      # calculate the conversion
      ssp_cols <- grep("^SSP", names(data_copy), value = TRUE)
      data_copy[ssp_cols] <- data_copy[ssp_cols] * (2500 / 1000000)



      rv_local$units_lookup_converted <- units_lookup

      new_ranges <- list()
      for (var in nutrient_vars) {
        columns_to_check <- paste(rep(ssps, each = length(years)), var, rep(years, times = length(ssps)), sep = "_")
        existing_columns <- columns_to_check[columns_to_check %in% colnames(data_copy)]
        all_values <- unlist(data_copy[, existing_columns, drop = FALSE])
        new_ranges[[var]] <- list(min = min(all_values, na.rm = TRUE), max = max(all_values, na.rm = TRUE))
      }
      for (var in static_vars) {
        values <- lev3_vars[[var]]
        new_ranges[[var]] <- list(min = min(values, na.rm = TRUE), max = max(values, na.rm = TRUE))
      }

      rv_local$global_variable_ranges_converted <- new_ranges

      return(data_copy)
    })

    observe({
      # disable if year < 2015
      if (input$year < 2015) {
        shinyjs::disable("climate_scenario")
      } else {
        shinyjs::enable("climate_scenario")
      }
    })


    # reactive expression to get the column name to be displayed
    layer_to_display <- eventReactive(rv_local$done, {
      if(rv_local$done != 0){
        if (rv_local$category_display == "nutrient_dataset" && input$nutrient_dataset != "legacy") {
          column <- paste(input$climate_scenario, input$nutrient_dataset, input$year, sep = "_")
        } else if (rv_local$category_display == "gbf_2") {
          column <- input$gbf_2
          if (!(column %in% colnames(lev3_vars_converted()))) {
            warning(paste("Warning: Column", column, "not found in dataset"))
            column <- NULL
          }
        } else {
          column <- NULL
        }
      } else {
        column <- NULL
      }
      rv$layer_selected_lev3 <- column
      return(column)
    })

    #  initial leaflet map rendering
    output$map <- leaflet::renderLeaflet({
      isolate(
        BasinATLASgol::leaf_base_lev3(max_zoom = 18) %>%
          leaflet::setView(lat = rv_local$current_view$lat, lng = rv_local$current_view$lng, zoom = rv_local$current_view$zoom) %>%
          leaflet::addPolylines(data = lev3_lines, color = "#292C2F", opacity = 1, group = "basins", weight = 0.2, layerId = "basin_lines_3")
      )
    })


    observeEvent(input$dismiss_logo, {
      rv_local$showLogo <- FALSE
    })


    output$logo_box_ui <- renderUI({
      req(rv_local$showLogo)
      absolutePanel(
        id = ns("logo_box"),
        class = "welcome-box",
        top = "100px",
        left = "100px",
        width = 320,
        style = "z-index: 9999; background: white; padding: 10px;",
        actionButton(ns("dismiss_logo"), label = NULL, class = "close-btn", icon = icon("times")),
        div("Logo test below:"),
        tags$img(src = "www/logos/diagram.png", style = "max-width: 100%; border: 1px solid red;")
      )
    })

    #  default nutrient dataset for beginning of session
    observeEvent(input$nutrient_dataset, {
      if (is.null(input$nutrient_dataset) || input$nutrient_dataset == "") {
        updateSelectInput(session, "nutrient_dataset", selected = "Phosphorus_Rivers_Psurface_runoff_nat")
      }
    }, once = TRUE, ignoreInit = FALSE)

    # welcome panel
    output$welcome_content <- renderUI({
      req(rv_local$showWelcome)
      tagList(
        actionButton(ns("dismiss_welcome"), label = NULL, class = "close-btn", icon = icon("times")),
        h4("Welcome to the Global Lakes Explorer"),
        p("Click into any region to get started."),
        tags$img(src = "www/logos/diagram.png", style = "max-width: 80%; margin-top: 3px;")
      )
    })

    # dismiss button
    observeEvent(input$dismiss_welcome, {
      rv_local$showWelcome <- FALSE
    })

    observe({
      req(input$map_bounds)
      rv_local$current_view <- list(
        lat = input$map_center$lat,
        lng = input$map_center$lng,
        zoom = input$map_zoom
      )
    })

    observeEvent(c(input$nutrient_dataset), ignoreInit = FALSE, label = "when a nutrient layer is chosen, set to nothing the others (gbf_2 and gbf_7)", {
      if (as.character(input$nutrient_dataset != "")){
        if (input$gbf_2 != ""){
          shinyjs::reset("gbf_2", asis = FALSE)
        }
        rv_local$category_display <- "nutrient_dataset"
        rv_local$done <- rv_local$done+1
      }
    })

    observeEvent(input$gbf_2, {
      if (input$gbf_2 != "") {
        if (!is.null(input$nutrient_dataset) && input$nutrient_dataset != "") {
          shinyjs::reset("nutrient_dataset", asis = FALSE)
          updateSelectInput(session, "nutrient_dataset", selected = "")
          print(input$nutrient_dataset)
        }
        rv_local$category_display <- "gbf_2"
        rv_local$done <- rv_local$done + 1
      }
    }, ignoreInit = FALSE, label = "when a gbf_2 layer is chosen, set to nothing the others (nutrient and gbf_7)")

    #observe({
    # selected_var <- input$nutrient_dataset
    #if (selected_var == "") selected_var <- input$gbf_2
    #descriptions <- list(
    # "Phosphorus_Rivers_Psurface_runoff_nat" = "Phosphorus load from surface runoff from natural land (kg P/km²/yr)",
    #"Phosphorus_Rivers_Psurface_runoff_agri" = "Phosphorus load from surface runoff from agricultural land (kg P/km²/yr)",
    #"Phosphorus_Rivers_Pweathering" = "Phosphorus load from weathering reaching surface water (kg P/km²/yr)",
    #"Phosphorus_Rivers_Pvegetation" = "Phosphorus load from allochtonous organic matter input to rivers (kg P/km²/yr)",
    #"Phosphorus_Rivers_Paquaculture" = "Phosphorus load from aquaculture to surface water (kg P/km²yr)",
    #"Phosphorus_Rivers_Psewage" = "Phosphorus load from waste water (human and industry) to surface water (kg P/km²/yr)",
    #"Nitrogen_Rivers_Nsurface_runoff_nat" = "Nitrogen load from surface runoff from natural land (kg N/km²/yr)",
    #"Nitrogen_Rivers_Nsurface_runoff_agri" = "Nitrogen load from surface runoff from agricultural land (kg N/km²/yr)",
    #"Nitrogen_Rivers_Ngroundwater_nat" = "Nitrogen load from groundwater from natural land (kg N/km²/yr)",
    #"Nitrogen_Rivers_Ngroundwater_agri" = "Nitrogen load from groundwater from agricultural land (kg N/km²/yr)",
    #"Nitrogen_Rivers_Nvegetation" = "Nitrogen load from allochtonous organic matter input to rivers (kg N/km²/yr)",
    #"Nitrogen_Rivers_Ndeposition_water" = "Direct nitrogen deposition on water (kg N/km²/yr)",
    #"Nitrogen_Rivers_Naquaculture" = "Nitrogen load from aquaculture to surface water (kg N/km²/yr)",
    #"Nitrogen_Rivers_Nsewage" = "Nitrogen load from aquaculture to surface water (kg N/km²yr)",
    #"dis_m3_pyr" = "Annual volume of natural river discharge. Higher values indicate more available water, which can dilute pollutants and support ecosystems.",
    #"dis_m3_pmn" = "Minimum annual discharge. Lower values suggest vulnerability to drought and ecological stress during dry periods.",
    #"dis_m3_pmx" = "Maximum annual discharge. High peaks can signal flood risks or seasonal extremes.",
    #"run_mm_syr" = "Annual surface runoff. Higher values reflect greater water movement over land, potentially increasing erosion and nutrient transport.",
    #"ari_ix_sav" = "Aridity index in the sub-basin. Lower values mean more arid conditions, influencing water availability and agricultural potential.",
    #"ari_ix_uav" = "Aridity index across the upstream watershed. Helps assess regional dryness and hydrological stress.",
    #"glc_cl_smj" = "Dominant land cover class. Indicates the prevailing land use, which shapes runoff, erosion, and nutrient cycling.",
    #"wet_cl_smj" = "Dominant wetland class. Reflects ecological characteristics important for biodiversity and water purification.",
    #"pac_pc_sse" = "%",
    #"pac_pc_use" = "%"
    #)
    #desc <- descriptions[[selected_var]] %||% "Select a dataset to see details."
    #output$variable_info <- renderUI({
    # if (selected_var != "") {
    #  div(
    #   h5("Your selected variable:", style = "margin-top: 0; font-weight: bold;"),
    #  p(desc, style = "font-size: 14px; color: #333;")
    #)
    #  } else {
    #   NULL
    #}
    #})
    #})

    observeEvent(c(input$climate_scenario, input$year), ignoreInit = T, label = "trigger the change in layer when changing scenario and year", {
      if (rv_local$category_display == "nutrient_dataset" && isTruthy(input$climate_scenario) && isTruthy(input$year)) {
        rv_local$done <- rv_local$done + 1
      }
    })

    observe({
      print("Debug: rv_local$done")
      print(rv_local$done)
      print("Debug: layer_to_display()")
      print(layer_to_display())
    })

    observeEvent(input$search_btn, {
      req(input$search_query)
      res <- tryCatch(
        tidygeocoder::geo(input$search_query, method = "osm", limit = 1, verbose = FALSE),
        error = function(e) NULL
      )
      if (!is.null(res) && !is.na(res$lat) && !is.na(res$long)) {
        leaflet::leafletProxy("map", session = session) %>%
          leaflet::clearGroup("search_marker") %>%
          leaflet::addMarkers(
            lng = res$long,
            lat = res$lat,
            popup = input$search_query,
            group = "search_marker"
          ) %>%
          leaflet::setView(
            lng = res$long,
            lat = res$lat,
            zoom = 8
          )
      } else {
        showNotification("No results found.", type = "error")
      }
    })

    ### MOUSEOVER LOGIC ###
    mouse_pos <- reactive({
      req(input$map_mousemove)
      list(lng = input$map_mousemove$lng, lat = input$map_mousemove$lat)
    }) %>%
      shiny::debounce(300)

    observe({
      req(is.null(rv$lev_3_chosen))  # only if no basin chosen
      pos <- mouse_pos()
      if (is.null(pos)) return()

      sf::sf_use_s2(FALSE)
      # create a single point in the same crs as shapes
      point_sf <- sf::st_sfc(sf::st_point(c(pos$lng, pos$lat)), crs = sf::st_crs(lev3_shapes_precise))
      point_sf <- sf::st_transform(point_sf, crs = sf::st_crs(lev3_shapes_precise))

      intersections <- sf::st_intersects(lev3_shapes_precise, point_sf, sparse = TRUE)
      hovered_idx <- integer(0)
      if (length(intersections) >= 1 && length(intersections[[1]]) > 0) {
        hovered_idx <- intersections[[1]]
      }

      new_hovered_id <- NULL
      if (length(hovered_idx) == 1) {
        new_hovered_id <- lev3_shapes_precise$HYBAS_ID[hovered_idx]
      }

      if (!is.null(new_hovered_id) && new_hovered_id != rv_local$hovered_id) {
        rv_local$hovered_id <- new_hovered_id
        hovered_polygon <- dplyr::filter(lev3_shapes_precise, HYBAS_ID == new_hovered_id)
        if (nrow(hovered_polygon) > 0) {
          layer <- layer_to_display()
          if (!is.null(layer) && layer != "empty") {
            selected_var <- input$nutrient_dataset
            if (selected_var == "") selected_var <- input$gbf_2
            # safe access the value without converting big dataframes repeatedly
            row_idx <- which(lev3_vars$HYBAS_ID == new_hovered_id)
            value <- NA
            if (length(row_idx) == 1 && layer %in% colnames(lev3_vars)) {
              value <- lev3_vars[row_idx, layer][[1]]
            }
            selected_unit <- rv_local$units_lookup_converted[[selected_var]]
            label_content <- if (is.numeric(value)) paste0(round(value, 2), " ", selected_unit) else paste0(value)

            leaflet::leafletProxy(ns("map"), session = session) %>%
              leaflet::clearGroup("hover_group") %>%
              leaflet::addPolygons(
                data = hovered_polygon,
                fillColor = "transparent",
                fillOpacity = 0,
                stroke = TRUE,
                color = "yellow",
                weight = 4,
                opacity = 1,
                group = "hover_group",
                layerId = "hovered_polygon",
                label = label_content,
                labelOptions = leaflet::labelOptions(
                  direction = "right",
                  textOnly = TRUE,
                  permanent = FALSE,
                  opacity = 0.95,
                  offset = c(120, -80),
                  style = list(
                    "font-weight" = "bold",
                    "font-size" = "14px",
                    "color" = "#222",
                    "background-color" = "rgba(255, 255, 255, 0.95)",
                    "border-radius" = "8px",
                    "padding" = "6px 10px",
                    "box-shadow" = "3px 3px 6px rgba(0,0,0,0.25)"
                  )
                ),
                options = leaflet::pathOptions(clickable = FALSE)
              )
          }
        }
      } else if (is.null(new_hovered_id) && !is.null(rv_local$hovered_id)) {
        rv_local$hovered_id <- NULL
        leaflet::leafletProxy(ns("map"), session = session) %>%
          leaflet::clearGroup("hover_group")
      }
    })

    ### END MOUSEOVER LOGIC ###

    observeEvent(c(layer_to_display()), ignoreNULL = TRUE, label = "display layers", {
      # remove existing dynamic layers and controls to ensure a fresh base.
      leaflet::leafletProxy(ns("map"), session = session)%>%
        leaflet::clearGroup("variables") %>%
        leaflet::clearGroup("selected_basin") %>%
        leaflet::clearGroup("lakes_layer") %>%
        leaflet::clearGroup("lake_borders") %>%
        leaflet::clearControls()

      layer <- layer_to_display()
      req(layer)

      values <- lev3_vars_converted() %>%
        dplyr::select(HYBAS_ID, !!layer) %>%
        sf::st_as_sf()

      values$values <- values[[layer]]

      if (rv_local$category_display == "nutrient_dataset") {
        base_var_name <- input$nutrient_dataset
      } else {
        base_var_name <- input$gbf_2
      }

      #  'is_numeric' outside pipe
      is_numeric <- base_var_name %in% names(rv_local$global_variable_ranges_converted)

      if (is_numeric) {
        range_vals <- rv_local$global_variable_ranges_converted[[base_var_name]]
        min_val <- max(0, range_vals$min)
        # ✅ CORRECTED TYPO HERE: range_vals$max
        max_val <- range_vals$max
        palette <- leaflet::colorNumeric("plasma", domain = c(min_val, max_val), na.color = "transparent")
        legend_values <- c(min_val, max_val)
      } else {
        # Factor variable logic
        legend_values <- levels(values$values)
        palette <- leaflet::colorFactor(viridisLite::viridis(length(legend_values), option = "turbo"), domain = legend_values)
      }

      name_leg <- BasinATLASgol::legend_names(layer)
      selected_var <- input$nutrient_dataset
      if (selected_var == "") selected_var <- input$gbf_2
      selected_unit <- rv_local$units_lookup_converted[[selected_var]]

      leaflet::leafletProxy(ns("map"), session = session) %>%
        leaflet::clearGroup("variables") %>%
        leaflet::addPolygons(
          data = values,
          fillColor = ~palette(values),
          fillOpacity = 0.6,
          stroke = TRUE,
          color = "white",
          weight = 2,
          group = "variables",
          layerId = ~HYBAS_ID,
          label = ~if (is.numeric(values)) {
            paste0(round(values, 2), " ", selected_unit)
          } else {
            paste0(values)
          },
          labelOptions = leaflet::labelOptions(
            direction = 'auto',
            textOnly = TRUE,
            style = list(
              "font-size" = "13px",
              "font-weight" = "500",
              "color" = "#222",
              "background" = "rgba(255,255,255,0.9)",
              "padding" = "4px 8px",
              "border-radius" = "6px",
              "box-shadow" = "0 1px 3px rgba(0,0,0,0.25)") )
        ) %>%
        leaflet::clearControls() %>%

        {
          if (is_numeric) {
            #  legend for numeric data
            leaflet::addLegend(., pal = palette, values = legend_values,
                               title = name_leg, position = "bottomright")
          } else {
            #  multi-column legend for factor data
            leaflet::addLegend(., pal = palette, values = legend_values,
                               title = name_leg, position = "bottomright",
                               className = "info legend multi-col-legend"
            )
          }
        }


      # redraw the selected basin if one is already chosen
      if (!is.null(rv$lev_3_chosen)) {
        chosen_basin_data <- dplyr::filter(lev3_vars_converted(), HYBAS_ID == rv$lev_3_chosen)
        leaflet::leafletProxy(ns("map"), session = session) %>%
          leaflet::addPolygons(
            data = chosen_basin_data,
            fillColor = "transparent",
            fillOpacity = 0,
            stroke = TRUE,
            color = "orange",
            weight = 5,
            group = "selected_basin",
            layerId = ~HYBAS_ID,
            options = leaflet::pathOptions(clickable = FALSE)
          ) %>%
          # add the lake lines on top
          leaflet::addPolylines(
            data = dplyr::filter(rv$lakes_in_lev3, !(Hylak_id %in% coastal_lake_ids)),
            color = "white",
            opacity = 0.7,
            group = "lake_borders",
            weight = 0.3,
            layerId = "lake_borders"
          ) %>%
          # add the lakes with highlight options
          leaflet::addPolygons(
            data = dplyr::filter(rv$lakes_in_lev3, !(Hylak_id %in% coastal_lake_ids)),
            fillColor = "#2a8a97",
            color = "white",
            group = "lakes_layer",
            layerId = ~Hylak_id,
            opacity = 1,
            fillOpacity = 1,
            highlightOptions = leaflet::highlightOptions(
              weight = 4,
              color = "yellow",
              fillOpacity = 0.9,
              bringToFront = TRUE
            )
          )
      }
    })

    # observer to handle map clicks.
    observeEvent(input$map_click, {
      sf::sf_use_s2(FALSE)
      leaflet::leafletProxy(ns("map"), session = session) %>%
        leaflet::clearGroup("hover_group") %>%
        leaflet::removePopup(layerId = "hover-popup")

      point_sf <- sf::st_sfc(
        sf::st_point(c(input$map_click$lng, input$map_click$lat)),
        crs = sf::st_crs(lev3_shapes_precise)
      ) %>%
        sf::st_transform(crs = sf::st_crs(lev3_shapes_precise))

      lev3_shapes <- sf::st_transform(lev3_shapes_precise, crs = sf::st_crs(point_sf))

      intersection <- sf::st_intersects(lev3_shapes, point_sf, sparse = FALSE)
      selected_idx <- which(intersection)

      if (length(selected_idx) == 1) {
        rv$lev_3_chosen <- lev3_shapes$HYBAS_ID[selected_idx]
        rv$lakes_in_lev3 <- dplyr::filter(globo_topo_poly, lev3_HYBAS_ID == rv$lev_3_chosen)
        rv$lake_lines_in_lev3 <- dplyr::filter(globo_topo_lines, lev3_HYBAS_ID == rv$lev_3_chosen)

        center <- sf::st_centroid(lev3_shapes[selected_idx, ]) %>%
          sf::st_coordinates() %>%
          as.data.frame()

        layer <- layer_to_display()
        if (is.null(layer) || layer == "empty") return()

        selected_var <- input$nutrient_dataset
        if (selected_var == "") selected_var <- input$gbf_2
        selected_unit <- rv_local$units_lookup_converted[[selected_var]]

        #lev3_vars_data <- lev3_vars_converted()
        #if (!layer %in% c("glc_cl_smj", "wet_cl_smj")) {
        # value <- sf::st_drop_geometry(lev3_vars_data)[lev3_vars_data$HYBAS_ID == rv$lev_3_chosen, layer][[1]]
        #rounded_value <- round(value, 1)
        #info_html <- paste0(
        # "<div id='basin-info-box' style='position: relative; padding: 6px 10px; background: white; border-radius: 10px; box-shadow: 0 2px 5px rgba(0,0,0,0.25); font-size: 18px; font-weight: 500; display: inline-block; margin-top: 6px;'>",
        #"<span style='position: absolute; top: 2px; right: 6px; cursor: pointer; font-size: 16px; font-weight: bold; color: #666;' onclick='this.parentElement.style.display=\"none\"'>&times;</span>",
        #" Your selected HydroBASIN: <b>", rounded_value, "&nbsp;&nbsp;", selected_unit, "</b></div>"
        #)
        #leaflet::leaflet::leafletProxy("map") %>%
        #leaflet::addControl(html = info_html, position = "topleft", layerId = "basin-info")
        #  }

        leaflet::leafletProxy(ns("map"), session = session) %>%
          leaflet::setView(lng = center$X, lat = center$Y, zoom = 6)
      } else {
        print("❌ No Level 3 Basin Found for Clicked Point")
      }
    })

    #  add highlight and lakes on top of the existing map
    observeEvent(rv$lev_3_chosen, {
      # remove ONLY the temp layers from the previous click
      leaflet::leafletProxy(ns("map"), session = session) %>%
        leaflet::clearGroup("selected_basin") %>%
        leaflet::clearGroup("lakes_layer") %>%
        leaflet::clearGroup("lake_borders")

      # if a basin is chosen, draw the highlight and lakes on top.
      if (!is.null(rv$lev_3_chosen)) {
        chosen_basin_data <- dplyr::filter(lev3_vars_converted(), HYBAS_ID == rv$lev_3_chosen)

        leaflet::leafletProxy(ns("map"), session = session) %>%
          leaflet::addPolygons(
            data = chosen_basin_data,
            fillColor = "transparent",
            fillOpacity = 0,
            stroke = TRUE,
            color = "orange",
            weight = 3,
            group = "selected_basin",
            layerId = ~HYBAS_ID,
            options = leaflet::pathOptions(clickable = FALSE)
          ) %>%
          leaflet::addPolylines(
            data = rv$lake_lines_in_lev3,
            color = "white",
            opacity = 0.7,
            group = "lake_borders",
            weight = 0.3,
            layerId = "lake_borders"
          ) %>%
          leaflet::addPolygons(
            data = rv$lakes_in_lev3,
            fillColor = "#2a8a97",
            color = "white",
            group = "lakes_layer",
            layerId = ~Hylak_id,
            opacity = 1,
            fillOpacity = 1,
            highlightOptions = leaflet::highlightOptions(
              weight = 4,
              color = "yellow",
              fillOpacity = 0.9,
              bringToFront = TRUE
            )
          )
      }
    }, ignoreNULL = FALSE)

    observeEvent(input$map_shape_click, {
      click <- input$map_shape_click
      print(paste("Clicked shape ID:", click$id))
      if (is.null(click$id)) {
        print("❌ No shape clicked.")
        return(NULL)
      }
      if (!is.null(rv$lev_3_chosen)) {
        lake_clicked <- globo_topo_poly$Hylak_id == click$id
        if (any(lake_clicked)) {
          lake_name <- globo_topo_poly$Hylak_id[lake_clicked]
          rv$lake_clicked <- lake_name
          print(paste("✅ Lake clicked:", lake_name))
          showModal(modal_lev7())
        } else {
          print("❌ Clicked on non-lake shape.")
        }
      } else {
        print("❌ No Level 3 Basin selected yet. Cannot click on lakes.")
      }
    })


    observeEvent(input$reset_button, {
      # remove all temporary layers added on click
      leaflet::leafletProxy(ns("map"), session = session) %>%
        leaflet::clearGroup("selected_basin") %>%
        leaflet::clearGroup("lakes_layer") %>%
        leaflet::clearGroup("lake_borders") %>%
        leaflet::removeControl("basin-info")

      rv$lev_3_chosen <- NULL
      rv$lakes_in_lev3 <- NULL
      rv$lake_lines_in_lev3 <- NULL
      rv$lake_clicked <- NULL

      rv_local$hovered_id <- NULL
      rv_local$category_display <- NULL
      rv_local$done <- 0

      shinyjs::reset("nutrient_dataset")
      shinyjs::reset("gbf_2")

      leaflet::leafletProxy(ns("map"), session = session) %>%
        leaflet::clearGroup("hover_group") %>%
        leaflet::removePopup(layerId = "hover-popup")
    })

    modal_lev7 <- function(){
      ns <- session$ns
      print("Modal function called")
      modalDialog(
        tagList(
          mod_lev_7_ui("lev_7_1")
        ),
        size = "xl",
        easyClose = T,
        footer = NULL,
        fade = FALSE
      )
    }
  })
}
## To be copied in the UI
# mod_basin_atlas_ui("basin_atlas_1")

## To be copied in the server
# mod_basin_atlas_server("basin_atlas_1")
