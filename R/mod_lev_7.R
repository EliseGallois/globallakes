#' lev_7 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_lev_7_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$head(
      tags$link(rel = "stylesheet",
                href = "https://fonts.googleapis.com/css2?family=Roboto:wght@400;700&display=swap"),

      # css base
      tags$style(HTML("
        .content {
          font-family: 'Roboto', sans-serif;
          color: #333;
          line-height: 1.6;
          padding: 10px;
        }
        .content h2, .content h3, .content h4, .content h5 {
          color: #37a635;
          margin-top: 10px;
        }
        .content p {
          text-align: justify;
          margin-bottom: 10px;
        }
        .content p.gray { color: gray; }

        .img-container {
          width: 100%;
          height: 150px;
          object-fit: cover;
          padding: 10px;
          margin-top: 10px;
          border-radius: 10px;
        }

        .download-box {
          text-align: center;
          margin: 10px auto;
          padding: 10px 0;
          border-bottom: 1px solid #ddd;
        }

        .download-button-group .btn {
          margin: 5px;
          font-size: 0.85em;
          padding: 6px 10px;
        }

        .disclaimer-box {
          background-color:#fdf3cd;
          border-left: 5px solid #ffc107;
          padding: 8px 10px;
          border-radius: 5px;
          font-size: 0.8em;
          color: #555;
          max-width: 900px;
          margin: 10px auto;
          text-align: left;
        }

        /* Factfile Redesign Styles */
        .lev7-box {
          background-color: #f9f9f9;
          border-radius: 12px;
          padding: 20px;
          font-size: 16px;
          margin-top: 10px;
          box-shadow: 0 4px 8px rgba(0,0,0,0.05);
        }
        .lev7-title {
          font-weight: bold;
          text-align: center;
          margin-bottom: 20px;
          font-size: 20px;
          color: #2c3e50;
        }
        .lev7-grid {
          display: grid;
          grid-template-columns: repeat(2, 1fr);
          gap: 15px 20px;
        }
        .lev7-item {
          display: flex;
          align-items: center;
          padding: 8px 0;
          border-bottom: 1px dashed #eee;
        }
        .lev7-item.full-row {
          grid-column: 1 / -1;
          border-bottom: none;
          margin-top: 10px;
          padding-top: 15px;
          border-top: 1px dashed #eee;
        }
        .lev7-item:last-child { border-bottom: none; }
        .lev7-icon {
          font-size: 24px;
          margin-right: 15px;
          color: #2e7d32;
          width: 30px;
          text-align: center;
        }
        .lev7-label {
          font-size: 15px;
          font-weight: bold;
          color: #2c3e50;
        }
        .lev7-value {
          font-size: 14px;
          color: #555;
          margin-top: 2px;
        }
      "))
    ),

    shinyjs::useShinyjs(),

    bslib::card(
      bslib::card_body(

        # download buttons
        fluidRow(
          column(
            12,
            div(class = "download-box",
                div(class = "download-button-group",
                    downloadButton(ns("download_level7_csv"), "Download Selected Lake Catchment Data", class = "btn-primary"),
                    #downloadButton(ns("download_level3_csv"), "Download All Regional Catchments Data", class = "btn-success"),
                    downloadButton(ns("download_metadata"), "Download Metadata", class = "btn-secondary")
                ),
                div(class = "disclaimer-box",
                    tags$strong("Disclaimer:"),
                    " For the data sources used, please see the metadata tab and document.
                      UKCEH did not collect this data, but presents a collation from secondary sources.
                      Please cite the original authors in your own analyses."
                )
            )
          )
        ),

        fluidRow(
          column(
            6,
            h4("Your selected lake (blue) within its catchment (yellow)",
               style = "text-align:center;"),
            leaflet::leafletOutput(ns("map_7"), height = "500px")
          ),

          column(
            6,
            div(
              style = "margin-top: 30px;",
              div(
                style = "display: flex; align-items: center; gap: 20px; margin-bottom: 10px;",
                radioButtons(ns("nutrient"), label = NULL,
                             choices = c("Phosphorus", "Nitrogen"),
                             selected = "Phosphorus", inline = TRUE),

                selectInput(
                  ns("ssp"), label = NULL,
                  choices = c(
                    "Sustainability (SSP1)" = "SSP1",
                    "Middle of the road (SSP2)" = "SSP2",
                    "Regional rivalry (SSP3)" = "SSP3",
                    "Global inequality (SSP4)" = "SSP4",
                    "Fossil-fueled development (SSP5)" = "SSP5"
                  ),
                  selected = "SSP2"
                ),

                selectInput(
                  ns("stacked_plot_type"),
                  label = NULL,
                  choices = c(
                    "Absolute totals" = "absolute",
                    "Relative (%)" = "relative",
                    "Per km²" = "per_area"
                  ),
                  selected = "per_area"
                )


              ),

              # ssp info link
              tags$p(
                style = "font-size: 14px; margin-top: -8px; margin-left: 5px; color: #555;",
                tags$a(
                  href = "https://ore.exeter.ac.uk/articles/online_resource/The_Scenario_Model_Intercomparison_Project_ScenarioMIP_for_CMIP6/29724578?file=56741828",
                  target = "_blank",
                  "Click here for more information on SSPs"
                )
              ),

              plotOutput(ns("stacked_area_plot"), height = "500px")
            )
          )
        ),

        # factfile left, graphs right
        fluidRow(
          column(6, uiOutput(ns("more_on_lev7"))),
          column(6, uiOutput(ns("variable_selection_ui")),
                 plotOutput(ns("combined_graph"), height = "500px"))
        )
      )
    )
  )
}


#' lev_7 Server Functions
#'
#'
#' @noRd
mod_lev_7_server <- function(id, rv, x, lev7_vars, globo_topo_poly) {
  moduleServer(id, session = x, function(input, output, session) {
    ns <- session$ns

    catch_df <- read.csv("data/topocat_lake_catch_Hylak_id.csv")

    lev_7_vars <- eventReactive(rv$lake_clicked, {
      if (!isTruthy(rv$lake_clicked)) {
        NULL
      } else {
        lake_data <- dplyr::filter(lev7_vars,
                                   Hylak_id == rv$lake_clicked) %>%
          sf::st_drop_geometry() %>%
          as.data.frame()
        lake_data <- dplyr::left_join(lake_data, catch_df, by = "Hylak_id")
        ssp_cols <- grep("^SSP", names(lake_data), value = TRUE)
        if (length(ssp_cols) > 0 && "Catch_area" %in% names(lake_data)) {
          scale_factor <- lake_data$Catch_area[1] / 2500
          lake_data[ssp_cols] <- lake_data[ssp_cols] * scale_factor
        }
        lake_data
      }
    })

    lev_3_vars <- eventReactive(rv$lake_clicked, {
      link_df <- read.csv("data/topocat_Hylak_id_lev03_HYBAS_ID_v1c.csv")
      lev3_id <- link_df %>% dplyr::filter(Hylak_id == rv$lake_clicked) %>% dplyr::pull(lev3_HYBAS_ID)
      if (length(lev3_id) == 0) return(NULL)
      lev7_ids <- link_df %>% dplyr::filter(lev3_HYBAS_ID == lev3_id) %>% dplyr::pull(Hylak_id)
      if (length(lev7_ids) == 0) return(NULL)
      data <- lev7_vars %>%
        dplyr::filter(Hylak_id %in% lev7_ids) %>%
        sf::st_drop_geometry() %>%
        as.data.frame()
      data <- dplyr::left_join(data, catch_df, by = "Hylak_id")
      ssp_cols <- grep("^SSP", names(data), value = TRUE)
      if (length(ssp_cols) > 0 && "Catch_area" %in% names(data)) {
        for (i in seq_len(nrow(data))) {
          scale_factor <- data$Catch_area[i] / 2500
          data[i, ssp_cols] <- data[i, ssp_cols] * scale_factor
        }
      }
      data
    })

    lev_7_lines <- eventReactive(rv$lake_clicked, {
      if(!isTruthy(rv$lake_clicked)){
        NULL
      }else{
        dplyr::filter(lev7_vars,
                      Hylak_id == rv$lake_clicked)
      }
    })

    output$map_7 <- leaflet::renderLeaflet({
      if (isTruthy(lev_7_lines())) {
        bbox  <- sf::st_bbox(lev_7_lines())
        lake_shape <- lev_7_lines()[, "geom"]
        filtered_lake_poly <- dplyr::filter(globo_topo_poly, Hylak_id == rv$lake_clicked)
        leaflet::leaflet(
          options = leaflet::leafletOptions(worldCopyJump = FALSE,
                                            maxBoundsViscosity = 1.0)) %>%
          leaflet::setMaxBounds(lng1 = -180, lat1 = -90, lng2 = 180, lat2 = 90) %>%
          leaflet::addProviderTiles("CartoDB.Voyager",
                                    options = leaflet::providerTileOptions(
                                      zIndex=0, minZoom = 2, maxZoom = 18, noWrap = TRUE, tileSize = 256
                                    )) %>%
          leaflet::setView(lng = mean(c(bbox["xmin"], bbox["xmax"])),
                           lat = mean(c(bbox["ymin"], bbox["ymax"])),
                           zoom = 6) %>%
          leaflet::addPolygons(data = lake_shape, color = "#ebab34", weight = 3, fillColor = "#FDDA0D", fill = TRUE, opacity = 0.5) %>%
          leaflet::addPolygons(data = filtered_lake_poly, color = "#146F79", weight = 3, fillColor = "#34b8c7", fill = TRUE, opacity = 0.9)
      } else {
        return(NULL)
      }
    })

    output$more_on_lev7 <- renderUI({
      more_on_lev7_vars <- dplyr::filter(globo_topo_poly,
                                         Hylak_id == rv$lake_clicked) %>%
        sf::st_drop_geometry() %>%
        as.data.frame()
      lev7_data <- lev_7_vars()
      if (!is.null(lev7_data) && nrow(lev7_data) > 0 && isTruthy(more_on_lev7_vars)) {
        div(
          class = "content",
          div(class = "lev7-box",
              div(class = "lev7-title", "Lake Catchment Factfile:"),
              div(class = "lev7-grid",
                  div(class = "lev7-item",
                      span(class = "lev7-icon", icon("water")),
                      div(class = "lev7-text",
                          span(class = "lev7-label", "Hylak ID"),
                          span(class = "lev7-value", more_on_lev7_vars$Hylak_id[1])
                      )
                  ),
                  div(class = "lev7-item",
                      span(class = "lev7-icon", icon("globe")),
                      div(class = "lev7-text",
                          span(class = "lev7-label", "Lake Type"),
                          span(class = "lev7-value", more_on_lev7_vars$Lake_type[1])
                      )
                  ),
                  div(class = "lev7-item",
                      span(class = "lev7-icon", icon("ruler-combined")),
                      div(class = "lev7-text",
                          span(class = "lev7-label", "Lake Area"),
                          span(class = "lev7-value", paste0(format(round(more_on_lev7_vars$Lake_area[1], 2), big.mark = ","), " km²"))
                      )
                  ),
                  div(
                    class = "lev7-item",
                    span(class = "lev7-icon", icon("mountain")),
                    div(
                      class = "lev7-text",
                      span(class = "lev7-label", "Catchment Area"),
                      span(
                        class = "lev7-value",
                        paste0(
                          format(round(lev_7_vars()$Catch_area[1])), " km²"
                        )
                      )
                    )
                  )
                  ,

                  div(class = "lev7-item",
                      span(class = "lev7-icon", icon("users")),
                      div(class = "lev7-text",
                          span(class = "lev7-label", "Pop. Density (people/km²) in Catchment"),
                          span(class = "lev7-value", round(lev7_data$popn[1], 2))
                      )
                  ),
                  div(class = "lev7-item",
                      span(class = "lev7-icon", icon("leaf")),
                      div(class = "lev7-text",
                          span(class = "lev7-label", "Dominant Land Use in Catchment"),
                          span(class = "lev7-value", lev7_data$glc_cl_smj[1])
                      )
                  ),
                  div(class = "lev7-item",
                      span(class = "lev7-icon", icon("tint")),
                      div(class = "lev7-text",
                          span(class = "lev7-label", "Avg. Annual Runoff in Catchment"),
                          span(class = "lev7-value", paste0(round(lev7_data$run_mm_syr[1], 1), " mm/yr"))
                      )
                  ),
                  div(class = "lev7-item full-row",
                      span(class = "lev7-icon", icon("flag")),
                      div(class = "lev7-text",
                          span(class = "lev7-label", "Countries covered by Catchment"),
                          span(class = "lev7-value", lev7_data$nam_en[1])
                      )
                  )
              )
          )
        )
      } else {
        return(tags$p("No info for this lev7", class = "gray"))
      }
    })

    variable_labels <- c(
      "Phosphorus_Rivers_Psurface_runoff_nat" = "Phosphorus load from surface runoff from natural land (kg P yr-1)",
      "Phosphorus_Rivers_Psurface_runoff_agri" = "Phosphorus load from surface runoff from agricultural land (kg P yr-1)",
      "Phosphorus_Rivers_Pweathering" = "Phosphorus load from weathering reaching surface water (kg P yr-1)",
      "Phosphorus_Rivers_Pvegetation" = "Phosphorus load from allochtonous organic matter input to rivers (kg P yr-1)",
      "Phosphorus_Rivers_Paquaculture" = "Phosphorus load from aquaculture to surface water (kg P yr-1)",
      "Phosphorus_Rivers_Psewage" = "Phosphorus load from waste water (human and industry) to surface water (kg P yr-1)",
      "Nitrogen_Rivers_Nsurface_runoff_nat" = "Nitrogen load from surface runoff from natural land (kg N yr-1)",
      "Nitrogen_Rivers_Nsurface_runoff_agri" = "Nitrogen load from surface runoff from agricultural land (kg N yr-1)",
      "Nitrogen_Rivers_Ngroundwater_nat" = "Nitrogen load from groundwater from natural land (kg N yr-1)",
      "Nitrogen_Rivers_Ngroundwater_agri" = "Nitrogen load from groundwater from agricultural land (kg N yr-1)",
      "Nitrogen_Rivers_Nvegetation" = "Nitrogen load from allochtonous organic matter input to rivers (kg N yr-1)",
      "Nitrogen_Rivers_Naquaculture" = "Nitrogen load from aquaculture to surface water (kg N yr-1)",
      "Nitrogen_Rivers_Nsewage" = "Nitrogen load from aquaculture to surface water (kg N yr-1)"
    )

    ssp_full_labels <- c(
      "SSP1" = "Sustainability (SSP1)",
      "SSP2" = "Middle of the road (SSP2)",
      "SSP3" = "Regional rivalry (SSP3)",
      "SSP4" = "Global inequality (SSP4)",
      "SSP5" = "Fossil-fueled development (SSP5)"
    )

    output$variable_selection_ui <- renderUI({
      req(lev_7_vars())
      variable_cols <- grep("SSP\\d+_", colnames(lev_7_vars()), value = TRUE)
      variable_matches <- stringr::str_match(variable_cols, "SSP\\d+_(.*)_\\d{4}")
      variable_types <- unique(variable_matches[, 2])
      variable_types <- variable_types[order(factor(variable_types, levels = c(
        "Phosphorus_Rivers_Psurface_runoff_nat",
        "Phosphorus_Rivers_Psurface_runoff_agri",
        "Phosphorus_Rivers_Pweathering",
        "Phosphorus_Rivers_Pvegetation",
        "Phosphorus_Rivers_Paquaculture",
        "Phosphorus_Rivers_Psewage",
        "Nitrogen_Rivers_Nsurface_runoff_nat",
        "Nitrogen_Rivers_Nsurface_runoff_agri",
        "Nitrogen_Rivers_Ngroundwater_nat",
        "Nitrogen_Rivers_Ngroundwater_agri",
        "Nitrogen_Rivers_Nvegetation",
        "Nitrogen_Rivers_Ndeposition_water",
        "Nitrogen_Rivers_Naquaculture",
        "Nitrogen_Rivers_Nsewage"
      )))]
      valid_variable_types <- variable_types[variable_types %in% names(variable_labels)]
      nice_labels <- variable_labels[valid_variable_types]
      names(nice_labels) <- nice_labels
      selectInput(
        ns("selected_variable"),
        "Select Variable:",
        choices = nice_labels,
        selected = isolate(input$selected_variable) %||% nice_labels[1],
        width = "100%"
      )
    })

    output$combined_graph <- renderPlot({
      req(input$selected_variable, lev_7_vars())

      selected_var <- names(variable_labels)[which(variable_labels == input$selected_variable)]
      variable_pattern <- paste0("SSP\\d+_", selected_var, "_\\d{4}")

      df_long <- lev_7_vars() %>%
        dplyr::select(matches(variable_pattern)) %>%
        tidyr::pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
        dplyr::mutate(
          Scenario = stringr::str_extract(Variable, "SSP\\d+"),
          Year = as.integer(stringr::str_extract(Variable, "\\d{4}"))
        ) %>%
        dplyr::select(-Variable)

      df_long$Scenario_Full <- factor(
        ssp_full_labels[df_long$Scenario],
        levels = ssp_full_labels
      )

      ggplot2::ggplot(df_long, ggplot2::aes(x = Year, y = Value, colour = Scenario_Full)) +
        ggplot2::geom_line(size = 2.5, alpha = 0.6, lineend = "round") +
        ggplot2::geom_vline(xintercept = 2015, linetype = "dashed", color = "black", size = 1) +
        ggplot2::labs(
          title = paste(input$selected_variable, "1970-2070"),
          x = "Year",
          y = input$selected_variable,
          colour = "Scenario"
        ) +
        ggplot2::scale_colour_manual(
          values = c(
            "Sustainability (SSP1)"        = "#2ECC71",
            "Middle of the road (SSP2)"    = "#3498DB",
            "Regional rivalry (SSP3)"      = "#E74C3C",
            "Global inequality (SSP4)"     = "#9B59B6",
            "Fossil-fueled development (SSP5)" = "#E67E22"
          )
        ) +
        ggplot2::theme_light(base_size = 14) +
        ggplot2::theme(
          legend.position = "right",
          legend.direction = "vertical",
          legend.box = "vertical",
          legend.text = ggplot2::element_text(size = 12),
          legend.title = ggplot2::element_text(size = 14, face = "bold"),
          axis.text = ggplot2::element_text(size = 14),
          axis.title = ggplot2::element_text(size = 14, face = "bold"),
          plot.title = ggplot2::element_text(size = 16, face = "bold"),
          panel.grid.major = ggplot2::element_line(color = "grey85"),
          panel.grid.minor = ggplot2::element_blank()
        )
    })


    output$stacked_area_plot <- renderPlot({
      req(input$nutrient, input$ssp, lev_7_vars())

      phosphorus_vars <- c(
        "Phosphorus_Rivers_Psurface_runoff_nat",
        "Phosphorus_Rivers_Psurface_runoff_agri",
        "Phosphorus_Rivers_Pweathering",
        "Phosphorus_Rivers_Pvegetation",
        "Phosphorus_Rivers_Paquaculture",
        "Phosphorus_Rivers_Psewage"
      )

      nitrogen_vars <- c(
        "Nitrogen_Rivers_Nsurface_runoff_nat",
        "Nitrogen_Rivers_Nsurface_runoff_agri",
        "Nitrogen_Rivers_Ngroundwater_nat",
        "Nitrogen_Rivers_Ngroundwater_agri",
        "Nitrogen_Rivers_Nvegetation",
        "Nitrogen_Rivers_Naquaculture",
        "Nitrogen_Rivers_Nsewage",
        "Nitrogen_Rivers_Ndeposition_water"
      )

      selected_vars <- if (input$nutrient == "Phosphorus") phosphorus_vars else nitrogen_vars

      short_labels <- c(
        "Phosphorus_Rivers_Psurface_runoff_nat" = "Runoff (Natural)",
        "Phosphorus_Rivers_Psurface_runoff_agri" = "Runoff (Agricultural)",
        "Phosphorus_Rivers_Pweathering" = "Weathering",
        "Phosphorus_Rivers_Pvegetation" = "Vegetation",
        "Phosphorus_Rivers_Paquaculture" = "Aquaculture",
        "Phosphorus_Rivers_Psewage" = "Sewage",
        "Nitrogen_Rivers_Nsurface_runoff_nat" = "Runoff (Natural)",
        "Nitrogen_Rivers_Nsurface_runoff_agri" = "Runoff (Agricultural)",
        "Nitrogen_Rivers_Ngroundwater_nat" = "Groundwater (Natural)",
        "Nitrogen_Rivers_Ngroundwater_agri" = "Groundwater (Agricultural)",
        "Nitrogen_Rivers_Nvegetation" = "Vegetation",
        "Nitrogen_Rivers_Naquaculture" = "Aquaculture",
        "Nitrogen_Rivers_Nsewage" = "Sewage",
        "Nitrogen_Rivers_Ndeposition_water" = "Deposition"
      )

      component_colors <- c(
        "Runoff (Natural)" = "#1b9e77",
        "Runoff (Agricultural)" = "#d95f02",
        "Weathering" = "#7570b3",
        "Aquaculture" = "#e7298a",
        "Vegetation" = "#66a61e",
        "Sewage" = "#e6ab02",
        "Groundwater (Natural)" = "#a6761d",
        "Groundwater (Agricultural)" = "#666666",
        "Deposition" = "lightblue"
      )

      all_components <- names(component_colors)

      # filter matching columns
      pattern <- paste0("^", input$ssp, "_(", paste(selected_vars, collapse = "|"), ")_\\d{4}$")
      matching_cols <- grep(pattern, colnames(lev_7_vars()), value = TRUE)
      df_long <- lev_7_vars() %>%
        dplyr::select(all_of(matching_cols)) %>%
        tidyr::pivot_longer(
          cols = everything(),
          names_to = "Variable",
          values_to = "Value"
        ) %>%
        dplyr::mutate(
          VarName = stringr::str_extract(Variable, paste(selected_vars, collapse = "|")),
          Year = as.integer(stringr::str_extract(Variable, "\\d{4}")),
          Component = short_labels[VarName]
        ) %>%
        dplyr::group_by(VarName, Year) %>%
        dplyr::summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
        dplyr::mutate(Component = short_labels[VarName])

      #  keep present components
      present_components <- intersect(all_components, unique(df_long$Component))
      df_long$Component <- factor(df_long$Component, levels = present_components)

      # apply plot type - absolute relative or percent
      plot_type <- input$stacked_plot_type %||% "absolute"
      if (plot_type == "relative") {
        df_long <- df_long %>%
          dplyr::group_by(Year) %>%
          dplyr::mutate(Value = Value / sum(Value, na.rm = TRUE) * 100) %>%
          dplyr::ungroup() %>%
          dplyr::filter(!is.na(Value) & !is.nan(Value))
        y_label <- "Relative %"
      } else if (plot_type == "per_area") {
        area_km2 <- as.numeric(lev_7_vars()$Catch_area[1])
        if (is.na(area_km2) || area_km2 == 0) area_km2 <- 1
        #area_km2 <- area_km2 / 100 # convert ha -> km²
        df_long <- df_long %>%
          dplyr::mutate(Value = Value / area_km2)
        y_label <- paste(input$nutrient, "Load (kg/km²/yr)")
      } else {
        y_label <- paste(input$nutrient, "Load (kg/yr)")
      }

      ggplot2::ggplot(df_long, ggplot2::aes(x = Year, y = Value, fill = Component)) +
        ggplot2::geom_area(position = "stack", alpha = 0.7, color = "white", size = 0.3) +
        ggplot2::labs(
          title = paste(input$nutrient, "Load Components in", input$ssp),
          x = "Year",
          y = y_label,
          fill = "Component"
        ) +
        ggplot2::scale_fill_manual(values = component_colors, drop = FALSE) +
        ggplot2::theme_minimal(base_size = 14) +
        ggplot2::theme(
          legend.position = "bottom",
          strip.text = ggplot2::element_text(size = 14, face = "bold"),
          axis.text = ggplot2::element_text(size = 14),
          axis.title = ggplot2::element_text(size = 14),
          plot.title = ggplot2::element_text(size = 16, face = "bold")
        )
    })



    output$download_level7_csv <- downloadHandler(
      filename = function() {
        paste0("Selected_Catchment_Lake_", rv$lake_clicked, "_Level7_Data_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(lev_7_vars())
        write.csv(lev_7_vars(), file, row.names = FALSE)
      }
    )

    output$download_level3_csv <- downloadHandler(
      filename = function() {
        paste0("HydroBASIN_Level3_Catchment_Data_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(lev_3_vars())
        write.csv(lev_3_vars(), file, row.names = FALSE)
      }
    )

    output$download_metadata <- downloadHandler(
      filename = function() {
        "metadata_gnm.docx"
      },
      content = function(file) {
        file.copy("data/metadata_gnm.docx", file)
      }
    )
  })
}



## To be copied in the UI
# mod_lev_7_ui("lev_7_1")

## To be copied in the server
# mod_lev_7_server("lev_7_1")
