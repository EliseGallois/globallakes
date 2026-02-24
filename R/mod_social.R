#' social UI/Server - interactive choropleth adn factfile
#'
#'
#' @noRd
mod_social_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Roboto:wght@400;700&display=swap"),
      tags$style(HTML("\
        .content { font-family: 'Roboto', sans-serif; color: #333; line-height: 1.6; text-align: center; }\
        .content h3 { color: #37a635; margin-top: 20px; }\
        .masonry-container { column-count: 2; column-gap: 30px; }\
        @media (max-width: 768px) { .masonry-container { column-count: 1; } }\
        .masonry-card { display: inline-block; width: 100%; animation: fadeSlideUp 0.6s ease; }\
        @keyframes fadeSlideUp { from { opacity: 0; transform: translateY(15px); } to { opacity: 1; transform: translateY(0); } }\
      "))
    ),

    fluidRow(
      column(10, offset = 1,
             div(class = "content", style = "margin-top: 20px; width: 100%; max-width: 1000px; margin-left: auto; margin-right: auto;",
                 h3("Global Phosphate Trade"),
                 p("Click a highlighted country on the map or use the bar charts below. Data reported in 2022 and collated by Chatham House. This data represents total yearly weight export and import of phosphatic fertilisers. View detailed data and trade flows at ",
                   tags$a(href = "https://resourcetrade.earth/?category=122&units=value&autozoom=1", "Resource Trade Earth", target = "_blank"), "."),

                 # map controls
                 div(style = "text-align:center; margin-bottom: 10px;",
                     radioButtons(ns("metric"), label = NULL,
                                  choices = c("Import" = "import", "Export" = "export"),
                                  selected = "import", inline = TRUE)
                 ),

                 leaflet::leafletOutput(ns("trade_map"), width = "100%", height = "400px"),

                 hr(),

                 # bar charts
                 fluidRow(
                   column(6, h4("Top 10 Importers"), plotly::plotlyOutput(ns("import_bar"), height = "300px")),
                   column(6, h4("Top 10 Exporters"), plotly::plotlyOutput(ns("export_bar"), height = "300px"))
                 ),

                 hr(),

                 # country selector
                 div(style = "background: #f4f4f4; padding: 20px; border-radius: 10px; margin-bottom: 20px;",
                     selectInput(ns("country_select"), "Select Country Factfile:", choices = NULL, width = "100%")
                 ),

                 uiOutput(ns("factfile")),

                 hr(),

                 # download data
                 div(style = "margin-top: 20px; padding-bottom: 40px;",
                     downloadButton(ns("downloadData"), "Download Socioeconomic Data (CSV)"),
                     downloadButton(ns("downloadMetadata"), "Download Metadata (DOCX)")
                 )
             )
      )
    )
  )
}

mod_social_server <- function(id, rv, x, data_socio, gbf_sf) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # efficiency averages
    get_efficiency_avg <- function(data, keyword) {
      cols <- grep(keyword, colnames(data), value = TRUE)
      if (length(cols) == 0) return(rep(NA, nrow(data)))
      mat <- as.matrix(
        sapply(
          data[, cols, drop = FALSE],
          function(c) suppressWarnings(as.numeric(as.character(c)))
        )
      )
      rowMeans(mat, na.rm = TRUE)
    }

    data_socio$P_eff_avg <- get_efficiency_avg(data_socio, "P_efficiency")
    data_socio$N_eff_avg <- get_efficiency_avg(data_socio, "N_efficiency")
    data_socio$P_export_weight_2022 <- as.numeric(data_socio$gbf7_P_export_weight_1000kg_2022)
    data_socio$P_import_weight_2022 <- as.numeric(data_socio$gbf7_P_import_weight_1000kg_2022)

    fact_vars <- c(
      "gbf7_P_export_weight_1000kg_2022",
      "gbf7_P_import_weight_1000kg_2022",
      "gbf7_RLI_2024",
      "gbf7_mean_deprivation",
      "P_eff_avg",
      "N_eff_avg"
    )

    countries_with_data <- data_socio %>%
      sf::st_drop_geometry() %>%
      dplyr::filter(dplyr::if_any(dplyr::any_of(fact_vars), ~ !is.na(.) & . != 0)) %>%
      dplyr::pull(nam_en) %>%
      unique() %>%
      sort()

    map_sf <- gbf_sf %>%
      dplyr::left_join(
        dplyr::distinct(sf::st_drop_geometry(data_socio), nam_en, .keep_all = TRUE),
        by = "nam_en"
      )

    observe({
      updateSelectInput(
        session,
        "country_select",
        choices = countries_with_data,
        selected = if ("Argentina" %in% countries_with_data)
          "Argentina" else countries_with_data[1]
      )
    })

    metric_col <- reactive({
      if (is.null(input$metric) || input$metric == "export")
        "P_export_weight_2022"
      else
        "P_import_weight_2022"
    })

    pal <- reactive({
      vals <- as.numeric(map_sf[[metric_col()]])
      clean_vals <- vals[is.finite(vals) & vals > 0]
      if (length(clean_vals) == 0) clean_vals <- c(0, 1)
      leaflet::colorNumeric(
        viridis::viridis(256),
        domain = range(clean_vals, na.rm = TRUE),
        na.color = "#EBEBEB"
      )
    })

    output$trade_map <- leaflet::renderLeaflet({
      p <- isolate(pal())
      m <- isolate(metric_col())

      leaflet::leaflet() %>%
        leaflet::addProviderTiles("CartoDB.Positron") %>%
        leaflet::setView(lng = 10, lat = 20, zoom = 2) %>%
        leaflet::addPolygons(
          data = map_sf,
          layerId = ~nam_en,
          fillColor = ~p(get(m)),
          fillOpacity = 0.7,
          weight = 0.6,
          color = "#fff",
          highlightOptions = leaflet::highlightOptions(
            weight = 2,
            color = "#000",
            bringToFront = TRUE
          ),
          popup = ~paste0(
            "<b>", nam_en, "</b><br/>",
            "Import: ",
            formatC(P_import_weight_2022, format = "f", big.mark = ",", digits = 0),
            " kt<br/>",
            "Export: ",
            formatC(P_export_weight_2022, format = "f", big.mark = ",", digits = 0),
            " kt"
          )
        )
    })

    observe({
      p <- pal()
      m <- metric_col()
      vals <- as.numeric(map_sf[[m]])

      leaflet::leafletProxy("trade_map", data = map_sf) %>%
        leaflet::clearShapes() %>%
        leaflet::addPolygons(
          layerId = ~nam_en,
          fillColor = ~p(vals),
          fillOpacity = 0.7,
          weight = 0.6,
          color = "#fff",
          highlightOptions = leaflet::highlightOptions(
            weight = 2,
            color = "#000",
            bringToFront = TRUE
          ),
          popup = ~paste0(
            "<b>", nam_en, "</b><br/>",
            "Import: ",
            formatC(P_import_weight_2022, format = "f", big.mark = ",", digits = 0),
            " kt<br/>",
            "Export: ",
            formatC(P_export_weight_2022, format = "f", big.mark = ",", digits = 0),
            " kt"
          )
        ) %>%
        leaflet::clearControls() %>%
        leaflet::addLegend(
          "bottomright",
          pal = p,
          values = vals,
          title = ifelse(m == "P_import_weight_2022", "Imports (kt)", "Exports (kt)"),
          na.label = "No Data",
          labFormat = leaflet::labelFormat(big.mark = ",")
        )
    })

    output$import_bar <- plotly::renderPlotly({
      df <- sf::st_drop_geometry(map_sf) %>%
        dplyr::filter(!is.na(P_import_weight_2022)) %>%
        dplyr::arrange(desc(P_import_weight_2022)) %>%
        head(10)

      plotly::plot_ly(
        df,
        x = ~P_import_weight_2022,
        y = ~reorder(nam_en, P_import_weight_2022),
        type = "bar",
        orientation = "h",
        marker = list(color = "#75A278"),
        source = "import_bar"
      ) %>%
        plotly::layout(
          yaxis = list(title = ""),
          xaxis = list(title = ""),
          margin = list(l = 120)
        )
    })

    output$export_bar <- plotly::renderPlotly({
      df <- sf::st_drop_geometry(map_sf) %>%
        dplyr::filter(!is.na(P_export_weight_2022)) %>%
        dplyr::arrange(desc(P_export_weight_2022)) %>%
        head(10)

      plotly::plot_ly(
        df,
        x = ~P_export_weight_2022,
        y = ~reorder(nam_en, P_export_weight_2022),
        type = "bar",
        orientation = "h",
        marker = list(color = "#f19509"),
        source = "export_bar"
      ) %>%
        plotly::layout(
          yaxis = list(title = ""),
          xaxis = list(title = ""),
          margin = list(l = 120)
        )
    })

    observeEvent(
      input$trade_map_shape_click,
      updateSelectInput(session, "country_select",
                        selected = input$trade_map_shape_click$id)
    )

    observe({
      ev1 <- plotly::event_data("plotly_click", source = "import_bar")
      if (!is.null(ev1))
        updateSelectInput(session, "country_select", selected = ev1$y)

      ev2 <- plotly::event_data("plotly_click", source = "export_bar")
      if (!is.null(ev2))
        updateSelectInput(session, "country_select", selected = ev2$y)
    })

    output$factfile <- renderUI({
      sel <- input$country_select
      req(sel)

      selected_data <- data_socio[data_socio$nam_en == sel, ]
      if (nrow(selected_data) == 0) return(NULL)

      fact_labels <- c(
        "Phosphate Export (kt)",
        "Phosphate Import (kt)",
        "Red List Index",
        "Mean Deprivation (%)",
        "P Efficiency (%)",
        "N Efficiency (%)"
      )

      fact_icons <- c(
        "fa-solid fa-arrow-up-right-from-square",
        "fa-solid fa-arrow-down",
        "fa-solid fa-leaf",
        "fa-solid fa-user-injured",
        "fa-solid fa-flask",
        "fa-solid fa-flask-vial"
      )

      fact_descriptions <- c(
        "Phosphate fertilisers exported in 2022 (kt).",
        "Phosphate fertilisers imported in 2022 (kt).",
        "Biodiversity status (0-1, higher is better).",
        "Index of Deprivation (0–100%).",
        "Cropland Phosphorus use efficiency: percentage of P applied as fertiliser that is taken up by crops.",
        "Cropland Nitrogen use efficiency: percentage of N applied as fertiliser that is taken up by crops."
      )
      fact_urls <- rep("https://resourcetrade.earth", 6)

      norm_data <- data_socio %>%
        sf::st_drop_geometry() %>%
        dplyr::select(nam_en, dplyr::all_of(fact_vars)) %>%
        dplyr::mutate(across(dplyr::all_of(fact_vars),
                             ~ as.numeric(as.character(unlist(.)))))

      scaled_info <- purrr::map(fact_vars, function(var) {
        v <- norm_data[[var]][!is.na(norm_data[[var]]) & norm_data[[var]] != 0]
        ctry <- norm_data$nam_en[!is.na(norm_data[[var]]) & norm_data[[var]] != 0]
        if (length(v) < 2) return(NULL)
        list(
          scale = scales::rescale(v, to = c(0, 100)),
          countries = ctry,
          min_val = min(v),
          max_val = max(v),
          min_country = ctry[which.min(v)],
          max_country = ctry[which.max(v)]
        )
      })
      names(scaled_info) <- fact_vars

      cards <- purrr::pmap(
        list(fact_vars, fact_icons, fact_labels, fact_descriptions, fact_urls),
        function(var, icon, label, desc, url) {

          info <- scaled_info[[var]]
          if (is.null(info)) return(NULL)

          val <- as.numeric(unlist(selected_data[[var]]))
          if (is.na(val) || val == 0) return(NULL)

          scale_val <- info$scale[which(info$countries == sel)[1]]
          is_efficiency <- var %in% c("P_eff_avg", "N_eff_avg")

          div(
            class = "masonry-card",
            div(
              style = "background-color: #f9f9f9; padding: 20px;
                 border-radius: 15px;
                 box-shadow: 2px 2px 10px rgba(0,0,0,0.06);
                 margin-bottom: 20px;",

              tags$i(class = icon,
                     style = "margin-right: 10px; font-size: 1.6em; color: #007bff;"),
              tags$a(href = url, target = "_blank",
                     strong(label),
                     style = "text-decoration: none; color: inherit;"),

              # PROGRESS BAR WITH ARROW (only for non-efficiency)
              if (!is_efficiency) {
                div(
                  style = "position: relative; height: 16px; background: white;
                     border: 1px solid #c8e6c9; border-radius: 10px;
                     margin-top: 25px; margin-bottom: 10px;",
                  div(style = paste0(
                    "position: absolute; left: 0; width: ", scale_val,
                    "%; height: 100%;
               background: linear-gradient(to right, #2e7d32, #66bb6a);
               border-radius: 10px;"
                  )),
                  tags$div(
                    style = paste0(
                      "position: absolute; left: ", scale_val,
                      "%; transform: translateX(-50%); top: -25px;"
                    ),
                    tags$i(class = "fa-solid fa-arrow-down",
                           style = "font-size: 1.8em; color: #388e3c;
                              text-shadow: 1px 1px #ccc;")
                  )
                )
              },

              span(style = "font-size: 0.95em; font-weight: bold;",
                   paste("Value:", formatC(val, format = "f", digits = 2, big.mark = ","))),

              br(),
              span(style = "font-size: 0.85em; color: #777;", desc),

              # Efficiency disclaimer
              if (is_efficiency) {
                tagList(
                  p(
                    style = "margin-top: 4px; font-size: 0.85em;
                       color: #888; font-style: italic;",
                    HTML(
                      "Nutrient use efficiency data are self-reported by countries and
                 sourced from <a href='https://www.fao.org/faostat/en/#data/ESB'
                 target='_blank'>FAOSTAT</a>, not UKCEH."
                    )
                  )
                )
              },

              # Min/Max with proper number formatting (only for non-efficiency)
              if (!is_efficiency) {
                div(
                  style = "display: flex; justify-content: space-between;
                     font-size: 0.8em; color: #666; margin-top: 8px;
                     border-top: 1px solid #eee; padding-top: 4px;",
                  span(paste0("Min: ", info$min_country, " (",
                              formatC(info$min_val, format = "f", digits = 2, big.mark = ","), ")")),
                  span(paste0("Max: ", info$max_country, " (",
                              formatC(info$max_val, format = "f", digits = 2, big.mark = ","), ")"))
                )
              }
            )
          )
        }
      )

      tagList(div(class = "masonry-container", cards))
    })

    output$downloadData <- downloadHandler(
      filename = function() {
        paste0("phosphate_socio_data_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(sf::st_drop_geometry(map_sf), file, row.names = FALSE)
      }
    )

    output$downloadMetadata <- downloadHandler(
      filename = function() {
        "metadata_socioeconomic.docx"
      },
      content = function(file) {
        file.copy("data/metadata_socioeconomic.docx", file)
      }
    )
  })
}

