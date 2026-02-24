#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#'
# Load data
load("data/lev3_vars.rda")
load("data/lev7_vars.rda")
load("data/lev3_shapes_precise.rda")
load("data/globo_topo_poly.rda")
load("data/globo_topo_lines.rda")
load("data/data_socio.rda")
gbf_sf <- sf::st_read(app_sys("data", "gbf7_nations.gpkg"), quiet = TRUE) %>%
  sf::st_transform(4326)

app_server <- function(input, output, session) {

  shinyjs::delay(1, {
    showModal(
      modalDialog(
        title = tags$div(
          style = "text-align: center; margin-bottom: 0;",
          tags$h2("Welcome to the Global Phosphorus Dashboard",
                  style = "font-size: 1.1em; font-weight: bold; margin-bottom: 0.2em;"),
          tags$div("Click outside the box to begin exploring the data",
                   style = "font-size: 0.9em; color: #666; margin-bottom: 0;")
        ),
        div(style = "text-align: center;",
            img(src = "www/logos/diagram.png",
                style = "display:block; margin:0 auto; max-width: 350px; max-height: 400px; width: auto; height: auto;")
        ),
        size = "s",
        easyClose = TRUE,
        footer = NULL
      )
    )
  })

  shinyjs::show("main-app")
  shinyjs::removeClass("main-app", "hidden-app")
  shinyjs::addClass("main-app", "shinyjs-show")

  rv <- reactiveValues()

  mod_basin_atlas_server("basin_atlas_1", rv, session,
                           lev3_vars, lev3_lines, lev3_shapes_precise,
                           globo_topo_poly, globo_topo_lines)
  mod_lev_7_server("lev_7_1", rv, session, lev7_vars, globo_topo_poly)
  mod_about_server("about_1", rv, session)
  mod_footer_server("footer_1", rv, session)
  mod_footer_server("footer_2", rv, session)
  mod_footer_server("footer_3", rv, session)
  mod_tutorial_server("tutorial_1", rv, session)
  mod_social_server("social_1", rv, session, data_socio, gbf_sf)
  mod_data_sources_server("data_sources_1", rv, session)
  mod_privacy_server("privacy_1", rv, session)
  mod_footer_server("footer_4", rv, session)
}
