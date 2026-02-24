#' footer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_footer_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(class = "footer",
        style = "background-color: #f97316; padding: 10px 20px; display: flex; align-items: center; justify-content: flex-end; gap: 15px;",

        # cookie consent button
        tags$button(
          class = "btn btn-outline-light",
          type = "button",
          `data-cc` = "show-preferencesModal",
          style = "font-size: 0.85rem; padding: 6px 12px; border-radius: 4px; border-color: rgba(255,255,255,0.7); color: rgba(255,255,255,0.9);",
          "View cookie preferences"
        ),

        # logos
        img(src = "www/logos/UKCEH-Logo_Short_Positive_RGB.png",
            id = "ukceh", alt = "UKCEH logo",
            style = "height: 30px; vertical-align: middle; margin-left: 10px;"
        ),
        img(src = "https://www.unep.org/themes/custom/UNEP_3Spot/img/full_unep_logo_en.svg",
            id = "unep", alt = "UNEP logo",
            style = "height: 30px; vertical-align: middle; margin-left: 10px;"
        ),
        img(src = "https://www.thegef.org/sites/default/files/2023-04/GEF_logo_main_vertical_RGB_270x203_2023.png",
            id = "gef", alt = "GEF logo",
            style = "height: 30px; vertical-align: middle; margin-left: 10px;"
        ),
        img(src = "https://upload.wikimedia.org/wikipedia/commons/c/cb/MmaChile.png",
            id = "mme", alt = "mme logo",
            style = "height: 30px; vertical-align: middle; margin-left: 10px;"
        ),
        img(src = "www/logos/upcycle-logo white.png",
            id = "mme2", alt = "mme logo 2",
            style = "height: 30px; vertical-align: middle; margin-left: 10px;"
        )
    )
  )
}



#' footer Server Functions
#'
#' @noRd
mod_footer_server <- function(id, rv, x){
  moduleServer( id,  session = x, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_footer_ui("footer_1")

## To be copied in the server
# mod_footer_server("footer_1")
