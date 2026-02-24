#' about UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_about_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$head(
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Roboto:wght@400;700&display=swap"),
      tags$style(HTML("
        .content {
          font-family: 'Roboto', sans-serif;
          color: #333;
          line-height: 1.6;
          padding: 40px;
        }
        .content h3 {
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
          height: 150px;
          object-fit: cover;
          display: block;
          padding: 10px;
          margin-top: 20px;
          border-radius: 10px;
        }
        a {
          color: #37a635;
          text-decoration: underline;
        }
      "))
    ),

    fluidRow(
      column(10, offset = 1,
             div(class = "content",
                 h3("uPcycle: Our Mission"),
                 p("Our mission is to widen the scope and accelerate the development of phosphorus and nitrogen emissions reduction programmes globally.
There is international recognition of the need for an Emergency Recovery Plan for freshwater ecosystems, as highlighted at the UN Water Conference 2023.
Unsustainable nutrient management is a key driver of water pollution, biodiversity loss, and disruptions to food security. By fostering collaboration and open data sharing, we aim to ensure
that actionable solutions for sustainable nutrient management are accessible and equitable."),

                 h3("The Nutrient Challenge"),
                 p("Phosphorus and nitrogen are essential components of fertilisers and critical for the global food system. However, the unsustainable management of these nutrients leads to their loss into
water bodies, driving eutrophication, biodiversity loss, and methane emissions, which exacerbate climate change. Addressing these interconnected challenges requires a circular approach to managing
phosphorus and nitrogen, optimising their use and recycling them to mitigate environmental impacts. Fragmented data on nutrient emissions and their socio-economic and environmental impacts has
hindered progress, and our dashboard seeks to address these gaps."),
                 p("This dashboard provides an open-access platform harmonising phosphorus and nitrogen data spanning agriculture, aquaculture, wastewater, and other sectors. It incorporates
data on nutrient management, socio-economic factors, and biodiversity risk across over 40,000 lake catchments worldwide. By offering hindcasted emission data (1970-2015) and future scenarios (2015–2070)
aligned with shared socio-economic pathways, this tool supports evidence-based decision-making and targeted efforts to meet global sustainability targets like the UN SDGs and the Kunming-Montreal Global
Biodiversity Framework goals. Through integrated governance and informed action, we seek to address nutrient hotspots, mitigate risks, and advance sustainable development."),

                 h3("About this Dashboard"),
                 p("This dashboard is part of the ",
                   tags$a(href = "https://www.upcyclelakes.org/", target = "_blank", "uPcycle project"),
                   ", and provides access to global data on nutrient use, socio-economic indicators, and environmental impacts related to phosphorus and nitrogen.
We have collected and harmonised open-access datasets covering various temporal and spatial resolutions and made them available for interactive exploration at the scale of hydrological basins, countries,
and individual lake catchments.",
                   style = "color:gray; text-align:justify;"),
                 p("The goal is to provide stakeholders with a simple, transparent interface to better understand global nutrient flows and their implications for sustainability targets like the UN SDGs.
Users can explore data visually, filter by region or variable, and download information for further analysis.",
                   style = "color:gray; text-align:justify;"),

                 tags$img(src = "www/logos/andreas-gucklhorn-mawU2PoJWfU-unsplash.jpg",
                          style = "width: 100%; height: 400px; object-fit: cover; display: block; margin-top: 20px; border-radius: 10px;"),

                 h3("Contact us"),
                 p("upcycle@ceh.ac.uk", style = "color:gray; text-align:justify;"),

             )
      )
    )
  )
}

#' about Server Functions
#'
#' @noRd
mod_about_server <- function(id, rv, x){
  moduleServer(id,  session = x, function(input, output, session){
    ns <- session$ns



  })
}

## To be copied in the UI
# mod_about_ui("about_1")

## To be copied in the server
# mod_about_server("about_1")
