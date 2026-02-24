#' about UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tutorial_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Roboto:wght@400;700&display=swap"),
      tags$style(HTML("
        /* Base styling */
        .content {
          font-family: 'Roboto', sans-serif;
          color: #333;
          line-height: 1.8;
          padding: 60px 30px 30px 30px;
          font-size: 16px;
        }
        .content h3, .content h5 {
          color: #37a635;
          margin-top: 35px;
          margin-bottom: 20px;
          text-align: left;
          font-weight: 700;
          text-shadow: 1px 1px 2px rgba(0,0,0,0.1);
        }
        .content h3 { font-size: 3.2em; }
        .content h5 { font-size: 1.5em; }
        .content p { text-align: left !important; margin-bottom: 15px; }
        .content p.gray { color: #666; }
        .content ul {
          list-style-type: disc;
          padding-left: 20px;
          margin-bottom: 15px;
          text-align: left !important;
        }
        .content ul li { margin-bottom: 5px; }
        .case-study-img {
          max-width: 60%;
          height: auto;
          display: block;
          border: 2px solid #ccc;
          padding: 0;
          margin: 25px auto;
          border-radius: 10px;
          box-shadow: 0 4px 8px rgba(0,0,0,0.1);
          transition: transform 0.3s ease-in-out;
        }
        .case-study-img:hover { transform: scale(1.02); }
        .img-wrapper { text-align: center; }
        a { color: #37a635; text-decoration: underline; }
        .code-container {
          position: relative;
          padding: 20px;
          background-color: #f8f8f8;
          border: 1px solid #ccc;
          border-radius: 10px;
          margin-top: 20px;
          overflow-x: auto;
          box-shadow: 0 2px 4px rgba(0,0,0,0.05);
        }
        .copy-btn {
          position: absolute;
          top: 10px;
          right: 10px;
          background-color: #eee;
          border: 1px solid #ccc;
          padding: 5px 10px;
          border-radius: 5px;
          cursor: pointer;
          transition: background-color 0.2s ease;
        }
        .copy-btn:hover { background-color: #ddd; }
        .code-buttons {
          display: flex;
          gap: 15px;
          margin-top: 20px;
        }
        .code-buttons button {
          background-color: #37a635;
          color: white;
          border: none;
          padding: 12px 20px;
          border-radius: 5px;
          cursor: pointer;
          font-weight: bold;
          transition: background-color 0.3s ease, transform 0.2s ease;
        }
        .code-buttons button:hover {
          background-color: #2e8b2c;
          transform: translateY(-2px);
        }
      "))
    ),

    div(class = "content",

        h3("How to use the Global Lakes Explorer for your own lakes"),
        p("This guide will walk you through the steps to use the Global Lakes Explorer effectively to explore and analyze data for a lake and its catchment. In this case, we use the example of Lake Villarica in Chile.", style = "color:gray; text-align:justify;"),

        h5("Step 1: Explore the Map Explorer view"),
        tags$ul(
          tags$li("The Global Lakes Explorer opens to a global map view showing nutrient emission and landscape variables."),
          tags$li("Use the ", tags$strong("variable drop-down menu"), " to toggle between variables."),
          tags$li("Use the ", tags$strong("zoom and pan controls"), " to find your region.")
        ),
        div(class = "img-wrapper", tags$img(src = "www/logos/case_1.png", class = "case-study-img")),

        h5("Step 2: Navigate to your basin"),
        tags$ul(
          tags$li("Click once on the basin of choice on the map."),
          tags$li("This loads lakes within that basin.")
        ),
        div(class = "img-wrapper", tags$img(src = "www/logos/case_2.png", class = "case-study-img")),

        h5("Step 3: Navigate to your lake"),
        tags$ul(
          tags$li("Hovering over a lake outlines it in yellow."),
          tags$li("Click to open detailed statistics for that lake and catchment.")
        ),
        div(class = "img-wrapper", tags$img(src = "www/logos/case_3.png", class = "case-study-img")),

        h5("Step 4: View lake catchment statistics"),
        tags$ul(
          tags$li("The pop-up displays various statistics like population density and land use."),
          tags$li("It also shows phosphorus and nitrogen emission projections under SSP scenarios.")
        ),
        div(class = "img-wrapper", tags$img(src = "www/logos/case_4.png", class = "case-study-img")),

        h5("Step 5: Download data for your lake catchment"),
        tags$ul(
          tags$li("Download the raw data via the ", tags$strong("'Download Data' button.")),
          tags$li("You get a ", tags$strong("CSV file"), " with all data shown in plots."),
          tags$li("Metadata and data sources are in the ", tags$strong("'Data Availability' tab."))
        ),
        div(class = "img-wrapper", tags$img(src = "www/logos/case_5.png", class = "case-study-img")),

        h5("Step 6: Analyse your downloaded data"),
        p("Use R, Python, or Excel to analyse downloaded data. Here are a couple of code examples in R and Python:", style = "color:gray; text-align:justify;"),

        div(class = "code-buttons",
            actionButton(ns("toggle_r_code"), "Show Example R Code"),
            actionButton(ns("toggle_python_code"), "Show Example Python Code")
        ),

        ## --- R CODE ---
        conditionalPanel(
          condition = "input.toggle_r_code % 2 == 1",
          ns = ns,
          div(class = "code-container",
              tags$button("Copy", class = "copy-btn", onclick = "copyCode('code-r')"),
              tags$pre(id = "code-r", '# 1 - import downloaded data and load libraries
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(viridis)

<villarica_data <- read_csv("your_filepath_")>

  # 2 - clean data
  villarica_data_long <- villarica_data %>%
  pivot_longer(cols = starts_with("SSP"),
               names_to = c("SSP", "Variable", "Year"),
               names_pattern = "(SSP\\d+)_(.*)_(\\d{4})",
               values_to = "Value")

# filter for only strings including Phosphorus_Rivers_Psurface_runoff_agri
villarica_data_long <- villarica_data_long %>%
  filter(Variable == "Phosphorus_Rivers_Psurface_runoff_agri")

# remove Variable col
villarica_data_long <- villarica_data_long %>%
  select(-Variable)

# spread by SSP
villarica_data_wide <- villarica_data_long %>%
  pivot_wider(names_from = SSP, values_from = Value)

# make Year col numeric
villarica_data_wide$Year <- as.numeric(villarica_data_wide$Year)

# make Value cols numeric
villarica_data_wide$SSP1 <- as.numeric(villarica_data_wide$SSP1)
villarica_data_wide$SSP2 <- as.numeric(villarica_data_wide$SSP2)
villarica_data_wide$SSP3 <- as.numeric(villarica_data_wide$SSP3)
villarica_data_wide$SSP4 <- as.numeric(villarica_data_wide$SSP4)
villarica_data_wide$SSP5 <- as.numeric(villarica_data_wide$SSP5)

# 3 - plot data
(agriculture_plot <- ggplot(villarica_data_wide, aes(x = Year)) +
    geom_line(aes(y = SSP1, color = "SSP1"), size = 1) +
    geom_line(aes(y = SSP2, color = "SSP2"), size = 1) +
    geom_line(aes(y = SSP3, color = "SSP3"), size = 1) +
    geom_line(aes(y = SSP4, color = "SSP4"), size = 1) +
    geom_line(aes(y = SSP5, color = "SSP5"), size = 1) +
    scale_color_viridis(discrete = TRUE, option = "D") +
    labs(title = "Phosphorus Runoff from Agriculture to Rivers in Villarrica Catchment",
         subtitle = "Projected under Different Socioeconomic Pathways (SSPs)",
         x = "Year",
         y = "Phosphorus Runoff (kg P/km2/year)",
         color = "Scenario") +
    theme_minimal() +
    theme(plot.title = element_text(size = 16, face = "bold"),
          plot.subtitle = element_text(size = 12),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10)))')
          )
        ),

        ## --- PYTHON CODE ---
        conditionalPanel(
          condition = "input.toggle_python_code % 2 == 1",
          ns = ns,
          div(class = "code-container",
              tags$button("Copy", class = "copy-btn", onclick = "copyCode('code-python')"),
              tags$pre(id = "code-python", '# 1 - import downloaded data and load libraries
                       import pandas as pd
                       import matplotlib.pyplot as plt
                       import seaborn as sns
                       import re

                       # Load the data
                       <villarica_data = pd.read_csv("your_filepath_here")>

                         # 2 - clean data
                         villarica_data_long = villarica_data.melt(
                           id_vars=[col for col in villarica_data.columns if not re.match(r"SSP.*", col)],
                           value_vars=[col for col in villarica_data.columns if re.match(r"SSP.*", col)],
                           var_name="Variable_Year",
                           value_name="Value"
                         )

                       # Split the "Variable_Year" column into "SSP", "Variable", and "Year"
                       villarica_data_long[["SSP", "Variable", "Year"]] = villarica_data_long["Variable_Year"].str.extract(r"(SSP\\d+)_(.*)_(\\d{4})")
                       villarica_data_long.drop(columns=["Variable_Year"], inplace=True)

                       # Filter for the specific variable
                       villarica_data_filtered = villarica_data_long[villarica_data_long["Variable"] == "Phosphorus_Rivers_Psurface_runoff_agri"].copy()

                       # Remove the now-unnecessary "Variable" column
                       villarica_data_filtered.drop(columns=["Variable"], inplace=True)

                       # Convert dtypes
                       villarica_data_filtered["Year"] = pd.to_numeric(villarica_data_filtered["Year"])
                       villarica_data_filtered["Value"] = pd.to_numeric(villarica_data_filtered["Value"])

                       # Pivot wider to get SSPs as columns
                       villarica_data_wide = villarica_data_filtered.pivot(index="Year", columns="SSP", values="Value").reset_index()

                       # 3 - plot data
                       plt.figure(figsize=(10, 6))
                       sns.set_style("whitegrid")
                       sns.lineplot(data=villarica_data_wide, x="Year", y="SSP1", label="SSP1", linewidth=2)
                       sns.lineplot(data=villarica_data_wide, x="Year", y="SSP2", label="SSP2", linewidth=2)
                       sns.lineplot(data=villarica_data_wide, x="Year", y="SSP3", label="SSP3", linewidth=2)
                       sns.lineplot(data=villarica_data_wide, x="Year", y="SSP4", label="SSP4", linewidth=2)
                       sns.lineplot(data=villarica_data_wide, x="Year", y="SSP5", label="SSP5", linewidth=2)

                       plt.title("Phosphorus Runoff from Agriculture to Rivers in Villarrica Catchment", fontsize=16, fontweight="bold")
                       plt.suptitle("Projected under Different Socioeconomic Pathways (SSPs)", fontsize=12)
                       plt.xlabel("Year", fontsize=14)
                       plt.ylabel("Phosphorus Runoff (kg P/km2/year)", fontsize=14)
                       plt.legend(title="Scenario")

                       plt.tight_layout()
                       plt.show()')
          )
        ),

        h5("Step 7: Combine with ground-truthed data for your lake"),
        p("Combine the downloaded data with local measurements to validate model projections and gain a better understanding of your lake’s health.", style = "color:gray; text-align:justify;"),

        tags$hr(style = "margin:40px 0; border-top:2px solid #37a635;"),

        h3("Tutorial Video: Exploring the Global Lakes Explorer", style = "text-align:center; font-size:24px; font-weight:bold; color:green;"),
        div(
          tags$iframe(
            width = "100%",
            height = "900px",
            src = "https://www.youtube.com/embed/AOvDzx2miSk?si=VnNzNK92zWVt7-54",
            frameborder = "0",
            style = "border-radius:10px; box-shadow: 0px 4px 10px rgba(0,0,0,0.2);",
            allow = "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture",
            allowfullscreen = NA
          ),
          style = "display:flex; justify-content:center; align-items:center; width:100%;"
        ),

        div(
          class = "transcript",
          style = "margin-top:20px; padding:15px; background-color:#f9f9f9; border-radius:10px; box-shadow:0 2px 5px rgba(0,0,0,0.1);",
          h4("Transcript:", style = "font-weight:bold;"),
          p("Welcome to the Global Lakes Explorer! This tool allows you to explore and download valuable data related to nutrient use, land use, and environmental variables for over 40,000 lake catchments worldwide."),
          p("To get started, select a variable from the drop-down menu. This will display the data on the map. You can also view the projected data for a specific year or for a shared socioeconomic pathway."),
          p("Click anywhere on the map to zoom in a specific HydroBasin. This will display the Lakes within. Once zoomed in click on the lake to generate a statistics card. This card will show you the visualizations of data about the lake and its catchment including relevant nutrient statistics. Download a CSV file for your selected lake catchment."),
          p("Visit the socioeconomic tab to view and download relevant socioeconomic data at a national resolution. To view the data sources behind the global lakes explorer, head to the data sources tab."),
          p("You're now ready to download lake catchment data and conduct your own analyses."),
          p("Thank you for watching, and happy exploring!")
        )
    ),

    tags$script(HTML("
      function copyCode(elementId) {
        var code = document.getElementById(elementId).textContent;
        navigator.clipboard.writeText(code).then(function() {
          alert('Code copied to clipboard!');
        });
      }
    "))
  )
}

# Server Function
mod_tutorial_server <- function(id, rv, x){
  moduleServer(id, session = x, function(input, output, session){
    ns <- session$ns
  })
}
## To be copied in the UI
# mod_about_ui("about_1")

## To be copied in the server
# mod_about_server("about_1")
