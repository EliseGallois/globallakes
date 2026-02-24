#' about UI Function - NOT CURRENTLY IN USE
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_case_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$head(
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Roboto:wght@400;700&display=swap"),
      tags$style(HTML("
        /* Base styling for a clean, readable layout */
        .content {
          font-family: 'Roboto', sans-serif;
          color: #333;
          line-height: 1.8; /* Increased line-height for readability */
          padding: 30px; /* More padding for a spacious feel */
          font-size: 16px; /* Larger base font size */
        }
        .content h3, .content h5 {
          color: #37a635;
          margin-top: 35px;
          margin-bottom: 20px; /* More space below h5 headers */
          text-align: left;
          font-weight: 700;
          text-shadow: 1px 1px 2px rgba(0,0,0,0.1); /* Subtle text shadow for depth */
        }
        .content h3 {
          font-size: 3.2em;
        }
        .content h5 {
          font-size: 1.5em;
        }
        .content p {
          text-align: left !important; /* Forces left justification */
          margin-bottom: 15px;
        }
        .content p.gray {
          color: #666; /* Softer gray */
        }
        .content ul {
          list-style-type: disc;
          padding-left: 20px;
          margin-bottom: 15px;
          text-align: left !important; /* Justifies list items */
        }
        .content ul li {
          margin-bottom: 5px;
        }
        /* Specific image styling for case study content */
        .case-study-img {
          max-width: 60%; /* Adjusted for single column */
          height: auto;
          display: block;
          border: 2px solid #ccc; /* Softer border */
          padding: 0;
          margin: 25px auto; /* More space around images */
          border-radius: 10px; /* More rounded corners */
          box-shadow: 0 4px 8px rgba(0,0,0,0.1); /* Adds a nice shadow */
          transition: transform 0.3s ease-in-out; /* Smooth hover effect */
        }
        .case-study-img:hover {
          transform: scale(1.02);
        }
        .img-wrapper {
          text-align: center;
        }
        a {
          color: #37a635;
          text-decoration: underline;
        }
        .code-container {
          position: relative;
          padding: 20px; /* More padding */
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
        .copy-btn:hover {
          background-color: #ddd;
        }
        .code-buttons {
          display: flex;
          gap: 15px; /* More space between buttons */
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
        h3("How to use the dashboard for your own lakes"),
        p("This guide will walk you through the steps to use the dashboard effectively to explore and analyze data for a lake and its catchment. In this case, we use the example of Lake Villarica in the Province of Cautín in Chile. This guide will show you how to interact with the Map Explorer, find statistics for your lake catchment of choice, download that data, and conduct your own analyses.", style = "color:gray; text-align:justify;"),

        # step 1
        h5("Step 1: Explore the Map Explorer view"),
        tags$ul(
          tags$li("The dashboard opens to a global map view, showing nutrient emission and landscape variables for larger global catchments (i.e. HydroBASIN Level 3 data)."),
          tags$li("Use the ", tags$strong("variable drop-down menu"), " on the left side of the window to toggle between different variables."),
          tags$li("Use the ", tags$strong("zoom and pan controls"), " on the map to find the region you are interested in.")
        ),
        div(class = "img-wrapper", tags$img(src = "www/logos/case_1.png", class = "case-study-img")),

        # step 2
        h5("Step 2: Navigate to your basin"),
        tags$ul(
          tags$li("Once you have zoomed into your area of interest, ", tags$strong("click once into the region of choice"), " on the map."),
          tags$li("This will take you to a more detailed view of the lakes over 250 hectares in size within that basin.")
        ),
        div(class = "img-wrapper", tags$img(src = "www/logos/case_2.png", class = "case-study-img")),

        # step 3
        h5("Step 3: Navigate to your lake"),
        tags$ul(
          tags$li("Within the basin view, you can see individual lakes. If you hover over a lake, its outline will turn yellow."),
          tags$li("Click on the specific lake you want to explore. This action will load a new pop-up window with detailed information and statistics for that lake and its catchment.")
        ),
        div(class = "img-wrapper", tags$img(src = "www/logos/case_3.png", class = "case-study-img")),

        # step 4
        h5("Step 4: View lake catchment statistics"),
        tags$ul(
          tags$li("The lake-specific pop-up window displays various statistics and graphs related to the lake catchment, such as ", tags$strong("population density, land-use type, and national borders"), " spanned by the catchment."),
          tags$li("There are interactive plots showing past and future ", tags$strong("phosphorus and nitrogen emission projections"), " for your catchment according to different climate and socio-economic scenarios."),
          tags$li("Take a moment to review these visualizations to understand the current and projected conditions.")
        ),
        div(class = "img-wrapper", tags$img(src = "www/logos/case_4.png", class = "case-study-img")),

        # step 5
        h5("Step 5: Download data for your lake catchment"),
        tags$ul(
          tags$li("To perform your own analysis, you can download the raw data for the selected lake catchment by clicking the ", tags$strong("Download Data' button.")),
          tags$li("This will provide you with a ", tags$strong("CSV file"), " containing all the underlying data shown in the plots."),
          tags$li("To access data descriptions, data sources, and licensing information, you can also download a metadata word document from the ", tags$strong("Download Metadata' button"), " or alternatively browse the ", tags$strong("Data Availablity' tab"), " within the dashboard.")
        ),
        div(class = "img-wrapper", tags$img(src = "www/logos/case_5.png", class = "case-study-img")),

        # step 6 with collapsible R snippet
        h5("Step 6: Analyse your downloaded data"),
        p("Use your favorite statistical software (like R, Python, Excel) to analyze the downloaded data. Here is a code example in R to get you started on plotting the agricultural phosphorus runoff.", style = "color:gray; text-align:justify;"),

        # buttons to toggle the code blocks
        div(class = "code-buttons",
            actionButton(ns("toggle_r_code"), "Show Example R Code"),
            actionButton(ns("toggle_python_code"), "Show Example Python Code")
        ),

        # R collapsible conditional panel
        conditionalPanel(
          condition = "input.toggle_r_code % 2 == 1",
          ns = ns,
          div(class = "code-container",
              tags$button("Copy", class = "copy-btn", onclick = "copyCode('code-r')"),
              tags$pre(id = "code-r",
                       '# 1 - import downloaded data and load libraries
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
    legend.text = element_text(size = 10)))'
              )
          )
        ),

        # python collapsible conditional panel
        conditionalPanel(
          condition = "input.toggle_python_code % 2 == 1",
          ns = ns,
          div(class = "code-container",
              tags$button("Copy", class = "copy-btn", onclick = "copyCode('code-python')"),
              tags$pre(id = "code-python",
                       '# 1 - import downloaded data and load libraries
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
plt.show()'
              )
          )
        ),

    h5("Step 7: Combine with ground-truthed data for your lake"),
    p("For a more complete analysis, you can combine the downloaded data with local, ground-truthed data you may have collected. This will help validate the model projections and provide a more accurate picture of your lake's health.", style = "color:gray; text-align:justify;"),
    ),

  # JavaScript for the copy button
  tags$script(HTML("
      function copyCode(elementId) {
        var code = document.getElementById(elementId).textContent;
        navigator.clipboard.writeText(code).then(function() {
          console.log('Code successfully copied to clipboard');
          alert('Code copied to clipboard!');
        }, function(err) {
          console.error('Could not copy text: ', err);
        });
      }
    "))
  )
}

#' about Server Functions
#'
#' @noRd
mod_case_server <- function(id, rv, x){
  moduleServer(id, session = x, function(input, output, session){
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_case_ui("case_1")

## To be copied in the server
# mod_case_server("case_1")
