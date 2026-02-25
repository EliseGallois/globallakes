Phosphorus and nitrogen are critical plant nutrients, essential for fertiliser production and global food security. However, poor management across the anthropogenic nutrient cycles leads to losses associated with pollution of water bodies, driving eutrophication, biodiversity loss, and methane emissions. Addressing these interconnected challenges requires a sustainable, integral, and where possible, circular approach across the entire nutrient cycle. However, data on phosphorus and nitrogen emissions and impacts are fragmented across sectors and lack standardisation. Here, we introduce a new open-access web-based application: the ‘Global Lakes Explorer’, which harmonises phosphorus and nitrogen emissions data alongside socio-economic and biodiversity risk factors spanning over 50 variables from multiple open data sources, covering more than 40,000 lake catchments worldwide. It enables users to visualise and download information on nutrient emissions across sectors, including agriculture, aquaculture, and wastewater, alongside socio-economic and environmental indicators. The app supports integrated governance on sustainable nutrient management in lake catchments by providing hindcasted emission data (1970-2015) in addition to future scenarios relating to shared socio-economic pathways (2015–2070) under various development projections.

## User manual

### uPcycle: Our Mission

Our mission is to widen the scope and accelerate the development of phosphorus and nitrogen emissions reduction programmes globally.
There is international recognition of the need for an Emergency Recovery Plan for freshwater ecosystems, as highlighted at the UN Water Conference 2023.
Unsustainable nutrient management is a key driver of water pollution, biodiversity loss, and disruptions to food security. By fostering collaboration and open data sharing, we aim to ensure
that actionable solutions for sustainable nutrient management are accessible and equitable.

### The Nutrient Challenge

Phosphorus and nitrogen are essential components of fertilisers and critical for the global food system. However, the unsustainable management of these nutrients leads to their loss into
water bodies, driving eutrophication, biodiversity loss, and methane emissions, which exacerbate climate change. Addressing these interconnected challenges requires a circular approach to managing
phosphorus and nitrogen, optimising their use and recycling them to mitigate environmental impacts. Fragmented data on nutrient emissions and their socio-economic and environmental impacts has
hindered progress, and our dashboard seeks to address these gaps.

This dashboard provides an open-access platform harmonising phosphorus and nitrogen data spanning agriculture, aquaculture, wastewater, and other sectors. It incorporates
data on nutrient management, socio-economic factors, and biodiversity risk across over 40,000 lake catchments worldwide. By offering hindcasted emission data (1970-2015) and future scenarios (2015–2070)
aligned with shared socio-economic pathways, this tool supports evidence-based decision-making and targeted efforts to meet global sustainability targets like the UN SDGs and the Kunming-Montreal Global
Biodiversity Framework goals. Through integrated governance and informed action, we seek to address nutrient hotspots, mitigate risks, and advance sustainable development.

### About this Dashboard

This dashboard is part of the [uPcycle project](https://www.upcyclelakes.org/), and provides access to global data on nutrient use, socio-economic indicators, and environmental impacts related to phosphorus and nitrogen.
We have collected and harmonised open-access datasets covering various temporal and spatial resolutions and made them available for interactive exploration at the scale of hydrological basins, countries,
and individual lake catchments.

The goal is to provide stakeholders with a simple, transparent interface to better understand global nutrient flows and their implications for sustainability targets like the UN SDGs.
Users can explore data visually, filter by region or variable, and download information for further analysis.

### Contact us

upcycle@ceh.ac.uk

## Repository navigation

Top-level directories and key files:

- `R/`: application code (UI, server, modules)
- `inst/`: installed app assets/resources (incl. `inst/app/www/`)
- `data/`: packaged data files used by the app
- `data-raw/`: data preparation notes/scripts
- `renv/`: renv bootstrap and settings
- `man/`: documentation (if present)

## Required packages

Core runtime dependencies (from your dependency list):

- bsicons
- bslib
- config (>= 0.3.2)
- dplyr
- fmsb
- ggplot2
- golem (>= 0.4.1)
- grDevices
- htmlwidgets
- leafgl
- leaflet
- lorem
- magrittr
- sf
- shiny (>= 1.8.1.1)
- shinyjqui
- shinyjs
- shinyWidgets
- tidyr
- viridis

Other packages used in the codebase (from your dependency list):

- tidygeocoder
- viridisLite
- stringr
- plotly
- purrr
- scales

Development / deployment helpers (from your dependency list):

- attachment
- covrpage
- desc
- devtools
- httpuv
- pkgload
- rhub
- rsconnect
- rstudioapi
- renv
- testthat
- usethis