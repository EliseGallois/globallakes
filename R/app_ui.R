#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  UKCEH_theme <- bslib::bs_theme(
    bg = "#fff",
    fg = "#274538",
    primary = "#B4A0E5",
    success = "#e3fdeb",
    secondary = "#9de9ac",
    info = "#5E2BFF",
    warning = "#E28413",
    base_font = bslib::font_collection("Yummo", "Calibri")
  )

  message("[ui] building UI at: ", Sys.time())

  UKCEH_theme <- bslib::bs_add_variables(UKCEH_theme,
                                         "headings-font-weight" = 2000)

  tagList(
    golem_add_external_resources(),

    tags$head(
      tags$style(HTML("
        /* Hide the main app initially */
        .hidden-app { display: none; opacity: 0; transition: opacity 0.3s ease-in-out; }
        .shinyjs-show { display: block !important; opacity: 1 !important; }
      "))
    ),

    fluidPage(
      theme = UKCEH_theme,
      includeCSS("inst/app/www/custom.css"),
      shinyjs::useShinyjs(),

      # login panel
      #shinyauthr::loginUI("login"),

      # main app hidden until login
      #div(
       # id = "main-app",
        #class = "hidden-app",
        #navbarPage(
         # id = "mypage",
          #title = NULL,
          #tabPanel("Map Explorer", icon = icon("map", lib = "font-awesome"),
           #        mod_basin_atlas_ui("basin_atlas_1"),
            #       mod_footer_ui("footer_1")
          #),
      navbarPage(
        id = "mypage",
        title = NULL,
        tabPanel("Map Explorer", icon = icon("map", lib = "font-awesome"),
                 mod_basin_atlas_ui("basin_atlas_1"),
                 mod_footer_ui("footer_1")
        ),
        tabPanel("About", icon = icon("circle-info", lib = "font-awesome"),
                 mod_about_ui("about_1"),
                 mod_footer_ui("footer_2")
        ),
        tabPanel("How to use", icon = icon("book", lib = "font-awesome"),
                 mod_tutorial_ui("tutorial_1"),
                 mod_footer_ui("footer_5")
        ),
        tabPanel("Socio-economic data", icon = icon("people-group", lib = "font-awesome"),
                 mod_social_ui("social_1"),
                 mod_footer_ui("footer_6")
        ),
        tabPanel("Data Sources", icon = icon("database", lib = "font-awesome"),
                 mod_data_sources_ui("data_sources_1"),
                 mod_footer_ui("footer_4")
        ),
        tabPanel("Privacy Notice and Terms of Use", icon = icon("shield-halved", lib = "font-awesome"),
                 mod_privacy_ui("privacy_1"),
                 mod_footer_ui("footer_3")
        )
      )
      )
    )

}


# External Resources
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd

golem_add_external_resources <- function() {
  golem::add_resource_path("www", app_sys("app/www"))

  tags$head(
    golem::favicon(ext = "png"),
    golem::bundle_resources(
      path = app_sys("app/www"),
      app_title = "Global Lakes Explorer"
    ),

    tags$head(
      tags$style(HTML("
    /* Hide the main app initially */
    .hidden-app { display: none; opacity: 0; transition: opacity 0.3s ease-in-out; }
    .shinyjs-show { display: block !important; opacity: 1 !important; }
  ")),

      tags$script(HTML("
    $(document).ready(function() {
      // Autofocus username field when page loads
      setTimeout(function() {
        $('#login-user_name').focus();
      }, 100);

      // Ensure Enter key works in password field
      $('#login-password').keypress(function(e) {
        if(e.which == 13) {
          $('#login-login_button').click();
        }
      });
    });
  "))
    ),

   # cookies
    tags$link(
      rel = "stylesheet",
      href = "ANONYMISED"
    ),
    tags$script(
      src = "ANONYMISED"
    ),

    # google analytics
    tags$script(
      type = "text/plain",
      `data-category` = "analytics",
      `data-service` = "Google Analytics 4",
      src = "ANONYMISED",
      async = NA
    ),

    tags$script(
      type = "text/plain",
      `data-category` = "analytics",
      `data-service` = "Google Analytics 4",
      HTML("
        window.dataLayer = window.dataLayer || [];
        function gtag(){dataLayer.push(arguments);}
        gtag('js', new Date());
        gtag('config', 'G-ANONYMISED');
      ")
    ),

    # cookie config
    tags$script(src = "www/cookieconsent-config.js")
  )
}
