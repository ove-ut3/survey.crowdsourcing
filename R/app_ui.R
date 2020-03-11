#' @import shiny shinydashboard
app_ui <- function(title) {
  
  ui <- tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    dashboardPage(
      dashboardHeader(title = golem::get_golem_options("title"), disable = TRUE),
      dashboardSidebar(collapsed = TRUE),
      dashboardBody(
        mod_stats_values_ui("stats_values_ui"),
        mod_filters_ui("filters_ui"),
        mod_contacts_table_ui("contacts_table_ui")
      )
    )
  )
  
  ui <- shinymanager::secure_app(ui, language = "fr")
  ui(title)
  
}

#' @import shiny
golem_add_external_resources <- function(){
  
  addResourcePath(
    'www', system.file('app/www', package = 'survey.crowdsourcing')
  )
 
  tags$head(
    golem::activate_js(),
    golem::favicon(),
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}
