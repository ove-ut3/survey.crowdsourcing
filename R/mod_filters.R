# Module UI
  
#' @title   mod_filters_ui and mod_filters_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_filters
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_filters_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("ui_filters"))
  )
}
    
# Module Server
    
#' @rdname mod_filters
#' @export
#' @keywords internal
    
mod_filters_server <- function(input, output, session, rv, res_auth){
  ns <- session$ns
  
  output$filter_formation <- renderUI({
    
    req(rv$df_crowdsourcing_user)
    
    rv$df_crowdsourcing_filter_formation <- callModule(
      module = shinyWidgets::selectizeGroupServer,
      id = "filter_formation",
      data = rv$df_crowdsourcing_user(),
      vars = "libelle_diplome"
    )
    
    shinyWidgets::selectizeGroupUI(
      ns("filter_formation"),
      params = list(
        libelle_diplome = 
          list(inputId = "libelle_diplome", title = "Formation :")
      )
    )
    
  })
  
  output$filter_status <- renderUI({
    
    req(rv$df_crowdsourcing_user)
    
    rv$df_crowdsourcing_filter_status <- callModule(
      module = shinyWidgets::selectizeGroupServer,
      id = "filter_status",
      data = rv$df_crowdsourcing_user(),
      vars = c("completed", "optout")
    )
    
    shinyWidgets::selectizeGroupUI(
      ns("filter_status"),
      params = list(
        completed = list(inputId = "completed", title = "Complété :"),
        optout = list(inputId = "optout", title = "Refus réponse :")
      )
    )
    
  })
  
  output$ui_filters <- renderUI({
    
    if (all(is.na(res_auth$code_diplome[[1]])) | length(res_auth$code_diplome[[1]]) >= 2) {
      
      tagList(
        div(
          style = "display: inline-block; width: 33%; vertical-align: top;",
          uiOutput(ns("filter_formation"))
        ),
        div(
          style = "display: inline-block; width: 66%; vertical-align: top;",
          uiOutput(ns("filter_status"))
        )
      )
      
    } else {
      
      tagList(
        div(
          style = "display: inline-block;",
          uiOutput(ns("filter_status"))
        )
      )
      
    }
    
  })
  
  rv$filters <- input
  
}
