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
    uiOutput(ns("ui_filters"), inline = TRUE),
    uiOutput(ns("download"), inline = TRUE)
  )
}
    
# Module Server
    
#' @rdname mod_filters
#' @export
#' @keywords internal
    
mod_filters_server <- function(input, output, session, rv, global, res_auth){
  ns <- session$ns
  
  output$filters <- renderUI({
    
    req(rv$df_crowdsourcing_user)
    
    rv$df_crowdsourcing_filters <- callModule(
      module = shinyWidgets::selectizeGroupServer,
      id = "filters",
      data = rv$df_crowdsourcing_user(),
      vars = global$fields_filter
    )
    
    inputId <- global$fields_filter
    title <- names(global$fields)[which(global$fields %in% global$fields_filter)]
    
    params <- purrr::map2(
      inputId,
      title,
      ~ list(inputId = .x, title = paste0(.y, " :"))
    )
    
    names(params) <- inputId

    shinyWidgets::selectizeGroupUI(
      ns("filters"),
      params = params
    )
    
  })
  
  output$ui_filters <- renderUI({
    
    tagList(
      div(
        style = "display: inline-block; width: 90%;",
        uiOutput(ns("filters"))
      )
    )
    
  })
  
  output$download <- renderUI({
    
    list(
      div(style="display: inline-block; float: right; margin-top: 1.4%;", downloadButton(ns("csv"), "CSV")),
      div(style="display: inline-block; float: right; margin-top: 1.4%;", downloadButton(ns("excel"), "Excel"))
    )
    
  })
  
  output$excel <- downloadHandler(
    
    filename = function() {
      "export.xlsx"
    },
    content = function(con) {
      
      rv$df_crowdsourcing_filters() %>% 
        dplyr::select(global$fields[which(names(global$fields) %in% global$fields_display)]) %>%   
        writexl::write_xlsx(con)
      
    }
    
  )
  
  output$csv <- downloadHandler(
    
    filename = function() {
      "export.csv"
    },
    content = function(con) {
      rv$df_crowdsourcing_filters() %>% 
        dplyr::select(global$fields[which(names(global$fields) %in% global$fields_display)]) %>%   
        utils::write.csv2(con, row.names = FALSE, na = "")
    }
    
  )
  
  rv$filters <- input
  
}
