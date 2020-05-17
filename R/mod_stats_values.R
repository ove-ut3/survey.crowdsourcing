# Module UI
  
#' @title   mod_stats_values_ui and mod_stats_values_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_stats_values
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_stats_values_ui <- function(id){
  ns <- NS(id)
  tagList(
    valueBoxOutput(ns("diplomes"), width = 3),
    valueBoxOutput(ns("diplomes_sans_refus"), width = 3),
    valueBoxOutput(ns("repondants"), width = 3),
    valueBoxOutput(ns("taux_reponse"), width = 3)
  )
}
    
# Module Server
    
#' @rdname mod_stats_values
#' @export
#' @keywords internal
    
mod_stats_values_server <- function(input, output, session, rv){
  ns <- session$ns
  
  df_crowdsourcing_stats <- reactive({
    
    req(rv$df_crowdsourcing_filters)
    
    rv$df_crowdsourcing_filters()
    
  })
  
  output$diplomes <- renderValueBox(
    valueBox(
      nrow(df_crowdsourcing_stats()),
      "Dipl\u00f4m\u00e9s",
      icon = icon("user-graduate")
    )
  )
  
  output$diplomes_sans_refus <- renderValueBox(
    valueBox(
      df_crowdsourcing_stats() %>%
        dplyr::filter(.data$optout == "Non") %>%
        nrow(),
      "\u00c9tudiants sans refus de r\u00e9pondre",
      icon = icon("user-check")
    )
  )
  
  date_jour <- Sys.Date() %>%
    format("%d %B %Y") %>% stringr::str_remove("^0") %>%
    stringr::str_replace("^1 ", "1er ")
  
  output$repondants <- renderValueBox({
    valueBox(
      df_crowdsourcing_stats() %>%
        dplyr::filter(.data$completed == "Oui") %>%
        nrow(),
      glue::glue("R\u00e9pondants au {date_jour}"),
      icon = icon("edit")
    )
  })
  
  output$taux_reponse <- renderValueBox({
    repondants <- df_crowdsourcing_stats() %>%
      dplyr::filter(.data$completed == "Oui")
    valueBox(
      scales::percent(
        nrow(repondants) / nrow(dplyr::filter(df_crowdsourcing_stats(), .data$optout == "Non")),
        decimal.mark = ",",
        suffix = "\U202F%",
        accuracy = .1
      ),
      glue::glue("Taux de r\u00e9ponse au {date_jour}"),
      icon = icon("chart-bar")
    )
  })
  
}
