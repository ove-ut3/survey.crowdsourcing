# Module UI
  
#' @title   mod_contacts_table_ui and mod_contacts_table_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_contacts_table
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_contacts_table_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("download"), inline = TRUE),
    rhandsontable::rHandsontableOutput(ns("hot_base_participants"))
  )
}
    
# Module Server
    
#' @rdname mod_contacts_table
#' @export
#' @keywords internal

mod_contacts_table_server <- function(input, output, session, rv, global, res_auth){
  ns <- session$ns
  
  rv$df_crowdsourcing_hot <- reactive({
    
    req(rv$df_crowdsourcing_filter_status)
    
    data <- rv$df_crowdsourcing_filter_status()
    
    if (!is.null(rv$df_crowdsourcing_filter_formation)) {
      
      data <- data %>% 
        dplyr::semi_join(
          rv$df_crowdsourcing_filter_formation(),
          by = "token"
        )
      
    }
    
    data
    
  })
  
  output$download <- renderUI({
    
    list(
      div(style="display: inline-block; float: right;", downloadButton(ns("csv"), "CSV")),
      div(style="display: inline-block; float: right;", downloadButton(ns("excel"), "Excel"))
    )
    
  })
  
  output$hot_base_participants <- rhandsontable::renderRHandsontable({
    
    req(rv$df_crowdsourcing_filter_status)
    
    data <- rv$df_crowdsourcing_hot() %>% 
      dplyr::select(global$fields)
    
    data %>%
      rhandsontable::rhandsontable(rowHeaders = NULL, height = 680, allowRemoveRow = FALSE, allowInsertRow = FALSE) %>%
      rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") %>%
      rhandsontable::hot_rows(rowHeights = 35) %>%
      rhandsontable::hot_cols(valign = "htMiddle") %>%
      rhandsontable::hot_col(col = names(data)[which(!names(data) %in% global$fields_edit)], readOnly = TRUE)

  })
  
  observeEvent(input$hot_base_participants, ignoreInit = TRUE, {
    
    req(input$hot_base_participants)
    
    changes <- input$hot_base_participants$changes
    
    # update
    req(changes[["changes"]])
    
    select <- names(global$fields)
    names(select) <- unname(global$fields)

    update <- input$hot_base_participants %>%
      rhandsontable::hot_to_r() %>%
      dplyr::select(select)

    key <- names(update)[changes$changes[[1]][[2]] + 1]
    old_value <- changes$changes[[1]][[3]]
    new_value <- changes$changes[[1]][[4]]
    token <- update$token[changes$changes[[1]][[1]] + 1]
    
    impexp::sqlite_execute_sql(
      golem::get_golem_options("sqlite_base"),
      glue::glue("UPDATE crowdsourcing SET {key} = \"{new_value}\" WHERE token = \"{token}\";")
    )
    
    rv$df_crowdsourcing <- impexp::sqlite_import(
      golem::get_golem_options("sqlite_base"),
      "crowdsourcing"
    )
    
    impexp::sqlite_append_rows(
      golem::get_golem_options("sqlite_base"),
      data = dplyr::tibble(
        token = token,
        key = key,
        new_value = new_value,
        old_value = old_value,
        user = rv$user,
        date = as.character(lubridate::today()),
        status = character(0)
      ),
      "crowdsourcing_log"
    )
    
  })
  
  output$excel <- downloadHandler(
    
    filename = function() {
      "export.xlsx"
    },
    content = function(con) {
      dplyr::select(rv$df_crowdsourcing_hot(), global$fields) %>% 
        writexl::write_xlsx(con)
    }
    
  )
  
  output$csv <- downloadHandler(
    
    filename = function() {
      "export.csv"
    },
    content = function(con) {
      dplyr::select(rv$df_crowdsourcing_hot(), global$fields) %>% 
        write.csv2(con, row.names = FALSE, na = "")
    }
    
  )
  
}
