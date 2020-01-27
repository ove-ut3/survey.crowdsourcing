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
    new_value <- changes$changes[[1]][[4]]
      
    old_value <- changes$changes[[1]][[3]]
    if (is.null(old_value)) old_value <- NA_character_
    token <- update$token[changes$changes[[1]][[1]] + 1]
    
    status <- dplyr::if_else(
      stringr::str_detect(key, "_invalid"),
      "invalid",
      "valid"
    )
    key <- stringr::str_remove(key, "_invalid")
    
    add_participants_contacts <- dplyr::tibble(
      token = token,
      key = key,
      value = new_value,
      source = "crowdsourcing",
      date = as.character(lubridate::today()),
      service = NA_character_,
      status = status,
      status_date = NA_character_
    )
    
    impexp::sqlite_execute_sql(
      golem::get_golem_options("sqlite_base"),
      glue::glue("DELETE FROM participants_contacts WHERE token = \"{token}\" AND key = \"{key}\" AND status = \"{status}\";")
    )
    
    impexp::sqlite_append_rows(
      golem::get_golem_options("sqlite_base"),
      add_participants_contacts,
      "participants_contacts"
    )
    
    rv$df_participants_contacts <- impexp::sqlite_import(
      golem::get_golem_options("sqlite_base"),
      "participants_contacts"
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
        status = status
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
