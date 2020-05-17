#' @import shiny
app_server <- function(input, output, session) {
  # List the first level callModules here
  
  res_auth <- shinymanager::secure_server(
    check_credentials = shinymanager::check_credentials(
      impexp::sqlite_import(
        golem::get_golem_options("sqlite_base"),
        "crowdsourcing_contributors"
      ) %>% 
        janitor::clean_names() %>% 
        tidyr::nest(restriction = -c(.data$user, .data$password))
    )
  )

  global <- list()
  rv <- reactiveValues()
  
  df_crowdsourcing_columns <- impexp::sqlite_import(
    golem::get_golem_options("sqlite_base"),
    "crowdsourcing_columns"
  ) %>% 
    dplyr::mutate_at("description", janitor::make_clean_names) %>% 
    dplyr::filter(
      description %in% c("token", "optout", "completed") |
      as.logical(.data$display) | 
      as.logical(.data$edit) |
      as.logical(.data$filter) |
      as.logical(.data$restriction)
    ) %>% 
    dplyr::arrange(.data$order)
  
  global$fields <- stats::setNames(df_crowdsourcing_columns$description, df_crowdsourcing_columns$description_new)
  
  global$fields_display <- df_crowdsourcing_columns %>% 
    dplyr::filter(as.logical(.data$display)) %>% 
    dplyr::pull(.data$description_new)
  
  global$fields_edit <- df_crowdsourcing_columns %>% 
    dplyr::filter(as.logical(.data$edit)) %>% 
    dplyr::pull(.data$description_new)
  
  global$fields_filter <- df_crowdsourcing_columns %>% 
    dplyr::filter(as.logical(.data$filter)) %>% 
    dplyr::pull(.data$description)
  
  global$fields_restriction <- df_crowdsourcing_columns %>% 
    dplyr::filter(as.logical(.data$restriction)) %>% 
    dplyr::pull(.data$description)
  
  rv$df_participants <- impexp::sqlite_import(
    golem::get_golem_options("sqlite_base"),
    "participants"
  ) %>% 
    janitor::clean_names()
  
  rv$df_participants_contacts <- impexp::sqlite_import(
    golem::get_golem_options("sqlite_base"),
    "participants_contacts"
  )
  
  cron_reponses <- golem::get_golem_options("cron_responses") %>% 
    impexp::r_import() %>% 
    dplyr::select(.data$token, .data$completed, .data$optout, .data$lastpage_rate) %>% 
    dplyr::mutate_if(is.logical, as.character) %>% 
    dplyr::mutate_at(c("completed", "optout"), dplyr::recode, "TRUE" = "Oui", "FALSE" = "Non") %>% 
    dplyr::mutate_at("lastpage_rate", scales::percent, decimal.mark = ",", suffix = "\U202F%", accuracy = .1)
  
  rv$df_crowdsourcing <- reactive({
    
    crowdsourcing <- rv$df_participants %>% 
      dplyr::left_join(
        rv$df_participants_contacts %>% 
          survey.admin::df_participants_contacts_crowdsourcing(),
        by = "token")
    
    add_column <- unname(global$fields) %>% 
      .[which(!. %in% c(names(crowdsourcing), names(cron_reponses)))]
    
    crowdsourcing %>% 
      dplyr::mutate(!!!stats::setNames(rep(NA_character_, length(add_column)), add_column)) %>% 
      dplyr::arrange_at(c("lastname", "firstname")) %>% 
      dplyr::left_join(cron_reponses, by = "token") %>% 
      dplyr::select(unname(global$fields))
    
  })
  
  rv$df_crowdsourcing_user <- reactive({
    
    df_crowdsourcing_user <- rv$df_crowdsourcing()
    
    rv$user <- res_auth$user
    
    if (nrow(res_auth$restriction[[1]]) >= 1) {
      
      df_crowdsourcing_user <- df_crowdsourcing_user %>% 
        dplyr::semi_join(
          res_auth$restriction[[1]],
          by = global$fields_restriction
        )
        
    }

    df_crowdsourcing_user
    
  })
  
  callModule(mod_filters_server, "filters_ui", rv, global, res_auth)
  
  callModule(mod_stats_values_server, "stats_values_ui", rv)
  
  callModule(mod_contacts_table_server, "contacts_table_ui", rv, global, res_auth)
  
}
