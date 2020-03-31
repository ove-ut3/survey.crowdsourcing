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
        dplyr::select(user, password, code_diplome) %>% 
        tidyr::nest(code_diplome = code_diplome) %>% 
        dplyr::mutate_at("code_diplome", purrr::map, 1)
    )
  )

  if (! "crowdsourcing_log" %in% impexp::sqlite_list_tables(golem::get_golem_options("sqlite_base"))) {
    
    impexp::sqlite_export(
      golem::get_golem_options("sqlite_base"), 
      dplyr::tibble(
        token = character(0),
        key = character(0),
        new_value = character(0),
        old_value = character(0),
        user = character(0),
        date = character(0),
        status = character(0)
      ),
      "crowdsourcing_log"
    )
  }
  
  global <- list()
  rv <- reactiveValues()
  
  df_crowdsourcing_columns <- impexp::sqlite_import(
    golem::get_golem_options("sqlite_base"),
    "crowdsourcing_columns"
  ) %>% 
    dplyr::mutate_at("description", janitor::make_clean_names) %>% 
    dplyr::filter(as.logical(display)) %>% 
    dplyr::arrange(order)
  
  global$fields <- stats::setNames(df_crowdsourcing_columns$description, df_crowdsourcing_columns$description_new)
  
  global$fields_edit <- df_crowdsourcing_columns %>% 
    dplyr::filter(as.logical(edit)) %>% 
    dplyr::pull(description_new)
  
  global$fields_filter <- df_crowdsourcing_columns %>% 
    dplyr::filter(as.logical(filter)) %>% 
    dplyr::pull(description)
  
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
    dplyr::select(token, completed, optout, lastpage_rate) %>% 
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
      .[which(!. %in% names(crowdsourcing))]
    
    crowdsourcing %>% 
      dplyr::mutate(!!!stats::setNames(rep(NA_character_, length(add_column)), add_column)) %>% 
      dplyr::select(unname(global$fields)) %>% 
      dplyr::arrange_at(c("libelle_diplome", "lastname", "firstname")) %>% 
      dplyr::select(-completed, -lastpage_rate, -optout) %>% 
      dplyr::left_join(cron_reponses, by = "token") %>% 
      dplyr::select(unname(global$fields))
    
  })
  
  rv$df_crowdsourcing_user <- reactive({
    
    df_crowdsourcing_user <- rv$df_crowdsourcing()
    
    rv$user <- res_auth$user
    
    if (any(!is.na(res_auth$code_diplome[[1]]))) {
      
      df_crowdsourcing_user <- df_crowdsourcing_user %>% 
        dplyr::filter(code_diplome %in% res_auth$code_diplome[[1]])
      
    }
    
    df_crowdsourcing_user
    
  })
  
  callModule(mod_filters_server, "filters_ui", rv, global, res_auth)
  
  callModule(mod_stats_values_server, "stats_values_ui", rv)
  
  callModule(mod_contacts_table_server, "contacts_table_ui", rv, global, res_auth)
  
}
