# select ship module ui
select_ship_module <- function(id) {
  ns <- NS(id)
  
      cards(
        class = "two",
        width = 3,
        card(
          style = 'text-align:center; background-color:white; color:#2185d0;',
        h3("Select the ship's type:"),
        dropdown_input(input_id = ns("select_ship_type"), choices = NULL)
            ),
      br(),
        card(
          style = 'text-align:center; background-color:white; color:#2185d0;',
        h3("Select the ship's name:"),
        dropdown_input(input_id = ns("select_ship_name"), choices = NULL)
          )
        )
  
}

# select ship module server logic
select_ship <- function(input, output, session, data) {
  
  
      h3(update_dropdown_input(session, "select_ship_type", choices = unique(data$ship_type)))
  
  observe({
    req(input$select_ship_type)
    
    selected_type_data <- data %>% filter(ship_type == input$select_ship_type)
    
    h3(update_dropdown_input(session, "select_ship_name", choices = unique(selected_type_data$SHIPNAME)))
  })
  
  selected_ship_info <- reactive({
    req(input$select_ship_name)
    
    get_longest_dist_obs(data, input$select_ship_name)
  })
  
  return(selected_ship_info)
}
