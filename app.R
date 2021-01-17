library(shiny)
library(DT)
library(leaflet)
library(shinythemes)
library(shiny.semantic)
library(semantic.dashboard)
library(tidyverse)
library(geodist)
library(htmltools)


# loading longest distance fonction and dropdown module

dir("modules", full.names = T) %>% walk(source)

#read the data with parallell processing
ships <- data.table::fread("data/ships.csv")

# importing ship icons

shipIcon <- iconList(
    start = makeIcon("ship_start_position.png", iconWidth = 25, iconHeight = 50),
    end = makeIcon("ship_end_position.png", iconWidth = 30, iconHeight = 60)
)

#ui
ui <- semanticPage(
    #segment that holding the ui page
    segment(class = "placeholder",
    #image and title of the app
        h1(img(src='shiny.png', width = "10%", height = "7%", align = "center"),
        "Ships Dashboard"),
    #selection menu at the rigth
    absolutePanel(
        draggable = FALSE,
        top = 50,right = 50, width = "30%", heigth = "10%",
            select_ship_module("select_ship")),
    #Pannel for ship selected info
    segment(
        a(class = "ui grey ribbon label", "Ship longest distance sailed"),
        uiOutput(
       "selected_ship_layout")
       ),
    segment(
        a(class = "ui blue ribbon label", "Ship selected info"),
        uiOutput("selected_ship_card")
    ),
    # The leaflet map,
   segment(
        a(class = "ui blue ribbon label", "The map"),
        leafletOutput("ship_path_map")
        )
        )
    )


#server

server <- shinyServer(function(input, output) {

    #Render relevant information from the ship selected

    selected_ship_info <- callModule(module = select_ship, id = "select_ship", data = ships)

    # Render map

    output$ship_path_map <- renderLeaflet({
        leaflet(data = selected_ship_info()) %>%
            addMarkers(lng = ~LON, lat = ~LAT,
                       icon = ~shipIcon[position],
                       popup = ~paste0(strong("Position: "), position, br(),
                                       strong("Latitude: "), LAT, "<br>",
                                       strong("Longitude: "), LON, "<br>",
                                       strong("Time of observation: "), DATETIME)) %>%
            addPolylines(lng = ~LON, lat = ~LAT,
                         weight = 2, dashArray = "2",
                         label = paste0("Distance: ", selected_ship_info()$dist_since_last_obs[1] , " m"),
                         labelOptions = labelOptions(permanent = TRUE)) %>%
            addControl(html = "<img src='ship_start_position.png' style='width:20px;height:40px;'> Start position<br/>
                               <img src='ship_end_position.png' style='width:20px;height:40px;'> End position",
                       position = "bottomleft") %>%
            addControl(html = "Click on the ship to get ship's statistic",
                       position = "topleft")%>%
            addTiles()


    })

    # Render Ship Card
    output$selected_ship_card <- renderUI({

       sidebar_layout(
           sidebar_panel(),
           main_panel(
           cards(
            class = "three",

                card(
                    class = "grey label",
                    div(class="content", h3("Ship ID:",strong(selected_ship_info()$SHIP_ID[1]))),
                    div(class="content", h3("Ship Flag:", selected_ship_info()$FLAG[1])),

                ),
                card(
                    class = "grey label",
                    div(class="content", h3("Ship Width: ", selected_ship_info()$WIDTH[1]," m")),
                    div(class="content", h3("Ship Deadweight Tonnage: ",selected_ship_info()$DWT[1]," tonne"))
                    ),

                card(
                    class = "grey label",
                    div(class="content", h3("Ship Length: ",selected_ship_info()$LENGTH[1]," m")),
                    div(class="content", h3("Ship Average Speed: ",round(selected_ship_info()$average_speed[1],2)," kn")),
                    div(class="content", h3("Ship Total Traveled Distance: ",selected_ship_info()$total_traveled_distance[1]," m"))
                 )
                )
             )
       )

})

    #render Ship layout longest distance
    output$selected_ship_layout <- renderUI({
        sidebar_layout(
            sidebar_panel(),
            main_panel(
                        cards(
                            class = "two",
                             card(
                                 class = "blue label",
                                 div(class =  "content",
                                     value_box(value = selected_ship_info()$dist_since_last_obs[1],
                                                      subtitle = "meters."))
                             ),
                            card(class = "blue label",
                                 div(class = "content", h1("The longest distance sailed between two consecutive observations is between", selected_ship_info()$DATETIME[2],
                                                           " and ",
                                                           selected_ship_info()$DATETIME[1], ". "))
                            ),
                            )
                        )
        )
    })

})

shinyApp(ui, server)
