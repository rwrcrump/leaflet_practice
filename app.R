library(tidyverse)
library(leaflet)
library(sf)
library(htmlwidgets)
library(shiny)

all_modzcta <- read_rds("all_modzcta.RDS")

ui <- fluidPage(

    titlePanel("Covid-19 NYC Trends by Modified ZCTA"),

    sidebarLayout(
        sidebarPanel(
            tags$a(href="https://github.com/nychealth/coronavirus-data", "Data Repository", target="_blank"),
            h5("Data Sourced from the NYC Department of Health"),
            selectInput("date",
                        "Select a date (week ending in):",
                        choices = unique(all_modzcta$week_ending))
        ),

        mainPanel(
            tabsetPanel(
                tabPanel("Case Rate", leafletOutput("cases")),
                tabPanel("Test Rate", leafletOutput("tests")),
                tabPanel("Percent Positive", leafletOutput("pctpos"))
            )
        )
    )
)

server <- function(input, output) {

    week_zcta <- reactive({
        w <- all_modzcta %>% filter(week_ending == input$date)
        return(w)
    })
    
    output$cases <- renderLeaflet({
        pal <- colorBin(palette = "YlGn", 9, domain = all_modzcta$caserate)
        
        labels <- sprintf(
            "<strong>%s</strong><br/>%g cases per 100,000 people",
            week_zcta()$MODZCTA, week_zcta()$caserate) %>% 
            lapply(htmltools::HTML)
        
        week_zcta() %>% 
            st_transform(crs = "+init=epsg:4326") %>% 
            leaflet() %>% 
            addProviderTiles(provider = "CartoDB.Positron") %>%
            setView(-73.9, 40.7, zoom = 10) %>% 
            addPolygons(label = labels,
                        stroke = FALSE,
                        smoothFactor = .5,
                        opacity = 1,
                        fillOpacity = 0.7,
                        fillColor = ~pal(week_zcta()$caserate),
                        highlightOptions = highlightOptions(weight = 5,
                                                            fillOpacity = 1,
                                                            color = "black",
                                                            opacity = 1,
                                                            bringToFront = TRUE)) %>% 
            addLegend("bottomright",
                      pal = pal,
                      values = ~caserate,
                      title = "Cases per 100,000 people",
                      opacity = 0.7)
    })
    
    output$tests <- renderLeaflet({
        pal <- colorBin(palette = "PuBu", 9, domain = all_modzcta$testrate)
        
        labels <- sprintf(
            "<strong>%s</strong><br/>%g tests per 100,000 people",
            week_zcta()$MODZCTA, week_zcta()$testrate) %>% 
            lapply(htmltools::HTML)
        
        week_zcta() %>% 
            st_transform(crs = "+init=epsg:4326") %>% 
            leaflet() %>% 
            addProviderTiles(provider = "CartoDB.Positron") %>%
            setView(-73.9, 40.7, zoom = 10) %>% 
            addPolygons(label = labels,
                        stroke = FALSE,
                        smoothFactor = .5,
                        opacity = 1,
                        fillOpacity = 0.7,
                        fillColor = ~pal(week_zcta()$testrate),
                        highlightOptions = highlightOptions(weight = 5,
                                                            fillOpacity = 1,
                                                            color = "black",
                                                            opacity = 1,
                                                            bringToFront = TRUE)) %>% 
            addLegend("bottomright",
                      pal = pal,
                      values = ~testrate,
                      title = "Tests per 100,000 people",
                      opacity = 0.7)
    })
    
    output$pctpos <- renderLeaflet({
        pal <- colorBin(palette = "OrRd", 9, domain = all_modzcta$pctpos)
        
        labels <- sprintf(
            "<strong>%s</strong><br/>%g positive per 100,000 people",
            week_zcta()$MODZCTA, week_zcta()$pctpos) %>% 
            lapply(htmltools::HTML)
        
        week_zcta() %>% 
            st_transform(crs = "+init=epsg:4326") %>% 
            leaflet() %>% 
            addProviderTiles(provider = "CartoDB.Positron") %>%
            setView(-73.9, 40.7, zoom = 10) %>% 
            addPolygons(label = labels,
                        stroke = FALSE,
                        smoothFactor = .5,
                        opacity = 1,
                        fillOpacity = 0.7,
                        fillColor = ~pal(week_zcta()$pctpos),
                        highlightOptions = highlightOptions(weight = 5,
                                                            fillOpacity = 1,
                                                            color = "black",
                                                            opacity = 1,
                                                            bringToFront = TRUE)) %>% 
            addLegend("bottomright",
                      pal = pal,
                      values = ~pctpos,
                      title = "% Positive per 100,000 people",
                      opacity = 0.7)
    })
}

shinyApp(ui = ui, server = server)
