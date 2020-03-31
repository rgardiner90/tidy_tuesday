library(shiny)
library(tidyverse)
library(readxl)
library(janitor)
library(maps)
library(wesanderson)
library(scales)
library(mapproj)

fifty_states <- read_excel("First-time Exam Takers and Repeaters in 2017.xlsx") %>%
  clean_names() %>%
  filter(jurisdiction != "District of Columbia") %>%
  mutate(jurisdiction = ifelse(jurisdiction == "Delaware *", "Delaware", jurisdiction),
         administration = ifelse(administration == "Total February and July", "Both", administration),
         first_time_takers_repeaters = ifelse(first_time_takers_repeaters == "First-Time Takers", "First",
                                              "Repeat"),
         jurisdiction = str_to_lower(jurisdiction))

us_states <- map_data("state") %>%
  mutate(region = str_to_lower(region))

pal <- wes_palette("Zissou1", 100, type = "continuous")
##########
# Things to Fix

## 1. Fixing color
## 2. Making color scale be the same
## 3. Double check the code to see if the map is showing what we expect
##########





ui <- fluidPage(
  
  
  titlePanel("Bar Passage Rate in 2017"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons(inputId = "taker", label = "Times Taken?",
                   choices = c("First", "Repeat", "Both"),
                   selected = "Both"),
      radioButtons(inputId = "administration", label = "Which Exam Administration?",
                   choices = c("February", "July", "Both"),
                   selected = "Both")
    ),
    mainPanel(plotOutput("map"),
              plotOutput("histogram"))
  )
)









server <- function(input, output, session) {
  output$map <- renderPlot (
    if (input$taker == "Both") {
      
      fifty_states <- fifty_states %>%
        filter(administration == input$administration) %>%
        mutate(taking = as.numeric(taking),
               passing = as.numeric(passing)) %>%
        group_by(jurisdiction) %>%
        summarise(taking = sum(taking),
                  passing = sum(passing),
                  percent_passing = passing / taking)
      
      us_states %>%
        left_join(fifty_states, by = c("region" = "jurisdiction")) %>%
        select(-subregion, -taking, - passing) %>%
        mutate(percent_passing = as.numeric(percent_passing)) %>%
        ggplot(aes(x= long, y = lat, fill = percent_passing, 
                   group = group)) +
        geom_polygon() +
        coord_map(projection = "albers", lat0 = 39, lat1 = 4) +
        theme_void() +
        scale_fill_gradientn(colours = pal, limits = c(0.1,0.9), labels = percent_format(accuracy = 1)) +
        labs(fill = "Passage Rate")
      
      
      
    } else {
      
      
      fifty_states <- fifty_states %>%
        filter(first_time_takers_repeaters == input$taker,
               administration == input$administration)
      
      us_states %>%
        left_join(fifty_states, by = c("region" = "jurisdiction")) %>%
        select(-subregion, -taking, -passing) %>%
        mutate(percent_passing = as.numeric(percent_passing)) %>%
        ggplot(aes(x= long, y = lat, fill = percent_passing, 
                   group = group)) +
        geom_polygon() +
        coord_map(projection = "albers", lat0 = 39, lat1 = 4) +
        theme_void() +
        scale_fill_gradientn(colours = pal, limits = c(0.1,0.9), labels = percent_format(accuracy = 1)) +
        labs(fill = "Passage Rate")
    })
  
  
  output$histogram <- renderPlot (
    
    if (input$taker == "Both") {
      
      fifty_states <- fifty_states %>%
        filter(administration == input$administration) %>%
        mutate(taking = as.numeric(taking),
               passing = as.numeric(passing)) %>%
        group_by(jurisdiction) %>%
        summarise(taking = sum(taking),
                  passing = sum(passing),
                  percent_passing = passing / taking) %>%
        mutate(means = mean(percent_passing, na.rm = TRUE))
      
      mean <- round(fifty_states[1, 5] * 100, 2)
      
      ggplot(fifty_states, aes(x = percent_passing)) +
        geom_histogram(fill = "#002b49") +
        theme_classic() +
        labs(x = "Passage Rate", y = "Count",
             title = glue::glue("Mean: ", "{mean}"),
             caption = "Source: https://thebarexaminer.org/statistics/2017-statistics/first-time-exam-takers-and-repeaters-in-2017/") +
        scale_x_continuous(labels = percent_format(accuracy = 1),
                           limits = c(0.1, 0.9)) +
        scale_y_continuous(limits = c(0, 10))
      
    } else {
      
      fifty_states <- fifty_states %>%
        filter(first_time_takers_repeaters == input$taker,
               administration == input$administration) %>%
        mutate(percent_passing = as.numeric(percent_passing, na.rm = TRUE)) %>%
        mutate(means = mean(percent_passing, na.rm = TRUE)) 
      
      mean <- round(fifty_states[1, 6] * 100, 2)
      
      ggplot(fifty_states, aes(x = percent_passing)) +
        geom_histogram(fill = "#002b49") +
        theme_classic() +
        labs(x = "Passage Rate", y = "Count",
             title = glue::glue("Mean: ", "{mean}"),
             caption = "Source: https://thebarexaminer.org/statistics/2017-statistics/first-time-exam-takers-and-repeaters-in-2017/") +
        scale_x_continuous(labels = percent_format(accuracy = 1),
                           limits = c(0.1, 0.9)) +
        scale_y_continuous(limits = c(0, 10))
    }
    
  )
}






shinyApp(ui, server)

