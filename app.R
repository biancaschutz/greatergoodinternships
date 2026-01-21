
library(shiny)
library(ggplot2)
library(bslib)
library(tidyverse)
library(DT)
library(lubridate)
source("text2.R")

jobs <- read.csv("jobs.csv", check.names = FALSE) %>% mutate(Deadline = mdy(Deadline), 
                                                             `Date Posted` = mdy(`Date Posted`))

jobs[jobs == ""] <- NA


ui <- page_fluid(theme = bs_theme(bootswatch = "journal"), h2("Policy and Social Science Research Internship Database"),
  navset_pill( 
    nav_panel("Internship Database", layout_columns(card(uiOutput("locations")), 
                                                    card(uiOutput("focus")),
                                                    card(selectInput("edumin", 
                                                               label = "Minimum Education", choices = c("Undergraduate", "Junior", "Senior", "Graduate/Law"), 
                                                               selected = "Undergraduate",
                                                               width = "100%"),
                                                         selectInput("edumax", 
                                                                     label = "Maximum Education", choices = c("Undergraduate", "Junior", "Senior", "Graduate/Law"), 
                                                                     selected = "Graduate/Law",
                                                                     width = "100%"))),
      DT::dataTableOutput(outputId = "t1")), 
    nav_panel("Other Resources", textbox, resources)
  ), 
  id = "tab" 
)

server <- function(input, output, session) {
  
  print(jobs)
  all_locations <- reactive({
    loc <- jobs %>%
      filter(is.na(Deadline) | Deadline > Sys.time()) %>%
               pull(Location)
    loc[!is.na(loc)]
  })
  
  all_areas <- reactive({
    loc <- jobs %>%
      filter(is.na(Deadline) | Deadline > Sys.time()) %>%
      pull(Focus)
    loc[!is.na(loc)]
  })
  
  education_levels <- reactive({
    req(input$edumin, input$edumax)
    levels <- list("Undergraduate" = 1, "Junior" = 2, "Senior" = 3, "Graduate/Law" = 4)
    c(levels[input$edumin], levels[input$edumax])
  })

  jobs2 <- reactive({
    req(input$locations, input$focus, input$edumin, input$edumax)
    jobs %>%
      mutate(Education = factor(Education, levels = c("Undergraduate", "Junior", "Senior", "Graduate/Law"))) %>%
      filter(is.na(Deadline) | Deadline > Sys.time(), 
             Location %in% input$locations, 
             Focus %in% input$focus,
             as.numeric(Education) >= education_levels()[1],
             as.numeric(Education) <= education_levels()[2]) %>%
      mutate(
        Title = Map(
          function(title, link) {
            as.character(tags$a(title, href = link))
          },
          Title, Link
        )
      ) %>% 
      select(-Link) %>%
      arrange(desc(`Date Posted`))
  })
  
  output$locations <- renderUI({
    req(all_locations())
    locations <- all_locations()
    
    selectInput("locations", 
                label = "Locations",
                choices = locations,
                selected = locations,
                multiple = TRUE,
                width = "100%")
  })
  
  output$focus <- renderUI({
    req(all_areas())
    areas <- all_areas()
    
    selectInput("focus", 
                label = "Areas of Focus",
                choices = areas,
                selected = areas,
                multiple = TRUE,
                width = "100%")
  })

    output$t1 <- DT::renderDT({
    DT::datatable(
      jobs2(), escape = FALSE, options = list(pageLength = 10)) %>% DT::formatDate(c(1, 2), "toDateString") %>% DT::formatCurrency("Salary (Minimum)")
  })
}

shinyApp(ui, server)
