library(shiny)
library(bslib)
library(dplyr)
library(DT)

textbox <- "The database is nonexhaustive and I encourage you to also check out opportunities listed on weekly job boards, Handshake, Linkedin, and regularly check with your local representatives or favorite think tanks. Here are some resources to follow:"

resources <- tags$div(
  tags$ul(
    tags$li(
      tags$a(
        href = "https://www.linkedin.com/in/william-godnick-3404525/",
        target = "_blank",
        rel = "noopener noreferrer",
        "William Godnick regularly posts openings for professionals of all levels"
      )
    ),
    tags$li(
      tags$a(
        href = "https://governmentworks.substack.com/?utm_campaign=pub&utm_medium=web",
        target = "_blank",
        rel = "noopener noreferrer",
        "Government Works Weekly Newsletter"
      )
    ),
    tags$li(
      tags$a(
        href = "https://texaspolitics.utexas.edu/internship",
        target = "_blank",
        rel = "noopener noreferrer",
        "Texas Politics Project Internships Database"
      )
    )
  )
)

jobs <- read.csv("https://raw.githubusercontent.com/biancaschutz/greatergoodinternships/refs/heads/main/jobs.csv", check.names = FALSE) 

jobs$Deadline <- as.Date(jobs$Deadline, format = "%m/%d/%Y")
jobs$`Date Posted` <- as.Date(jobs$`Date Posted`, format = "%m/%d/%Y")

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
  
  all_locations <- reactive({
    loc <- jobs %>%
      filter(is.na(Deadline) | Deadline > Sys.Date()) %>%
               pull(Location)
    loc[!is.na(loc)]
  })
  
  all_areas <- reactive({
    loc <- jobs %>%
      filter(is.na(Deadline) | Deadline > Sys.Date()) %>%
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
      filter(is.na(Deadline) | Deadline > Sys.Date(), 
             Location %in% input$locations, 
             Focus %in% input$focus,
             as.numeric(Education) >= education_levels()[1],
             as.numeric(Education) <= education_levels()[2]) %>%
      mutate(
        Title = Map(
          function(title, link) {
            as.character(tags$a(title, href = link, target = "_blank", rel = "noopener noreferrer",))
          },
          Title, Link
        )
      ) %>% 
      select(-Link) %>%
      arrange(desc(`Date Posted`))
  })
  
  output$locations <- renderUI({
    req(length(all_locations()) > 0)
    
    locations <- all_locations()
    
    selectInput("locations", 
                label = "Locations",
                choices = locations,
                selected = locations,
                multiple = TRUE,
                width = "100%")
  })
  
  output$focus <- renderUI({
    req(length(all_areas()) > 0)
    
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
      jobs2(), escape = FALSE, options = list(pageLength = 10)) %>% DT::formatDate(c("Date Posted", "Deadline"), "toDateString") %>% DT::formatCurrency("Salary (Minimum)")
  })
}

shinyApp(ui, server)
