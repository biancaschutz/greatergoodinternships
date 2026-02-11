# to deploy: shinylive::export(appdir = "", destdir = "docs")


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
        "William Godnick regularly posts openings for professionals of all levels"
      )
    ),
    tags$li(
      tags$a(
        href = "https://governmentworks.substack.com/?utm_campaign=pub&utm_medium=web",
        "Government Works Weekly Newsletter"
      )
    ),
    tags$li(
      tags$a(
        href = "https://texaspolitics.utexas.edu/internship",
        "Texas Politics Project Internships Database"
      )
    )
  )
)

header1 <- h3("List Columns")
columns <- tags$ul(
  tags$li(tags$b("Due Date:"), 'If specified, the date that applications for the opportunity are due. Otherwise, left blank and regularly removed once the listing has been taken down.'),
  tags$li(tags$b("Date Posted:"), 'The list is sorted in descending order based on this column. If specified, the date that the opportunity was posted. Otherwise, the date that the opportunity was added to the database.'),
  tags$li(tags$b("Company:"), 'The company hosting the opportunity.'),
  tags$li(tags$b("Title:"), 'The name of the opportunity, with any additional information about stipends, location, etc.'),
  tags$li(tags$b("Location:"), 'If on-site, where the opportunity is located. Hybrid opportunities are listed under the city they are located. If remote, listed as "Remote." If multiple locations, labeled as "Various."'),
  tags$li(tags$b("Education:"), 'The minimum education required for the opportunity, from undergraduate (includes underclassmen), junior (you must be in your third year), senior (you must be in your final year), and law/graduate (you must be in grad school or law school, see the link for more specific requirements)'),
  tags$li(tags$b("Duration:"),'The timing of the internship, typically either summer, fall, spring, full-time, or temporary part-time.'),
  tags$li(tags$b("Salary (Minimum):"), 'If specified, the hourly or stipend salary. If unspecified but paid, left blank. If unpaid, $0. Note that the salary listed might be the minimum of the range specified. See the link for specific information.'),
  tags$li(tags$b("Focus:"),'The general focus of the internship, such as specific policy domains (ex. transportation) or government level (local, state, US, global).')
  )

header2 <- h3("Filters")
filters_explanation <- tags$ul(
  tags$li(tags$b("Location:"), 'Filters the locations of opportunities shown By default, all locations are selected using the "View All Locations" checkbox. To change this, you can either use the delete key on your keyboard in the box where the locations are listed, or unselect the checkbox and choose locations from the dropdown.'),
  tags$li(tags$b("Areas:"), 'Filters the focus areas of the opportunities shown. By default, all areas are selected using the "View All Areas" checkbox. To change this, you can either use the delete key on your keyboard in the box where the areas are listed, or unselect the checkbox and choose areas from the dropdown.'),
  tags$li(tags$b("Education:"), 'The column in the dataset called "Education" shows the minimum education required for the opportunity. For undergraduates, you can set the "Maximum Education" to Undergraduate, Junior, or Senior depending on your level. For masters and law students wanting to only see opportunities for graduate students, set the "Minimum Education" to Graduate/Law.'))

questions <- p("If you have any additional questions, please reach out to me via email, biancaschutz11@gmail.com.")

jobs <- read.csv("https://raw.githubusercontent.com/biancaschutz/greatergoodinternships/refs/heads/main/jobs.csv", check.names = FALSE) 

jobs$Deadline <- as.Date(jobs$Deadline, format = "%m/%d/%Y")
jobs$`Date Posted` <- as.Date(jobs$`Date Posted`, format = "%m/%d/%Y")

jobs[jobs == ""] <- NA

jobs <- jobs %>% filter(is.na(Deadline) | Deadline > Sys.Date())

ui <- page_fluid(title = "Greater Good Internships", 
                 windowTitle = "Your title",
                 tags$head(HTML("<title>App Title</title>"), 
                           tags$base(target = "_blank", 
                                     rel = "noopener noreferrer")), 
                 theme = bs_theme(bootswatch = "journal"), 
                 h2("Policy Research Internship Database"), 
                 p("Created to aid recent political science graduates, undergraduates, and graduate students interested in applying quantitative research and policy analysis to serving the greater good, at NGOs, think tanks, and other organizations. I am not a recruiter and do not have any connection to any of these positions."),
                 p("Updated at least weekly. Last Update: 2/11/2026"),
                 navset_pill(
                   nav_panel("Internship Database", 
                             layout_columns(card(checkboxInput("loccheck", "View All Locations", TRUE),
                                                 uiOutput("locations")), 
                                            card(checkboxInput("areacheck", "View All Areas", TRUE),
                                                 uiOutput("focus")),
                                            card(
                                              selectInput("edumin", 
                                                          label = "Minimum Education", 
                                                          choices = c("Undergraduate", "Junior", "Senior", "Graduate/Law"), 
                                                          selected = "Undergraduate",
                                                          width = "100%"),
                                              selectInput("edumax", 
                                                          label = "Maximum Education", 
                                                          choices = c("Undergraduate", "Junior", "Senior", "Graduate/Law"), 
                                                          selected = "Graduate/Law",
                                                          width = "100%"))),
                             DT::dataTableOutput(outputId = "t1")), 
                   nav_panel("Other Resources", textbox, resources),
                   nav_panel("User Guide", header1, columns, header2, filters_explanation, questions)), 
                 p("Created by Bianca Schutz"),
  id = "tab" 
)

server <- function(input, output, session) {

  all_locations <- reactive({
    loc <- jobs %>%
      pull(Location)
    loc[!is.na(loc)]
  })
  
  all_areas <- reactive({
    loc <- jobs %>%
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
      filter(Location %in% input$locations, 
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
    
    selection <- if (input$loccheck) {locations} else {NULL}
    selectInput("locations", 
                label = "Locations",
                choices = sort(locations),
                multiple = TRUE,
                width = "100%",
                selected = selection)
  })
  
  output$focus <- renderUI({
    req(length(all_areas()) > 0)
    
    areas <- all_areas()
    
    selection <- if (input$areacheck) {areas} else {NULL}
    
    selectInput("focus", 
                label = "Areas of Focus",
                choices = sort(areas),
                multiple = TRUE,
                width = "100%",
                selected = selection)
  })

    output$t1 <- DT::renderDT({
    DT::datatable(
      jobs2(), escape = FALSE, options = list(pageLength = 10)) %>% DT::formatDate(c("Date Posted", "Deadline"), "toDateString") %>% DT::formatCurrency("Salary (Minimum)")
  })
}

shinyApp(ui, server)
