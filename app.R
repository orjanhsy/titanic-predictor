library(dplyr)
library(shiny)
library(bslib)
library(tibble)

ui <- fluidPage(
  theme = bs_theme(
    version = 4,
    bootswatch = "minty"          
  ),
  
  titlePanel("Kjøp billett til Titanic"),
  sidebarLayout(
    sidebarPanel(
      h3("Fyll ut billettinformasjonen"),
      
      selectInput(
        inputId = "title",
        label = "Velg tittel:",
        choices = c("Herr", "Fru", "Frøken", "Mester", "Annet"),
        selected = "Herr"
      ),
      
      textInput(
        inputId = "first_name",
        label = "Fornavn:",
        placeholder = "Skriv navnet ditt her"
      ),
      
      textInput(
        inputId = "last_name",
        label = "Etternavn:",
        placeholder = "Skriv navnet ditt her"
      ),
      
      numericInput(
        inputId = "age",
        label = "Oppgi alder:",
        value = 20,
        min = 1,
        max = 120
      ),
      
      selectInput(
        inputId = "gender",
        label = "Velg kjønn:",
        choices = c("Mann", "Kvinne"),
        selected = "Mann"
      ),
      
      selectInput(
        inputId = "port",
        label = "Hvor reiser du fra?",
        choices = c("Southampton", "Cherbourg", "Queenstown"),
        selected = "Southampton"
      ),
      
      selectInput(
        inputId = "pclass",
        label = "Velg billettklasse:",
        choices = c("1. klasse", "2. klasse", "3. klasse"),
        selected = "1. klasse"
      ),
      
      sliderInput(
        inputId = "siblings",
        label = "Reiser du med søsken?",
        min = 0,
        max = 10,
        value = 0
      ),
      
      sliderInput(
        inputId = "children",
        label = "Reiser du med barn?",
        min = 0,
        max = 10,
        value = 0
      ),
      
      radioButtons(
        inputId = "partner",
        label = "Reiser du med ektefelle eller kjæreste?",
        choices = c("Ja", "Nei"),
        selected = "Nei"
      ),
      
      radioButtons(
        inputId = "parents",
        label = "Reiser du med foreldre?",
        choices = c("Nei", "En", "Begge"),
        selected = "Nei"
      ),
      
      
      actionButton(
        inputId = "submit_btn",
        label = "Kjøp billett!"
      )
    ),
    
    mainPanel(
      h3(textOutput("info_header")),
      br(),
      textOutput("confirmation"),
      br(),
      uiOutput("ticket_info")
    )
  )
)
server <- function(input, output) {
  # hard coded for now.
  avg_fare_data <- tibble(
    Embarked = c("C", "C", "C", "Q", "Q", "Q", "S", "S", "S"),
    Pclass = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
    average_fare = c(104, 25.4, 11.2, 90, 12.4, 11.2, 70.4, 20.3, 14.6)
  )
  
  ticket_data <- eventReactive(input$submit_btn, {
    # Determine SibSp and Parch based on input
    sibsp <- input$siblings + ifelse(input$partner == "Ja", 1, 0)
    parch <- input$children + 
      ifelse(input$parents == "En", 1, ifelse(input$parents == "Begge", 2, 0))
    
    # Map gender to 'male'/'female'
    sex <- ifelse(input$gender == "Mann", "male", "female")
    # Map pclass
    pclass <- ifelse(input$pclass == "1. klasse", 1, 
                     ifelse(input$pclass == "2. klasse", 2, 3))
    # Map embarked
    embarked <- ifelse(input$port == "Southampton", "S", 
                       ifelse(input$port == "Queenstown", "Q", "C"))
    
    # find average fare for Pclass and Embarked
    fare_lookup <- avg_fare_data %>%
      filter(Embarked == embarked, Pclass == pclass) %>%
      pull(average_fare)
    
    # Create a tibble for model
    tibble(
      PassengerId = 1,
      Survived = 0,
      Pclass = pclass,
      Name = paste0(input$last_name, ", ", input$title, ". ", input$first_name),
      Sex = sex,
      Age = as.numeric(input$age),
      SibSp = sibsp,
      Parch = parch,
      Ticket = "1",
      Fare = fare_lookup,
      Cabin = NA_character_,
      Embarked = embarked
    )
  })
  
  output$info_header <- renderText({
    if (input$submit_btn > 0) {
      "Din billettinformasjon"  
    } else {
      "Fyll ut for å kjøpe billett" 
    }
  })
  
  output$ticket_info <- renderUI({
    if (input$submit_btn > 0) {
      tagList(
        h4("Billetten din er registrert"),
        tableOutput("tibble_output")
      )
    }
  })
  
  # Display tibble in UI
  output$tibble_output <- renderTable({
    ticket_data()
  })
}

shinyApp(ui = ui, server = server)

