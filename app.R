# Dependencies
dependencies <- c("dplyr", "shiny", "bslib", "tibble")
for (pkg in dependencies) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

library(dplyr)
library(shiny)
library(bslib)
library(tibble)


ticket_tibble <- NULL
print(getwd())
trained_rf <- readRDS("code/models/tuned_models/tuned_random_forest_model.rds")

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
  median_fare_data <- tibble(
    Embarked = c("C", "C", "C", "Q", "Q", "Q", "S", "S", "S"),
    Pclass = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
    average_fare = c(79.2, 24, 7.90, 90, 12.4, 7.75, 52, 13.5, 8.05)
  )
  
  ticket_data <- eventReactive(input$submit_btn, {
    # Determine SibSp and Parch based on input
    sibsp <- input$siblings + ifelse(input$partner == "Ja", 1, 0)
    parch <- input$children + 
      ifelse(input$parents == "En", 1, ifelse(input$parents == "Begge", 2, 0))
    
    # Map gender to 'male'/'female'
    sex <- ifelse(input$gender == "Mann", "male", "female")
    # Map pclass
    pclass <- as.double(ifelse(input$pclass == "1. klasse", 1, 
                               ifelse(input$pclass == "2. klasse", 2, 3)))
    # Map embarked
    embarked <- ifelse(input$port == "Southampton", "S", 
                       ifelse(input$port == "Queenstown", "Q", "C"))
    
    # find average fare for Pclass and Embarked
    fare_lookup <- median_fare_data %>%
      filter(Embarked == embarked, Pclass == pclass) %>%
      pull(average_fare)
    
    # Create a tibble for model
    tibble(
      Pclass = pclass,
      Age = as.double(input$age),
      SibSp = sibsp,
      Parch = parch,
      Fare = as.double(fare_lookup),
      Survived = 0,
      Sex = sex,
      Embarked = embarked,
      Title = input$title
    )
  })
  
  observeEvent(input$submit_btn, {

    ticket_tibble <<- ticket_data()

    dummy_data <- ticket_tibble %>%
      mutate(across(where(is.character), as.factor))

    dummy_data <- dummy_data %>%
      mutate(

        Sex_male = ifelse(Sex == "male", 1, 0),
        Sex_female = ifelse(Sex == "female", 1, 0),

        Embarked_C = ifelse(Embarked == "C", 1, 0),
        Embarked_Q = ifelse(Embarked == "Q", 1, 0),
        Embarked_S = ifelse(Embarked == "S", 1, 0),

        Title_Mr = ifelse(Title == "Herr", 1, 0),
        Title_Mrs = ifelse(Title == "Fru", 1, 0),
        Title_Miss = ifelse(Title == "Frøken", 1, 0),
        Title_Master = ifelse(Title == "Mester", 1, 0),
        Title_Other = ifelse(Title == "Annet", 1, 0)
      )%>%
      select(-Embarked, -Title, -Sex) %>%
      mutate(Survived = as.factor(Survived))
    
    prediction <- predict(trained_rf, dummy_data, type = "class")
    
    # add survival to tibble
    ticket_tibble <<- ticket_tibble %>%
      mutate(Survived = prediction)
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
      survival_prediction <- ticket_tibble$Survived$.pred_class
      tagList(
        h4("Billetten din er registrert"),
        br(),
        h5(ifelse(survival_prediction == "1",
                  "Gratulerer!! Du overlevde Titanic forliset :) ",
                  "Dette gikk ikke så bra du... Desverre"))
      )
    }
  })
}

shinyApp(ui = ui, server = server)
