

library(shiny)
library(bslib)

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
        choices = c("Herr", "Fru", "Frøken", "Mester", "Don", "Dr.", "Sir", "Oberst", "Grevinne"),
        selected = "Herr"
      ),
      
      textInput(
        inputId = "name",
        label = "Skriv inn navnet ditt:",
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
        choices = c("1. klasse - 10.000kr", "2. klasse - 5.000kr", "3. klasse - 2.500kr"),
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
      h3("Fyll ut informasjonen for å kjøpe billett."),
      br(),
      textOutput("confirmation")
    )
  )
)
server <- function(input, output) {
  # Server logic (not needed for now)
}

shinyApp(ui = ui, server = server)

