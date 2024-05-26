#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Histogram danych mtcars"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Ile przedziałów:",
                        min = 1,
                        max = 50,
                        value = 30),
            selectInput("listaKolorow", "Wybierz kolor:",
                        choices = list ("czerwony" = "red", "niebieski" = "blue", "zielony" = "green")),
            checkboxInput("wyborEtykiet", "Czy pokazać etykiety?", value=TRUE),
            selectInput("column", "Wybierz kolumnę:",
                        choices = colnames(mtcars),
                        selected = 'hp'),
            selectInput("plotType", "Wybierz typ wykresu:",
                        choices = list("Histogram" = "hist", "Wykres punktowy" = "scatter", "Wykres słupkowy" = "bar"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- mtcars[[input$column]] #zbiór danych 'mtcars', kolumna do wyboru
        bins <- seq(min(x), max(x), length.out = input$bins + 1) #generuje przedziały histogramu na podstawie slidera 'bins'

        if (input$plotType == "hist"){
          # draw the histogram with the specified number of bins
          hist(x, breaks = bins, col = input$listaKolorow, 
               labels = input$wyborEtykiet,
               xlab = input$column, #oś x
               ylab = 'Częstotliwość', #oś y
               main = paste('Histogram ', input$column))
        } else if(input$plotType == "scatter"){
          plot(mtcars$mpg, x, col = input$listaKolorow,
               xlab = "MPG",
               ylab = input$column,
               main = paste('Wykres punktowy MPG vs', input$column))
        } else if (input$plotType == "bar"){
          barplot(table(x), col = input$listaKolorow,
                  xlab = input$column,
                  ylab = "Częstotliwość",
                  main = paste ('Wykres słupkowy ', input$column))
        }
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
