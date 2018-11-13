library(shiny)

ui <- fluidPage(
  textInput(inputId = "keyword",label<-"keyword",value<-"car"),
  numericInput(inputId = "numOftopics",label<-"number of topics",value<-2),
  numericInput(inputId = "topTerms",label<-"number of top words",value<-10),
  numericInput(inputId = "minFreq",label<-"minimum frequency of words",value<-10),
  submitButton("Update View", icon("refresh")),
  helpText("When you click the button above, you should see",
           "the output below update to reflect the value you",
          "entered at the top:"),
  plotOutput("cloud"),
  tableOutput("network")
)

server <- function(input, output) {
  
  output$cloud <- renderPlot(processWordcloud(input$keyword,input$minFreq))

  output$network <- renderTable(plotNetwork(input$keyword,input$numOftopics,input$topTerms)[3])
}

shinyApp(ui = ui, server = server)