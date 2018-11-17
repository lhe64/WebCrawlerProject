library(shiny)
library(visNetwork)
ui <- fluidPage(
  textInput(inputId = "keyword",label<-"keyword",value<-"car"),
  numericInput(inputId = "topTerms",label<-"number of top words",value<-10),
  numericInput(inputId = "minFreq",label<-"minimum frequency of words",value<-10),
  submitButton("Update View", icon("refresh")),
  helpText("When you click the button above, you should see",
           "the output below update to reflect the value you",
          "entered at the top:"),
  plotOutput("cloud"),
  visNetworkOutput("network",width = "100%", height = "auto")
)

server <- function(input, output) {
  
  output$cloud <- renderPlot(processWordcloud(input$keyword,input$minFreq))

  output$network <-  renderVisNetwork(plotNetwork(input$keyword,input$topTerms))
}

shinyApp(ui = ui, server = server)