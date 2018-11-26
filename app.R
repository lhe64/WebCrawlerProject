library(shiny)
library(visNetwork)
ui <- fluidPage(
  textInput(inputId = "keyword",label<-"keyword",value<-"car"),
  numericInput(inputId = "topTerms",label<-"number of top links",value<-2),
  numericInput(inputId = "iterTimes",label<-"iteration times",value<-2),
  submitButton("Update View", icon("refresh")),
  helpText("When you click the button above, you should see",
           "the output below update to reflect the value you",
          "entered at the top:"),
  visNetworkOutput("network",width = "1000px", height = "1000px")
)

server <- function(input, output) {
  

  output$network <-  renderVisNetwork(searchTopic(input$keyword,input$topTerms,input$iterTimes))
}

shinyApp(ui = ui, server = server)