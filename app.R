library(shiny)
library(visNetwork)
ui <- fluidPage(
  textInput(inputId = "keyword1",label<-"keyword1",value<-"car"),
  textInput(inputId = "keyword2",label<-"keyword2",value<-"steel"),
  numericInput(inputId = "topTerms",label<-"number of top links x2",value<-2),
  numericInput(inputId = "iterTimes",label<-"iteration times",value<-1),
  submitButton("Update View", icon("refresh")),
  helpText("When you click the button above, you should see",
           "the output below update to reflect the value you",
          "entered at the top:"),
  visNetworkOutput("network",width = "1000px", height = "1000px")
)

server <- function(input, output) {
  

  output$network <-  renderVisNetwork(connectTopic(input$keyword1,input$keyword2,input$topTerms,input$iterTimes))
}

shinyApp(ui = ui, server = server)