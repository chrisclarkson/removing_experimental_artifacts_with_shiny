data=read.table('data.txt',header=T)
library(shiny)
ui <- basicPage(
  selectInput("select", "Select column to plot and manipulate", choices=names(data)),
  plotOutput("plot1", click = "plot_click", brush = "plot_brush"),
  actionButton('button', 'button')
)

server <- function(input, output) {
  
  my <- reactiveValues(df=data) # Initialize df
  
  output$plot1 <- renderPlot({plot(my$df[, input$select], type = 'l')})
  
  observeEvent(input$plot_brush,{
    rowmin <- round(input$plot_brush$xmin)
    rowmax <- round(input$plot_brush$xmax)
    my$df[rowmin:rowmax, input$select] <- mean(my$df[c(c(rowmin-1),c(rowmax+1)), input$select])
  })
  observeEvent(input$button, {
    isolate(write.table(my$df,"./corrected.txt", sep='\t', quote=F, col.names = T))})
}

shinyApp(ui, server)
