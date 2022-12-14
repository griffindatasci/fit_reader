library(shiny)

source("scratchpad.R")

ui <- fluidPage(
  tags$head(tags$style("
    body {
      margin: 0 auto;
      max-width: 600px;
      width: 90%;}
    .card {
      border: none;
      border-radius: 10px;
      box-shadow: 0 0 2px 2px #00000010;
      padding: 12px;
      height: 274px;
      width: 524px;
      margin: 18px;}
    .tall-card {
      height:524px;}")),
  tagAppendAttributes(tags$div(plotlyOutput("fig_map")), class="card tall-card"),
  tagAppendAttributes(tags$div(plotlyOutput("fig_alt")), class="card"),
  tagAppendAttributes(tags$div(plotlyOutput("fig_pwr_crv")), class="card"),
  tagAppendAttributes(tags$div(plotlyOutput("fig_pwr_hist")), class="card"),
  tagAppendAttributes(tags$div(plotlyOutput("fig_hr_zones")), class="card"),
  tagAppendAttributes(tags$div(plotlyOutput("fig_pwr_zones")), class="card")
)

server <- function(input, output) {

    output$fig_map <- renderPlotly(plotly_figures$map)
    output$fig_alt <- renderPlotly(plotly_figures$alt)
    output$fig_pwr_crv <- renderPlotly(plotly_figures$pwr_crv)
    output$fig_pwr_hist <- renderPlotly(plotly_figures$pwr_hist)
    output$fig_hr_zones <- renderPlotly(plotly_figures$hr_zones)
    output$fig_pwr_zones <- renderPlotly(plotly_figures$pwr_zones)
}

# Run the application 
shinyApp(ui = ui, server = server)
