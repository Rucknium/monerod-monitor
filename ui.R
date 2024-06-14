
library(shiny)




ui <- fluidPage(

  titlePanel("monerod monitor"),

  radioButtons("recency",
    "Recency",
    c("Hour" = 60*60, "Day" = 24*60*60, "Week" = 7*24*60*60, "All" = 365*24*60*60)
  ),
  plotly::plotlyOutput("line_chart1", height = "500px"),
  plotly::plotlyOutput("line_chart2", height = "500px"),
  plotly::plotlyOutput("line_chart3", height = "500px"),
  plotly::plotlyOutput("line_chart4", height = "500px"),
  plotly::plotlyOutput("line_chart5", height = "500px"),
  plotly::plotlyOutput("line_chart6", height = "500px"),
  plotly::plotlyOutput("line_chart7", height = "500px"),
  plotly::plotlyOutput("line_chart8", height = "500px")

)

