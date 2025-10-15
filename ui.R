
library(shiny)
# library(thematic)

# thematic::thematic_shiny()


ui <- fluidPage(

  titlePanel("monerod monitor"),
  theme = bslib::bs_theme(preset = "vapor"), # https://bootswatch.com/vapor/
  shiny::fluidRow(
    shiny::column(5,
      shiny::h5("FCMP++ & Carrot alpha stressnet v1"),
      shiny::h6("Charts auto-update every 10 minutes."),
      shiny::h6("Data poll frequency: 30 seconds."),

      # shiny::h5(shiny::HTML("<u><a href=\"https://explorer.stressnet.net/\">Block Explorer</a></u>")),
      shiny::h5(shiny::HTML("<u><a href=\"https://github.com/seraphis-migration/monero/releases\">Run a stressnet node</a></u>")),
      shiny::h5(shiny::HTML("<u><a href=\"https://monero.town/post/6763165/\">Stressnet announcement</a></u>")),
      shiny::h6(shiny::HTML("Source code of this webapp: <u><a href=\"https://github.com/Rucknium/monerod-monitor\">https://github.com/Rucknium/monerod-monitor</a></u>"))
    ),
    shiny::column(7,
      shiny::textOutput("topline_text1"),
      shiny::textOutput("topline_text2"),
      shiny::textOutput("topline_text3"),
      shiny::textOutput("topline_text4"),
      shiny::textOutput("topline_text5")
    )
  ),



  radioButtons("recency",
    "Choose time window:",
    c("Hour" = 60*60, "Day" = 24*60*60, "Week" = 7*24*60*60, "All" = 365*24*60*60),
    inline = TRUE, width = "100%"
  ),
  radioButtons("chart_type",
    "Chart type:",
    c("Static image (loads fast, but non-interactive)" = "static",
      "Javascript (loads slow, but with tooltips, and one hour time window only)" = "javascript"),
    inline = FALSE, width = "100%"
  ),
  shiny::tabsetPanel(id = "chart_switcher", type = "hidden",
    tabPanelBody("static",
      shiny::br(),
      shiny::plotOutput("static_line_chart1", height = "500px"),
      shiny::br(),
      shiny::plotOutput("static_line_chart2", height = "500px"),
      shiny::br(),
      shiny::plotOutput("static_line_chart2_1_1", height = "500px"),
      shiny::br(),
      shiny::plotOutput("static_line_chart2_1_2", height = "500px"),
      shiny::br(),
      shiny::plotOutput("static_line_chart2_3", height = "500px"),
      shiny::br(),
      shiny::plotOutput("static_line_chart2_4", height = "500px"),
      shiny::br(),
      shiny::plotOutput("static_line_chart3", height = "500px"),
      shiny::br(),
      shiny::plotOutput("static_line_chart4", height = "500px"),
      shiny::br(),
      shiny::plotOutput("static_line_chart4_1", height = "500px"),
      shiny::br(),
      shiny::plotOutput("static_line_chart4_2", height = "500px"),
      shiny::br(),
      shiny::plotOutput("static_line_chart5", height = "500px"),
      shiny::br(),
      shiny::plotOutput("static_line_chart6", height = "500px"),
      shiny::br(),
      shiny::plotOutput("static_line_chart7", height = "500px"),
      shiny::br(),
      shiny::plotOutput("static_line_chart8", height = "500px"),
      shiny::br(),
      shiny::plotOutput("static_line_chart9", height = "500px")
      ),
    tabPanelBody("javascript",
      shiny::br(),
      plotly::plotlyOutput("line_chart1", height = "500px"),
      shiny::br(),
      plotly::plotlyOutput("line_chart2", height = "500px"),
      shiny::br(),
      plotly::plotlyOutput("line_chart2_1_1", height = "500px"),
      shiny::br(),
      plotly::plotlyOutput("line_chart2_1_2", height = "500px"),
      shiny::br(),
      plotly::plotlyOutput("line_chart2_3", height = "500px"),
      shiny::br(),
      plotly::plotlyOutput("line_chart2_4", height = "500px"),
      shiny::br(),
      plotly::plotlyOutput("line_chart3", height = "500px"),
      shiny::br(),
      plotly::plotlyOutput("line_chart4", height = "500px"),
      shiny::br(),
      plotly::plotlyOutput("line_chart4_1", height = "500px"),
      shiny::br(),
      plotly::plotlyOutput("line_chart4_2", height = "500px"),
      shiny::br(),
      plotly::plotlyOutput("line_chart5", height = "500px"),
      shiny::br(),
      plotly::plotlyOutput("line_chart6", height = "500px"),
      shiny::br(),
      plotly::plotlyOutput("line_chart7", height = "500px"),
      shiny::br(),
      plotly::plotlyOutput("line_chart8", height = "500px"),
      shiny::br(),
      plotly::plotlyOutput("line_chart9", height = "500px")
    )
  ),
  shiny::br(),
  shiny::plotOutput("corr_plot", height = "800px", width = "800px")



)

