
library(shiny)



con <- DBI::dbConnect(RSQLite::SQLite(), "data/xmr-stressnet-diagnostics.db")
DBI::dbExecute(con, "PRAGMA journal_mode=WAL;")
# export-csv.R can read while collect-archive.R writes
# https://stackoverflow.com/questions/15143871/simplest-way-to-retry-sqlite-query-if-db-is-locked


read.stressnet <- function(file = NULL) {

  list(
    pool_stats = DBI::dbGetQuery(con, "SELECT * FROM pool_stats"),
    pool_stats_histo = DBI::dbGetQuery(con, "SELECT * FROM pool_stats_histo"),
    info = DBI::dbGetQuery(con, "SELECT * FROM info"),
    fee_estimate = DBI::dbGetQuery(con, "SELECT * FROM fee_estimate"),
    connections = DBI::dbGetQuery(con, "SELECT * FROM connections"),
    process_info = DBI::dbGetQuery(con, "SELECT * FROM process_info")
  )

}

# system.time(read.stressnet())


stressnet.db_fn <- shiny::reactiveFileReader(60000, NULL,
  filePath = paste0("data/xmr-stressnet-diagnostics.db-wal"), readFunc = read.stressnet)
# Poll every 1 minutes



server <- function(input, output) {


  shiny::observe({

    stressnet.db <- stressnet.db_fn()

    for (i in seq_along(stressnet.db)) {
      stressnet.db[[i]]$time <- as.POSIXct(as.numeric(stressnet.db[[i]]$time), origin = "1970-01-01")
      stressnet.db[[i]] <- stressnet.db[[i]][stressnet.db[[i]]$time >= (as.POSIXct(Sys.time()) - as.numeric(input$recency)), , drop = FALSE]
    }




    output$line_chart1 <- plotly::renderPlotly({

      data <- stressnet.db$pool_stats

      fig <- plotly::plot_ly(name = "Daily", data = data, x = ~time, y = ~bytes_total, type = 'scatter',
        mode = 'lines', fill = 'tozeroy', line = list(color = 'transparent', fillcolor = "#467c9e") ) |>
        plotly::layout(xaxis = list(rangeslider = list(visible = TRUE)))


      fig <- fig |>
        plotly::layout(
          title = list(text = "txpool bytes"),
          margin = list(t = 100, l = 0, r = 0),
          xaxis = list(title = '',
            zerolinecolor = '#ffff',
            zerolinewidth = 2,
            gridcolor = 'ffff'),
          yaxis = list(title = '',
            zerolinecolor = '#ffff',
            zerolinewidth = 2,
            gridcolor = 'ffff'),
          plot_bgcolor= '#f2f8ee', hovermode = 'x', paper_bgcolor = '#f2f8ee',
          legend = list(orientation = "h", xanchor = "center", x = 0.5, yanchor = "top", y = 1.1)) |>
        plotly::config(displayModeBar = FALSE)
      fig

    })




    output$line_chart2 <- plotly::renderPlotly({

      data <- stressnet.db$pool_stats

      fig <- plotly::plot_ly(name = "Daily", data = data, x = ~time, y = ~txs_total, type = 'scatter',
        mode = 'lines', fill = 'tozeroy', line = list(color = 'transparent', fillcolor = "#467c9e") ) |>
        plotly::layout(xaxis = list(rangeslider = list(visible = TRUE)))


      fig <- fig |>
        plotly::layout(
          title = list(text = "txpool number of txs"),
          margin = list(t = 100, l = 0, r = 0),
          xaxis = list(title = '',
            zerolinecolor = '#ffff',
            zerolinewidth = 2,
            gridcolor = 'ffff'),
          yaxis = list(title = '',
            zerolinecolor = '#ffff',
            zerolinewidth = 2,
            gridcolor = 'ffff'),
          plot_bgcolor= '#f2f8ee', hovermode = 'x', paper_bgcolor = '#f2f8ee',
          legend = list(orientation = "h", xanchor = "center", x = 0.5, yanchor = "top", y = 1.1)) |>
        plotly::config(displayModeBar = FALSE)
      fig

    })



    output$line_chart3 <- plotly::renderPlotly({

      data <- stressnet.db$info

      fig <- plotly::plot_ly(name = "Daily", data = data, x = ~time, y = ~outgoing_connections_count, type = 'scatter',
        mode = 'lines', fill = 'tozeroy', line = list(color = 'transparent', fillcolor = "#467c9e") ) |>
        plotly::layout(xaxis = list(rangeslider = list(visible = TRUE)))


      fig <- fig |>
        plotly::layout(
          title = list(text = "Number of outgoing connections"),
          margin = list(t = 100, l = 0, r = 0),
          xaxis = list(title = '',
            zerolinecolor = '#ffff',
            zerolinewidth = 2,
            gridcolor = 'ffff'),
          yaxis = list(title = '',
            zerolinecolor = '#ffff',
            zerolinewidth = 2,
            gridcolor = 'ffff'),
          plot_bgcolor= '#f2f8ee', hovermode = 'x', paper_bgcolor = '#f2f8ee',
          legend = list(orientation = "h", xanchor = "center", x = 0.5, yanchor = "top", y = 1.1)) |>
        plotly::config(displayModeBar = FALSE)
      fig

    })



    output$line_chart4 <- plotly::renderPlotly({

      data <- stressnet.db$info

      fig <- plotly::plot_ly(name = "Daily", data = data, x = ~time, y = ~incoming_connections_count, type = 'scatter',
        mode = 'lines', fill = 'tozeroy', line = list(color = 'transparent', fillcolor = "#467c9e") ) |>
        plotly::layout(xaxis = list(rangeslider = list(visible = TRUE)))


      fig <- fig |>
        plotly::layout(
          title = list(text = "Number of incoming connections"),
          margin = list(t = 100, l = 0, r = 0),
          xaxis = list(title = '',
            zerolinecolor = '#ffff',
            zerolinewidth = 2,
            gridcolor = 'ffff'),
          yaxis = list(title = '',
            zerolinecolor = '#ffff',
            zerolinewidth = 2,
            gridcolor = 'ffff'),
          plot_bgcolor= '#f2f8ee', hovermode = 'x', paper_bgcolor = '#f2f8ee',
          legend = list(orientation = "h", xanchor = "center", x = 0.5, yanchor = "top", y = 1.1)) |>
        plotly::config(displayModeBar = FALSE)
      fig

    })



    output$line_chart5 <- plotly::renderPlotly({

      data <- stressnet.db$process_info

      data$cpu_time_user <- c(NA, diff(data$cpu_time_user) / as.numeric(diff(data$time)))

      fig <- plotly::plot_ly(name = "Daily", data = data, x = ~time, y = ~cpu_time_user, type = 'scatter',
        mode = 'lines', fill = 'tozeroy', line = list(color = 'transparent', fillcolor = "#467c9e") ) |>
        plotly::layout(xaxis = list(rangeslider = list(visible = TRUE)))


      fig <- fig |>
        plotly::layout(
          title = list(text = "monerod's CPU load (user mode)"),
          margin = list(t = 100, l = 0, r = 0),
          xaxis = list(title = '',
            zerolinecolor = '#ffff',
            zerolinewidth = 2,
            gridcolor = 'ffff'),
          yaxis = list(title = '',
            zerolinecolor = '#ffff',
            zerolinewidth = 2,
            gridcolor = 'ffff'),
          plot_bgcolor= '#f2f8ee', hovermode = 'x', paper_bgcolor = '#f2f8ee',
          legend = list(orientation = "h", xanchor = "center", x = 0.5, yanchor = "top", y = 1.1)) |>
        plotly::config(displayModeBar = FALSE)
      fig

    })



    output$line_chart6 <- plotly::renderPlotly({

      data <- stressnet.db$process_info

      data$cpu_time_system <- c(NA, diff(data$cpu_time_system) / as.numeric(diff(data$time)))

      fig <- plotly::plot_ly(name = "Daily", data = data, x = ~time, y = ~cpu_time_system, type = 'scatter',
        mode = 'lines', fill = 'tozeroy', line = list(color = 'transparent', fillcolor = "#467c9e") ) |>
        plotly::layout(xaxis = list(rangeslider = list(visible = TRUE)))


      fig <- fig |>
        plotly::layout(
          title = list(text = "monerod's CPU load (kernel mode)"),
          margin = list(t = 100, l = 0, r = 0),
          xaxis = list(title = '',
            zerolinecolor = '#ffff',
            zerolinewidth = 2,
            gridcolor = 'ffff'),
          yaxis = list(title = '',
            zerolinecolor = '#ffff',
            zerolinewidth = 2,
            gridcolor = 'ffff'),
          plot_bgcolor= '#f2f8ee', hovermode = 'x', paper_bgcolor = '#f2f8ee',
          legend = list(orientation = "h", xanchor = "center", x = 0.5, yanchor = "top", y = 1.1)) |>
        plotly::config(displayModeBar = FALSE)
      fig

    })


    output$line_chart7 <- plotly::renderPlotly({

      data <- stressnet.db$process_info

      fig <- plotly::plot_ly(name = "Daily", data = data, x = ~time, y = ~mem_uss, type = 'scatter',
        mode = 'lines', fill = 'tozeroy', line = list(color = 'transparent', fillcolor = "#467c9e") ) |>
        plotly::layout(xaxis = list(rangeslider = list(visible = TRUE)))


      fig <- fig |>
        plotly::layout(
          title = list(text = "monerod's RAM (Unique Set Size)"),
          margin = list(t = 100, l = 0, r = 0),
          xaxis = list(title = '',
            zerolinecolor = '#ffff',
            zerolinewidth = 2,
            gridcolor = 'ffff'),
          yaxis = list(title = '',
            zerolinecolor = '#ffff',
            zerolinewidth = 2,
            gridcolor = 'ffff'),
          plot_bgcolor= '#f2f8ee', hovermode = 'x', paper_bgcolor = '#f2f8ee',
          legend = list(orientation = "h", xanchor = "center", x = 0.5, yanchor = "top", y = 1.1)) |>
        plotly::config(displayModeBar = FALSE)
      fig

    })


    output$line_chart8 <- plotly::renderPlotly({

      data <- stressnet.db$process_info

      fig <- plotly::plot_ly(name = "Daily", data = data, x = ~time, y = ~mem_swap, type = 'scatter',
        mode = 'lines', fill = 'tozeroy', line = list(color = 'transparent', fillcolor = "#467c9e") ) |>
        plotly::layout(xaxis = list(rangeslider = list(visible = TRUE)))


      fig <- fig |>
        plotly::layout(
          title = list(text = "monerod's RAM (Swap)"),
          margin = list(t = 100, l = 0, r = 0),
          xaxis = list(title = '',
            zerolinecolor = '#ffff',
            zerolinewidth = 2,
            gridcolor = 'ffff'),
          yaxis = list(title = '',
            zerolinecolor = '#ffff',
            zerolinewidth = 2,
            gridcolor = 'ffff'),
          plot_bgcolor= '#f2f8ee', hovermode = 'x', paper_bgcolor = '#f2f8ee',
          legend = list(orientation = "h", xanchor = "center", x = 0.5, yanchor = "top", y = 1.1)) |>
        plotly::config(displayModeBar = FALSE)
      fig

    })





}, domain = NULL)



}
