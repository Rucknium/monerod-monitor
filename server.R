
library(shiny)
library(data.table)
library(ggplot2)
# library(thematic)

# thematic::thematic_shiny()

shinyOptions(cache = cachem::cache_disk(file.path(dirname(tempdir()), "monerod-monitor-cache")),
  max_size = 5 * 1024 * 1024^2) # Up to 5 GB cache

con <- DBI::dbConnect(RSQLite::SQLite(), "data/xmr-stressnet-diagnostics.db")
DBI::dbExecute(con, "PRAGMA journal_mode=WAL;")
# export-csv.R can read while collect-archive.R writes
# https://stackoverflow.com/questions/15143871/simplest-way-to-retry-sqlite-query-if-db-is-locked


read.stressnet <- function(file = NULL) {

  list(
    pool_stats = DBI::dbGetQuery(con, "SELECT * FROM pool_stats"),
    # pool_stats_histo = DBI::dbGetQuery(con, "SELECT * FROM pool_stats_histo"), # pool_stats_hist is unused
    info = DBI::dbGetQuery(con, "SELECT * FROM info"),
    last_block_header = DBI::dbGetQuery(con, "SELECT * FROM last_block_header"),
    # fee_estimate = DBI::dbGetQuery(con, "SELECT * FROM fee_estimate"), # unused
    connections = DBI::dbGetQuery(con, "SELECT * FROM connections"),
    bans = DBI::dbGetQuery(con, "SELECT * FROM bans"),
    process_info = DBI::dbGetQuery(con, "SELECT * FROM process_info")
  )

}

# system.time(read.stressnet())


stressnet.db_fn <- shiny::reactiveFileReader(10 * 60000, session = NULL,
  filePath = paste0("data/xmr-stressnet-diagnostics.db-wal"), readFunc = read.stressnet)
# Poll every 10 minutes


bs.colors <- bslib::bs_get_variables(bslib::bs_theme(preset = "vapor"),
  c("blue", "indigo", "purple", "pink", "red", "orange", "yellow", "green",
    "teal", "cyan", "light", "dark", "body-bg", "white", "font-family-sans-serif", "text-muted",
    "gray-800", "gray-500"))

is.pruned <- FALSE
# TODO: Make this not hard-coded

plot.style <- function(x, title) {
  x |>
  plotly::layout(
    title = list(text = title), # , font = list( family = "Courier New", size = 14, color = bs.colors["white"])
    font = list(color = bs.colors["light"]),
    margin = list(t = 100, l = 0, r = 0),
    xaxis = list(title = '',
      zerolinecolor = '#ffff',
      zerolinewidth = 2,
      gridcolor = 'ffff'),
    yaxis = list(title = '',
      zerolinecolor = '#ffff',
      zerolinewidth = 2,
      gridcolor = 'ffff'),
    plot_bgcolor = bs.colors["dark"], hovermode = 'x', paper_bgcolor = bs.colors["dark"],
    legend = list(orientation = "h", xanchor = "center", x = 0.5, yanchor = "top", y = 1.1)) |>
  plotly::config(displayModeBar = FALSE)
}

ggplot.theme <- theme(
  plot.background = element_rect(fill = bs.colors["dark"]),
  panel.background = element_rect(fill = bs.colors["dark"]),
  text = element_text(color = bs.colors["light"], size = 15),
  axis.text = element_text(color = bs.colors["light"], size = 15),
  plot.title = element_text(size = 25, hjust = 0.5),
  axis.title.x = element_blank()
)




server <- function(input, output) {

  shiny::onStop(function() {DBI::dbDisconnect(con)}, session = NULL)
  # "If NULL, it is the same as calling onStop outside of the server function,
  # and the callback will be invoked when the application exits."

  observeEvent(input$chart_type, {
    updateTabsetPanel(inputId = "chart_switcher", selected = input$chart_type)
  })




  stressnet.db <- reactive({

    stressnet.db <- stressnet.db_fn()

    recency <- as.numeric(input$recency)
    if (input$chart_type == "javascript") {
      recency <- 60*60
    }

    for (i in seq_along(stressnet.db)) {
      stressnet.db[[i]]$time <- as.POSIXct(as.numeric(stressnet.db[[i]]$time), origin = "1970-01-01")
      stressnet.db[[i]] <- stressnet.db[[i]][stressnet.db[[i]]$time >=
          (as.POSIXct(Sys.time()) - recency), , drop = FALSE]
    }

    stressnet.db

  }) |> bindCache(stressnet.db_fn(), input$recency, input$chart_type)


  observe({

    topline_data <- stressnet.db()$info[nrow(stressnet.db()$info), , drop = FALSE]

    output$topline_text1 <- renderText({
      paste0("Data last updated: ", round(topline_data$time), " UTC")
    })

    output$topline_text2 <- renderText({
      paste0("Current node height: ", as.integer(topline_data$height))
    })

    output$topline_text3 <- renderText({
      paste0("Top block hash: ", topline_data$top_block_hash)
    })

    output$topline_text4 <- renderText({
      paste0("Last node restart time: ",
        as.POSIXct(as.numeric(topline_data$start_time), origin = "1970-01-01"), " UTC"
      )
    })

    output$topline_text5 <- renderText({
      paste0("Node version: ", topline_data$version
      )
    })

})


    output$line_chart1 <- plotly::renderPlotly({

      data <- stressnet.db()$pool_stats

      fig <- plotly::plot_ly(name = "30 seconds poll time", data = data, x = ~time, y = ~bytes_total, type = 'scatter',
        mode = 'lines', fill = 'tozeroy', fillcolor = adjustcolor(bs.colors["pink"], alpha.f = 0.85),
        line = list(color = bs.colors["pink"]) ) |>
        plotly::layout(xaxis = list(rangeslider = list(visible = TRUE)),
          yaxis = list(tickformat = "~s"))
      # tickformat
      # https://community.plotly.com/t/graphing-storage-in-units-of-1024-kilobytes-megabytes-etc/14305
      # https://d3js.org/d3-format#locale_format

      fig <- plot.style(fig, "txpool bytes")

      fig

    }) |> bindCache(stressnet.db(), input$recency, input$chart_type)


    output$static_line_chart1 <- renderPlot({

      data <- stressnet.db()$pool_stats

      ggplot(data, aes(x = time, y = bytes_total)) +
        ggtitle("txpool bytes") +
        geom_area(fill = adjustcolor(bs.colors["pink"], alpha.f = 0.85)) +
        scale_y_continuous(labels = scales::label_number(suffix = " MB", scale = 1e-6)) +
        expand_limits(y = 0) +
        ggplot.theme

    }) |> bindCache(stressnet.db(), input$recency, input$chart_type)
# , sizePolicy = function(width, height) {list(width = 800, height = 500)}


    output$line_chart2 <- plotly::renderPlotly({

      data <- stressnet.db()$pool_stats

      fig <- plotly::plot_ly(name = "30 seconds poll time", data = data, x = ~time, y = ~txs_total, type = 'scatter',
        mode = 'lines', fill = 'tozeroy', fillcolor = adjustcolor(bs.colors["pink"], alpha.f = 0.85),
        line = list(color = bs.colors["pink"]) ) |>
        plotly::layout(xaxis = list(rangeslider = list(visible = TRUE)))

      fig <- plot.style(fig, "txpool number of txs")

      fig

    }) |> bindCache(stressnet.db(), input$recency, input$chart_type)


    output$static_line_chart2 <- shiny::renderPlot({

      data <- stressnet.db()$pool_stats

      ggplot(data, aes(x = time, y = txs_total)) +
        ggtitle("txpool number of txs") +
        geom_area(fill = adjustcolor(bs.colors["pink"], alpha.f = 0.85)) +
        expand_limits(y = 0) +
        ggplot.theme

    }) |> bindCache(stressnet.db(), input$recency, input$chart_type)



    output$line_chart2_1_1 <- plotly::renderPlotly({

      data <- stressnet.db()$last_block_header

      fig <- plotly::plot_ly(name = "30 seconds poll time", data = data, x = ~time, y = ~block_weight, type = 'scatter',
        mode = 'lines', fill = 'tozeroy', fillcolor = adjustcolor(bs.colors["pink"], alpha.f = 0.85),
        line = list(color = bs.colors["pink"])) |>
        plotly::layout(xaxis = list(rangeslider = list(visible = TRUE)),
          yaxis = list(tickformat = "~s"))

      fig <- plot.style(fig, "Block weight")

      fig

    }) |> bindCache(stressnet.db(), input$recency, input$chart_type)

    output$static_line_chart2_1_1 <- shiny::renderPlot({

      data <- stressnet.db()$last_block_header

      ggplot(data, aes(x = time, y = block_weight)) +
        ggtitle("Block weight") +
        geom_area(fill = adjustcolor(bs.colors["pink"], alpha.f = 0.85)) +
        scale_y_continuous(labels = scales::label_number(suffix = " MB", scale = 1e-6)) +
        expand_limits(y = 0) +
        ggplot.theme

    }) |> bindCache(stressnet.db(), input$recency, input$chart_type)

    output$line_chart2_1_2 <- plotly::renderPlotly({

      data <- stressnet.db()$last_block_header

      fig <- plotly::plot_ly(name = "30 seconds poll time", data = data, x = ~time, y = ~reward/1e+12, type = 'scatter',
        mode = 'lines', fill = 'tozeroy', fillcolor = adjustcolor(bs.colors["pink"], alpha.f = 0.85),
        line = list(color = bs.colors["pink"])) |>
        plotly::layout(xaxis = list(rangeslider = list(visible = TRUE)))

      fig <- plot.style(fig, "Total block coinbase reward to miner (XMR)")

      fig

    }) |> bindCache(stressnet.db(), input$recency, input$chart_type)

    output$static_line_chart2_1_2 <- shiny::renderPlot({

      data <- stressnet.db()$last_block_header

      ggplot(data, aes(x = time, y = reward/1e+12)) +
        ggtitle("Total block coinbase reward to miner (XMR)") +
        geom_area(fill = adjustcolor(bs.colors["pink"], alpha.f = 0.85)) +
        expand_limits(y = 0) +
        ggplot.theme

    }) |> bindCache(stressnet.db(), input$recency, input$chart_type)


    output$line_chart2_3 <- plotly::renderPlotly({

      data <- stressnet.db()$info

      fig <- plotly::plot_ly(name = "30 seconds poll time", data = data, x = ~time, y = ~block_weight_median, type = 'scatter',
        mode = 'lines', # fill = 'tozeroy', fillcolor = adjustcolor(bs.colors["pink"], alpha.f = 0.85),
        line = list(color = bs.colors["pink"], width = 5)) |>
        plotly::layout(xaxis = list(rangeslider = list(visible = TRUE)),
          yaxis = list(tickformat = "~s"))

      fig <- plot.style(fig, "Block weight median")

      fig

    }) |> bindCache(stressnet.db(), input$recency, input$chart_type)

    output$static_line_chart2_3 <- shiny::renderPlot({

      data <- stressnet.db()$info

      ggplot(data, aes(x = time, y = block_weight_median)) +
        ggtitle("Block weight median") +
        geom_line(linewidth = 2, color = adjustcolor(bs.colors["pink"], alpha.f = 0.85)) +
        scale_y_continuous(labels = scales::label_number(suffix = " MB", scale = 1e-6)) +
        ggplot.theme

    }) |> bindCache(stressnet.db(), input$recency, input$chart_type)




    output$line_chart2_4 <- plotly::renderPlotly({

      data <- stressnet.db()$info

      fig <- plotly::plot_ly(name = "30 seconds poll time", data = data, x = ~time, y = ~database_size, type = 'scatter',
        mode = 'lines', # fill = 'tozeroy', fillcolor = adjustcolor(bs.colors["pink"], alpha.f = 0.85),
        line = list(color = bs.colors["pink"], width = 5)) |>
        plotly::layout(xaxis = list(rangeslider = list(visible = TRUE)),
          yaxis = list(tickformat = "~s"))

      fig <- plot.style(fig, paste0("Blockchain size", ifelse(is.pruned, " (pruned)", " (unpruned)")) )

      fig

    }) |> bindCache(stressnet.db(), input$recency, input$chart_type)

    output$static_line_chart2_4 <- shiny::renderPlot({

      data <- stressnet.db()$info

      ggplot(data, aes(x = time, y = database_size)) +
        ggtitle(paste0("Blockchain size", ifelse(is.pruned, " (pruned)", " (unpruned)"))) +
        geom_line(linewidth = 2, color = adjustcolor(bs.colors["pink"], alpha.f = 0.85)) +
        scale_y_continuous(labels = scales::label_number(suffix = " GB", scale = 1e-9)) +
        ggplot.theme

    }) |> bindCache(stressnet.db(), input$recency, input$chart_type)



    output$line_chart3 <- plotly::renderPlotly({

      data <- stressnet.db()$info

      fig <- plotly::plot_ly(name = "30 seconds poll time", data = data, x = ~time, y = ~outgoing_connections_count, type = 'scatter',
        mode = 'lines', fill = 'tozeroy', fillcolor = adjustcolor(bs.colors["pink"], alpha.f = 0.85),
        line = list(color = bs.colors["pink"]) ) |>
        plotly::layout(xaxis = list(rangeslider = list(visible = TRUE)))

      fig <- plot.style(fig, "Number of outgoing peer node connections")

      fig

    }) |> bindCache(stressnet.db(), input$recency, input$chart_type)


    output$static_line_chart3 <- shiny::renderPlot({

      data <- stressnet.db()$info

      ggplot(data, aes(x = time, y = outgoing_connections_count)) +
        ggtitle("Number of outgoing peer node connections") +
        geom_area(fill = adjustcolor(bs.colors["pink"], alpha.f = 0.85)) +
        expand_limits(y = 0) +
        ggplot.theme

    }) |> bindCache(stressnet.db(), input$recency, input$chart_type)



    output$line_chart4 <- plotly::renderPlotly({

      data <- stressnet.db()$info

      fig <- plotly::plot_ly(name = "30 seconds poll time", data = data, x = ~time, y = ~incoming_connections_count, type = 'scatter',
        mode = 'lines', fill = 'tozeroy', fillcolor = adjustcolor(bs.colors["pink"], alpha.f = 0.85),
        line = list(color = bs.colors["pink"]) ) |>
        plotly::layout(xaxis = list(rangeslider = list(visible = TRUE)))

      fig <- plot.style(fig, "Number of incoming peer node connections")

      fig

    }) |> bindCache(stressnet.db(), input$recency, input$chart_type)

    output$static_line_chart4 <- shiny::renderPlot({

      data <- stressnet.db()$info

      ggplot(data, aes(x = time, y = incoming_connections_count)) +
        ggtitle("Number of incoming peer node connections") +
        geom_area(fill = adjustcolor(bs.colors["pink"], alpha.f = 0.85)) +
        expand_limits(y = 0) +
        ggplot.theme

    }) |> bindCache(stressnet.db(), input$recency, input$chart_type)




    output$line_chart4_1 <- plotly::renderPlotly({

      data <- stressnet.db()$bans

      data$time <- factor(data$time)

      data <- data[complete.cases(data), , drop = FALSE]
      # Remove empty rows that have no bans, but keep the factor levels

      data <- as.data.frame(table(data[, "time", drop = FALSE]))

      data$time <- as.POSIXct(as.character(data$time), origin = "1970-01-01")

      colnames(data)[2] <- "banned_peers"

      fig <- plotly::plot_ly(name = "30 seconds poll time", data = data, x = ~time, y = ~banned_peers, type = 'scatter',
        mode = 'lines', fill = 'tozeroy', fillcolor = adjustcolor(bs.colors["pink"], alpha.f = 0.85),
        line = list(color = bs.colors["pink"]) ) |>
        plotly::layout(xaxis = list(rangeslider = list(visible = TRUE)))

      fig <- plot.style(fig, "Number of active bans against peers")

      fig <- fig |>
        plotly::add_trace(name = "30-minute moving average", data = data, x = ~time,
          y = ~frollmean(banned_peers, 30 * 2, align = "center", na.rm = TRUE),
          type = 'scatter', mode = 'lines',  fill = '', line = list(color = bs.colors["blue"], width = 3))

      fig

    }) |> bindCache(stressnet.db(), input$recency, input$chart_type)



    output$static_line_chart4_1 <- shiny::renderPlot({

      data <- stressnet.db()$bans

      data$time <- factor(data$time)

      data <- data[complete.cases(data), , drop = FALSE]
      # Remove empty rows that have no bans, but keep the factor levels

      data <- as.data.frame(table(data[, "time", drop = FALSE]))

      data$time <- as.POSIXct(as.character(data$time), origin = "1970-01-01")

      colnames(data)[2] <- "banned_peers"

      ggplot(data, aes(x = time, y = banned_peers)) +
        ggtitle("Number of active bans against peers") +
        geom_area(fill = adjustcolor(bs.colors["pink"], alpha.f = 0.85)) +
        expand_limits(y = 0) +
        ggplot.theme

    }) |> bindCache(stressnet.db(), input$recency, input$chart_type)








    output$line_chart4_2 <- plotly::renderPlotly({

      data <- stressnet.db()$connections

      setDT(data)

      data <- data[, .(live_time = median(live_time)), by = "time"]

      fig <- plotly::plot_ly(name = "30 seconds poll time", data = data, x = ~time, y = ~live_time/60, type = 'scatter',
        mode = 'lines', fill = 'tozeroy', fillcolor = adjustcolor(bs.colors["pink"], alpha.f = 0.85),
        line = list(color = bs.colors["pink"]) ) |>
        plotly::layout(xaxis = list(rangeslider = list(visible = TRUE)))

      fig <- plot.style(fig, "Median live_time of peer node connections (minutes)")

      fig

    }) |> bindCache(stressnet.db(), input$recency, input$chart_type)






    output$static_line_chart4_2 <- shiny::renderPlot({

      data <- stressnet.db()$connections

      setDT(data)

      data <- data[, .(live_time = median(live_time)), by = "time"]

      ggplot(data, aes(x = time, y = live_time)) +
        ggtitle("Median live_time of peer node connections (minutes)") +
        geom_area(fill = adjustcolor(bs.colors["pink"], alpha.f = 0.85)) +
        expand_limits(y = 0) +
        ggplot.theme

    }) |> bindCache(stressnet.db(), input$recency, input$chart_type)







    output$line_chart5 <- plotly::renderPlotly({

      data <- stressnet.db()$process_info

      data$cpu_time_user <- c(NA, diff(data$cpu_time_user)) / as.numeric(c(NA, diff(data$time)))
      data$cpu_time_user <- ifelse(data$cpu_time_user >= 0, data$cpu_time_user, NA)

      fig <- plotly::plot_ly(name = "30 seconds poll time", data = data, x = ~time, y = ~cpu_time_user, type = 'scatter',
        mode = 'lines', fill = 'tozeroy', fillcolor = adjustcolor(bs.colors["pink"], alpha.f = 0.85),
        line = list(color = bs.colors["pink"]) ) |>
        plotly::layout(xaxis = list(rangeslider = list(visible = TRUE)))

      fig <- plot.style(fig, "monerod's CPU load (user mode)")

      fig <- fig |>
        plotly::add_trace(name = "30-minute moving average", data = data, x = ~time,
          y = ~frollmean(cpu_time_user, 30 * 2, align = "center", na.rm = TRUE),
          type = 'scatter', mode = 'lines',  fill = '', line = list(color = bs.colors["blue"], width = 3))

      fig

    }) |> bindCache(stressnet.db(), input$recency, input$chart_type)



    output$static_line_chart5 <- shiny::renderPlot({

      data <- stressnet.db()$process_info

      data$cpu_time_user <- c(NA, diff(data$cpu_time_user)) / as.numeric(c(NA, diff(data$time)))
      data$cpu_time_user <- ifelse(data$cpu_time_user >= 0, data$cpu_time_user, NA)

      ggplot(data, aes(x = time, y = cpu_time_user)) +
        ggtitle("monerod's CPU load (user mode)") +
        geom_area(fill = adjustcolor(bs.colors["pink"], alpha.f = 0.85)) +
        expand_limits(y = 0) +
        ggplot.theme

    }) |> bindCache(stressnet.db(), input$recency, input$chart_type)



    output$line_chart6 <- plotly::renderPlotly({

      data <- stressnet.db()$process_info

      data$cpu_time_system <- c(NA, diff(data$cpu_time_system)) / as.numeric(c(NA, diff(data$time)))
      data$cpu_time_system <- ifelse(data$cpu_time_system >= 0, data$cpu_time_system, NA)

      fig <- plotly::plot_ly(name = "30 seconds poll time", data = data, x = ~time, y = ~cpu_time_system, type = 'scatter',
        mode = 'lines', fill = 'tozeroy', fillcolor = adjustcolor(bs.colors["pink"], alpha.f = 0.85),
        line = list(color = bs.colors["pink"]) ) |>
        plotly::layout(xaxis = list(rangeslider = list(visible = TRUE)))

      fig <- plot.style(fig, "monerod's CPU load (kernel mode)")

      fig <- fig |>
        plotly::add_trace(name = "30-minute moving average", data = data, x = ~time,
          y = ~frollmean(cpu_time_system, 30 * 2, align = "center", na.rm = TRUE),
          type = 'scatter', mode = 'lines',  fill = '', line = list(color = bs.colors["blue"], width = 3))

      fig

    }) |> bindCache(stressnet.db(), input$recency, input$chart_type)



    output$static_line_chart6 <- shiny::renderPlot({

      data <- stressnet.db()$process_info

      data$cpu_time_system <- c(NA, diff(data$cpu_time_system)) / as.numeric(c(NA, diff(data$time)))
      data$cpu_time_system <- ifelse(data$cpu_time_system >= 0, data$cpu_time_system, NA)

      ggplot(data, aes(x = time, y = cpu_time_system)) +
        ggtitle("monerod's CPU load (kernel mode)") +
        geom_area(fill = adjustcolor(bs.colors["pink"], alpha.f = 0.85)) +
        expand_limits(y = 0) +
        ggplot.theme

    }) |> bindCache(stressnet.db(), input$recency, input$chart_type)


    output$line_chart7 <- plotly::renderPlotly({

      data <- stressnet.db()$process_info

      fig <- plotly::plot_ly(name = "30 seconds poll time", data = data, x = ~time, y = ~mem_uss, type = 'scatter',
        mode = 'lines', fill = 'tozeroy', fillcolor = adjustcolor(bs.colors["pink"], alpha.f = 0.85),
        line = list(color = bs.colors["pink"]) ) |>
        plotly::layout(xaxis = list(rangeslider = list(visible = TRUE)),
          yaxis = list(tickformat = "~s"))

      fig <- plot.style(fig, "monerod's RAM (Unique Set Size)")

      fig

    })

    output$static_line_chart7 <- shiny::renderPlot({

      data <- stressnet.db()$process_info

      ggplot(data, aes(x = time, y = mem_uss)) +
        ggtitle("monerod's RAM (Unique Set Size)") +
        geom_area(fill = adjustcolor(bs.colors["pink"], alpha.f = 0.85)) +
        expand_limits(y = 0) +
        ggplot.theme

    }) |> bindCache(stressnet.db(), input$recency, input$chart_type)



    output$line_chart8 <- plotly::renderPlotly({

      data <- stressnet.db()$process_info

      fig <- plotly::plot_ly(name = "30 seconds poll time", data = data, x = ~time, y = ~mem_swap, type = 'scatter',
        mode = 'lines', fill = 'tozeroy', fillcolor = adjustcolor(bs.colors["pink"], alpha.f = 0.85),
        line = list(color = bs.colors["pink"]) ) |>
        plotly::layout(xaxis = list(rangeslider = list(visible = TRUE)),
          yaxis = list(tickformat = "~s"))

      fig <- plot.style(fig, "monerod's RAM (Swap)")

      fig

    }) |> bindCache(stressnet.db(), input$recency, input$chart_type)

    output$static_line_chart8 <- shiny::renderPlot({

      data <- stressnet.db()$process_info

      ggplot(data, aes(x = time, y = mem_swap)) +
        ggtitle("monerod's RAM (Swap)") +
        geom_area(fill = adjustcolor(bs.colors["pink"], alpha.f = 0.85)) +
        expand_limits(y = 0) +
        ggplot.theme

    }) |> bindCache(stressnet.db(), input$recency, input$chart_type)


    output$line_chart9 <- plotly::renderPlotly({

      data <- stressnet.db()$pool_stats

      fig <- plotly::plot_ly(name = "30 seconds poll time", data = data, x = ~time, y = ~rpc_response_time, type = 'scatter',
        mode = 'lines', fill = 'tozeroy', fillcolor = adjustcolor(bs.colors["pink"], alpha.f = 0.85),
        line = list(color = bs.colors["pink"]) ) |>
        plotly::layout(xaxis = list(rangeslider = list(visible = TRUE)))

      fig <- plot.style(fig, "Response time of get_transaction_pool_stats RPC call (seconds)")

      fig

    }) |> bindCache(stressnet.db(), input$recency, input$chart_type)

    output$static_line_chart9 <- shiny::renderPlot({

      data <- stressnet.db()$pool_stats

      ggplot(data, aes(x = time, y = rpc_response_time)) +
        ggtitle("Response time of get_transaction_pool_stats RPC call (seconds)") +
        geom_area(fill = adjustcolor(bs.colors["pink"], alpha.f = 0.85)) +
        expand_limits(y = 0) +
        ggplot.theme

    }) |> bindCache(stressnet.db(), input$recency, input$chart_type)






    output$corr_plot <- shiny::renderPlot({

      corr.data <- data.table::merge.data.table(stressnet.db()$pool_stats, stressnet.db()$process_info, by = "time")

      data.table::setDT(corr.data)

      corr.data[, cpu_time_user := c(NA, diff(cpu_time_user)) / as.numeric(c(NA, diff(time)))]
      corr.data[cpu_time_user < 0, cpu_time_user := NA]

      corr.data <- cor(corr.data[, .(txpool_bytes = bytes_total, diff_txpool_bytes = c(NA, diff(bytes_total)),
        cpu_time_user = cpu_time_user,
        diff_mem_uss = c(NA, diff(mem_uss)))], use = "pairwise.complete.obs")



      ggcorrplot::ggcorrplot(corr.data,
      type = "upper", lab = TRUE, lab_size = 8,
        title = "Correlation",
      colors = c(bs.colors[["pink"]], bs.colors[["gray-500"]], bs.colors[["green"]]),
      legend.title = "Correlation",
        ggtheme = ggplot2::theme(
          title = element_text(color = bs.colors["cyan"]),
          plot.background = element_rect(fill = bs.colors["dark"], colour = bs.colors["dark"]),
          panel.background = element_rect(fill = bs.colors["dark"], colour = bs.colors["dark"])
        )
        ) + # ggplot2::theme() ggplot2::theme(bslib::bs_theme(preset = "vapor"))
      scale_x_discrete(position = "top") +
        theme(axis.text.x = element_text(size = 20, angle = 25, hjust = 0, color = bs.colors["cyan"]),
          axis.text.y = element_text(size = 20, color = bs.colors["cyan"]),
          plot.title = element_text(size = 40, color = bs.colors["cyan"]),
          plot.subtitle = element_text(size = 20, color = bs.colors["cyan"]),
          legend.title = element_text(size = 20, color = bs.colors["cyan"]),
          legend.text = element_text(size = 20, color = bs.colors["cyan"]),
          legend.key.size = unit(30, "pt"),
          legend.background = element_rect(fill = bs.colors["dark"]),
          plot.margin = margin(t = 0, r = 0, b = 75, l = 0, unit = "pt"),
          plot.background = element_rect(color = NA))



    }, height = 718, width = 800) |> bindCache(stressnet.db(), input$recency, input$chart_type,
      sizePolicy = sizeGrowthRatio(width = 800, height = 718, growthRate = 1)
)
    # Height is strange because there is are white bars on top/bottom
    # or left/right otherwise



# }, domain = NULL)



}
