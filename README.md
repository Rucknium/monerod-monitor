# monerod-monitor

Data collection and visualization of a Monero node process. Useful for monitoring Monero [stressnet](https://monero.town/post/6763165) tests.

## Local deployment

`monerod-monitor` only works on Linux. Deployment on Windows and macOS requires changing how the code handles CPU and RAM consumption data.

You need to have `monerod` running on your machine and its unrestricted RPC available for queries. By default, the local URL of the testnet unrestricted RPC is `http://127.0.0.1:28081`. Mainnet is `http://127.0.0.1:18081`.

Install [R](https://www.r-project.org/). Linux users should install the `r-base` and `r-base-dev` system packages.

Clone this repo into a directory on your local machine with:

``` bash
git clone https://github.com/Rucknium/monerod-monitor.git
```

Go into the `monerod-monitor` directory by inputting `cd monerod-monitor`. Then start an R session by inputting `R` into the terminal. Install the required package dependencies:

```R
install.packages(c("argparser", "DBI", "RSQLite", "RJSONIO", 
  "RCurl", "ps", "shiny", "bslib", "cachem", "data.table",
  "ggplot2", "scales", "plotly", "ggcorrplot"), Ncpus = 4)
```

If you have greater or fewer than 4 threads available on your machine, you can adjust the `Ncpus` argument. On Linux, compilation of all the package dependencies may take some time. Close the R session after it is finished installing by inputting `quit(save = "no")`.

Next, initialize the database. While inside the `monerod-monitor` directory in your terminal, input:

```bash
Rscript inst/initialize-db.R
```

The "Database initialization complete" message should appear.

Next, you will start the continuous data collection process. It is useful to start a new `screen` for the process or just keep the terminal window open. While inside the `monerod-monitor` directory in your terminal, input:

```bash
Rscript inst/collect-monerod-info.R --rpcmonerod http://127.0.0.1:28081 --net testnet
```

If your unrestricted `monerod` RPC port is different from `http://127.0.0.1:28081`, change it.  The script also has the optional `--pidmonerod` flag that can be used if there are multiple `monerod` instances running on your machine on a given network (i.e. mainnet/testnet). If you use `--pidmonerod`, don't forget to update it if you restart `monerod`. The `--collectioninterval` flag can be used to change the collection interval from the default 30 seconds. To change it to every 10 seconds, for example, use `--collectioninterval 10`.

You should see messages about data collection being successful every 30 seconds.

Wait a few minutes for some data to be collected. Then in a new terminal window inside the `monerod-monitor` directory, start a new R session by inputting `R` and then input:

```R
shiny::runApp()
```

A window of your default browser should automatically launch and display the monitor info. If you do not want to automatically launch a window of your default browser, instead input:

```R
shiny::runApp(launch.browser = FALSE)
```

A local URL should print: `Listening on...`. You can view the monitor by pasting the URL into your browser's address bar.

