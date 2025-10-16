#!/usr/bin/env Rscript

# library(argparser)

p <- argparser::arg_parser("collect-monerod-info")

p <- argparser::add_argument(p, "--rpcmonerod", default = "http://127.0.0.1:28081",
  help = "URL of unrestricted monerod RPC.")

p <- argparser::add_argument(p, "--net", default = "testnet",
  help = "'testnet' or 'mainnet'.")

p <- argparser::add_argument(p, "--pidmonerod", default = NA,
  help = "Process ID of monerod. Only necessary if multiple monerod instances on a given network (i.e. mainnet/testnet) are running.")

p <- argparser::add_argument(p, "--collectioninterval", default = 30,
  help = "Number of seconds to wait between data collection requests.")

argv <- argparser::parse_args(p)

url.rpc <- argv$rpcmonerod
check.testnet.flags <- argv$net == "testnet"
pid.monerod <- argv$pidmonerod
poll.interval <- argv$collectioninterval
# in seconds

con <- DBI::dbConnect(RSQLite::SQLite(), "data/xmr-stressnet-diagnostics.db")
DBI::dbExecute(con, "PRAGMA journal_mode=WAL;")
# Can read while writing
# https://stackoverflow.com/questions/15143871/simplest-way-to-retry-sqlite-query-if-db-is-locked


# Modified from TownforgeR::tf_rpc_curl function
xmr.rpc <- function(
    url.rpc = "http://127.0.0.1:18081/json_rpc",
  method = "",
  params = list(),
  userpwd = "",
  num.as.string = FALSE,
  nonce.as.string = FALSE,
  keep.trying.rpc = FALSE,
  ...
){

  json.ret <- RJSONIO::toJSON(
    list(
      jsonrpc = "2.0",
      id = "0",
      method = method,
      params = params
    ), digits = 50
  )

  rcp.ret <- 	tryCatch(RCurl::postForm(url.rpc,
    .opts = list(
      userpwd = userpwd,
      postfields = json.ret,
      httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')
      # https://stackoverflow.com/questions/19267261/timeout-while-reading-csv-file-from-url-in-r
    )
  ), error = function(e) {NULL})

  if (keep.trying.rpc && length(rcp.ret) == 0) {
    while (length(rcp.ret) == 0) {
      rcp.ret <- 	tryCatch(RCurl::postForm(url.rpc,
        .opts = list(
          userpwd = userpwd,
          postfields = json.ret,
          httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')
          # https://stackoverflow.com/questions/19267261/timeout-while-reading-csv-file-from-url-in-r
        )
      ), error = function(e) {NULL})
    }
  }

  if (is.null(rcp.ret)) {
    stop("Cannot connect to monerod. Is monerod running?")
  }

  if (num.as.string) {
    rcp.ret <- gsub("(: )([-0123456789.]+)([,\n\r])", "\\1\"\\2\"\\3", rcp.ret )
  }

  if (nonce.as.string & ! num.as.string) {
    rcp.ret <- gsub("(\"nonce\": )([-0123456789.]+)([,\n\r])", "\\1\"\\2\"\\3", rcp.ret )
  }

  RJSONIO::fromJSON(rcp.ret) # , simplify = FALSE
}


# Check that node is responding

info <- xmr.rpc(paste0(url.rpc, "/json_rpc"), method = "get_info")$result

if (info$restricted) {
  stop("RPC is in restricted mode. Possible solution: remove '--restricted-rpc' monerod flag.")
}



while (TRUE) {

  compute.time <- system.time({


    poll.time <- as.character(as.numeric(Sys.time()))

    get.pool_stats <- function() {

      rpc.response.time <- system.time({
        transaction_pool_stats <- xmr.rpc(paste0(url.rpc, "/get_transaction_pool_stats"))
      }, gcFirst = FALSE)[["elapsed"]]

      if (transaction_pool_stats$pool_stats[["bytes_total"]] != 0 &&
          !is.atomic(transaction_pool_stats$pool_stats)) {
        histo <- transaction_pool_stats$pool_stats$histo
        histo.names <- paste0("histo_", formatC(seq_along(histo), width = 2, flag = "0"))
        histo <- matrix(unlist(histo), ncol = 2, byrow = TRUE)
      } else {
        histo.names <- "histo_01"
        histo <- matrix(c(0, 0), ncol = 2, byrow = TRUE)
        # RPC gives different structure if there are no txs in the pool
      }


      colnames(histo) <- c("bytes", "txs")
      histo <- cbind(time = poll.time, histo_num = histo.names, as.data.frame(histo))

      suppressWarnings(transaction_pool_stats$pool_stats$histo <- NULL)
      # Occasionally, see this warning message:
      # In transaction_pool_stats$pool_stats$histo <- NULL : Coercing LHS to a list

      list(pool_stats = cbind(time = poll.time, as.data.frame(transaction_pool_stats$pool_stats),
            rpc_response_time = rpc.response.time),
        histo = histo)

    }

    pool_stats <- tryCatch(get.pool_stats(), error = function(e) NULL)

    if (length(pool_stats) == 0) {
      message(base::date(), " No connection to node. Retrying in ", poll.interval, " seconds.")
      Sys.sleep(poll.interval)
      next
    }


    get.info <- function() {

      info <- xmr.rpc(paste0(url.rpc, "/json_rpc"), method = "get_info")$result

      cbind(time = poll.time, as.data.frame(info))

    }

    info <- tryCatch(get.info(), error = function(e) NULL)

    if (length(info) == 0) {
      message(base::date(), " No connection to node. Retrying in ", poll.interval, " seconds.")
      Sys.sleep(poll.interval)
      next
    }


    get.last_block_header <- function() {

      last_block_header <- xmr.rpc(paste0(url.rpc, "/json_rpc"), method = "get_last_block_header")$result

      cbind(time = poll.time, as.data.frame(last_block_header$block_header))

    }

    last_block_header <- tryCatch(get.last_block_header(), error = function(e) NULL)

    if (length(last_block_header) == 0) {
      message(base::date(), " No connection to node. Retrying in ", poll.interval, " seconds.")
      Sys.sleep(poll.interval)
      next
    }


    get.fee_estimate <- function() {

      fee_estimate <- xmr.rpc(paste0(url.rpc, "/json_rpc"), method = "get_fee_estimate")$result

      fee_estimate.tiers <- fee_estimate$fees
      fee_estimate$fees <- NULL
      fee_estimate.tiers <- as.data.frame(as.list(fee_estimate.tiers))
      colnames(fee_estimate.tiers) <- paste0("fee_tier_", 1:4)

      cbind(time = poll.time, fee = fee_estimate$fee, fee_estimate.tiers)

    }



    fee_estimate <- tryCatch(get.fee_estimate(), error = function(e) NULL)

    if (length(fee_estimate) == 0) {
      message(base::date(), " No connection to node. Retrying in ", poll.interval, " seconds.")
      Sys.sleep(poll.interval)
      next
    }


    get.connections <- function() {

      connections <- xmr.rpc(paste0(url.rpc, "/json_rpc"), method = "get_connections")$result

      if (length(connections$connections) > 0) {

        connections <- do.call(rbind, lapply(connections$connections, as.data.frame))

      } else {
        # If there are no connections, make a data frame with a missing row
        connections <- data.frame(address = NA_character_,
          address_type = NA_real_, avg_download = NA_real_, avg_upload = NA_real_,
          connection_id = NA_character_, current_download = NA_real_,
          current_upload = NA_real_, height = NA_real_, host = NA_character_,
          incoming = NA, ip = NA_character_, live_time = NA_real_,
          local_ip = NA, localhost = NA, peer_id = NA_character_, port = NA_character_,
          pruning_seed = NA_real_, recv_count = NA_real_, recv_idle_time = NA_real_,
          rpc_credits_per_hash = NA_real_, rpc_port = NA_real_, send_count = NA_real_,
          send_idle_time = NA_real_, state = NA_character_, support_flags = NA_real_)
      }

      cbind(time = poll.time, connections)

    }

    connections <- tryCatch(get.connections(), error = function(e) NULL)

    if (length(connections) == 0) {
      message(base::date(), " No connection to node. Retrying in ", poll.interval, " seconds.")
      Sys.sleep(poll.interval)
      next
    }



    get.bans <- function() {

      bans <- xmr.rpc(paste0(url.rpc, "/json_rpc"), method = "get_bans")$result

      if (length(bans$bans) > 0) {

        bans <- do.call(rbind, lapply(bans$bans, as.data.frame))

      } else {
        # If there are no bans, make a data frame with a missing row
        bans <- data.frame(host = NA_character_,
          ip = NA_real_, seconds = NA_real_)
      }

      cbind(time = poll.time, bans)

    }

    bans <- tryCatch(get.bans(), error = function(e) NULL)

    if (length(bans) == 0) {
      message(base::date(), " No connection to node. Retrying in ", poll.interval, " seconds.")
      Sys.sleep(poll.interval)
      next
    }



    get.process_info <- function() {

      if (! is.na(pid.monerod)) {
        monerod.proc <- tryCatch(ps::ps_handle(pid = pid.monerod), error = function(e) NULL)
        if (length(monerod.proc) == 0) {
          stop("You supplied pid ", pid.monerod, " as the monerod pid. No process with that pid is running.")
        }
      } else {

        processes <- ps::ps()

        monerod.proc <- processes$ps_handle[processes$name %in% "monerod"]
        # Rarely, "name" will be NA for some process. "%in%" instead of "==" avoids problems
        # when "name" is NA.

        if (check.testnet.flags) {

          if (length(monerod.proc) == 0) {
            stop("No process called 'monerod' is running.")
          }

          cmdline.flags <- lapply(monerod.proc, FUN = function(x) {
            y <- ps::ps_cmdline(x)
            data.frame(testnet = "--testnet" %in% y, non.default.p2p.port = any(grepl("--p2p-bind-port", y)))
          })

          cmdline.flags <- do.call(rbind, cmdline.flags)

          monerod.proc <- monerod.proc[which(cmdline.flags$testnet)]

          if (length(monerod.proc) == 0) {
            stop("No 'monerod' process with '--testnet' is running.")
          }

          if (length(monerod.proc) > 1) {
            monerod.proc <- monerod.proc[which(cmdline.flags$non.default.p2p.port)]
          }
          # If there are more than one testnet monerod processes running, at most
          # one of them can have the default p2p port

          if (length(monerod.proc) > 1) {
            stop("More than one 'monerod' process with '--testnet' is running with non-default ports. Cannot determine the intended node process. Please specify the pid with --pidmonerod command line argument to collect-monerod-info.R")
          }

        }

        stopifnot(length(monerod.proc) == 1)
        # Can only have exactly one monerod process
        monerod.proc <- monerod.proc[[1]]

      }

      cpu_times <- as.data.frame(as.list(ps::ps_cpu_times(monerod.proc)))
      colnames(cpu_times) <- paste0("cpu_time_", colnames(cpu_times))

      num_threads <- ps::ps_num_threads(monerod.proc)

      memory_info <- as.data.frame(as.list(ps::ps_memory_full_info(monerod.proc)))
      colnames(memory_info) <- paste0("mem_", colnames(memory_info))

      process_info <- cbind(time = poll.time, cpu_times, num_threads = num_threads, memory_info)

      process_info

    }

    process_info <- tryCatch(get.process_info(), error = function(e) NULL)

    if (length(process_info) == 0) {
      message(base::date(), " No connection to node. Retrying in ", poll.interval, " seconds.")
      Sys.sleep(poll.interval)
      next
    }




    pool_stats.statement <- DBI::dbSendQuery(con,
      "INSERT INTO pool_stats VALUES (:time,:bytes_max,:bytes_med,:bytes_min,:bytes_total,:fee_total,:histo_98pc,:num_10m,:num_double_spends,:num_failing,:num_not_relayed,:oldest,:txs_total,:rpc_response_time)")
    DBI::dbBind(pool_stats.statement, params = pool_stats$pool_stats)
    DBI::dbClearResult(pool_stats.statement)

    pool_stats_histo.statement <- DBI::dbSendQuery(con,
      "INSERT INTO pool_stats_histo VALUES (:time,:histo_num,:bytes,:txs)")
    DBI::dbBind(pool_stats_histo.statement, params = pool_stats$histo)
    DBI::dbClearResult(pool_stats_histo.statement)


    info.statement <- DBI::dbSendQuery(con,
      "INSERT INTO info VALUES (:time,:adjusted_time,:alt_blocks_count,:block_size_limit,:block_size_median,:block_weight_limit,:block_weight_median,:bootstrap_daemon_address,:busy_syncing,:credits,:cumulative_difficulty,:cumulative_difficulty_top64,:database_size,:difficulty,:difficulty_top64,:free_space,:grey_peerlist_size,:height,:height_without_bootstrap,:incoming_connections_count,:mainnet,:nettype,:offline,:outgoing_connections_count,:restricted,:rpc_connections_count,:stagenet,:start_time,:status,:synchronized,:target,:target_height,:testnet,:top_block_hash,:top_hash,:tx_count,:tx_pool_size,:untrusted,:update_available,:version,:was_bootstrap_ever_used,:white_peerlist_size,:wide_cumulative_difficulty,:wide_difficulty)")
    DBI::dbBind(info.statement, params = info)
    DBI::dbClearResult(info.statement)

    last_block_header.statement <- DBI::dbSendQuery(con,
      "INSERT INTO last_block_header VALUES (:time,:block_size,:block_weight,:cumulative_difficulty,:cumulative_difficulty_top64,:depth,:difficulty,:difficulty_top64,:hash,:height,:long_term_weight,:major_version,:miner_tx_hash,:minor_version,:nonce,:num_txes,:orphan_status,:pow_hash,:prev_hash,:reward,:timestamp,:wide_cumulative_difficulty,:wide_difficulty)")
    DBI::dbBind(last_block_header.statement, params = last_block_header)
    DBI::dbClearResult(last_block_header.statement)


    fee_estimate.statement <- DBI::dbSendQuery(con,
      "INSERT INTO fee_estimate VALUES (:time,:fee,:fee_tier_1,:fee_tier_2,:fee_tier_3,:fee_tier_4)")
    DBI::dbBind(fee_estimate.statement, params = fee_estimate)
    DBI::dbClearResult(fee_estimate.statement)


    connections.statement <- DBI::dbSendQuery(con,
      "INSERT INTO connections VALUES (:time,:address,:address_type,:avg_download,:avg_upload,:connection_id,:current_download,:current_upload,:height,:host,:incoming,:ip,:live_time,:local_ip,:localhost,:peer_id,:port,:pruning_seed,:recv_count,:recv_idle_time,:rpc_credits_per_hash,:rpc_port,:send_count,:send_idle_time,:state,:support_flags)")
    DBI::dbBind(connections.statement, params = connections)
    DBI::dbClearResult(connections.statement)


    connections.statement <- DBI::dbSendQuery(con,
      "INSERT INTO connections VALUES (:time,:address,:address_type,:avg_download,:avg_upload,:connection_id,:current_download,:current_upload,:height,:host,:incoming,:ip,:live_time,:local_ip,:localhost,:peer_id,:port,:pruning_seed,:recv_count,:recv_idle_time,:rpc_credits_per_hash,:rpc_port,:send_count,:send_idle_time,:state,:support_flags)")
    DBI::dbBind(connections.statement, params = connections)
    DBI::dbClearResult(connections.statement)


    bans.statement <- DBI::dbSendQuery(con,
      "INSERT INTO bans VALUES (:time,:host,:ip,:seconds)")
    DBI::dbBind(bans.statement, params = bans)
    DBI::dbClearResult(bans.statement)


    process_info.statement <- DBI::dbSendQuery(con,
      "INSERT INTO process_info VALUES (:time,:cpu_time_user,:cpu_time_system,:cpu_time_children_user,:cpu_time_children_system,:num_threads,:mem_rss,:mem_vms,:mem_shared,:mem_text,:mem_lib,:mem_data,:mem_dirty,:mem_maxrss,:mem_uss,:mem_pss,:mem_swap)")
    DBI::dbBind(process_info.statement, params = process_info)
    DBI::dbClearResult(process_info.statement)


  }, gcFirst = FALSE)
  # Do gcFirst = FALSE because gc() time would not be counted in the compute.time
  message(Sys.time(), " Data collection completed in ",
    round(compute.time[["elapsed"]], 4), " seconds")
  Sys.sleep(max(c(0, poll.interval - compute.time["elapsed"])))
  # Should poll once per poll.interval unless data processing takes more than poll.interval seconds. In
  # that case, polls as frequently as possible.
}















