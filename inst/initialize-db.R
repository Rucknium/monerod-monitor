

con <- DBI::dbConnect(RSQLite::SQLite(), "data/xmr-stressnet-diagnostics.db")
DBI::dbExecute(con, "PRAGMA journal_mode=WAL;")
# Can read while writing
# https://stackoverflow.com/questions/15143871/simplest-way-to-retry-sqlite-query-if-db-is-locked



DBI::dbExecute(con, "CREATE TABLE pool_stats (
time TEXT,
bytes_max REAL,
bytes_med REAL,
bytes_min REAL,
bytes_total REAL,
fee_total REAL,
histo_98pc REAL,
num_10m REAL,
num_double_spends REAL,
num_failing REAL,
num_not_relayed REAL,
oldest REAL,
txs_total REAL
)")

DBI::dbExecute(con, "ALTER TABLE pool_stats ADD COLUMN rpc_response_time REAL")



DBI::dbExecute(con, "CREATE TABLE pool_stats_histo (
time TEXT,
histo_num TEXT,
bytes REAL,
txs REAL
)")


DBI::dbExecute(con, "CREATE TABLE info (
time TEXT,
adjusted_time TEXT,
alt_blocks_count REAL,
block_size_limit REAL,
block_size_median REAL,
block_weight_limit REAL,
block_weight_median REAL,
bootstrap_daemon_address REAL,
busy_syncing REAL,
credits REAL,
cumulative_difficulty REAL,
cumulative_difficulty_top64 REAL,
database_size REAL,
difficulty REAL,
difficulty_top64 REAL,
free_space REAL,
grey_peerlist_size REAL,
height TEXT,
height_without_bootstrap REAL,
incoming_connections_count REAL,
mainnet REAL,
nettype REAL,
offline REAL,
outgoing_connections_count REAL,
restricted REAL,
rpc_connections_count REAL,
stagenet REAL,
start_time REAL,
status REAL,
synchronized REAL,
target REAL,
target_height REAL,
testnet REAL,
top_block_hash TEXT,
top_hash REAL,
tx_count REAL,
tx_pool_size REAL,
untrusted REAL,
update_available REAL,
version REAL,
was_bootstrap_ever_used REAL,
white_peerlist_size REAL,
wide_cumulative_difficulty REAL,
wide_difficulty REAL
)")

DBI::dbExecute(con, "CREATE TABLE last_block_header (
time TEXT,
block_size REAL,
block_weight REAL,
cumulative_difficulty REAL,
cumulative_difficulty_top64 REAL,
depth REAL,
difficulty REAL,
difficulty_top64 REAL,
hash TEXT,
height REAL,
long_term_weight REAL,
major_version REAL,
miner_tx_hash TEXT,
minor_version REAL,
nonce REAL,
num_txes REAL,
orphan_status REAL,
pow_hash TEXT,
prev_hash TEXT,
reward REAL,
timestamp REAL,
wide_cumulative_difficulty TEXT,
wide_difficulty TEXT
)")



DBI::dbExecute(con, "CREATE TABLE fee_estimate (
time TEXT,
fee REAL,
fee_tier_1 REAL,
fee_tier_2 REAL,
fee_tier_3 REAL,
fee_tier_4 REAL
)")




DBI::dbExecute(con, "CREATE TABLE connections (
time TEXT,
address TEXT,
address_type REAL,
avg_download REAL,
avg_upload REAL,
connection_id TEXT,
current_download REAL,
current_upload REAL,
height REAL,
host TEXT,
incoming REAL,
ip TEXT,
live_time REAL,
local_ip REAL,
localhost REAL,
peer_id TEXT,
port TEXT,
pruning_seed REAL,
recv_count REAL,
recv_idle_time REAL,
rpc_credits_per_hash REAL,
rpc_port REAL,
send_count REAL,
send_idle_time REAL,
state TEXT,
support_flags REAL
)")


DBI::dbExecute(con, "CREATE TABLE bans (
time TEXT,
host TEXT,
ip REAL,
seconds REAL
)")



DBI::dbExecute(con, "CREATE TABLE process_info (
time TEXT,
cpu_time_user REAL,
cpu_time_system REAL,
cpu_time_children_user REAL,
cpu_time_children_system REAL,
num_threads REAL,
mem_rss REAL,
mem_vms REAL,
mem_shared REAL,
mem_text REAL,
mem_lib REAL,
mem_data REAL,
mem_dirty REAL,
mem_uss REAL,
mem_pss REAL,
mem_swap REAL
)")



