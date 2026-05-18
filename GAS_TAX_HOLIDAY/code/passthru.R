# ============================================================
# COUNTY-LEVEL GAS TAX PASS-THROUGH
#
# Inputs:
#   county_passthrough.csv      — AAA county gas prices (cols A-H)
#   psw05.xls                   — EIA WPSR Table 5, gasoline stocks
#   psw02.xls                   — EIA WPSR Table 2, refinery utilization (all 5 PADDs)
#
# Output:
#   county_passthrough_final.xlsx
# ============================================================

library(tidyverse)
library(readxl)
library(writexl)

# ── 2. Refinery utilization by PADD ──────────────────────────────────
# Source: psw02.xls — EIA WPSR Table 2, Data 1 sheet
# Series: W_NA_YUP_R{1-5}0_PER — weekly % utilization, all 5 PADDs
cat("Reading refinery utilization from psw02.xls...\n")

psw02_all   <- read_xls("C:/Users/kchanwong/Downloads/psw02.xls", sheet = "Data 1",
                        col_names = TRUE, col_types = "text", skip = 2)
util_data   <- psw02_all[-(1:2), ]                     # drop sourcekey + label rows
util_cols   <- 21:25                                   # PADD 1-5 utilization % columns

util_raw <- map_dfr(seq_along(util_cols), function(i) {
  tibble(
    period = as.Date(as.numeric(util_data[[1]]),        # Excel serial dates
                     origin = "1899-12-30"),
    padd   = i,
    util   = as.numeric(util_data[[util_cols[i]]])
  )
}) |> filter(!is.na(period), !is.na(util))

util_flags <- util_raw |>
  group_by(padd) |>
  slice_max(period, n = 1) |>
  ungroup() |>
  left_join(util_raw |> group_by(padd) |>
              summarise(median_util = median(util), .groups = "drop"),
            by = "padd") |>
  mutate(hi_util = as.integer(util >= median_util)) |>
  select(padd, hi_util)

cat("  Utilization flags:\n"); print(util_flags)
### 3

read_xls("C:/Users/kchanwong/Downloads/psw05.xls", sheet = "Data 1")
psw_all  <- read_xls("C:/Users/kchanwong/Downloads/psw05.xls", sheet = "Data 1", skip = 2,
                     col_names = TRUE, col_types = "text")
data_raw   <- psw_all              # drop label row (col names = sourcekeys after skip)
stock_cols <- 3:7                           # PADD 1-5 total gasoline stocks
stocks_raw <- map_dfr(seq_along(stock_cols), function(i) {
  tibble(
    period = as.Date(as.numeric(data_raw[[1]]), origin = "1899-12-30"),
    padd   = i,
    stocks = as.numeric(data_raw[[stock_cols[i]]])
  )
}) |> filter(!is.na(period), !is.na(stocks))
inv_flags <- stocks_raw |>
  group_by(padd) |>
  slice_max(period, n = 1) |>
  ungroup() |>
  left_join(stocks_raw |> group_by(padd) |>
              summarise(q25 = quantile(stocks, 0.25), .groups = "drop"),
            by = "padd") |>
  mutate(lo_inv = as.integer(stocks < q25)) |>
  select(padd, lo_inv)

cat("  Inventory flags:\n"); print(inv_flags)


# ── 4. PADD flags — full_join so PADD 2 lo_inv is not dropped ─────────
padd_flags <- full_join(util_flags, inv_flags, by = "padd")
cat("\nPADD flags:\n"); print(padd_flags)



# ── 5. RFG FIPS set ───────────────────────────────────────────────────
rfg_t1 <- c(
  6037,6059,6111,6071,6065,6073,
  9001,9003,9005,9007,9009,9011,9013,9015,
  10001,10003,
  17031,17043,17063,17089,17093,17097,17111,17197,
  18089,18127,
  24003,24005,24013,24015,24025,24027,24510,
  34003,34013,34017,34019,34023,34025,34027,34029,34031,34035,34037,34039,
  34005,34007,34011,34015,34021,34033,
  36005,36047,36059,36061,36071,36079,36081,36085,36087,36103,36119,
  42017,42029,42045,42091,42101,
  48039,48071,48157,48167,48201,48291,48339,48473,
  55059,55079,55089,55101,55131,55133
)
rfg_t2 <- c(
  6029,
  8001,8005,8013,8014,8031,8035,8059,
  48085,48113,48121,48139,48251,48257,48367,48397,48439,48497
)
rfg_optin <- c(
  6017,6061,6067,6095,6113,
  6019,6031,6039,6047,6077,6099,6107,
  17119,17163,
  21111,
  33011,33013,33015,33017,
  36027,
  51036,51041,51085,51087,51095,51199,51760
)
rfg_fips <- unique(c(rfg_t1, rfg_t2, rfg_optin))


# ── 6. PADD crosswalk ─────────────────────────────────────────────────
padd_states <- tibble(
  state = c(
    "CT","DC","DE","FL","GA","MA","MD","ME","NC","NH","NJ","NY",
    "PA","RI","SC","VA","VT","WV",
    "IA","IL","IN","KS","KY","MI","MN","MO","ND","NE","OH","OK","SD","TN","WI",
    "AL","AR","LA","MS","NM","TX",
    "CO","ID","MT","UT","WY",
    "AK","AZ","CA","HI","NV","OR","WA"
  ),
  padd = c(rep(1L,18), rep(2L,15), rep(3L,6), rep(4L,5), rep(5L,7))
)


# ── 7. Build county dataset ───────────────────────────────────────────
county <- read_csv("county_passthru.csv", show_col_types = FALSE) |>
  select(state, county, regular, fips, fips_county_name,
         match_score, match_method, county_lower)

county_out <- county |>
  mutate(rfg = as.integer(fips %in% rfg_fips)) |>
  left_join(padd_states, by = "state") |>
  left_join(padd_flags,  by = "padd") |>
  mutate(
    pt_rfg               = if_else(rfg    == 1, PT_RFG_YES, PT_RFG_NO),
    pt_inv               = if_else(lo_inv == 1, PT_INV_LO,  PT_INV_HI),
    passthrough          = (pt_rfg + pt_inv) / 2,
    consumer_savings_cpg = passthrough * FED_TAX_CPG,
    foregone_cpg         = (1 - passthrough) * FED_TAX_CPG
  ) |>
  select(state, county, regular, fips, fips_county_name,
         match_score, match_method, county_lower,
         padd, rfg, hi_util, lo_inv,
         pt_rfg, pt_inv, passthrough,
         consumer_savings_cpg, foregone_cpg)


# ── 8. Diagnostics ────────────────────────────────────────────────────
cat("\n── PASS-THROUGH COMBINATIONS ────────────────────────────────\n")
county_out |>
  group_by(rfg, lo_inv) |>
  summarise(n = n(), passthrough = first(passthrough),
            savings_cpg = first(consumer_savings_cpg), .groups = "drop") |>
  arrange(rfg, lo_inv) |>
  print()

cat(sprintf("\nPass-through range: %.4f \u2013 %.4f\n",
            min(county_out$passthrough, na.rm = TRUE),
            max(county_out$passthrough, na.rm = TRUE)))
cat(sprintf("NA lo_inv: %d\n", sum(is.na(county_out$lo_inv))))

write_xlsx(county_out, "county_passthrough_final.xlsx")
cat("Saved \u2192 county_passthrough_final.xlsx\n")