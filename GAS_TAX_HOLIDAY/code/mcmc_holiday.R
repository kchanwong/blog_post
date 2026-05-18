# ============================================================
# HIGHWAY TRUST FUND (TOTAL) — Baseline + Gas Tax Holiday
#
# Models Highway Account and Transit Account SEPARATELY,
# exactly matching CBO Jan 2025 baseline table.
#
# Transit Account: floors at $0 when exhausted (FTA
#   constrains payments; net flow = 0 once insolvent).
# Highway Account: policy-relevant insolvency event (~Mar 2028).
# Combined balance = Highway + Transit (for charts).
#
# CBO Jan 2025 source:
#   https://www.cbo.gov/system/files/2025-01/51300-2025-01-highwaytrustfund.pdf
#
# Holiday: Jun/Jul/Aug FY2026 Highway receipts = $0.
#   Mass Transit receipts unaffected (funded by separate
#   gas tax allocation + flexed transfer).
#
# ARTICLE CLAIMS TRACED IN THIS SCRIPT:
#   [CLAIM-1] Baseline median insolvency: March 2028
#             → Section 10 cat() output: med_base
#   [CLAIM-2] Insolvency range: December 2027 – June 2028
#             → Section 10 cat() output: 5th and 95th pctiles of base_dates
#   [CLAIM-3] Holiday moves insolvency to October 2027
#             → Section 10 cat() output: med_policy
#   [CLAIM-4] That is ~5 months earlier than baseline
#             → Section 10 cat() output: days_shift / 30.4
#   [CLAIM-5] September is historically the highest-revenue month
#             → Section 4b: monthly_rcpt_by_month
#   [CLAIM-6] Table 1 deficit ranges by scenario
#             → Section 4c: table1 (low = ×0.75 JCT offset, high = ×1.00)
# ============================================================

library(tidyverse)
library(lubridate)
library(scales)

# ── 0. Config ──────────────────────────────────────────────────────────
FE1_LONG       <- "C:/Users/kchanwong/Downloads/fe1_long.csv"
HOLIDAY_MONTHS <- c("June", "July", "August", "September", "October", "November")
N_SIM          <- 50000L
N_PATHS        <- 2000L
SEED           <- 69420L


# ── CBO Jan 2025 — Highway Account (millions) ─────────────────────────
CBO_H <- list(
  fy2026 = list(start = 56280, rcpt = 39068, out = 61353,
                flexed = -1200, interest = 1328),
  fy2027 = list(start = 34122, rcpt = 38962, out = 62259,
                flexed = -1200, interest =  530),
  fy2028 = list(start = 10156, rcpt = 38643, out = 63333,
                flexed = -1200, interest =   17)
)

# ── CBO Jan 2025 — Transit Account (millions) ─────────────────────────
# Flexed = +1200: receives the $1.2B transfer FROM Highway each year.
# FY2028 start ≈ 0 (Transit exhausts during FY2027 per CBO;
#   cumulative shortfall = -$1,124M in FY2027 → floored at 0).
CBO_T <- list(
  fy2026 = list(start = 18688, rcpt = 5105, out = 16111,
                flexed = 1200, interest = 419),
  fy2027 = list(start =  9301, rcpt = 5020, out = 16741,
                flexed = 1200, interest =  95),
  fy2028 = list(start =     0, rcpt = 4897, out = 17234,
                flexed = 1200, interest =   0)
)

# Combined CBO anchors (Highway + Transit, for charts)
cbo_anchors <- tibble(
  date    = as.Date(c("2025-10-01", "2026-10-01", "2027-10-01")),
  balance = c(
    CBO_H$fy2026$start + CBO_T$fy2026$start,
    CBO_H$fy2027$start + CBO_T$fy2027$start,
    CBO_H$fy2028$start + CBO_T$fy2028$start
  ) / 1e3
)


# ── 1. Closing balance — combined historical chart ─────────────────────
closing_balance <- read.csv(FE1_LONG) |>
  filter(value > 0, field == "closing_balance") |>
  group_by(date) |>
  summarise(value = sum(value), .groups = "drop") |>
  mutate(date = as.Date(date, format = "%m/%d/%Y")) |>
  arrange(date)


# ── 2. Load bootstrap data ─────────────────────────────────────────────
# Highway: use Highway Account historical distributions.
# Transit: use Transit Account but exclude extraordinary GF bailout years
#   (FY2010: $14.7B, FY2016: FAST Act, FY2021: IIJA $28B).
#   These one-time transfers inflate receipt distributions far above
#   what CBO projects going forward.
long <- read_csv(FE1_LONG, show_col_types = FALSE) |>
  mutate(date  = as.Date(date),
         value = as.numeric(value))

# Highway bootstrap
monthly_h <- long |>
  filter(account == "Highway",
         field %in% c("net_tax_receipts", "outlay_htf", "closing_balance")) |>
  group_by(fiscal_year, date, month, year, field) |>
  summarise(value = sum(value), .groups = "drop") |>
  pivot_wider(id_cols    = c(fiscal_year, date, month, year),
              names_from  = field,
              values_from = value) |>
  filter(!is.na(net_tax_receipts), !is.na(outlay_htf),
         closing_balance > 0) |>
  arrange(date)

# Transit bootstrap — exclude fiscal years with extraordinary GF transfers
TRANSIT_EXCLUDE_FY <- c(2010, 2012, 2016, 2021, 2022)  # IIJA + prior bailouts

monthly_t <- long |>
  filter(account == "Mass Transit",
         !(fiscal_year %in% TRANSIT_EXCLUDE_FY),
         field %in% c("net_tax_receipts", "outlay_htf", "closing_balance")) |>
  group_by(fiscal_year, date, month, year, field) |>
  summarise(value = sum(value), .groups = "drop") |>
  pivot_wider(id_cols    = c(fiscal_year, date, month, year),
              names_from  = field,
              values_from = value) |>
  filter(!is.na(net_tax_receipts), !is.na(outlay_htf),
         closing_balance > 0) |>
  arrange(date)

cat(sprintf("Highway bootstrap: %d months, FY%d-FY%d\n",
            nrow(monthly_h), min(monthly_h$fiscal_year), max(monthly_h$fiscal_year)))
cat(sprintf("Transit bootstrap: %d months (excl. FY %s)\n\n",
            nrow(monthly_t),
            paste(TRANSIT_EXCLUDE_FY, collapse = ", ")))


# ── 3. Empirical monthly distributions ────────────────────────────────
make_emp <- function(monthly_df, field_rcpt, field_out) {
  list(
    rcpt = monthly_df |>
      group_by(month) |>
      summarise(vals = list(!!sym(field_rcpt)), .groups = "drop"),
    out = monthly_df |>
      group_by(month) |>
      summarise(vals = list(!!sym(field_out)), .groups = "drop")
  )
}

emp_h <- make_emp(monthly_h, "net_tax_receipts", "outlay_htf")
emp_t <- make_emp(monthly_t, "net_tax_receipts", "outlay_htf")

sample_from <- function(emp, acct, m) {
  vals <- emp[[acct]]$vals[emp[[acct]]$month == m][[1]]
  sample(vals, 1)
}


# ── 4. Holiday share ───────────────────────────────────────────────────
annual_rcpt_mean_h <- sum(map_dbl(emp_h$rcpt$vals, mean))
holiday_share_h <- sum(map_dbl(
  emp_h$rcpt$vals[emp_h$rcpt$month %in% HOLIDAY_MONTHS], mean
)) / annual_rcpt_mean_h

annual_rcpt_mean_t <- sum(map_dbl(emp_t$rcpt$vals, mean))
holiday_share_t <- sum(map_dbl(
  emp_t$rcpt$vals[emp_t$rcpt$month %in% HOLIDAY_MONTHS], mean
)) / annual_rcpt_mean_t

foregone_fy2026_h <- CBO_H$fy2026$rcpt * holiday_share_h
foregone_fy2026_t <- CBO_T$fy2026$rcpt * holiday_share_t
total_foregone    <- (foregone_fy2026_h + foregone_fy2026_t) / 1e3

cat(sprintf("Holiday months:  %s\n", paste(HOLIDAY_MONTHS, collapse = ", ")))
cat(sprintf("Holiday share (Highway): %.1f%% | Transit: %.1f%%\n",
            100 * holiday_share_h, 100 * holiday_share_t))
cat(sprintf("Foregone Highway: $%.1fB  |  Transit: $%.1fB  |  Total: $%.1fB\n\n",
            foregone_fy2026_h / 1e3, foregone_fy2026_t / 1e3, total_foregone))


# ── 4b. Monthly receipt pattern ────────────────────────────────────────
# [CLAIM-5] "September is historically the month with the highest gas
# tax revenues (partly due to a technicality with IRS filing rules)"
monthly_rcpt_by_month <- monthly_h |>
  group_by(month) |>
  summarise(mean_rcpt   = mean(net_tax_receipts) / 1e6,
            median_rcpt = median(net_tax_receipts) / 1e6,
            .groups = "drop") |>
  mutate(month = factor(month, levels = month.name)) |>
  arrange(month)

highest_month     <- as.character(monthly_rcpt_by_month$month[which.max(monthly_rcpt_by_month$mean_rcpt)])
highest_month_avg <- max(monthly_rcpt_by_month$mean_rcpt)

cat("── MONTHLY AVERAGE HIGHWAY RECEIPTS ($M) ────────────────────\n")
print(monthly_rcpt_by_month, n = 12)
cat(sprintf("  [CLAIM-5] Highest month: %s ($%.0fM mean, $%.0fM median)\n\n",
            highest_month,
            highest_month_avg,
            monthly_rcpt_by_month$median_rcpt[which.max(monthly_rcpt_by_month$mean_rcpt)]))


# ── 4c. Table 1 fiscal scores ──────────────────────────────────────────
# [CLAIM: Table 1] Deficit impact by scenario.
# Methodology: foregone highway receipts for each holiday month set,
# scaled to CBO FY2026 projected receipts.
# Range = [raw × 0.75, raw × 1.0], where 0.75 applies the JCT standard
# 25% behavioral offset for excise tax cuts.
# Transit receipts unaffected (separate allocation); scoring is Highway only.

score_holiday <- function(months) {
  share <- sum(map_dbl(
    emp_h$rcpt$vals[emp_h$rcpt$month %in% months], mean
  )) / annual_rcpt_mean_h
  foregone_b <- CBO_H$fy2026$rcpt * share / 1e3   # $B
  tibble(
    months    = paste(months, collapse = "/"),
    n_months  = length(months),
    share_pct = round(100 * share, 1),
    low_b     = round(foregone_b * 0.75, 1),       # with JCT 25% offset
    high_b    = round(foregone_b * 1.00, 1)        # no behavioral offset
  )
}

TABLE1_SCENARIOS <- list(
  list(label = "Jun start, 3 months",  months = c("June", "July", "August")),
  list(label = "Jul start, 3 months",  months = c("July", "August", "September")),
  list(label = "Jun start, 6 months",  months = c("June", "July", "August",
                                                  "September", "October", "November"))
)

table1 <- map_dfr(TABLE1_SCENARIOS, function(s) {
  score_holiday(s$months) |> mutate(scenario = s$label, .before = 1)
})

cat("── TABLE 1: FISCAL COST BY SCENARIO ($B) ────────────────────\n")
cat("  JCT methodology: low = foregone × 0.75, high = foregone × 1.00\n\n")
print(table1, n = nrow(table1))
cat("\n")


# ── 5. Month sequences ────────────────────────────────────────────────
fy_months <- function(fy) {
  dates <- seq(as.Date(sprintf("%d-10-01", fy - 1)),
               as.Date(sprintf("%d-09-01", fy)), by = "month")
  list(dates = dates, names = month.name[month(dates)])
}

fy2026    <- fy_months(2026)
fy2027    <- fy_months(2027)
fy2028    <- fy_months(2028)
all_dates <- c(fy2026$dates, fy2027$dates, fy2028$dates)
N         <- length(all_dates)


# ── 6. Flow samplers ──────────────────────────────────────────────────
sample_flows_h <- function(fy_obj, cbo_yr, holiday = FALSE) {
  raw_r <- map_dbl(fy_obj$names, ~sample_from(emp_h, "rcpt", .x))
  raw_o <- map_dbl(fy_obj$names, ~sample_from(emp_h, "out",  .x))
  
  if (holiday) raw_r[fy_obj$names %in% HOLIDAY_MONTHS] <- 0
  
  rcpt_target <- cbo_yr$rcpt * (if (holiday) 1 - holiday_share_h else 1) * 1e6
  r <- if (sum(raw_r) > 0) raw_r * (rcpt_target / sum(raw_r))
  else rep(0, length(raw_r))
  o <- raw_o * (cbo_yr$out * 1e6 / sum(raw_o))
  
  r + cbo_yr$interest * 1e6 / 12 + cbo_yr$flexed * 1e6 / 12 - o
}

sample_flows_t <- function(fy_obj, cbo_yr) {
  raw_r <- map_dbl(fy_obj$names, ~sample_from(emp_t, "rcpt", .x))
  raw_o <- map_dbl(fy_obj$names, ~sample_from(emp_t, "out",  .x))
  
  r <- if (sum(raw_r) > 0) raw_r * (cbo_yr$rcpt * 1e6 / sum(raw_r))
  else rep(0, length(raw_r))
  o <- raw_o * (cbo_yr$out * 1e6 / sum(raw_o))
  
  r + cbo_yr$interest * 1e6 / 12 + cbo_yr$flexed * 1e6 / 12 - o
}


# ── 7. Simulate one path — Highway exhaustion date ────────────────────
# Transit balance is floored at 0 once it exhausts (FTA constrains
# payments to match receipts; Transit net flow = 0 after insolvency).
# Insolvency event = Highway Account balance hits 0.
simulate_path <- function(holiday = FALSE) {
  bal_h <- CBO_H$fy2026$start * 1e6
  bal_t <- CBO_T$fy2026$start * 1e6
  
  run_year <- function(fy_obj, cbo_h, cbo_t, holiday_h = FALSE) {
    flows_h <- sample_flows_h(fy_obj, cbo_h, holiday_h)
    flows_t <- sample_flows_t(fy_obj, cbo_t)
    for (i in seq_along(fy_obj$dates)) {
      bal_h <<- bal_h + flows_h[i]
      bal_t <<- max(bal_t + flows_t[i], 0)   # Transit floored at 0
      if (bal_h <= 0) return(fy_obj$dates[i])
    }
    NULL
  }
  
  result <- run_year(fy2026, CBO_H$fy2026, CBO_T$fy2026, holiday)
  if (!is.null(result)) return(result)
  
  result <- run_year(fy2027, CBO_H$fy2027, CBO_T$fy2027, FALSE)
  if (!is.null(result)) return(result)
  
  result <- run_year(fy2028, CBO_H$fy2028, CBO_T$fy2028, FALSE)
  if (!is.null(result)) return(result)
  
  return(NA_Date_)
}


# ── 8. Simulate full trajectory (combined balance) ────────────────────
simulate_path_full <- function(holiday = FALSE) {
  balances <- numeric(N)
  bal_h    <- CBO_H$fy2026$start * 1e6
  bal_t    <- CBO_T$fy2026$start * 1e6
  idx      <- 0L
  done     <- FALSE
  
  run_year_full <- function(fy_obj, cbo_h, cbo_t, holiday_h = FALSE) {
    flows_h <- sample_flows_h(fy_obj, cbo_h, holiday_h)
    flows_t <- sample_flows_t(fy_obj, cbo_t)
    for (i in seq_along(fy_obj$dates)) {
      if (done) { idx <<- idx + 1L; balances[idx] <<- 0; next }
      idx   <<- idx + 1L
      bal_h <<- bal_h + flows_h[i]
      bal_t <<- max(bal_t + flows_t[i], 0)
      combined <- max(bal_h, 0) + bal_t
      balances[idx] <<- combined
      if (bal_h <= 0) {
        done <<- TRUE
        balances[idx] <<- 0
      }
    }
  }
  
  run_year_full(fy2026, CBO_H$fy2026, CBO_T$fy2026, holiday)
  run_year_full(fy2027, CBO_H$fy2027, CBO_T$fy2027, FALSE)
  run_year_full(fy2028, CBO_H$fy2028, CBO_T$fy2028, FALSE)
  
  balances
}


# ── 9. Run simulations ────────────────────────────────────────────────
date_quantile <- function(x, p)
  as.Date(quantile(as.numeric(x[!is.na(x)]), p), origin = "1970-01-01")

run_sims <- function(holiday, label, seed_offset) {
  set.seed(SEED + seed_offset)
  cat(sprintf("Running %s (%s sims)...\n", label, comma(N_SIM)))
  dates <- map(seq_len(N_SIM), ~simulate_path(holiday)) |> do.call(what = c)
  n_ex  <- sum(!is.na(dates))
  cat(sprintf("  Highway exhausted: %d / %d (%.1f%%)\n\n",
              n_ex, N_SIM, 100 * n_ex / N_SIM))
  dates
}

base_dates   <- run_sims(FALSE, "Baseline",        0L)
policy_dates <- run_sims(TRUE,  "Gas tax holiday", 1L)


# ── 10. Summary ───────────────────────────────────────────────────────
med_base   <- date_quantile(base_dates,   0.50)
med_policy <- date_quantile(policy_dates, 0.50)
days_shift <- as.numeric(med_policy - med_base)

cat("── BASELINE ────────────────────────────────────────────────\n")
for (p in c(0.05, 0.25, 0.50, 0.75, 0.95))
  cat(sprintf("  %3.0f%%: %s\n", 100 * p, date_quantile(base_dates, p)))

cat(sprintf("\n── GAS TAX HOLIDAY (%s = $0) ──────────────\n",
            paste(HOLIDAY_MONTHS, collapse = "/")))
for (p in c(0.05, 0.25, 0.50, 0.75, 0.95))
  cat(sprintf("  %3.0f%%: %s\n", 100 * p, date_quantile(policy_dates, p)))

cat(sprintf(
  "\n[CLAIM-1] Baseline median insolvency:  %s\n",
  med_base
))
cat(sprintf(
  "[CLAIM-2] Baseline insolvency range:   %s (5th pctile) to %s (95th pctile)\n",
  date_quantile(base_dates, 0.05), date_quantile(base_dates, 0.95)
))
cat(sprintf(
  "[CLAIM-3] Holiday median insolvency:   %s\n",
  med_policy
))
cat(sprintf(
  "[CLAIM-4] Months shifted earlier:      %.1f months (%+d days)\n",
  -days_shift / 30.4, days_shift
))
cat(sprintf(
  "Highway foregone (FY2026): $%.1fB\n",
  total_foregone ### 32.6 if interest costs!
))
cat("────────────────────────────────────────────────────────────\n\n")

results <- bind_rows(
  tibble(exhaustion_date = base_dates[!is.na(base_dates)],
         scenario = "Baseline"),
  tibble(exhaustion_date = policy_dates[!is.na(policy_dates)],
         scenario = sprintf("Gas tax holiday (%s = $0)",
                            paste(HOLIDAY_MONTHS, collapse = "/")))
)
write_csv(results, "htf_policy_comparison.csv")


# ── 11. Fan chart data ────────────────────────────────────────────────
cat(sprintf("Building fan charts (%s paths each)...\n", comma(N_PATHS)))

build_fan <- function(holiday, seed_offset) {
  set.seed(SEED + seed_offset)
  mat <- matrix(0, nrow = N_PATHS, ncol = N)
  for (i in seq_len(N_PATHS))
    mat[i, ] <- simulate_path_full(holiday)
  q <- apply(mat, 2, quantile, probs = c(0.05, 0.25, 0.50, 0.75, 0.95))
  tibble(
    date = all_dates,
    q05 = q[1,] / 1e9, q25 = q[2,] / 1e9, med = q[3,] / 1e9,
    q75 = q[4,] / 1e9, q95 = q[5,] / 1e9
  )
}

fan_base   <- build_fan(FALSE, 10L)
fan_policy <- build_fan(TRUE,  11L)
cat("Done.\n\n")


# ── 12. Plot A: exhaustion histogram ──────────────────────────────────


# ── 13. Plot B: combined balance fan chart ────────────────────────────
p_fan <- ggplot() +
  
  geom_ribbon(data = fan_base,
              aes(x = date, ymin = q05, ymax = q95),
              fill = "#2B5DAA", alpha = 0.12) +
  geom_ribbon(data = fan_base,
              aes(x = date, ymin = q25, ymax = q75),
              fill = "#2B5DAA", alpha = 0.25) +
  geom_line(data = fan_base, aes(x = date, y = med),
            color = "#2B5DAA", linewidth = 1.2) +
  
  geom_ribbon(data = fan_policy,
              aes(x = date, ymin = q05, ymax = q95),
              fill = "firebrick", alpha = 0.10) +
  geom_ribbon(data = fan_policy,
              aes(x = date, ymin = q25, ymax = q75),
              fill = "firebrick", alpha = 0.20) +
  geom_line(data = fan_policy, aes(x = date, y = med),
            color = "firebrick", linewidth = 1.2, linetype = "dashed") +
  
  annotate("segment",
           x = as.Date("2026-01-01"), xend = as.Date("2026-03-15"),
           y = 68, yend = 68, color = "#2B5DAA", linewidth = 1.2) +
  annotate("text", x = as.Date("2026-04-01"), y = 68,
           label = "Baseline (CBO Jan 2025)", hjust = 0,
           size = 3.2, color = "#2B5DAA") +
  annotate("segment",
           x = as.Date("2026-01-01"), xend = as.Date("2026-03-15"),
           y = 64, yend = 64, color = "firebrick",
           linewidth = 1.2, linetype = "dashed") +
  annotate("text", x = as.Date("2026-04-01"), y = 64,
           label = sprintf("Gas tax holiday (-$%.1fB foregone)", total_foregone),
           hjust = 0, size = 3.2, color = "firebrick") +
  
  geom_point(data = cbo_anchors, aes(x = date, y = balance),
             color = "#2B5DAA", size = 3.5, shape = 18) +
  annotate("text",
           x = cbo_anchors$date + 20, y = cbo_anchors$balance + 3,
           label = paste0("$", round(cbo_anchors$balance, 0), "B"),
           size = 3, color = "#2B5DAA", hjust = 0) +
  
  geom_hline(yintercept = 0, linewidth = 0.8, linetype = "dashed") +
  annotate("text", x = as.Date("2025-10-15"), y = 1.5,
           label = "Insolvency threshold",
           hjust = 0, size = 3, color = "black") +
  
  scale_x_date(
    breaks      = seq(as.Date("2025-10-01"), as.Date("2028-09-01"),
                      by = "3 months"),
    date_labels = "%b %Y",
    limits      = c(as.Date("2025-10-01"), as.Date("2028-08-01"))
  ) +
  scale_y_continuous(
    labels = dollar_format(suffix = "B", prefix = "$"),
    limits = c(-2, NA)
  ) +
  labs(
    title    = "Highway Trust Fund: Combined Balance vs. Gas Tax Holiday",
    subtitle = sprintf(
      "Highway + Transit | Holiday: -$%.1fB foregone | Highway insolvency shifts %+.1f months",
      total_foregone, days_shift / 30.4
    ),
    x = NULL, y = "Combined Balance ($B)",
    caption = paste0(
      "CBO Jan 2025 baseline. Highway and Transit Accounts modeled separately; ",
      "Transit floored at $0 once insolvent (FY2027). Combined balance = Highway + Transit. ",
      "Bands = 50% and 90% credible intervals. Diamonds = CBO start-of-year anchors."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_fan)
ggsave("htf_policy_fan.png", p_fan, width = 12, height = 7, dpi = 150)
cat("Saved → htf_policy_fan.png\n")
