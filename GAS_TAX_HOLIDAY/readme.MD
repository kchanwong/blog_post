# Federal Gas Tax Holiday Analysis
 
## Replication
 
### Non-Parametric Simulation (`mcmc_holiday.R`)
 
Simulates potential Federal Highway Trust Fund (FHTF) paths to insolvency using historical monthly data.
 
1. Take historical distributions of FHTF receipts, income, and outlays by month from 2008 to 2026, excluding periods with large spikes corresponding to general fund bailouts.
2. Sample next month's receipts and outlays from these historical distributions, anchored to CBO's 2026 projections for FHTF status.
3. Use accept-reject sampling to find all viable paths to insolvency by 2028.
This procedure is run separately for each FHTF account and repeated 50,000 times.
 
### Pass-Through Estimation (`passthru.R`)
 
Provides a preliminary estimate of excise gas tax pass-through at the county level.
 
**Coefficients** are taken from Tsvetanov (2024):
- RFG mandate effect — Table 5, p. 9
- Regional inventory coefficients — Appendix Table A5, p. 7
**Data sources:**
- County gas prices scraped from AAA for May 13, 2026 (`county_passthru.csv`)
- RFG mandate county FIPS codes from EPA (hardcoded)
- Regional gasoline inventory data from EIA WPSR Table 2 (`psw02.xls`) and Table 5 (`psw05.xls`)
For each county, pass-through is estimated by averaging all applicable combinations of RFG status and regional inventory coefficients.
 
## Reference
 
Tsvetanov, Tsvetan (2024). "Tax Holidays and the Heterogeneous Pass-Through of Gasoline Taxes." *Energy Economics*, 136, 107682.
 





















