# IRMAAs (income-related monthly adjustment amounts)
# calculates base premium, deductibles, coinsurances, and IRMAA surcharges
# SSA uses those figures (and income two years prior to determine who owes IRMAA and at *which* bracket you fall)
# calculated by CMS

# CMS sets the numbers, SSA applies them to individuals

# CMS amounts: https://www.cms.gov/newsroom/fact-sheets/2025-medicare-parts-b-premiums-and-deductibles?utm_source=chatgpt.com

# A: inpatient (hospital)
# B: outpatient (doc visits)
# C: Medicare Advantage (offered by private insurers -- bundles A, B, usually includes D 
#-- can add extras like vision, dental, hearing)
# D: Rx Drug coverage

library(tidyverse)

part_b <- tribble(
  ~year, ~premium, ~deductible,
  2024L, 174.70, 240,
  2025L, 185.00, 257
) %>% print

# Based on MAGI
part_b_irmaa <- tribble(
  ~year, ~filing, ~magi_low, ~magi_high, ~adjustment,
  # Part B - Single
  2025, "single", 0, 106000, 0,
  2025, "single", 106001, 133000, 74,
  2025, "single", 133001, 167000, 185,
  2025, "single", 167001, 200000, 295.90,
  2025, "single", 200001, 500000, 406.90,
  2025, "single", 500001, Inf, 443.90,
  # Part B - MFJ
  2025, "mfj", 0, 212000, 0,
  2025, "mfj", 212001, 266000, 74,
  2025, "mfj", 266001, 334000, 185,
  2025, "mfj", 334001, 400000, 295.90,
  2025, "mfj", 400001, 750000, 406.90,
  2025, "mfj", 750001, Inf, 443.90,
  # Part B - MFS
  2025, "mfs", 0, 106000, 0,
  2025, "mfs", 106001, 394000, 406.90,
  2025, "mfs", 394001, Inf, 443.90,
) %>%
  mutate(total_premium = adjustment + part_b %>% filter(year == 2025) %>% pull(premium)) %>%
  print
 

part_b_irmaa

# Most Medicare beneficiaries do not have a part A premium (10 quarter of employment)
