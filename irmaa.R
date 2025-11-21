#pak::pkg_install("davechilders/irslimits")


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
library(scales)
library(irslimits)
data(package = "irslimits")

premiums_b
irmaa

part_b_irmaa

# Most Medicare beneficiaries do not have a part A premium (10 quarter of employment)

# initial plot
mfj <- irmaa %>% filter(filing == "mfj")

ggplot(mfj, aes(x = magi_low, xend = magi_high, y = total_premium * 12)) +
  geom_segment() +
  theme_minimal() +
  expand_limits(y = 0, x = 900000) +
  scale_y_continuous(labels = label_currency(suffix = "K", scale = .001), breaks = pretty_breaks(7)) +
  scale_x_continuous(labels = label_currency(suffix = "K", scale = .001), breaks = pretty_breaks(9)) +
  geom_point(aes(x = magi_low, y = total_premium * 12)) +
  labs(
    title = "How does income impact Medicare Part B premiums?",
    #subtitle = "By Trailing 2 year MAGI (modified adjusted gross income)",
    x = "MAGI (Income Two Years Prior)",
    y = "Annual\nPremium"
  ) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.7))

# fear-mongering
# cutoff in tax-code
# rates change
# what's included in MAGI
# two year trailing
# how to appeal
# how to keep it low
# comment about $200K vs $800K (should be at cost)

# plot rate payroll taxes

dd <- tibble(
  magi = 0:9e5
) %>% print

dd

dd2 <- dd %>%
  left_join(
    mfj %>% select(magi_low, total_premium),
    by = c("magi"="magi_low")
  )

dd2 %>% filter(!is.na(total_premium))

dd3 <- dd2 %>%
  fill(total_premium) %>%
  mutate(
    annual_premium = total_premium * 12,
    premium_rate = annual_premium / magi
    ) %>%
  print

dd4 <- dd3 %>%
  mutate(
    medicare_amt = (0.0145 * magi) + (0.009 * pmax(0, magi - 250000)),
    medicare_rate = medicare_amt / magi
  )

dd3 %>%
  select(magi, premium_rate)

dd5 <- dd4 %>%
  select(magi, premium_rate, medicare_rate) %>%
  pivot_longer(-magi, names_to = "rate_type", values_to = "rate") %>%
  mutate(rate_type = str_remove_all(rate_type, "_rate")) %>%
  filter(magi >= 100000) %>% print

dd5 %>%
  #filter(magi >= 100000) %>%
  ggplot(aes(x = magi, y = rate, color = rate_type)) +
  geom_line() +
  theme_minimal() +
  expand_limits(y = 0, x = 900000) +
  scale_y_continuous(labels = label_percent(), breaks = pretty_breaks(7)) +
  scale_x_continuous(labels = label_currency(suffix = "K", scale = .001), breaks = pretty_breaks(9)) +
  #geom_point(aes(x = magi_low, y = total_premium * 12)) +
  labs(
    title = "What are premiums as a percentage of MAGI?",
    #subtitle = "By Trailing 2 year MAGI (modified adjusted gross income)",
    x = "MAGI (Income Two Years Prior)",
    y = "Premium Rate"
  ) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.7))

dd3 %>% 
  filter(total_premium != lag(total_premium))

# still working - maximize your pre-tax retirement benefits
# if you give to charity anyway, give via a QCD.
# avoid capital gains (part of MAGI)
# spread gain/withdrawal over multiple years
# draw living expense from Roth instead of IRA
# plan in advance
 

# What are the exceptions to minimum $185 per month
# immunosuppressive-drug only premium
# state-level support via Medicare Shared Savings Program

# plot MAGI income rate vs payroll taxes


