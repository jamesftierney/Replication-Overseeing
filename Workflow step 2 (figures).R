
#=====================================
# To create the figures without replicating the dataset... 
#=====================================

# uncomment the following, which reflects what is posted on Dataverse

# overseeing_input <- readRDS("cleaned_dataset_for_public.RDS")

#=====================================
# Figure 1
#=====================================

figure_1_data <- readRDS("figure_1_data.RDS")

sro_filing <- figure_1_data %>%
  filter(sro == 1) |>
  group_by(month = lubridate::floor_date(fedreg_date, 'month')) |>
  summarize("sro filing" = n()) |>
  ungroup()

non_sro_filing <- figure_1_data %>%
  filter(sro == 0) |>
  group_by(month = lubridate::floor_date(fedreg_date, 'month')) |>
  summarize("sec (non-sro) filing" = n()) |>
  ungroup()

sro_filing <- sro_filing |>
  left_join(non_sro_filing) |>
  mutate_all(~replace(., is.na(.), 0)) |>
  rowwise() |>
  mutate(total = sum(c_across(c("sro filing", "sec (non-sro) filing"))),
         prop_sro = `sro filing` / total) |>
  mutate(`After Dodd-Frank` = ifelse(month > ymd("2011-02-01"), TRUE, FALSE))

sro_filing |>
  ggplot(aes(x = month, y = prop_sro, color = `After Dodd-Frank`)) +
  geom_point(alpha = .25) +
  geom_smooth(method = "lm") +
  theme_classic() +
  geom_vline(xintercept = ymd("2011-02-01"), linetype = "dotted") +
  theme(
    #    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "SRO filings as a proportion of SEC filings in the Federal Register",
    x = "Year",
    y = "Percentage of SEC filings involving SRO rule changes",
    color = "Before Dodd-Frank?"
  ) 

ggsave("proportion-of-sro-filings-month.png", height = 6.5, width = 8, dpi = 300, unit = "in")

#=====================================
# Figure 2
#=====================================

# produced with TikZ code

#=====================================
# Figure 3
#=====================================

zero_d <- overseeing_input |> 
  #  filter(currentname %in% elapsed_top_20) |>
  filter(immediate_effectiveness == TRUE) |>
  group_by(category, month = lubridate::floor_date(fedreg_date, 'month')) |>
  summarize("zero duration" = n()) |>
  ungroup()

non_zero_d <- overseeing_input |> 
  #  filter(currentname %in% elapsed_top_20) |>
  filter(immediate_effectiveness == FALSE) |>
  group_by(category, month = lubridate::floor_date(fedreg_date, 'month')) |>
  summarize("non-zero duration" = n())  |>
  ungroup()

zero_d <- zero_d |>
  left_join(non_zero_d) |>
  filter(!category %in% c("securities futures associations", "notice registered securities future product exchanges"),
         !is.na(category)) |>
  mutate_all(~replace(., is.na(.), 0)) |>
  mutate(total = `zero duration` + `non-zero duration`,
         `proportion zero` = `zero duration` / total) |>
  mutate(`After Dodd-Frank` = ifelse(month > ymd("2011-02-01"), TRUE, FALSE))

zero_d |>
  ggplot(aes(x = month, y = `proportion zero`)) +
  geom_point(alpha = .05) +
  geom_smooth(method = "lm", aes(color = `After Dodd-Frank`)) +
  facet_wrap(~category) +
  theme_classic() +
  geom_vline(xintercept = ymd("2011-02-01"), linetype = "dotted") +
  theme(
    #    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Procedural Outcomes: SRO filings going immediately effective",
    x = "Year",
    y = "Percentage of SRO Filings going immediately effective",
    color = "After Dodd-Frank Act?"
  )

ggsave("facet wrap of percentages of zero duration - binned at month level.png", height = 6.5, width = 8, dpi = 300, unit = "in")




#=====================================
# Figure 4
#=====================================

overseeing_input |>
  mutate(`After Dodd-Frank` = ifelse(fedreg_date >= ymd("2011-02-01"), TRUE, FALSE)) |>
  group_by(category, month = lubridate::floor_date(fedreg_date, 'year'), `After Dodd-Frank`) |>
  select(category, `After Dodd-Frank`, filenum_19b4, month) |>
  unique() |>
  summarize(count = n()) |>
  ungroup() |>
  filter(!category %in% c("securities futures associations", "notice registered securities future product exchanges", "exempt exchanges", "joint industry plans"),
         !is.na(category)) |>
  ggplot(aes(x = month, y = count, color = `After Dodd-Frank`)) +
  geom_point(alpha = .1) +
  facet_wrap(~category, scales = "free_y") +
  geom_smooth(method = "lm", alpha = .2) +
  theme_classic() +
  geom_vline(xintercept = ymd("2011-02-01"), linetype = "dotted") +
  theme(
    #    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  ) +
  labs(
    title = "Annual count of SRO filings in the Federal Register",
    x = "Year",
    y = "Count of unique Form 19b-4 file numbers for that category of SRO",
    color = "Before Dodd-Frank?"
  ) 

ggsave("counts of filenums.png", height = 6.5, width = 8, dpi = 300, unit = "in")



#=====================================
# Figure 5
#=====================================



overseeing_input |>
  mutate(`After Dodd-Frank` = ifelse(fedreg_date >= ymd("2011-02-01"), TRUE, FALSE)) |>
  group_by(category, currentname, month = lubridate::floor_date(fedreg_date, 'year'), `After Dodd-Frank`) |>
  select(category, currentname, `After Dodd-Frank`, filenum_19b4, month) |>
  unique() |>
  summarize(count = n()) |>
  ungroup() |>
  filter(!category %in% c("securities futures associations", "notice registered securities future product exchanges", "exempt exchanges", "joint industry plans"),
         !is.na(category)) |>
  ggplot(aes(x = month, y = count, color = `After Dodd-Frank`)) +
  geom_point(alpha = .1) +
  facet_wrap(~category, scales = "free_y") +
  geom_smooth(method = "lm", alpha = .2) +
  theme_classic() +
  geom_vline(xintercept = ymd("2011-02-01"), linetype = "dotted") +
  theme(
    #    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  ) +
  labs(
    title = "Annual count of SRO filings in the Federal Register",
    subtitle = "Binned at the individual SRO level",
    x = "Year",
    y = "Count of unique Form 19b-4 file numbers",
    color = "Before Dodd-Frank?"
  ) 

ggsave("counts of filenums binned.png", height = 6.5, width = 8, dpi = 300, unit = "in")


#=====================================
# Figure 6
#=====================================






elapsed_times <- overseeing_input %>% 
  select(index, category, currentname, fedreg_date, fedreg_vol, fedreg_num, prtpage, filenum_19b4) %>%
  filter(!is.na(filenum_19b4)) %>%
  group_by(filenum_19b4) %>%
  arrange(fedreg_date) %>%
  mutate(rank_within_filenum = row_number())

elapsed_times <- elapsed_times %>%
  summarize(
    elapsed_time = max(fedreg_date) - min(fedreg_date),
    .groups = 'drop',
    across()
  ) 

elapsed_times |>
  mutate(`After Dodd-Frank` = ifelse(fedreg_date >= ymd("2011-02-01"), TRUE, FALSE)) |>
  filter(elapsed_time > 0 & elapsed_time < 300) |>
  group_by(category, month = lubridate::floor_date(fedreg_date, 'month'), `After Dodd-Frank`) |>
  summarize(avg = mean(elapsed_time)) |>
  ungroup() |>
  filter(!category %in% c("securities futures associations", "exempt exchanges", "notice registered securities future product exchanges"),
         !is.na(category)) |>
  #  filter( %in% elapsed_top_20) |>
  ggplot(aes(x = month, y = avg, color = `After Dodd-Frank`)) +
  geom_point(alpha = .1) +
  geom_smooth(method = "lm") +
  facet_wrap(~category) +
  theme_classic() +
  geom_vline(xintercept = year(ymd("2011-02-01")), linetype = "dotted") +
  theme(
    #    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  ) +
  geom_vline(xintercept = ymd("2011-02-01"), linetype = "dotted") +
  labs(
    title = "Procedural Outcomes: Time elapsed to SEC decision",
    subtitle = "SRO rule changes not going effect immediately",
    x = "Year",
    y = "Days between first and last filing in the SRO rule file",
    color = "Before Dodd-Frank?"
  ) 

ggsave("elapsed time.png", height = 6.5, width = 6.5, dpi = 300, unit = "in")


#=====================================
# Figure 7
#=====================================


fee_filings <- overseeing_input |> 
  #  filter(currentname %in% elapsed_top_20) |>
  filter(fee_filing == TRUE) |>
  group_by(category, currentname, year = lubridate::floor_date(fedreg_date, 'year')) |>
  summarize("fee filing" = n()) |>
  ungroup()

non_fee_filings <- overseeing_input |> 
  #  filter(currentname %in% elapsed_top_20) |>
  filter(fee_filing == FALSE) |>
  group_by(category, currentname, year = lubridate::floor_date(fedreg_date, 'year')) |>
  summarize("non-fee filing" = n())  |>
  ungroup()

fee_filings <- fee_filings |>
  left_join(non_fee_filings) |>
  filter(!category %in% c("securities futures associations", "notice registered securities future product exchanges"),
         !is.na(category)) |>
  complete(category, year) |>
  mutate_all(~replace(., is.na(.), 0)) |>
  mutate(total = `fee filing` + `non-fee filing`,
         `proportion fee filing` = ifelse(total == 0, 0, `fee filing` / total))


fee_filings |>
  mutate(`After Dodd-Frank` = ifelse(year >= ymd("2011-02-01"), TRUE, FALSE)) |>
  ggplot(aes(x = year, y = `proportion fee filing`)) +
  facet_wrap(~category) +
  geom_point(alpha=.1) +
  geom_smooth(method = "lm", aes(color = `After Dodd-Frank`)) +
  theme_classic() +
  geom_vline(xintercept = ymd("2011-02-01"), alpha = .2)+
  theme(
    #    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  ) +
  labs(
    title = "Procedural Outcomes: SRO filings involving fees",
    x = "Year",
    y = "Percentage of SRO Filings involving fees under 19(b)(3)",
    color = "Before Dodd-Frank?"
  ) +
  scale_y_continuous(labels = scales::percent) 

ggsave("fee filings.png", height = 6.5, width = 6.5, dpi = 300, unit = "in")




#=====================================
# Figure 8
#=====================================

proceedings_temp <- overseeing_input |>
  filter(instituting_proceedings == TRUE) |>
  group_by(category, month = lubridate::floor_date(fedreg_date, 'month')) |>
  summarize("proceedings" = n())

non_proceedings_temp <- overseeing_input |>
  filter(instituting_proceedings == FALSE) |>
  group_by(category, month = lubridate::floor_date(fedreg_date, 'month')) |>
  summarize("non-proceedings" = n())

proceedings_temp <- proceedings_temp |>
  left_join(non_proceedings_temp) |>
  ungroup() |>
  complete(category, month) |>
  mutate_all(~replace(., is.na(.), 0)) |>
  mutate(total = proceedings + `non-proceedings`,
         `proportion proceedings` = ifelse(total == 0, 0, `proceedings` / total))

proceedings_temp |>
  filter(month > ymd("2008-01-01")) |>
  filter(category %in% c("national securities exchanges", 
                         "registered securities associations")) |>
  group_by(category, year = lubridate::floor_date(month, "year")) |>
  summarize(average = mean(`proportion proceedings`)) |>
  ggplot(aes(x = year, y = average, linetype = category)) +
  geom_line() +
  theme_classic() +
  labs(
    title = "Procedural Outcomes: SEC orders instituting proceedings to approve or disapprove",
    x = "Year",
    y = "Percentage of SRO Filings involving institution of proceedings under 19(b)(3)",
    color = "Stock Exchange"
  ) +
  geom_vline(xintercept = ymd("2011-02-15"), alpha = .2) +
  theme(
    #    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(0, .125)) 



ggsave("instituting proceedings percentage.png", height = 6.5, width = 8, dpi = 300, unit = "in")



#=====================================
# Figure 9
#=====================================


fee_filings_1 <- overseeing_input |> 
  #  filter(currentname %in% elapsed_top_20) |>
  filter(immediate_effectiveness == TRUE) |>
  filter(fee_filing == TRUE) |>
  group_by(category, currentname, year = lubridate::floor_date(fedreg_date, 'year')) |>
  summarize("fee filing" = n()) |>
  ungroup()

non_fee_filings_1 <- overseeing_input |> 
  #  filter(currentname %in% elapsed_top_20) |>
  filter(immediate_effectiveness == TRUE) |>
  filter(fee_filing == FALSE) |>
  group_by(category, currentname, year = lubridate::floor_date(fedreg_date, 'year')) |>
  summarize("non-fee filing" = n())  |>
  ungroup()

fee_filings_1 <- fee_filings_1 |>
  left_join(non_fee_filings_1)

fee_filings_1 <- fee_filings_1  |>
  filter(!category %in% c("securities futures associations", "notice registered securities future product exchanges"),
         !is.na(category)) |>
  complete(category, year) |>
  mutate_all(~replace(., is.na(.), 0)) |>
  mutate(total = `fee filing` + `non-fee filing`,
         `proportion fee filing` = ifelse(total == 0, 0, `fee filing` / total))


fee_filings_1  |>
  mutate(`After Dodd-Frank` = ifelse(year >= ymd("2011-02-01"), TRUE, FALSE)) |>
  ggplot(aes(x = year, y = `proportion fee filing`)) +
  facet_wrap(~category) +
  geom_point(alpha=.05) +
  geom_smooth(method = "lm", aes(color = `After Dodd-Frank`)) +
  theme_classic() +
  geom_vline(xintercept = ymd("2011-02-01"), alpha = .2)+
  theme(
    #    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  ) +
  labs(
    title = "Procedural Outcomes: SRO filings involving fees",
    subtitle = "Subset of immediately effective filings",
    x = "Year",
    y = "Percentage of immediately-effective filings involving fees under 19(b)(3)",
    color = "Before Dodd-Frank?"
  ) +
  scale_y_continuous(labels = scales::percent) 

ggsave(filename = "fee filings 1.png", height = 6.5, width = 6.5, dpi = 300, unit = "in")



#=====================================
# Figure 10
#=====================================

revenue_data <- tibble(
  sro = c(rep("nyse euronext", 6), rep("ice", 4), rep("nasdaq", 10), rep("bats", 3)),
  year = c(2008:2013, 2014:2017, 2008:2017, 2015:2017),
  total_revenues = c(4702, 4684, 4425, 4552, 3749, 3797, 4352, 4682, 5958, 5834,
                     3650, 3410, 3191, 3438, 3120, 3211, 3500, 3403, 3705, 3965,
                     1779, 1869, 2229),
  market_data_revenues = c(428, 403, 373, 371, 348, 353, 446, 470, 535, 556,
                           330, 325, 313, 333, 337, 362, 384, 399, 427, 454,
                           131, 146, 165),
  time_to_event = year - 2011
)


normalized_revenue_data <- revenue_data |>
  group_by(sro) |>
  arrange(year) |>
  mutate(
    # Get the first available total and market data revenues for each SRO
    first_total_revenues = first(total_revenues),
    first_market_data_revenues = first(market_data_revenues),
    # Calculate year-over-year growth for total revenues, with the first year being 0
    total_revenues_growth = if_else(
      lag(total_revenues) != 0, (total_revenues / lag(total_revenues)) - 1, 0
    ),
    # Calculate year-over-year growth for market data revenues, with the first year being 0
    market_data_revenues_growth = if_else(
      lag(market_data_revenues) != 0,
      (market_data_revenues / lag(market_data_revenues)) - 1, 0
    ),
  ) %>%
  ungroup()

ggplot(normalized_revenue_data, aes(x = time_to_event, y = market_data_revenues_growth, color = sro)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") + # Baseline year
  labs(
    title = "Year-Over-Year Growth in Reported Market Data Revenues",
    subtitle = "Normalized to First Observation of Each Exchange's Market Data Revenues",
    x = "Years Relative to Dodd-Frank",
    y = "Year-Over-Year Growth in Market Data Revenues (%)",
    color = "Stock Exchange"
  ) +
  theme_classic() +
  theme(
    #    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  ) +
  scale_y_continuous(labels = scales::percent)

ggsave("normalized revenue data figure.png", scale = .75)


#=====================================
# Figure 11
#=====================================

revenue_data |>
  mutate(pct_mkt_data = market_data_revenues / total_revenues) |>
  # Plot cumulative change in total revenues
  ggplot(aes(x = time_to_event, y = pct_mkt_data, color = sro)) +
  geom_line(size = 1.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") + # Baseline year
  labs(
    title = "Market data contributions to stock exchange revenue",
    x = "Years Relative to Dodd-Frank",
    y = "Market Data revenues as a percentage of Total revenues",
    color = "SRO"
  ) +
  theme_classic() +
  theme(
    #    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(0, .125))

ggsave("market data as share of stock exchange revenue.png", scale = .75)


#=====================================
# Table 1
#=====================================


effective_totals <- overseeing_input %>%
  mutate(year = year(fedreg_date)) %>%
  #  filter(category == "national securities exchanges" & 
  #           searchname %in% c("nyse", "nasdaq")) %>%
  group_by(category) %>%
  summarize(total = n()) %>%
  arrange(desc(total)) 

# drop the small-n category observations, totaling 103+43+1+1 = 148
effective_totals |>
  filter(total>104) |>
  add_row(category = "small-n sro categories (dropped)", total = 148) |>
  add_row(category = "total", total = 25138+3472+2697+422+148)



#=====================================
# Footnote 146
#=====================================

# calendar year 2021
overseeing_input |>
  filter(fedreg_date > ymd("2020-12-31") & fedreg_date < ymd("2022-01-01")) |>
  group_by(currentname) |>
  summarize(count = mean(n())) |>
  ungroup() |>
  summarize(average = mean(count))

# fiscal year 2021
overseeing_input |>
  filter(fedreg_date > ymd("2020-09-30") & fedreg_date < ymd("2021-10-01")) |>
  group_by(currentname) |>
  summarize(count = mean(n())) |>
  ungroup() |>
  summarize(average = mean(count))
