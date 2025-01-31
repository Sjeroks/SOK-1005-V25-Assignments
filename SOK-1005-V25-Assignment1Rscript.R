
#Assignment 1 Andreas Brandsegg

rm(list = ls())
library(tidyverse)
library(zoo)
library(purrr)

df_lt <- read_table2( "http://vortex.nsstc.uah.edu/data/msu/v6.1/tlt/uahncdc_lt_6.1.txt")

df_mt <- read_table2( "http://vortex.nsstc.uah.edu/data/msu/v6.1/tmt/uahncdc_mt_6.1.txt")

df_tp <- read_table2( "http://vortex.nsstc.uah.edu/data/msu/v6.1/ttp/uahncdc_tp_6.1.txt")

df_ls <- read_table2( "http://vortex.nsstc.uah.edu/data/msu/v6.1/tls/uahncdc_ls_6.1.txt")

df_lt <- df_lt %>% select(Year, Mo, Globe)
df_mt <- df_mt %>% select(Year, Mo, Globe)
df_tp <- df_tp %>% select(Year, Mo, Globe)
df_ls <- df_ls %>% select(Year, Mo, Globe)

df_lt <- df_lt %>%
  mutate(Globe = as.numeric(Globe)) %>% #Converting Globe to numeric value
  rename(Temp_LT = Globe) %>%
  mutate(
    Date = as.Date(paste(Year, Mo, "1", sep="-"), format="%Y-%m-%d")  #Making a Date column
  ) %>%
  filter(Date >= as.Date("1980-01-01")) %>%
  arrange(Date) %>%
  select(Date, Temp_LT)

df_mt <- df_mt %>%
  mutate(Globe = as.numeric(Globe)) %>%
  rename(Temp_MT = Globe) %>%
  mutate(
    Date = as.Date(paste(Year, Mo, "1", sep="-"), format="%Y-%m-%d")
  ) %>%
  filter(Date >= as.Date("1980-01-01")) %>%
  arrange(Date) %>%
  select(Date, Temp_MT)

df_tp <- df_tp %>%
  mutate(Globe = as.numeric(Globe)) %>%
  rename(Temp_TP = Globe) %>%
  mutate(
    Date = as.Date(paste(Year, Mo, "1", sep="-"), format="%Y-%m-%d")
  ) %>%
  filter(Date >= as.Date("1980-01-01")) %>%
  arrange(Date) %>%
  select(Date, Temp_TP)

df_ls <- df_ls %>%
  mutate(Globe = as.numeric(Globe)) %>%
  rename(Temp_LS = Globe) %>%
  mutate(
    Date = as.Date(paste(Year, Mo, "1", sep="-"), format="%Y-%m-%d")
  ) %>%
  filter(Date >= as.Date("1980-01-01")) %>%
  arrange(Date) %>%
  select(Date, Temp_LS)

df_all <- reduce(
  list(df_lt, df_mt, df_tp, df_ls),
  full_join,
  by = "Date"
)

df_all <- df_all %>% #Compute 12-month (right-aligned) rolling averages
  arrange(Date) %>%
  mutate(
    MA_LT = rollmean(Temp_LT, k = 12, fill = NA, align = "right"),
    MA_MT = rollmean(Temp_MT, k = 12, fill = NA, align = "right"),
    MA_TP = rollmean(Temp_TP, k = 12, fill = NA, align = "right"),
    MA_LS = rollmean(Temp_LS, k = 12, fill = NA, align = "right")
  ) %>%
  mutate(Average = rowMeans(select(., MA_LT, MA_MT, MA_TP, MA_LS), na.rm = TRUE))

df_plot <- df_all %>%
  select(Date, MA_LT, MA_MT, MA_TP, MA_LS, Average) %>%
  pivot_longer(
    cols = c(MA_LT, MA_MT, MA_TP, MA_LS, Average),
    names_to  = "Series",
    values_to = "Temp"
  ) %>%
  mutate(
    Series = recode(
      Series,
      "MA_LT" = "Lower Troposphere",
      "MA_MT" = "Mid Troposphere",
      "MA_TP" = "Tropopause",
      "MA_LS" = "Lower Stratosphere",
      "Average" = "Average of 4 Layers"
    )
  )

ggplot(df_plot, aes(x = Date, y = Temp, color = Series)) +
  geom_line(na.rm = TRUE, size = 1) +
  labs(
    title = "12-Month Averages of UAH Global Temperature",
    x = "Year",
    y = "Temperature Anomaly in Celsius",
    color = "Atmospheric Layer"
  ) +
  theme_minimal()










