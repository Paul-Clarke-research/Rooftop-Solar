rm(list=ls())
library(tidyverse)

# Extract, clean, and convert to numeric rooftop solar installation data 
# from most recent NTSCA Status Report (available at: https://www.ntcsa.co.za/system-status-reports/)

library(pdftools)

eskom_pdf <- pdf_text("/Users/Paul/Documents/Data/Eskom - Status Reports/Report_Week_22-2.pdf")[6]

lines <- str_split(eskom_pdf, "\n") %>% unlist()

rooftop_pv_raw <- lines %>%
  str_trim() %>%
  as_tibble() %>%
  filter(str_detect(value, "^(?i)[a-z]{3}-\\d{2}"))

rooftop_pv_raw <- rooftop_pv_raw %>%
  separate(value, into = c("Date", "Eastern_Cape", "Free_State", 
                           "Gauteng", "KwaZulu_Natal", "Limpopo", 
                           "Mpumalanga", "Northern_Cape", "North_West", 
                           "Western_Cape", "Total"), 
           sep = "\\s{2,}", extra = "merge")

rooftop_pv <- rooftop_pv_raw %>% mutate(across(-Date, ~ as.numeric(gsub(",", "", .))))

rooftop_pv <- rooftop_pv %>%
  mutate(Date = as.Date(paste0("01-", Date), format = "%d-%b-%y"))

# create additional tibble in GWH from existing data in MW

# first convert MW to GW

rooftop_pv_gw <- rooftop_pv %>%
  mutate(across(-Date, ~ . * 0.001))

month_hours <- rooftop_pv_gw %>%
  mutate(Date = dmy(paste0("01-", Date))) %>% 
  mutate(Hours = days_in_month(Date) * 24)     

# Multiply GW by hours to get GWh
rooftop_pv_gwh <- rooftop_pv_gw %>%
  mutate(Date = dmy(paste0("01-", Date))) %>%
  left_join(select(month_hours, Date, Hours), by = "Date") %>%
  mutate(across(-c(Date, Hours), ~ . * Hours))

# Apply average solar capacity factor 
capacity_factor <- 0.194

rooftop_pv_gwh <- rooftop_pv_gwh %>%
  mutate(across(-c(Date), ~ . * capacity_factor))

# now that we have the data for solar installations, let's investigate factors
# driving changes in the rate of installations

# let's start with loadshedding

library(tidyr)
library(googlesheets4)

# extract Eskom se Push data (available at: https://docs.google.com/spreadsheets/d/1ZpX_twP8sFBOAU6t--Vvh1pWMYSvs60UXINuD5n-K08/edit?gid=863218371#gid=863218371)

loadshedding_raw <- read_sheet("1ZpX_twP8sFBOAU6t--Vvh1pWMYSvs60UXINuD5n-K08")
  
loadshedding_raw <- rename(loadshedding_raw, DateTime = created_at, Stage = stage)

# insert dates where no loadshedding occurred into data

date_sequence <- seq(
  from = ymd_hms("2014-03-06 00:00:00"),
  to = ymd_hms("2025-05-16 00:00:00"),
  by = "1 day"
)

new_dates <- tibble(DateTime = date_sequence, Stage = 0)

loadshedding_full <- bind_rows(loadshedding_raw, new_dates) %>%
    arrange(DateTime) %>%
    distinct(DateTime, .keep_all = TRUE)

# calculate the number of loadshedding hours per day

loadshedding_full <- loadshedding_full %>%
  mutate(day = as_date(DateTime)) %>%
  group_by(day) %>%
  mutate(
    hours_loadshedding = if (all(Stage == 0)) {
      0
    } else {
      as.numeric(
        difftime(
          max(DateTime[Stage != 0]),
          min(DateTime[Stage != 0]),
          units = "hours"
        )
      )
    }
  ) %>%
  ungroup()

# now let's calculate the average number of hours of loadshedding that the average Eskom user
# experienced during each period of loadshedding

# first we need to find the percent of each day that was shed

loadshedding_full <- loadshedding_full %>% 
  mutate(percentofday_shed = (hours_loadshedding / 24))

# next we need to calculate the intensity of the shedding by hours per stage
# stage 1 = 1.5 hrs per day, 2 = 3 hrs, 3 = 4.5, 4 = 6, 5 = 7.5, 6 = 9, 7 = 10.5, 8 = 12
# so the formula for 3 of hrs shed per day by stage is 1.5(stage) = hrs shed per day

loadshedding_full <- loadshedding_full %>% 
  group_by(Stage) %>% 
  mutate(
    exp_hours_shed = if (all(Stage == 0)) {
      0
    } else if (all(Stage >= 1)) {
      percentofday_shed * (Stage * 1.5)
    } else {
      NA_real_
    }
  )

# now let's calculate the average hrs shed per month

loadshedding_full <- loadshedding_full %>% 
  mutate(Date = format(DateTime, "%b-%y"))


loadshedding_full <- loadshedding_full %>% 
  group_by(Date) %>% 
  mutate(hours_loadshedding_month = sum(exp_hours_shed) / n_distinct(day))

# now let's calculate the number of days per month where shedding occurred
loadshedding_full <- loadshedding_full %>% 
  group_by(Date) %>%
  mutate(Month_Days = n_distinct(day[Stage != 0]))

loadshedding_full <- loadshedding_full %>%
  mutate(Date = as.Date(paste0("01-", Date), format = "%d-%b-%y"))

# since we have a good handle on loadshedding, now let's investigate other macroeconomic factors
# let's start with cpi  (available at:https://www.statssa.gov.za/publications/P0141/CPIHistory.pdf?)

library(pdftools)
library(tidyverse)

cpi_pdf_url <- "https://www.statssa.gov.za/publications/P0141/CPIHistory.pdf?"
cpi_pdf_data <- pdf_text(cpi_pdf_url)[2:3]

lines_cpi <- str_split(cpi_pdf_data, "\n") %>% unlist() %>% str_trim()

# data pre-1922 is incomplete, let's remove
cpi_raw <- lines_cpi %>%
  keep(~ str_detect(., "^\\d{4}") && !str_detect(., "^191[1-9]|^1920|^1921"))

# let's drop the yearly avg
cpi_raw <- str_split_fixed(cpi_raw, "\\s+", n = 14)[, 1:13]

# let's convert to longform
col_names <- c("Year", "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

cpi_raw <- as_tibble(cpi_raw, .name_repair = "minimal") %>%
  set_names(col_names)

cpi <- cpi_raw %>%
  pivot_longer(cols = Jan:Dec, names_to = "Month", values_to = "CPI") %>%
  mutate(
    CPI = str_replace_all(CPI, "[^0-9\\.,-]", ""),
    CPI = str_replace_all(CPI, ",", "."),          
    CPI = as.numeric(CPI),
    Date = parse_date(paste(Year, Month, "1"), format = "%Y %b %d")
  ) %>%
  select(Date, CPI)

# now let's add in historical data on interest rates 
# using data from 1980-2013 collected by St. Louis FED (available at: https://fred.stlouisfed.org/data/INTDSRZAM193N)

library(fredr)

fredr_set_key("72abfbf14f62d5988fdad45a79cee81d")

repo_80.1_13.4 <- fredr(
  series_id = "INTDSRZAM193N",
  observation_start = as.Date("1980-01-01"),
  observation_end = Sys.Date(),
  frequency = "m"
)

repo_80.1_13.4 <- repo_80.1_13.4 %>% select("date", "value") %>% rename(RepoRate = value, Date = date)

# since the fed only has data from 1980 to 2013 we need to supplement 
# with two datasets from SARB (available at: https://www.southafricanreservebank.co.za/content/dam/sarb/publications/quarterly-bulletins/quarterly-bulletin-publications/2025/march-/04Statistical%20tables%20Money%20and%20Banking.pdf)
# (and at: https://www.southafricanreservebank.co.za/content/dam/sarb/publications/quarterly-bulletins/quarterly-bulletin-publications/2020/qb-dec-2020/05%20Statistical%20tables%20Money%20and%20Banking.pdf)

repo_25_pdf <-  pdf_text("~/Documents/Data/Repo Rates/04Statistical tables Money and Banking.pdf")[33]

lines <- str_split(repo_25_pdf, "\n") %>% unlist() %>% str_trim()

cutoff <- which(str_detect(lines, "KB129"))[1]
lines <- lines[1:(cutoff - 1)]  

repo_25 <- lines %>%
  keep(~ str_detect(., "^\\d{4}/\\d{2}/\\d{2}"))

repo_25 <- repo_25 %>%
  map(~ str_match(.x, "^(\\d{4}/\\d{2}/\\d{2})\\s+([0-9.]+)")[, 2:3]) %>%
  map_df(~ tibble(Date = .x[1], RepoRate = as.numeric(.x[2])))

repo_20_pdf <-  pdf_text("~/Documents/Data/Repo Rates/05 Statistical tables Money and Banking.pdf")[31]

lines <- str_split(repo_20_pdf, "\n") %>% unlist() %>% str_trim()

cutoff <- which(str_detect(lines, "KB129"))[1]
lines <- lines[1:(cutoff - 1)]  # Only keep lines before "KB129"

repo_20 <- lines %>%
  keep(~ str_detect(., "^\\d{4}/\\d{2}/\\d{2}"))

repo_20 <- repo_20 %>%
  map(~ str_match(.x, "^(\\d{4}/\\d{2}/\\d{2})\\s+([0-9.]+)")[, 2:3]) %>%
  map_df(~ tibble(Date = .x[1], RepoRate = as.numeric(.x[2])))

repo_20_25 <- bind_rows(repo_20, repo_25) %>%
  distinct() %>%
  arrange(Date)

repo_20_25 <- repo_20_25 %>%
  mutate(Date = lubridate::ymd(Date))

month_sequence <- seq(
  from = ymd("2013-05-01"),
  to = ymd("2025-05-01"),
  by = "1 month"
)

new_dates <- tibble(Date = month_sequence, RepoRate = NA)

repo_full <- bind_rows(repo_80.1_13.4, new_dates) 

repo_full <- bind_rows(repo_full, repo_20_25)%>%
  arrange(Date) %>%
  distinct(Date, .keep_all = TRUE)

repo_full <- repo_full %>%
  filter(row_number() >= which(!is.na(RepoRate))[1]) %>%
  fill(RepoRate, .direction = "down")

repo_full <- repo_full %>%
  filter(day(Date) == 1)

rates <- repo_full %>%
  left_join(cpi, by = "Date")

rates <- rates %>%
  drop_na()

# now let's calculate the prime rate and the real prime rate

rates <- rates %>% mutate(PrimeRate = (RepoRate + 3.5))

rates <- rates %>% mutate(RealPrime = (PrimeRate - CPI))


# now lets export all of our datasets (loadshedding_full, rates, and rooftop_pv)
# for analysis in Tableau

write_csv(rates, "~/Documents/Data/Interest Rates/rates1.csv")

write_csv(loadshedding_full, "~/Documents/Data/Loadshedding/loadshedding_data.csv")

write_csv(rooftop_pv, "~/Documents/Data/Rooftop Solar/rooftop_pv.csv")


