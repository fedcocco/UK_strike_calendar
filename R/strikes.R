# Extract dates

# Imports ---------------------------------------------------------------------

library(here)
library(lubridate)
library(tidyverse)

# Constants -------------------------------------------------------------------

DATAFILE <- here("data", "strikes-2022-12-06-13-25.csv")

# Functions -------------------------------------------------------------------

compile_date <- function(date_string) {
  if(is.na(date_string)) return(NA)
  if (str_detect(date_string, "January")) {
    str_c(date_string, " 2023")
  } else {
    str_c(date_string, " 2022")
  }
}

# Analysis --------------------------------------------------------------------

df <- read_csv(DATAFILE)
dfs <- df |> 
  distinct(more_information) |> 
  mutate(info_id = as.character(row_number()))

# Most dates are RE1 so get those first
re1 <- "\\d{1,2}(?:th|rd){0,1}\\s{0,}(?:January|February|March|April|May|June|July|August|September|October|November|December){1}"

initial_dates <- str_match_all(dfs$more_information, re1) |> 
  set_names(as.character(1:nrow(dfs))) |> 
  imap_dfr(function(row, info_id) {
    if (length(row) == 0) return(tibble(info_id = info_id, date = NA))
    dates <- c(row)
    tibble(info_id = info_id, date = dates)
  }) |> 
  filter(! is.na(date))

initial_dates$date <- map_chr(initial_dates$date, compile_date)
initial_dates$date <- as_date(map_dbl(initial_dates$date, dmy))

# Handle RE2 dates
re2 <- "(?:\\& ){1}\\d{1,2}(?:th){0,1}\\s{0,}(?:January|February|March|April|May|June|July|August|September|October|November|December){1}"
str_match_all(dfs$more_information, re2)

re2_ids <- as.character(c(14, 19, 33, 48, 68, 75, 103, 104, 106))
re2_dates <- expand_grid(
  info_id = re2_ids, 
  date = as_date(c(
    "2022-11-24",
    "2022-11-25",
    "2022-11-30")))

# Remove existing RE2 dates and add the new dates
initial_dates <- initial_dates |> filter(! info_id %in% re2_ids)
initial_dates <- bind_rows(initial_dates, re2_dates) |> 
  mutate(info_id = as.double(info_id)) |> 
  arrange(info_id) |> 
  mutate(info_id = as.character(info_id))

# Handle RE3 dates
re3 <- "(?:-\\s{0,}){1}\\d{1,2}(?:th){0,1}\\s{0,}(?:January|February|March|April|May|June|July|August|September|October|November|December){1}"
str_match_all(dfs$more_information, re3)

re3_ids <- as.character(c(23))
re3_dates <- expand_grid(
  info_id = re3_ids, 
  date = as_date(c(
    "2022-12-02",
    "2022-12-03",
    "2022-12-04",
    "2022-12-05")))

# Remove existing RE3 dates and add the new dates
initial_dates <- initial_dates |> filter(! info_id %in% re3_ids)
initial_dates <- bind_rows(initial_dates, re3_dates) |> 
  mutate(info_id = as.double(info_id)) |> 
  arrange(info_id) |> 
  mutate(info_id = as.character(info_id))

# Handle RE4 dates
re4 <- "(?:and ){1}\\d{1,2}(?:th){0,1}\\s{0,}(?:January|February|March|April|May|June|July|August|September|October|November|December){1}"
str_match_all(dfs$more_information, re4)

re4_dates <- tibble(
  info_id = c(
    "5",
    "5",
    "5",
    "5",
    "5",
    "5",
    "5",
    "5",
    "5",
    "5",
    "5",
    "5",
    "65",
    "65",
    "67",
    "67",
    "67",
    "91",
    "91",
    "91",
    "91",
    "91",
    "91",
    "91",
    "91",
    "91",
    "91",
    "91",
    "91",
    "98",
    "98",
    "98",
    "98",
    "98",
    "98",
    "98",
    "98"), 
  date = as_date(c(
    "2022-10-21",
    "2022-10-24",
    "2022-10-28",
    "2022-10-31",
    "2022-11-04",
    "2022-11-07",
    "2022-11-11",
    "2022-11-14",
    "2022-11-18",
    "2022-11-21",
    "2022-11-25",
    "2022-11-28",
    "2022-12-15",
    "2022-12-20",
    "2022-11-24",
    "2022-11-25",
    "2022-11-30",
    "2022-11-25",
    "2022-11-26",
    "2022-11-27",
    "2022-12-03",
    "2022-12-04",
    "2022-12-10",
    "2022-12-11",
    "2022-12-17",
    "2022-12-18",
    "2022-12-23",
    "2022-12-24",
    "2022-12-26",
    "2022-12-13",
    "2022-12-14",
    "2022-12-16",
    "2022-12-17",
    "2023-01-03",
    "2023-01-04",
    "2023-01-06",
    "2023-01-07")))

# Remove existing RE4 dates and add the new dates
initial_dates <- initial_dates |> filter(! info_id %in% re4_dates$info_id)
initial_dates <- bind_rows(initial_dates, re4_dates) |> 
  mutate(info_id = as.double(info_id)) |> 
  arrange(info_id) |> 
  mutate(info_id = as.character(info_id))

# Handle manual ranges --------------------------------------------------------
# This is the big hard-coded list of exceptions!

range_dates <- tibble(
  info_id = c(
    "15",
    "15",
    "15",
    "15",
    "15",
    "15",
    "15",
    "15",
    "15",
    "15",
    "15",
    "15",
    "15",
    "15",
    "17",
    "17",
    "17",
    "17",
    "17",
    "17",
    "17",
    "17",
    "17",
    "17",
    "17",
    "18",
    "18",
    "18",
    "18",
    "18",
    "18",
    "18",
    "18",
    "21",
    "21",
    "22",
    "22",
    "22",
    "22",
    "22",
    "26",
    "26",
    "26",
    "28",
    "28",
    "29",
    "29",
    "31",
    "31",
    "31",
    "31",
    "31",
    "31",
    "31",
    "31",
    "31",
    "31",
    "31",
    "31",
    "31",
    "34",
    "34",
    "42",
    "42",
    "42",
    "58",
    "58",
    "58",
    "58",
    "58",
    "62",
    "62",
    "62",
    "72",
    "72",
    "72",
    "72",
    "72",
    "72",
    "72",
    "72",
    "72",
    "72",
    "77",
    "77",
    "77",
    "79",
    "79",
    "79",
    "79",
    "79",
    "79",
    "79",
    "81",
    "81",
    "81",
    "81",
    "81",
    "86",
    "86",
    "83",
    "83",
    "83",
    "88",
    "88",
    "89",
    "89",
    "89",
    "89",
    "89",
    "89",
    "89",
    "92",
    "92",
    "92",
    "94",
    "94",
    "94",
    "94",
    "94",
    "94",
    "94",
    "94",
    "94",
    "94",
    "94",
    "94",
    "94",
    "94",
    "94",
    "94",
    "94",
    "94",
    "94",
    "94",
    "94",
    "94",
    "94",
    "94",
    "94",
    "94",
    "94",
    "95",
    "95",
    "95",
    "95",
    "95",
    "95",
    "95",
    "95",
    "95",
    "95",
    "95",
    "100",
    "100",
    "100",
    "102",
    "102",
    "102",
    "102"), 
  date = as_date(c(
    "2022-11-28",
    "2022-11-29",
    "2022-11-30",
    "2022-12-01",
    "2022-12-02",
    "2022-12-08",
    "2022-12-09",
    "2022-12-12",
    "2022-12-13",
    "2022-12-14",
    "2022-12-15",
    "2022-12-16",
    "2022-12-19",
    "2022-12-20",
    "2022-10-24",
    "2022-10-28",
    "2022-10-31",
    "2022-11-04",
    "2022-11-07",
    "2022-11-11",
    "2022-11-14",
    "2022-11-18",
    "2022-11-21",
    "2022-11-25",
    "2022-11-28",
    "2022-12-12",
    "2022-12-13",
    "2022-12-15",
    "2022-12-16",
    "2023-01-03",
    "2023-01-04",
    "2023-01-06",
    "2023-01-07",
    "2022-12-12",
    "2022-12-13",
    "2022-10-24",
    "2022-10-25",
    "2022-10-26",
    "2022-10-27",
    "2022-10-28",
    "2022-11-24",
    "2022-11-25",
    "2022-11-30",
    "2022-11-28",
    "2022-12-01",
    "2022-08-19",
    "2022-08-20",
    "2022-12-01",
    "2022-12-02",
    "2022-12-05",
    "2022-12-06",
    "2022-12-07",
    "2022-12-08",
    "2022-12-09",
    "2022-12-10",
    "2022-12-12",
    "2022-12-13",
    "2022-12-14",
    "2022-12-14",
    "2022-12-16",
    "2022-08-19",
    "2022-08-20",
    "2022-11-05",
    "2022-11-07",
    "2022-11-09",
    "2022-12-05",
    "2022-12-06",
    "2022-12-07",
    "2022-12-08",
    "2022-12-09",
    "2022-11-24",
    "2022-11-25",
    "2022-11-30",
    "2022-12-12",
    "2022-12-13",
    "2022-12-14",
    "2022-12-15",
    "2022-12-16",
    "2022-12-19",
    "2022-12-20",
    "2022-12-21",
    "2022-12-22",
    "2022-12-23",
    "2022-11-14",
    "2022-11-15",
    "2022-11-16",
    "2022-10-27",
    "2022-10-28",
    "2022-10-29",
    "2022-10-30",
    "2022-10-31",
    "2022-11-01",
    "2022-11-02",
    "2022-11-30",
    "2022-12-07",
    "2022-12-14",
    "2022-12-21",
    "2023-01-04",
    "2022-08-19",
    "2022-08-20",
    "2022-11-24",
    "2022-11-25",
    "2022-11-30",
    "2022-10-01",
    "2022-10-08",
    "2022-12-05",
    "2022-12-06",
    "2022-12-07",
    "2022-12-08",
    "2022-12-09",
    "2022-12-10",
    "2022-12-11",
    "2022-11-24",
    "2022-11-25",
    "2022-11-30",
    "2022-10-24",
    "2022-10-26",
    "2022-10-28",
    "2022-10-31",
    "2022-11-02",
    "2022-11-04",
    "2022-11-07",
    "2022-11-09",
    "2022-11-11",
    "2022-11-14",
    "2022-11-16",
    "2022-11-18",
    "2022-11-21",
    "2022-11-23",
    "2022-11-25",
    "2022-11-28",
    "2022-11-30",
    "2022-12-02",
    "2022-12-05",
    "2022-12-07",
    "2022-12-09",
    "2022-12-12",
    "2022-12-14",
    "2022-12-16",
    "2022-12-19",
    "2022-12-21",
    "2022-12-23",
    "2022-10-19",
    "2022-10-20",
    "2022-10-21",
    "2022-10-22",
    "2022-10-23",
    "2022-10-24",
    "2022-10-25",
    "2022-10-26",
    "2022-10-27",
    "2022-10-28",
    "2022-10-29",
    "2022-12-06",
    "2022-12-07",
    "2022-12-15",
    "2022-11-23",
    "2022-11-24",
    "2022-11-25",
    "2022-11-30")))

# Remove existing range dates and add the new dates
initial_dates <- initial_dates |> filter(! info_id %in% range_dates$info_id)
initial_dates <- bind_rows(initial_dates, range_dates) |> 
  mutate(info_id = as.double(info_id)) |> 
  arrange(info_id) |> 
  mutate(info_id = as.character(info_id))

# Handle GMB  -----------------------------------------------------------------

gmb_ids <- as.character(c(35, 37, 38, 39, 40, 41, 43, 44))
gmb_dates <- expand_grid(
  info_id = gmb_ids, 
  date = seq(as.Date("2022-07-20"), as.Date("2022-08-11"), "day"))

# Remove existing gmb dates and add the new dates
initial_dates <- initial_dates |> filter(! info_id %in% gmb_ids)
initial_dates <- bind_rows(initial_dates, gmb_dates) |> 
  mutate(info_id = as.double(info_id)) |> 
  arrange(info_id) |> 
  mutate(info_id = as.character(info_id))

# Handle Unite  ---------------------------------------------------------------

unite_ids <- as.character(c(51, 52, 54, 66))
unite_dates <- expand_grid(
  info_id = unite_ids, 
  date = as_date(c(
    "2022-08-19",
    "2022-08-20")))

# Remove existing gmb dates and add the new dates
initial_dates <- initial_dates |> filter(! info_id %in% unite_ids)
initial_dates <- bind_rows(initial_dates, unite_dates) |> 
  mutate(info_id = as.double(info_id)) |> 
  arrange(info_id) |> 
  mutate(info_id = as.character(info_id))

# Handle Shelter --------------------------------------------------------------

shelter_ids <- as.character(c(47, 49, 53, 55, 57, 59, 61, 63, 69))
shelter_dates <- expand_grid(
  info_id = shelter_ids, 
  date = as_date(c(
    "2022-12-05",
    "2022-12-06",
    "2022-12-07",
    "2022-12-08",
    "2022-12-09",
    "2022-12-12",
    "2022-12-13",
    "2022-12-14",
    "2022-12-15",
    "2022-12-16")))

# Remove existing shelter dates and add the new dates
initial_dates <- initial_dates |> filter(! info_id %in% shelter_ids)
initial_dates <- bind_rows(initial_dates, shelter_dates) |> 
  mutate(info_id = as.double(info_id)) |> 
  arrange(info_id) |> 
  mutate(info_id = as.character(info_id))

# Merge and write out ---------------------------------------------------------

dates_by_description <- left_join(dfs, initial_dates, by = "info_id")
strike_dates <- left_join(df, dates_by_description, by = "more_information")

dates_by_description |> 
  write_csv(here("data", "dates-by-description-2022-12-06-13-25.csv"))

strike_dates |> 
  mutate(month_date = format(date, "%b %d")) |> 
  distinct() |> 
  write_csv(here("data", "strike-dates-2022-12-06-13-25.csv"))

