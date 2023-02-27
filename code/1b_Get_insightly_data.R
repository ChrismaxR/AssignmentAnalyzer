library(tidyverse)

set.seed(42)

# Get data ----------------------------------------------------------------

insightly <- read_csv(here::here("data", "20230118_insightly_opportunities_export.csv"))

insightly_wrangle <- insightly |> 
  select(RecordId, DateCreated, Details) |> 
  tidylog::mutate(
    DateCreated = lubridate::mdy(DateCreated), 
    Length = str_length(Details)
  ) |> 
  tidylog::filter(
    !str_detect(Details, "^http"), # filter out all references to urls
    Length > 500 # only keep rows with Details that have a string length of over 500 - very arbritrary
  )

# Wrangle data ------------------------------------------------------------

# Insightly contains duplicates in, which I don't want to take into account. 
# This object will serve as a filter to deduplicate the insightly data.
filter_duplicates <- insightly_wrangle |> 
  janitor::get_dupes(Details) |> 
  group_by(Details) |>  
  mutate(dupe_id = row_number()) |> 
  filter(dupe_id != 1) |> 
  pull(RecordId)

final_insightly_set <- insightly_wrangle |> 
  tidylog::filter(!RecordId %in% filter_duplicates)

# Check data --------------------------------------------------------------

# breakdown of usable opportunities in 
final_insightly_set |> 
  count(year = lubridate::year(DateCreated), name = "usefulOpportunities")



# Clean data --------------------------------------------------------------

insightly_cleaned <- final_insightly_set |> 
  mutate(
    year = lubridate::year(DateCreated),
    clean_string = str_replace_all(
      string = str_remove_all(Details, "\\*|\\-{2,}|\\t"), 
      pattern = "\\s{2,}", 
      replacement = " "
    ),
    clean_string_length = str_length(clean_string)
  ) |>
  arrange(year)
  
insightly_year <- insightly_cleaned |> 
  group_split(year, group_id = row_number() %/% 200) |> 
  setNames(
    c(
      "2016", 
      "2017", "2017", "2017",
      "2018", "2018", 
      "2019", "2019",
      "2020", "2020", "2020",
      "2021", "2021", "2021",
      "2022", "2022", 
      "2023"
    )
  )
  
