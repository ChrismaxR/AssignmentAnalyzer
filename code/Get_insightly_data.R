library(tidyverse)

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
  count(lubridate::year(DateCreated))



