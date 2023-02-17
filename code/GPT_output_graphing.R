library(tidyverse)


# Get data ----------------------------------------------------------------

latest_compiled_parsed_set <- fs::dir_ls(here::here("data"), regexp = "_compiled_parsed_output.rds$") |> 
  enframe() |> 
  arrange(desc(name)) |> 
  slice(1) |> 
  pull(value)

# all the data (insigthly & email data)compiled into one file after gpt throughput and manual cleaning 
compiled_parsed_output <- read_rds(latest_compiled_parsed_set) |> 
  mutate(
    broker = map(
    .x = extracted_broker, 
    .f = \(x) pluck(x, "result")
    ), 
    broker = as.character(broker)
  ) |> 
  select(-extracted_email, -extracted_broker)

# Share data

share_file <- compiled_parsed_output |> 
  transmute(
    id, 
    date,
    body, 
    result,
    job_title,
    job_title_derived,
    organisation, 
    organisation_derived,
    location,
    location_derived,
    experience = required_years_of_experience,
    experience_derived,
    eduction = required_education_level,
    education_derived,
    required_skills,
    certification = required_certification,
    certification_derived,
    hours = working_hours,
    hours_derived,
    job_duration,
    hourly_rate,
    hourly_rate_derived, 
    broker
  ) 

# share_file |> 
  # writexl::write_xlsx(here::here("output", str_c(BAutils::dater(Sys.Date()), "_compiled_parsed_data.xlsx")))



# Graphing ----------------------------------------------------------------

simple_cats <- compiled_parsed_output |> 
  select(
    id,
    job_title_derived, 
    organisation_derived,
    location_derived,
    education_derived, 
    experience_derived,
    hours_derived, 
    certification_derived, 
    broker
  )

simple_cats |> 
  pivot_longer(cols = 2:ncol(simple_cats)) |> 
  count(name, value) |> 
  #na.omit() |> 
  ggplot(aes(y = tidytext::reorder_within(x = value, by = n, within = name), x = n)) +
  geom_col() +
  tidytext::scale_y_reordered() +
  facet_wrap(~ name, scales = "free") +
  BAutils::gg_theme_ba1() +
  labs(
    title = "Telling van mijn opdrachten op Job Title, Organisatie, Locatie en Start Datum", 
    subtitle = str_wrap("Met behulp van GPT-3 heb ik emails van Gerrie met nieuwe opdrachten aan mij gestructureerd. GPT-3 is een zogenaamde Large Language Model die je o.a. in staat stelt op geschreven tekst te interpreteren, classificeren en groeperen. Hiermee kan je de (relatief) ongestructureerde opdrachtomschrijvingen terugbrengen tot analyseerbare data. Ik heb in totaal 34 emails door dit proces gehaald. Het is nog wat ruw, maar komt in de buurt.", 130),
    x = "Count", 
    y = NULL
  )

multiple_cats <- compiled_parsed_output |> 
  transmute(
    id,
    required_skills = map(
      .x = str_split(pattern = "\\,\\s", string = required_skills), 
      .f = enframe
    )
  ) |> 
  unnest_longer(col = required_skills) |> 
  mutate(
    skills = str_to_sentence(required_skills$value)
  ) |> 
  count(skills, sort = T)


multiple_cats_wrangle |> 
  ggplot(aes(y = value, x = n)) +
  geom_col() +
  facet_wrap(~ var, scales = 'free')


# Impute hourly rates -----------------------------------------------------

# D.d. 2023-02-17 I find the following breakdown in hourly rates:

share_file |> 
  transmute(
    hourly_rate_original = str_to_lower(hourly_rate),
    market_conformity = if_else(str_detect(str_to_lower(hourly_rate), "market|markt|conform|mc") == T, T, F),
    buckets = case_when(
      str_detect(str_to_lower(hourly_rate), "[:digit:]{2,}") == T ~ "price",
      str_detect(str_to_lower(hourly_rate), "market|markt|conform|mc") == T ~ "market conformity",
      str_detect(str_to_lower(hourly_rate), "negotiable|overleg|t.b.d.|n.t.b.|n.o.t.k.|negotiation") == T ~ "negotiable",
      T ~ "Unknown"
    )
  ) |> 
  count(buckets) |>
  mutate(
    perc = n/sum(n)
  ) |> 
  View()

# Result:
# hourly_rate       |   # |   %
# ------------------|-----|----
# market conformity |  74 | 19%
# negotiable        |  87 | 22%
# price             |  74 | 19%
# Unknown           | 157 | 40% 

# Can I impute in some way what missing hourly rates could be?


# I think that education, years of experience, industry sector and possibly
# job title influence the market conformity price.

impute_rates <- share_file |> 
  tidylog::mutate(
    hourly_rate_stripped = as.numeric(na_if(str_remove_all(hourly_rate_derived, "[:alpha:]"), ""))
  ) 

# On the basis of this dist
impute_rates |> 
  select(ends_with("derived"), hourly_rate_stripped, -hourly_rate_derived) |> 
  tidylog::filter(between(hourly_rate_stripped, 60, 200)) |> 
  pivot_longer(cols = 1:7) |> 
  ggplot(aes(y = value, x = hourly_rate_stripped)) +
  geom_jitter() +
  geom_boxplot() +
  facet_wrap(~name, scales = "free")
  