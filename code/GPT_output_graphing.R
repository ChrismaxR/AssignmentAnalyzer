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

compiled_parsed_output |> 
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
  ) # |> 
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

  
  
  