library(tidyverse)

compiled_parsed_output <- bind_rows(
  manual_cleaning_parsed, # add data from emails up until 2022-02-11
  manual_cleaning_insightly_2022_group16_parsed |> filter(!str_detect(result_manual, "^Error")) # add data from insightly group 16 ()
)


# Share data

compiled_parsed_output |> 
  select(
    id, 
    date,
    body, 
    result,
    job_title,
    job_title_derived,
    organisation, 
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
    hourly_rate_derived
  ) #|> 
  #writexl::write_xlsx(here::here("output", str_c(BAutils::dater(Sys.Date()), "_compiled_parsed_data.xlsx")))



# Graphing ----------------------------------------------------------------

simple_cats <- compiled_parsed_output |> 
  select(
    id,
    job_title_derived, 
    organisation,
    location_derived,
    education_derived, 
    experience_derived,
    hours_derived, 
    certification_derived
  )

simple_cats |> 
  pivot_longer(cols = 2:ncol(simple_cats)) |> 
  count(name, value) |> 
  na.omit() |> 
  ggplot(aes(y = fct_reorder(value, n), x = n)) +
  geom_col() +
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

  
  
  