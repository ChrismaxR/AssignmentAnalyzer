library(tidyverse)

# Initial application of gpt on job ads:
# see here::here("code", "gpt_model_throughtput.R") for how I got this data
saved_test_function <- read_rds(here::here("data", "20230105_test_function_partial_success.rds"))


# Cleaning ----------------------------------------------------------------

clean_test_function <- saved_test_function |> 
  tidylog::mutate(
    gpt_3 = as.character(gpt_3)
  ) |> 
  separate(col = gpt_3, sep = "\r\n", into = c("a", "b", "model_output")) |> # the model outputs a formatting that I need to get rid of
  mutate(
    model_output = str_remove(
      string = str_replace_all(
        string = model_output, 
        pattern = "[:blank:]\\|[:blank:]|[:blank:]\\||\\|[:blank:]", 
        replacement = "\\|"
      ),
      pattern = "^\\|"
    )
  ) |> 
  select(-a, -b) |> # get rid of redundant columns
  separate(
    col = model_output, 
    remove = F,
    sep = "\\|", 
    into = c(
      "job_title", "organisation", "start_date", 
      "key_competences", "nice_to_have_competences", 
      "key_job_characteristics", "location"
    )
  ) |> 
  mutate(
    across(
      c("key_competences", "nice_to_have_competences", "key_job_characteristics"), 
      \(x) str_split(string = x, pattern = "\\,|\\;")
    )
  )


# Graphing ----------------------------------------------------------------

simple_cats <- clean_test_function |> 
  select(
    msg_ids,
    job_title, 
    organisation, 
    start_date, 
    location
  )

simple_cats |> 
  pivot_longer(cols = 2:5) |> 
  count(name, value) |> 
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

multiple_cats <- clean_test_function |> 
  select(
    msg_ids,
    key_competences, 
    nice_to_have_competences, 
    key_job_characteristics
  )

multiple_cats_wrangle <- multiple_cats |> 
  transmute(
    msg_ids,
    key_comps = map(.x = key_competences, .f = enframe), 
    nice_comps = map(.x = nice_to_have_competences, .f = enframe), 
    job_chars = map(.x = key_job_characteristics, .f = enframe)
  ) |> 
  pivot_longer(cols = 2:4) |> 
  unnest_longer(col = value) |> 
  unnest(cols = value, names_repair = "unique") |> 
  select(msg_ids, var = 2, value) |>
  mutate(
    value = str_trim(value, side = "both")
  ) |> 
  na.omit() |> 
  count(var, value, sort = T)

multiple_cats_wrangle |> 
  ggplot(aes(y = value, x = n)) +
  geom_col() +
  facet_wrap(~ var, scales = 'free')


multiple_cats$key_competences[[1]] |> 
  enframe()