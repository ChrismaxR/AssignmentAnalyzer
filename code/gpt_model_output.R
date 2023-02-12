library(tidyverse)

# Initial application of gpt on job ads:
# see here::here("code", "gpt_model_throughtput.R") for how I got this data
saved_test_function <- read_rds(here::here("data", "20230211_test_function_partial_success.rds"))


# Cleaning method 1: Programmatically ----------------------------------------------------------------

saved_test_function$gpt_3[[1]] |> 
  pluck("result")


gpt_output_cleaner <- function(x) {

  
  linebreaks <- str_count(x, pattern = "\\n")
  
  if (linebreaks == 0) {
    
    return(str_remove(string = x, pattern = "\\s\\r\\s\\|"))
    
  } else if (linebreaks == 1) {
    
    
    
  }
  else if (linebreaks == 2) {}
  else if (linebreaks == 3) {}
  else if(linebreaks == 13) {} 
  else {
    return(as.character(linebreaks))
  }
    
}

clean_test_function <- saved_test_function |> 
  mutate(
    result = as.character(
      map(
        .x = gpt_3 , 
        .f = \(x) pluck(x, "result")
      )
    ),
    no_of_linebreaks = map(.x = result, .f = \(x) str_count(x, pattern = "\\n"))
    #result_clean = str_remove_all(string = result, pattern = "\\-{2,}|\\n|\\|\\s{2}\\|{2,}")
  ) |> View()
  separate(col = result, sep = "\\n|\\r", into = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j")) |> # the model outputs a formatting that I need to get rid of
  glimpse() 
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


# Cleaning method 2: Manually ---------------------------------------------

manual_cleaning <- saved_test_function |> 
    mutate(
      result = as.character(
        map(
          .x = gpt_3 , 
          .f = \(x) pluck(x, "result")
        )
      )
    ) |> 
    select(id, date, from, to, subject, body, result)
  
# writexl::write_xlsx(
#   manual_cleaning, 
#   here::here(
#     "output", 
#     str_c(BAutils::dater(Sys.Date()), "_manual_gpt_3_output_cleaning.xlsx")
#   )
# )  
  
manual_cleaning_cleaned <- readxl::read_excel(here::here("output", "20230212_manual_gpt_3_output_cleaning_chris.xlsx"))
    
manual_cleaning_cleaned |> 
  separate(result_manual, sep = "\\|", into = letters, remove = F) |> 
  View()
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