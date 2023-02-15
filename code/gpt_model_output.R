library(tidyverse)

# Initial application of gpt on job ads on 2023-02-11:
# see here::here("code", "gpt_model_throughtput.R") for how I got this data
saved_test_function <- read_rds(here::here("data", "20230211_test_function_partial_success.rds"))

# First try of feeding insightly data into openai API on 2023-02-13:
insightly_2022_group16 <- read_rds(here::here("data", "20230213_test_function_2022_group16.rds"))
insightly_2022_group15 <- read_rds(here::here("data", "20230215_test_function_2022_group15.rds"))


# Extra data: Woonplaatsen data via: 
# https://opendata.cbs.nl/#/CBS/nl/dataset/85210NED/table?searchKeywords=woonplaatsen

# Idea is to use this reference data to better parse the location column coming 
# from the gpt_3 output data.

plaatsen <- read_delim(delim = ";", here::here("data", "Woonplaatsen_in_Nederland_2022_12022023_165126.csv")) |> 
  janitor::clean_names() |> 
  mutate(
    woonplaatsen = str_to_lower(woonplaatsen)
  ) |> 
  pull(woonplaatsen) |> 
  append(c("den haag", "den bosch"))

# Cleaning method 1: Programmatically ----------------------------------------------------------------

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


# Cleaning method 2: Manually - Email data -------------------------- 
# This section preps the data to be exported to Excel, to do some manual cleaning
# as method 1 proves that programmatic cleaning is quite difficult.
  
# The input data frame saved_test_function is piped into the function mutate.
# In the mutate call, a new column named result is created, which contains the result value of each element in the gpt_3 column.
# The as.character function is used to convert the result column to character data type.
# The select function is used to keep only the columns id, date, from, to, subject, body, and result in the resulting data frame, manual_cleaning  
  
  
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

# This R code performs various data cleaning and transformation operations on a data frame named manual_cleaning_cleaned.
# First, it uses the separate() function to split a column named result_manual into multiple columns using the "|" character as a separator. 
# The resulting columns are named job_title, organisation, location, required_years_of_experience, required_education_level, start_date, required_skills, required_certification, working_hours, job_duration, hourly_rate, and extra1 to extra6.
# Next, it applies the str_trim() function to all columns to remove any leading or trailing white space.
# Then, it creates several new columns based on the values in existing columns using the mutate() and case_when() functions.
# The job_title_derived column uses several regular expressions to match patterns in the job_title column and assign a standardized job title value.
# The location_derived column removes common location-related words and patterns from the location column using regular expressions and trims any remaining white space.
# The education_derived column assigns a standardized education level value based on patterns in the required_education_level column.
# The experience_derived column extracts the numeric values from the required_years_of_experience column and removes any non-numeric characters.
# The certification_derived column assigns a standardized certification value based on patterns in the required_certification column.
# The hours_derived column extracts the numeric values from the working_hours column and removes any non-numeric characters.
# The tidylog package is used to provide a verbose output of the data cleaning and transformation process.

    
manual_cleaning_parsed <- manual_cleaning_cleaned |> 
  separate(
    col = result_manual, 
    sep = "\\|", 
    into = c(
      "job_title", "organisation", "location", "required_years_of_experience", 
      "required_education_level", "start_date", "required_skills", 
      "required_certification", "working_hours", "job_duration", "hourly_rate",
      "extra1", "extra2", "extra3", "extra4", "extra5", "extra6"
    ), 
    remove = F
  ) |> 
  tidylog::mutate_all(.funs = \(x) str_trim(x, side = "both")) |> 
  tidylog::mutate(
    job_title_derived = case_when(
      str_detect(str_to_lower(job_title), "business analist|business analyst") == T ~ "Business Analist",
      str_detect(str_to_lower(job_title), "scrum|master") == T ~ "Scrum Master", 
      str_detect(str_to_lower(job_title), "agile") == T ~ "Agile Coach", 
      str_detect(str_to_lower(job_title), "product") == T ~ "Product Owner", 
      str_detect(str_to_lower(job_title), "functioneel") == T ~ "Functioneel Ontwerper", 
      str_detect(str_to_lower(job_title), "informatie") == T ~ "Informatie Analist", 
      str_detect(str_to_lower(job_title), "proces") == T ~ "Process Analist", 
      T ~ "other"
    ), 
    location_derived = str_trim(
      string = str_remove_all(
        str_to_lower(location), 
        pattern = "nederland|thuis|thuiswerkplek|werkplek|werken|omgeving|hybride|\\sand\\s|remote|\\/|\\s\\-|\\,|\\sen|location|[0-9]{2,}|\\s[a-z]{1,2}$"
      ), 
      side = "both" 
    ), 
    education_derived = case_when(
      str_detect(str_to_lower(required_education_level), "hbo") == T ~ "HBO",
      str_detect(str_to_lower(required_education_level), "wo") == T ~ "WO",
      str_detect(str_to_lower(required_education_level), "bachelor") == T ~ "HBO",
      str_detect(str_to_lower(required_education_level), "master") == T ~ "WO",
      str_detect(str_to_lower(required_education_level), "academisch") == T ~ "WO",
      str_detect(str_to_lower(required_education_level), "university|universitair") == T ~ "WO",
      str_detect(required_education_level, "N/A") == T ~ NA_character_,
      T ~ "other"
    ), 
    experience_derived = case_when(
      #str_detect(required_years_of_experience, "[:digit:]") == T ~ str_remove_all(required_years_of_experience, "[:alpha:]"),
      str_detect(required_years_of_experience, "[:digit:]") == T ~ str_extract(required_years_of_experience, "[:digit:]"),
      str_detect(required_years_of_experience, "N/A|^\\-") == T ~ NA_character_,
      T ~ "other"
    ), 
    certification_derived = case_when(
      str_detect(str_to_lower(required_certification), "n/a|^\\-|none") == T ~ NA_character_,
      str_detect(str_to_lower(required_certification), "psm") == T ~ "PSM I/II",
      str_detect(str_to_lower(required_certification), "ireb") == T ~ "IREB",
      str_detect(str_to_lower(required_certification), "jstd") == T ~ "JSTD",
      str_detect(str_to_lower(required_certification), "bpmn") == T ~ "BPMN",
      str_detect(str_to_lower(required_certification), "itil") == T ~ "ITIL",
      str_detect(str_to_lower(required_certification), "wwft") == T ~ "WWFT",
      str_detect(str_to_lower(required_certification), "safe") == T ~ "SAFe",
      str_detect(str_to_lower(required_certification), "cism") == T ~ "CISM",
      str_detect(str_to_lower(required_certification), "togaf") == T ~ "TOGAF",
      T ~ "other"
    ), 
    hours_derived = case_when(
      #str_detect(working_hours, "[:digit:]") == T ~ str_trim(str_remove_all(working_hours, "[:alpha:]|\\.{1,2}|\\/|\\-$"), side = "both"),
      str_detect(working_hours, "[:digit:]") == T ~ str_extract(working_hours, "[:digit:]{1,2}"),
      str_detect(working_hours, "N/A|^\\-") == T ~ NA_character_,
      T ~ "other"
    ), 
    hourly_rate_derived = case_when(
      str_detect(hourly_rate, "[:digit:]") == T ~ str_extract(hourly_rate, "[:digit:]{1,}"),
      str_detect(hourly_rate, "N/A|^\\-") == T ~ NA_character_,
      T ~ "other"
    )
  )



# Cleaning method 2: Manually - Insightly data --------------------------

# I need to manually clean the output from GPT3. Before I do this, I can do 
# prepatory cleaning of the GPT3 output data, before exporting it to Excel. 
  
manual_insightly_prep <- function(df, group_no_char) {
  
  prep <- df |> 
    mutate(
      result = as.character(
        map(
          .x = gpt_3 , 
          .f = \(x) pluck(x, "result")
        )
      )
    ) |>  
    transmute(
      id = RecordId, 
      date = DateCreated, 
      from = "insightly", 
      to = "insightly", 
      subject = "insightly", 
      body = Details, 
      result, 
      result_manual = result # duplicate column to do the manual cleaning in
    )
  
  # Export to Excel for manual work:
  
  writexl::write_xlsx(
    prep,
    here::here(
      "output",
      str_c(
        BAutils::dater(Sys.Date()), 
        "_manual_gpt_3_output_cleaning_insightly_2022_group",
        group_no_char,
        ".xlsx")
    )
  )
  
}

# manual_cleaning_insightly_2022_group16 <- manual_insightly_prep(
#   df = insightly_2022_group16, 
#   group_no_char = "16"
# )
 
# manual_cleaning_insightly_2022_group15 <- manual_insightly_prep(
#   df = insightly_2022_group15,
#   group_no_char = "15"
# )




# Ingest manually cleaned data --------------------------------------------


insightly_compiled_groups_cleaned <- bind_rows(
  manual_cleaning_insightly_2022_group16_cleaned <- readxl::read_excel(here::here("output", "20230213_manual_gpt_3_output_cleaning_insightly_2022_group16_chris.xlsx")),
  manual_cleaning_insightly_2022_group15_cleaned <- readxl::read_excel(here::here("output", "20230215_manual_gpt_3_output_cleaning_insightly_2022_group15_chris.xlsx"))
)


manual_cleaning_insightly_parsed <- insightly_compiled_groups_cleaned |> 
  separate(
    col = result_manual, 
    sep = "\\|", 
    into = c(
      "job_title", "organisation", "location", "required_years_of_experience", 
      "required_education_level", "start_date", "required_skills", 
      "required_certification", "working_hours", "job_duration", "hourly_rate",
      "extra1", "extra2", "extra3", "extra4", "extra5", "extra6"
    ), 
    remove = F
  ) |> 
  tidylog::mutate_all(.funs = \(x) str_trim(x, side = "both")) |> 
  tidylog::mutate(
    job_title_derived = case_when(
      str_detect(str_to_lower(job_title), "business analist|business analyst") == T ~ "Business Analist",
      str_detect(str_to_lower(job_title), "scrum|master") == T ~ "Scrum Master", 
      str_detect(str_to_lower(job_title), "agile") == T ~ "Agile Coach", 
      str_detect(str_to_lower(job_title), "product") == T ~ "Product Owner", 
      str_detect(str_to_lower(job_title), "functioneel") == T ~ "Functioneel Ontwerper", 
      str_detect(str_to_lower(job_title), "informatie") == T ~ "Informatie Analist", 
      str_detect(str_to_lower(job_title), "proces") == T ~ "Process Analist", 
      T ~ "other"
    ), 
    location_derived = str_trim(
      string = str_remove_all(
        str_to_lower(location), 
        pattern = "nederland|thuis|thuiswerkplek|werkplek|werken|omgeving|hybride|\\sand\\s|remote|\\/|\\s\\-|\\,|\\sen|location|[0-9]{2,}|\\s[a-z]{1,2}$"
      ), 
      side = "both" 
    ), 
    education_derived = case_when(
      str_detect(str_to_lower(required_education_level), "hbo") == T ~ "HBO",
      str_detect(str_to_lower(required_education_level), "wo") == T ~ "WO",
      str_detect(str_to_lower(required_education_level), "bachelor") == T ~ "HBO",
      str_detect(str_to_lower(required_education_level), "master") == T ~ "WO",
      str_detect(str_to_lower(required_education_level), "academisch") == T ~ "WO",
      str_detect(str_to_lower(required_education_level), "university|universitair") == T ~ "WO",
      str_detect(required_education_level, "N/A") == T ~ NA_character_,
      T ~ "other"
    ), 
    experience_derived = case_when(
      #str_detect(required_years_of_experience, "[:digit:]") == T ~ str_remove_all(required_years_of_experience, "[:alpha:]"),
      str_detect(required_years_of_experience, "[:digit:]") == T ~ str_extract(required_years_of_experience, "[:digit:]"),
      str_detect(required_years_of_experience, "N/A|^\\-") == T ~ NA_character_,
      T ~ "other"
    ), 
    certification_derived = case_when(
      str_detect(str_to_lower(required_certification), "n/a|^\\-|none") == T ~ NA_character_,
      str_detect(str_to_lower(required_certification), "psm") == T ~ "PSM I/II",
      str_detect(str_to_lower(required_certification), "ireb") == T ~ "IREB",
      str_detect(str_to_lower(required_certification), "jstd") == T ~ "JSTD",
      str_detect(str_to_lower(required_certification), "bpmn") == T ~ "BPMN",
      str_detect(str_to_lower(required_certification), "itil") == T ~ "ITIL",
      str_detect(str_to_lower(required_certification), "wwft") == T ~ "WWFT",
      str_detect(str_to_lower(required_certification), "safe") == T ~ "SAFe",
      str_detect(str_to_lower(required_certification), "cism") == T ~ "CISM",
      str_detect(str_to_lower(required_certification), "togaf") == T ~ "TOGAF",
      T ~ "other"
    ), 
    hours_derived = case_when(
      #str_detect(working_hours, "[:digit:]") == T ~ str_trim(str_remove_all(working_hours, "[:alpha:]|\\.{1,2}|\\/|\\-$"), side = "both"),
      str_detect(working_hours, "[:digit:]") == T ~ str_extract(working_hours, "[:digit:]{1,2}"),
      str_detect(working_hours, "N/A|^\\-") == T ~ NA_character_,
      T ~ "other"
    ),
    hourly_rate_derived = case_when(
      str_detect(hourly_rate, "[:digit:]") == T ~ str_extract(hourly_rate, "[:digit:]{1,}"),
      str_detect(hourly_rate, "N/A|^\\-") == T ~ NA_character_,
      T ~ "other"
    )
  )


 