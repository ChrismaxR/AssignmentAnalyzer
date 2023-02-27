# Libraries and functions -------------------------------------------------

library(tidyverse)

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
gpt_output_parser <- function(df) {
  
  # This R code performs various data cleaning and transformation operations on a data frame named manual_cleaning_cleaned.
  # The tidylog package is used to provide a verbose output of the data cleaning and transformation process.
  
  df |> 
    separate(
      # First, it uses the separate() function to split a column named result_manual into multiple columns using the "|" character as a separator. 
      col = result_manual, 
      sep = "\\|", 
      into = c(
        # The resulting columns are named job_title, organisation, location, required_years_of_experience, required_education_level, start_date, required_skills, required_certification, working_hours, job_duration, hourly_rate, and extra1 to extra6.
        "job_title", "organisation", "location", "required_years_of_experience", 
        "required_education_level", "start_date", "required_skills", 
        "required_certification", "working_hours", "job_duration", "hourly_rate",
        "extra1", "extra2", "extra3", "extra4", "extra5", "extra6"
      ), 
      remove = F
    ) |> 
    # Next, it applies the str_trim() function to all columns to remove any leading or trailing white space.
    tidylog::mutate_all(.funs = \(x) str_trim(x, side = "both")) |> 
    tidylog::mutate(
      # Then, it creates several new columns based on the values in existing columns using the mutate() and case_when() functions.
      job_title_derived = case_when(
        # The job_title_derived column uses several regular expressions to match patterns in the job_title column and assign a standardized job title value.
        str_detect(str_to_lower(job_title), "business analist|business analyst") == T ~ "Business Analist",
        str_detect(str_to_lower(job_title), "scrum|master") == T ~ "Scrum Master", 
        str_detect(str_to_lower(job_title), "agile") == T ~ "Agile Coach", 
        str_detect(str_to_lower(job_title), "product") == T ~ "Product Owner", 
        str_detect(str_to_lower(job_title), "functioneel") == T ~ "Functioneel Ontwerper", 
        str_detect(str_to_lower(job_title), "informatie") == T ~ "Informatie Analist", 
        str_detect(str_to_lower(job_title), "proces") == T ~ "Process Analist", 
        T ~ "Anders"
      ), 
      location_derived_prep = str_to_title(
        str_trim(
          # The location_derived column removes common location-related words and patterns from the location column using regular expressions and trims any remaining white space.
          string = str_remove_all(
            str_to_lower(location), 
            pattern = "nederland|thuis|thuiswerkplek|werkplek|werken|omgeving|hybride|\\sand\\s|remote|\\/|\\s\\-|\\,|\\sen|location|[0-9]{2,}|\\s[a-z]{1,2}$|\\(|\\)|the netherlands|global|n.t.b.|regio"
          ), 
          side = "both" 
        )
      ), 
      location_derived = case_when(
        str_to_lower(location_derived_prep) == "'s gravenhage"     ~ "Den Haag",
        str_to_lower(location_derived_prep) == "'s hertogenbosch"  ~ "Den Bosch",
        str_to_lower(location_derived_prep) == "^na"               ~ NA_character_,
        location_derived_prep == ""                                ~ NA_character_,
        is.na(location_derived_prep)                               ~ NA_character_, 
        T                                                          ~ location_derived_prep
      ),
      organisation_derived = case_when(
        # The organisation_derived column classifies common organisation-related words and patterns from the organisation column using regular expressions.
        str_detect(str_to_lower(organisation), "gemeente|provincie") == T ~ "Gemeente/Provincies",
        str_detect(str_to_lower(organisation), "ministerie|szw|buza|bzk|defensie|jenv|^ezk|sociale zaken|justitie") == T ~ "Rijksoverheid",
        str_detect(str_to_lower(organisation), "rijks|dienst|rechtspraak|cjib|acm|rivm|forensisch|nfi|cibg|koophandel|nvwa|duo|dictu|autoriteit|rvo|dji|knmi|logius|politie|raad|uwv|^ind|koop|justid|p-direkt|ictu|cak|cjib|eurojust|ggd|inspectie") == T ~ "Rijksuitvoeringsorg",
        str_detect(str_to_lower(organisation), "school|universiteit|^hva|^vu|tu delft|windesheim|college|roc") == T ~ "Onderwijs",
        str_detect(str_to_lower(organisation), "bank|lease|hiltermann|stater|bovemij|^ing") == T ~ "Financiële Dienstverlening",
        str_detect(str_to_lower(organisation), "nederlandse spoorwegen|^ns$|klm|schiphol|luchtverkeersleiding|havenbedrijf|transavia|lvnl|^gvb|^htm|^ret|prorail") == T ~ "Transport/Logistiek",
        str_detect(str_to_lower(organisation), "stedin|waternet|tennet|vattenfall|alliander|edsn|vitens|liander|eneco|essent|gasunie|hoogovens") == T ~ "Nuts/Energie",
        str_detect(str_to_lower(organisation), "alphabet|asml|tikkie|bol.com") == T ~ "Tech",
        str_detect(str_to_lower(organisation), "aegon|goudse|vgz|verzekeraar|verzekering|nationale nederlanden|^nn$|^asr") == T ~ "Verzekeraars",
        str_detect(str_to_lower(organisation), "dela|postnl|enza|evbox|hallmark|fondo|kpn") == T ~ "Overige Dienstverlening",
        str_detect(str_to_lower(organisation), "plus|jumbo|ahold") == T ~ "FMCG",
        str_detect(str_to_lower(organisation), "^bmw|automotive") == T ~ "Automotive",
        str_detect(str_to_lower(organisation), "n/a") == T ~ NA_character_,
        T ~ "Anders"
      ),
      education_derived = case_when(
        # The education_derived column assigns a standardized education level value based on patterns in the required_education_level column.
        str_detect(str_to_lower(required_education_level), "hbo") == T                     ~ "HBO",
        str_detect(str_to_lower(required_education_level), "wo") == T                      ~ "WO",
        str_detect(str_to_lower(required_education_level), "bachelor") == T                ~ "HBO",
        str_detect(str_to_lower(required_education_level), "master") == T                  ~ "WO",
        str_detect(str_to_lower(required_education_level), "academisch") == T              ~ "WO",
        str_detect(str_to_lower(required_education_level), "university|universitair") == T ~ "WO",
        str_detect(required_education_level, "N/A") == T                                    ~ NA_character_,
        T ~ "Anders"
      ), 
      experience_prep = case_when(
        # The experience_derived column extracts the numeric values from the required_years_of_experience column and removes any non-numeric characters.
        #str_detect(required_years_of_experience, "[:digit:]") == T ~ str_remove_all(required_years_of_experience, "[:alpha:]"),
        str_detect(required_years_of_experience, "[:digit:]") == T ~ str_extract(required_years_of_experience, "[:digit:]"),
        str_detect(required_years_of_experience, "N/A|^\\-") == T ~ NA_character_,
        T ~ "Anders"
      ), 
      experience_derived = case_when(
        as.numeric(experience_prep) %in% c(0:3) ~ "Junior",
        as.numeric(experience_prep) %in% c(4:6) ~ "Medior",
        as.numeric(experience_prep) > 6         ~ "Senior",
        T ~ experience_prep
      ),
      certification_derived = case_when(
        # The certification_derived column assigns a standardized certification value based on patterns in the required_certification column.
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
        T ~ "Anders"
      ), 
      hours_derived = case_when(
        # The hours_derived column extracts the numeric values from the working_hours column and removes any non-numeric characters.
        #str_detect(working_hours, "[:digit:]") == T ~ str_trim(str_remove_all(working_hours, "[:alpha:]|\\.{1,2}|\\/|\\-$"), side = "both"),
        str_detect(working_hours, "[:digit:]") == T ~ str_extract(working_hours, "[:digit:]{1,2}"),
        str_detect(working_hours, "N/A|^\\-") == T ~ NA_character_,
        T ~ "Anders"
      ),
      hourly_rate_derived = case_when(
        str_detect(hourly_rate, "[:digit:]") == T ~ str_extract(hourly_rate, "[:digit:]{1,}"),
        str_detect(hourly_rate, "N/A|^\\-") == T ~ NA_character_,
        T ~ "Anders"
      ), 
      #pull all strings that contain an @-symbol so I can find the broker/recruiter. 
      extracted_email = str_extract_all(
        body, 
        pattern = "\\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}\\b"
      ),
      broker_in_organisation = if_else(
        condition = str_detect(
          string = str_to_lower(organisation), 
          pattern = "atos|bergler|braincap|cluster data|computer futures|destaffing|fixedtoday|harvey nash|hero|isense|itaq|openstaffing|maandag|progressive|sevenstars|seven stars|ultimum|ubuntu"
        ) == T, 
        true = organisation, 
        false = NULL
      ),
      extracted_broker = map(
        .x = extracted_email,
        .f = safely(
          \(x) {
            
            domains_list <- str_extract(x, "(?<=@).*?(?=\\.)")
            
            if (length(domains_list) > 0 ) {
              
              non_entrador_list <- keep(domains_list, ~.x != "entrador")
              
              return(non_entrador_list[[1]])
              
            } else {
              
              return(list <- c(NA_character_))
              
            }
            
          }
        )
      )
    )
}


# Get Data ----------------------------------------------------------------

# Data from gmail api

# Cleaning method: Manually - Email data -------------------------- 
# This section preps the data to be exported to Excel, to do some manual cleaning
# as method 1 proves that programmatic cleaning is quite difficult.

manual_cleaning_cleaned <- readxl::read_excel(
  here::here("output", "20230212_manual_gpt_3_output_cleaning_chris.xlsx")
)

manual_cleaning_parsed <- gpt_output_parser(manual_cleaning_cleaned) |> 
  mutate(
    source = "email until 2023-02-11"
  )


# Cleaning method: Manually - Insightly data --------------------------

# First try of feeding insightly data into openai API on 2023-02-13:
# insightly_2022_group16 <- read_rds(
#   here::here("data", "20230213_test_function_2022_group16.rds")
# )
# insightly_2022_group15 <- read_rds(
#   here::here("data", "20230215_test_function_2022_group15.rds")
# )

# # Second try of feeding insightly data into openai API on 2023-02-17:
# insightly_2022_group14 <- read_rds(
#   here::here("data", "20230217_test_function_2022_group14.rds")
# )
 
# insightly_2021_group13 <- read_rds(
#   here::here("data", "20230217_test_function_2021_group13.rds")
# )
 
# insightly_2021_group12 <- read_rds(
#   here::here("data", "20230217_test_function_2021_group12.rds")
# )

# Third try of feeding insightly data into openai API on 2023-02-20:
# insightly_2020_group11 <- read_rds(
#   here::here("data", "20230220_test_function_2020_group11.rds")
# )
# 
# insightly_2020_group10 <- read_rds(
#   here::here("data", "20230220_test_function_2020_group10.rds")
# )



# Prep data for manual cleaning -------------------------------------------

# I need to manually clean the output from GPT3. Before I do this, I can do 
# prepatory cleaning of the GPT3 output data, before exporting it to Excel. 
  
# manual_cleaning_insightly_2022_group16 <- manual_insightly_prep(
#   df = insightly_2022_group16, 
#   group_no_char = "16"
# )
 
# manual_cleaning_insightly_2022_group15 <- manual_insightly_prep(
#   df = insightly_2022_group15,
#   group_no_char = "15"
# )

# manual_cleaning_insightly_2022_group14 <- manual_insightly_prep(
#   df = insightly_2022_group14,
#   group_no_char = "14"
# )
  
# manual_cleaning_insightly_2021_group13 <- manual_insightly_prep(
#   df = insightly_2021_group13,
#   group_no_char = "13"
# )
 
# manual_cleaning_insightly_2021_group12 <- manual_insightly_prep(
#   df = insightly_2021_group12,
#   group_no_char = "12"
# )

# manual_cleaning_insightly_2020_group11 <- manual_insightly_prep(
#   df = insightly_2020_group11,
#   group_no_char = "11"
# )
# 
# manual_cleaning_insightly_2020_group10 <- manual_insightly_prep(
#   df = insightly_2020_group10,
#   group_no_char = "10"
# )


# Ingest manually cleaned data --------------------------------------------

cleaned_file_reader <- function(file_path) {
  
  readxl::read_excel(file_path) |> 
    mutate(
      source = basename(file_path)
    )
  
}

manually_cleaned_file_paths <- fs::dir_ls(
  path = here::here("output"), 
  regexp = "[0-9]{2,}_chris.xlsx$"
)

insightly_compiled_groups_cleaned <- map_df(
  .x = manually_cleaned_file_paths,
  .f = cleaned_file_reader
)

manual_cleaning_insightly_parsed <- gpt_output_parser(insightly_compiled_groups_cleaned) |> 
  select(everything(), source)

# Combine all parsed data into 1 df ---------------------------------------

compiled_parsed_output <- bind_rows(
  manual_cleaning_parsed, # add data from emails up until 2022-02-11
  manual_cleaning_insightly_parsed # add data from insightly data
) |> 
  tidylog::filter(
    !str_detect(result_manual, "^Error|^NULL")
  ) 

# Save cleaned and parsed data 
 write_rds(compiled_parsed_output, here::here("data", str_c(BAutils::dater(Sys.Date()), "_compiled_parsed_output.rds")))


# Do some extra checking on compiled set ----------------------------------

# A number of call to the OpenAI api didn't work (error 500, 503, 429 and NULLS)
# If I find the time, retry these rows once more. 
bounced_api_calls <- bind_rows(
  manual_cleaning_parsed, # add data from emails up until 2022-02-11
  manual_cleaning_insightly_parsed 
) |> 
  tidylog::filter(
    str_detect(result_manual, "^Error|NULL")
  ) |> 
  pull(result_manual)

# A number of rows seem to be misaligned -> values in wrong columns

misaligned <- compiled_parsed_output |> 
  filter(
    is.na(job_duration) |
      is.na(hourly_rate) |
      !is.na(extra1) |
      !is.na(extra2) |
      !is.na(extra3) |
      !is.na(extra4) | 
      !is.na(extra5)
  ) |> 
  select(id, source, job_title:job_duration, hourly_rate, extra1:extra5)

