library(httr)

# See here::here("code", "Clean_email_corpus.R") for the input for this script

# Structure data with GPT-3 -----------------------------------------------

# api function
parse_davinci <- function(input) {
  
  openai_api_key <- Sys.getenv("OPENAI_API_KEY")
  table_format <- "| job title | organisation | location | required years of experience | required education level | start date | required skills | required certification | working hours| job duration | hourly rate |"
  
  response <- POST(
    "https://api.openai.com/v1/completions",
    add_headers(Authorization = paste0("Bearer ", openai_api_key)), 
    encode = "json",
    body = list(
      model = "text-davinci-003",
      prompt = str_c(
        "A table summarizing this job posting:",
        input,
        table_format, 
        sep = " "
      ), 
      temperature = 0,
      max_tokens = 600,
      top_p = 1,
      frequency_penalty = 0,
      presence_penalty = 0
    )
  )
  
  if (status_code(response) == 200) {
    
    list_response <- content(response)
    
    return(list_response$choices[[1]]$text)
    
    
  } else {
    
    return(paste("Error:", status_code(response)))
    
  }
  
}


# Feed Email data into GPT-3 --------------------------------------------------------------

# Apply the function to the email data (commented out, so I don't inadvertently use the API)
# tictoc::tic()
# messages_data_parsed <- messages_data_cleaned |>
#   mutate(
#     gpt_3 = map(
#       .x = method_3,
#       .f = safely(parse_davinci)
#     )
#   )
# tictoc::toc()
# beepr::beep()

# Saved test data to disk to manipulate later. 
# write_rds(messages_data_parsed, here::here("data", "20230211_test_function_partial_success.rds"))

# Insightly data---------------------------------------------------------------

# Apply the function to the insightly data (commented out, so I don't inadvertently use the API)

# I split the insightly data up in smaller parts to feed the data piecemeal into
# the openai api. 

# tictoc::tic()
# insightly_parsed_2022_group16 <- insightly_year[[16]] |>
#   mutate(
#     gpt_3 = map(
#       .f = safely(parse_davinci),
#       .x = Details,
#       .progress = T
#     )
#   )
# tictoc::toc()
# beepr::beep()

# tictoc::tic()
# insightly_parsed_2022_group15 <- insightly_year[[15]] |>
#   mutate(
#     gpt_3 = map(
#       .f = safely(parse_davinci),
#       .x = Details,
#       .progress = T
#     )
#   )
# tictoc::toc()
# beepr::beep()


# Saved test data to disk to manipulate later. 
# First try with a sample set
# write_rds(insightly_parsed, here::here("data", "20230105_test_function_partial_success.rds"))

# Second try with partial set of data (namely group 16 - see Get_insightly_data.R for further details). This was on 2023-02-13
# write_rds(insightly_parsed_2022_group16, here::here("data", str_c(BAutils::dater(Sys.Date()), "_test_function_2022_group16.rds")))

# third try with partial set of data (namely group 16 - see Get_insightly_data.R for further details). This was on 2023-02-13
# write_rds(insightly_parsed_2022_group15, here::here("data", str_c(BAutils::dater(Sys.Date()), "_test_function_2022_group15.rds")))

