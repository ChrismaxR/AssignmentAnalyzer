

# Simple httr request -----------------------------------------------------

# Try leveraging GPT-3 model with only httr (after asking ChatGPT how to do it)
# doesn't seem to work... yet(?)

# initial try
library(httr)

analyze_text <- function(api_key, text) {
  endpoint <- "https://api.openai.com/v1/engines//completions"
  headers <- c(Authorization = paste0("Bearer ", api_key))
  payload <- list(prompt = text)
  response <- POST(endpoint, headers = headers, body = payload, encode = "json")
  if (status_code(response) == 200) {
    content(response, as = "text")
  } else {
    stop(paste("Error:", status_code(response)))
  }
}

analyze_text(
  api_key = Sys.getenv("OPENAI_API_KEY"), text = "Say this is a test call" 
)


# second try:


input <- primary_mails |> 
  slice(6) |> 
  pull(msg_body)

parse_unstructured_data_call <- function(input, model) {
  
  openai_api_key <- Sys.getenv("OPENAI_API_KEY")
  table_format <- "| job title | organisation | start date | key competences | nice-to-have competences | key job characteristics | location |"
  
  response <- POST(
    "https://api.openai.com/v1/completions",
    add_headers(Authorization = paste0("Bearer ", openai_api_key)), 
    encode = "json",
    body = list(
      model = model,
      prompt = str_c(
        "A table summarizing this job posting:",
        input,
        table_format, 
        sep = " "
      ), 
      temperature = 0,
      max_tokens = 281,
      top_p = 1,
      frequency_penalty = 0,
      presence_penalty = 0
    )
  )
  
  if (status_code(response) == 200) {
    content(response)
  } else {
    stop(paste("Error:", status_code(response)))
  }
  
}

tictoc::tic()
test_davinci <- parse_unstructured_data_call(input = input, model = "text-davinci-003")
tictoc::toc()

tictoc::tic()
test_curie <- parse_unstructured_data_call(input = input, model = "text-curie-001")
tictoc::toc()

tictoc::tic()
test_babbage <- parse_unstructured_data_call(input = input, model = "text-babbage-001")
tictoc::toc()



# rgpt3 package -----------------------------------------------------------

# Found another package, mentioned by the GPT-3 website, doesn't seem to work either...

library(rgpt3)

# Run this command first to authenticate:
gpt3_authenticate(here::here("access_key.txt"))

# Use out-of-the-box test function to see if I get a response:
gpt3_test_completion()

# Use the single_completion function to do a single call
gpt3_single_completion(
  prompt_input = "Write a single sentence about Feyenoord Football Club", 
  model = "text-curie-001", 
  output_type = "text"
)

gpt3_single_completion(
  prompt_input = 'Write a cynical text about human nature:', 
  temperature = 0.9, 
  max_tokens = 100,
  n = 5
)


# Use the (multiple) completions function for it to operate on a data frame iteratively 
dt_prompts <-  data.table::data.table(
  'prompts' = c(
    'What is the meaning of life?', 
    'Write a tweet about London:', 
    'Write a research proposal for using AI to fight fake news:'
  ), 
  'prompt_id' = c(LETTERS[1:3])
)

test_completions <- rgpt3::gpt3_completions(
  prompt_var = dt_prompts$prompts, 
  id_var = dt_prompts$prompt_id
)


# openai package ----------------------------------------------------------


library(openai)

create_completion(
  engine_id = "davinci",
  prompt = "Generate a question and an answer", 
)

input <- primary_mails |> 
  tidylog::filter(str_length(msg_body) > 5) |> 
  slice(33) |> 
  pull(msg_body)

prompt <- str_c(
  "Is the following Dutch text a job advertisement? Answer only with Yes/No. ", 
  input
)


test <- openai::create_completion(prompt = prompt, engine_id = "text-davinci-003")
