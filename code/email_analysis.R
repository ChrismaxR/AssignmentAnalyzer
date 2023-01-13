library(tidyverse)
library(httr)

source(here::here("code", "Get_email_corpus.R"))

# Structure data with GPT-3 -----------------------------------------------

# api function
parse_davinci <- function(input) {
  
  openai_api_key <- Sys.getenv("OPENAI_API_KEY")
  table_format <- "| job title | organisation | start date | key competences | nice-to-have competences | key job characteristics | location |"
  
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
      max_tokens = 300,
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

# Apply the function to the email data (commented out, so I don't inadvertently use the API)
# tictoc::tic()
# test_function <- primary_mails |> 
#   mutate(
#     gpt_3 = map(
#       .f = parse_davinci,
#       .x = msg_body
#     )
#   )
# tictoc::toc()

# Saved test data to disk to manipulate later. 
# write_rds(test_function, here::here("data", "20230105_test_function_partial_success.rds"))


saved_test_function <- read_rds(here::here("data", "20230105_test_function_partial_success.rds"))


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

# Old ---------------------------------------------------------------------
library(tidytext)
library(tm)
library(stopwords)


filter_vector <- c(
  "hi",
  "hallo",
  "goeiedag",
  "beste",
  "christian", 
  "wanrooij", 
  "directeur", 
  "entrador", 
  "zand 44", 
  "gerrie", 
  "amersfoort", 
  "interesse", 
  "forwarded message", 
  "geachte", 
  "directeur", 
  "vriendelijke groet", 
  "origineel bericht", 
  "oorspronkelijk bericht", 
  "mobile", 
  "https",
  "isense", 
  "disclaimer", 
  "afdrukt", 
  "kind regards", 
  "kvk", 
  "liability for damages", 
  "image"
)

filter <- str_c(filter_vector, collapse = "|")

lines <- primary_mails |> 
  unnest_tokens(token = "lines", msg_body, output = "lines") |> 
  mutate(
    chars = str_length(lines), 
  ) |> 
  tidylog::filter(
    !str_detect(lines, filter)
  )

lines_cleaned <- lines |> 
  group_by(
    msg_ids, msg_thread_ids, msg_datetime, msg_from, msg_subject, thread_seq
  ) |> 
  transmute(
    text = str_c(lines, collapse = " ")
  ) |> 
  distinct()

sentences <- lines_cleaned |> 
  unnest_tokens(token = "sentences", text, output = "sentence") |> 
  mutate(
    chars = str_length(sentence)
  )

  
paragraphs <- primary_mails |> 
  unnest_tokens(token = "paragraphs", value, output = "paragraph") |> 
  mutate(
    chars = str_length(paragraph)
  )

words <- primary_mails |> 
  unnest_tokens(word, value) |> 
  tidylog::filter(!word %in% data_stopwords_stopwordsiso$nl) |> 
  count(word, sort = T) |> 
  tidylog::filter(!str_detect(word, "[:digit:]"))

trigram <- primary_mails |> 
  unnest_tokens(
    input = value, 
    token = "ngrams", 
    n = 3, 
    output = trigram
  ) |> 
  separate(trigram, c("word1", "word2", "word3"), sep = " ") |> 
  tidylog::filter(
    !word1 %in% data_stopwords_stopwordsiso$nl, 
    !word2 %in% data_stopwords_stopwordsiso$nl,
    !word3 %in% data_stopwords_stopwordsiso$nl,
    !str_detect(word1, "[:digit:]"),
    !str_detect(word2, "[:digit:]"),
    !str_detect(word3, "[:digit:]")
  ) 


# TM package --------------------------------------------------------------

corpus <- Corpus(VectorSource(primary_mails$msg_body))

# Clean and preprocess the corpus
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("dutch"))
corpus <- tm_map(corpus, stemDocument)

# Create a term-document matrix
tdm <- TermDocumentMatrix(corpus)

# Use k-means clustering to classify the documents
k <- 5  # Set the number of clusters
kmeans_model <- kmeans(tdm, k)

# Add the cluster labels to the text data
text_data$cluster <- kmeans_model$cluster

# Visualize the clusters using a wordcloud
for (i in 1:k) {
  wordcloud(corpus[text_data$cluster == i], min.freq = 3)
}







