library(tidyverse)
library(tidytext)
library(stopwords)

#source(here::here("code", "Get_email_corpus.R"))


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

