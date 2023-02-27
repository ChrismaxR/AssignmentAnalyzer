


# Method 2: extract wanted lines -----------------------------------

# I notice some general patterns: important text is forwarded, so first row of 
# interest mostly starts with the string "--- forwarded message ----" or 
# something similair. 

# However, I don't seem to find a general rule for finding the last row...


# ChatGPT suggested the following function to clean the text up, it doesn't work...
# but gave me some ideas...

# extract_body_text <- function(email_text){
#   # Split the email text into lines
#   lines <- strsplit(email_text, "\n")[[1]]
#   
#   # Find the line index where the body text starts
#   start_line <- which(lines == "")[1] + 1
#   
#   # Find the line index where the body text ends
#   end_line <- length(lines)
#   for (i in seq(from = start_line, to = length(lines))){
#     if (startsWith(lines[i], "--") || startsWith(lines[i], "__")){
#       end_line <- i - 1
#       break
#     }
#   }
#   
#   # Extract the body text
#   body_text <- paste(lines[start_line:end_line], collapse = "\n")
#   
#   return(body_text)
#   
# }



# function to test if the email bodies contain the string "forwarded message"

forward_tester <- function(x) {
  
  frame <- strsplit(x, "\n")[[1]] |> 
    enframe() |> 
    mutate(
      test_van = if_else(str_detect(str_to_lower(value), "^van:"), T, F),
      test_met_vr = if_else(str_detect(str_to_lower(value), "^met vriendelijke"), T, F), 
      test_forward = if_else(str_detect(str_to_lower(value), "--- forwarded message ----"), T, F), 
    )
  
  return(
    tibble::tibble(
      id = names(x),
      frame_lines = nrow(frame),
      forward = frame |> 
        filter(test_forward == T) |> 
        transmute(
          index = str_c(name, collapse = ", ")
        ) |> 
        distinct(index) |> 
        pull(index)
    )
  )
  
}

test <- map_df(
  .x = sample_bodies,
  .f = extract_body_text
)

frame <- strsplit(sample_bodies[[8]], "\n")[[1]] |> 
  enframe() |> 
  mutate(
    #test_van = if_else(str_detect(str_to_lower(value), "^van:"), T, F),
    #test_met_vr = if_else(str_detect(str_to_lower(value), "^met vriendelijke"), T, F), 
    test_forward = if_else(str_detect(str_to_lower(value), "--- forwarded message ----"), T, F)
  )

puller <- function(df, column) {
  
  value <- df |> 
    filter(column == T) |> 
    transmute(
      index = str_c(name, collapse = ", ")
    ) |> 
    distinct(index) |> 
    pull(index)
  
  return(value)
  
}

test_extractor <- map_df(
  .x = sample_bodies, 
  .f = text_extractor
)


# Method 3: only remove absolute necessary stuff (easier, but dirtier) -----------------------

# 


# Old ---------------------------------------------------------------------
# 
# library(tidytext)
# library(stopwords)

# lines <- messages_data |> 
#   unnest_tokens(token = "lines", body, output = "lines") |> 
#   mutate(
#     chars = str_length(lines), 
#   ) |> 
#   tidylog::filter(
#     !str_detect(lines, filter)
#   )
# 
# messages_data_cleaned <- lines |> 
#   group_by(
#     id, date, from, to, subject, attachments
#   ) |> 
#   transmute(
#     text = str_c(lines, collapse = " ")
#   ) |> 
#   ungroup() |> 
#   distinct() |> 
#   mutate(
#     cleaned_lenght = str_length(text)
#   ) |> 
#   filter(str_detect(from, "Gerrie"))
# 
# # Check how many characters where dropped by the cleaning
# check <- messages_data_cleaned |> 
#   left_join(
#     messages_data |> 
#       transmute(
#         id,
#         date,
#         from,
#         body,
#         original_length = str_length(body)
#       ), by = c("id", "date", "from")
#   ) |> 
#   #select(original_length, cleaned_lenght) |> 
#   mutate(
#     perc_chars_remained = cleaned_lenght/original_length
#   ) 
# 
# check |> 
#   writexl::write_xlsx(
#     here::here(
#       "output", 
#       str_c(
#         BAutils::dater(ts = Sys.Date()), 
#         "_messages_data_cleaned_check.xlsx"
#       )
#     )
#   )

# 
# sentences <- lines_cleaned |> 
#   unnest_tokens(token = "sentences", text, output = "sentence") |> 
#   mutate(
#     chars = str_length(sentence)
#   )
# 
#   
# paragraphs <- messages_data |> 
#   unnest_tokens(token = "paragraphs", value, output = "paragraph") |> 
#   mutate(
#     chars = str_length(paragraph)
#   )
# 
# words <- messages_data |> 
#   unnest_tokens(word, value) |> 
#   tidylog::filter(!word %in% data_stopwords_stopwordsiso$nl) |> 
#   count(word, sort = T) |> 
#   tidylog::filter(!str_detect(word, "[:digit:]"))
# 
# trigram <- messages_data |> 
#   unnest_tokens(
#     input = value, 
#     token = "ngrams", 
#     n = 3, 
#     output = trigram
#   ) |> 
#   separate(trigram, c("word1", "word2", "word3"), sep = " ") |> 
#   tidylog::filter(
#     !word1 %in% data_stopwords_stopwordsiso$nl, 
#     !word2 %in% data_stopwords_stopwordsiso$nl,
#     !word3 %in% data_stopwords_stopwordsiso$nl,
#     !str_detect(word1, "[:digit:]"),
#     !str_detect(word2, "[:digit:]"),
#     !str_detect(word3, "[:digit:]")
#   ) 
# 
