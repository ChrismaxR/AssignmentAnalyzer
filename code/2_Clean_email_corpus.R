# This script serves to clean the email texts, so that I can reduce the amount 
# of text I want to feed into GPT-3. This will hopefully 

library(tidyverse)

# See here::here("code", "Get_email_corpus.R")) for the input for the logic in 
# this script. 

# Initial data prep -------------------------------------------------------

# only get emails from Gerrie and with bodies with actual content
messages_data_content <- messages_data |> 
  filter(
    str_detect(from, "^Gerrie"), 
    str_length(body) > 4, 
    date > lubridate::ymd("2023-01-01")
  ) |> 
  mutate(
    # function to split the body text into lines and frame them into a nested tibble
    lined_bodies = map(
      .x = body, 
      .f = \(x) enframe(strsplit(x, "\n")[[1]]) 
    ), 
    original_length = str_length(body)
  )

# put the bodies in a list to iterate over
# bodies <- messages_data_content$body |> 
#   setNames(messages_data_content$id)
# 
# lined_bodies <- map(
#   .x = bodies, 
#   .f = \(x) enframe(strsplit(x, "\n")[[1]]) # function to split the body text into lines and frame them into a tibble
# )


# Cleaning methods --------------------------------------------------------

# I see three separate ways of doing the cleaning, each with their drawbacks and perks:
# 1. filter unwanted stuff away, and keep the (hard to abstract into a general function though)
# 2. only keep the necessary stuff (remove headers/footer) (hard to abstract into a general function though)
# 3. only remove the easy stuff (strange chars), but keeps a lot residual dirtyness...

# Method 1: Filter unwanted lines --------------------------------

# This method is quite hit or miss: on the basis of the inspection of a subset of 
# email, I arbitrarily gather a list of words that are contained in the body texts
# and filter them away once I find a partial match. This will probably also result 
# filtering away stuff I otherwise would want to keep...

filter_vector <- c(
  "^hi",
  "^hallo",
  "^goeiedag",
  "^beste",
  "christian", 
  "wanrooij", 
  "directeur", 
  "entrador", 
  "\\'t zand 44", 
  "gerrie", 
  "amersfoort", 
  "forwarded message", 
  "doorgestuurd bericht",
  "^geachte", 
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
  "image", 
  "null",
  "mobiel",
  "walikerstraat", 
  "komt u naar ons kantoor", 
  "om met de auto ons pand", 
  "www.computerfutures.com", 
  "klik hier om",
  "deze email is gestuurd door computer futures",
  "^holdings b.v.",
  "^dit electronisch bericht is vertrouwelijk", 
  "geaddresseerde(n)", 
  "^bericht niet openbaar maken, kopiëren, verspreiden",
  "^verrichten vertrouwend op dit bericht", 
  "^op audit.data@computerfutures.com zo spoedig",
  "^stellen indien u dit bericht abusievelijk heeft ontvangen",
  "^http://url1854.computerfutures", 
  "^http://www.cimsolutions.nl",
  "havenweg 24, postbus 183", 
  "meer weten",
  "^mail\\:",
  "^website\\:",
  "^t\\:",
  "^e\\:", 
  "^ultimum bv",
  "unsubscribe from",
  "update subscription preferences",
  "audit.data@huxley.com",
  "dit bericht abusievelijk",
  "url1779.huxley.nl",
  "www.huxleyit.com", 
  "qualogy resources b.v.",
  "sent by qualogy",
  "^\\<", 
  "why did I get this",
  "^guarantee that the information",
  "this e-mail is confidential",
  "received this e-mail incorrectly",
  "intended recipient is prohibited",
  "copying and", 
  "audit.data@progressiverecruitment.com"
)

filter <- str_c(filter_vector, collapse = "|")

# test out the filter
lined_bodies[[1]] |> 
  tidylog::filter(
    !str_detect(str_to_lower(value), filter) &
      str_length(value) > 1
  ) |> 
  summarise(
    body = str_c(value, collapse = " ")
  )



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

#str_remove_all(pattern = "[::]")

messages_data_content |> 
  tidylog::mutate(
    body_regex_remove = str_remove_all(body, "\\*|\\-{2,}|\\t|\\s{2,}"),
    regex_remove_lenght = str_length(body_regex_remove)
  ) |> 
  View()

# Clean email data --------------------------------------------------------

# apply the workable cleaning from all methods:
messages_data_cleaned <- messages_data_content |> 
  mutate(
    method_1 = map(
      .x = lined_bodies, 
      .f = \(x) x |> 
        filter(
          !str_detect(str_to_lower(value), filter) &
            str_length(value) > 1
        )
    ), 
    collapsed_body = map(
      .x = method_1,
      .f = \(x) x |>
        summarise(
          body = str_c(value, collapse = " ")
        ) |>
        pull(body)
    ),
    method_1_length = str_length(unlist(collapsed_body)), 
    method_3 = str_replace_all(
      string = str_remove_all(collapsed_body, "\\*|\\-{2,}|\\t"), 
      pattern = "\\s{2,}", 
      replacement = " "
    ),
    method_3_length = str_length(method_3),
    length_remaining_perc = method_3_length/original_length
  )



# 