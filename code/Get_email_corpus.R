{
  library(tidyverse)
  library(gmailr)
} # libraries

# Authorization
gm_auth_configure(path = here::here("client_secret_151617107482-m73i66391midd7lvhmsmg6fmh27u1mom.apps.googleusercontent.com.json"))
gm_auth()

my_messages <- gm_messages(label_ids = "Label_8", num_results = 1000) |> 
  flatten() |> 
  flatten()

message_ids <- my_messages |> 
  enframe() |> 
  tidylog::filter(name == "") |> 
  unnest_auto(col = value)

messages <- map(
  .x = message_ids$id,
  .f = gm_message, 
  .progress = T
)

messages_data <- tibble::tibble(
  id = map(.x = messages, .f = gm_id),
  date = map(.x = messages, .f = gm_date),
  from = map(.x = messages, .f = gm_from),
  to = map(.x = messages, .f = gm_to),
  subject = map(.x = messages, .f = gm_subject),
  body = map(.x = messages, .f = gm_body), 
  attachments = map(.x = messages, .f = gm_attachments)
) |> 
  tidylog::mutate(
    body = map(.x = body, .f = unlist), 
    date = lubridate::date(lubridate::dmy_hms(str_sub(date, start = 5)))
  ) |> 
  tidylog::mutate_if(.predicate = is.list, .funs = as.character)


# Old ---------------------------------------------------------------------

# Search for all emails with the label "Opportunity" (This is how I archive opportunites)
# 
# lables <- gm_labels() |> 
#   flatten() |> 
#   enframe() |>
#   unnest_wider(col = value, names_repair = "minimal")
# # I find that the label_id = "Label_5096562256871952832" when looking at the available ids
# 
# # Get the threads contained within "Opportunities" label:
# # threads <- gm_threads(label_ids = "Label_5096562256871952832") 
# 
# # Get the threads contained within "AssignmentAnalyzer" label:
# # This is the label I use for Gerrie's emails.
# threads <- gm_threads(label_ids = "Label_8", num_results = 600) 
# 
# # Get the id's of these threads:
# thread_ids <- gm_id(threads)
# 
# # Get the messages within threads:
# threads_expanded <- map(
#   thread_ids, 
#   gm_thread, 
#   .progress = T
# )
# 
# msgs <- vector()
# 
# for(i in (1:length(threads_expanded))){
#   
#   msgs <- append(msgs, values = threads_expanded[[i]]$messages)
#   
# }
# 
# msg_ids <- unlist(map(msgs, gm_id))
# 
# thread_id_puller <- function(x) {
#   x |> 
#     pluck("threadId")
# }
# 
# msg_thread_ids <- unlist(map(msgs, thread_id_puller))
# 
# 
# msg_body <- vector()
# #get message body, store in vector
# for(msg in msgs){
#   body <- gm_body(msg)
#   attchmnt <- nrow(gm_attachments(msg))
#   if(length(body) != 0 && attchmnt == 0){
#     #does not return a null value, rather an empty list or listof length 0, so if,
#     #body is not 0 (there is something there) and there are no attachemts, 
#     #add it to vector
#     
#     msg_body <- append(msg_body, body)
#     #if there is no to info, fill that spot with an empty space
#   }
#   else{
#     msg_body <- append(msg_body, "")
#     #if there is no attachment but the body is also empty add "" to the list
#   }
# }
# 
# 
# msg_body <- unlist(msg_body)
# 
# msg_datetime <- msgs %>%
#   map(gm_date) %>%
#   unlist()%>%
#   lubridate::dmy_hms()
# 
# msg_from <- msgs |> 
#   map(gm_from) |> 
#   unlist()
# 
# msg_subject <- msgs |> 
#   map(gm_subject) |> 
#   unlist()
# 
# message_df <- tibble(
#   msg_ids, 
#   msg_thread_ids, 
#   msg_datetime, 
#   msg_from, 
#   msg_subject, 
#   msg_body
# ) |> 
#   arrange(msg_thread_ids, msg_datetime) |> 
#   group_by(
#     msg_thread_ids
#   ) |> 
#   mutate(
#     thread_seq = row_number(msg_thread_ids)
#   ) |> 
#   ungroup()
# 
# primary_mails <- message_df |> 
#   mutate(
#     msg_body = gsub(pattern = '\\\\[a-z]|>', replacement = '', x = str_to_lower(msg_body))
#   ) |> 
#   filter(
#     msg_from == "Gerrie van Wanrooij <wanrooij@entrador.com>",
#     thread_seq == 1, 
#     str_length(msg_body) > 5
#   )
# 
# 
