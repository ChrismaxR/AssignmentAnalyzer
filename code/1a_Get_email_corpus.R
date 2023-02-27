{
  library(tidyverse)
  library(gmailr)
} # libraries

# Authorization
gm_auth_configure(path = here::here(Sys.getenv("CLIENT_SECRET")))
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

