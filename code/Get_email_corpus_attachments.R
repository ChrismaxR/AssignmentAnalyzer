# source(here::here("code", "Get_email_corpus.R"))

#  Get Attachments --------------------------------------------------------

sample <- threads_expanded[[4]]$messages 

listviewer::jsonedit(sample)

id <- sample[[1]]$id
fileName <- sample[[1]]$payload$parts[[2]]$filename
attachmentId <- sample[[1]]$payload$parts[[2]]$headers[[4]]$value

tictoc::tic()
gm_save_attachment(x = gm_message(id), attachment_id = attachmentId, path = here::here("data", "attachments"), user_id = "me")
tictoc::toc()

threads_attachments <- map(
  thread_ids, 
  gm_attachment()
)

test <- gm_message(threads[[1]][["threads"]][[1]][["id"]])

gm_save_attachments(x = test, filename = here::here("data", "attachements"))