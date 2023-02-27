{
  library(tidyverse)
  library(gmailr)
}



# Logic to save all attachments to disk, so I can pick up the job advert texts
# in case they are included in an attachments instead of the email itself. 

walk(
  .x = messages,
  .f = \(x) gm_save_attachments(x, path = here::here("data", "attachments")), 
  .progress = T
)


# Attachments which are pngs -> delete! -----------------------------------

# emails contain a lot of .png attachments that don't offer any value
# this function set deals with finding the .png file on disk which where created
# with the gm_save_attachments function and delete them. 

pngs <- fs::dir_ls(regexp = ".png$|.jpg$|.gif$|.jpeg$", here::here("data", "attachments"))

walk(
  .x = pngs,
  .f = fs::file_delete
)

# Read attachments as flat texts ------------------------------------------

#??

