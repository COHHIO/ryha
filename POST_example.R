
library(glue)
library(httr)

# You can get a new (temporary) access token here:
# https://dropbox.github.io/dropbox-api-v2-explorer/#files_list_folder


# List Files in a Folder on DropBox ---------------------------------------

path <- "/ODH/Data/Grantee Data Reports/YWCA"
recursive <- "false"

req <- httr::POST(
  url = "https://api.dropboxapi.com/2/files/upload",
  httr::add_headers(
    Authorization = glue::glue("Bearer {Sys.getenv('DBOX_TOKEN')}")
  ),
  httr::content_type("application/json"),
  body = glue::glue('{{"path":"{path}","recursive":{recursive}}}')
)

files <- httr::content(req)$entries

files[[2]]$path_display


# Upload a File to a DropBox Folder ---------------------------------------

file <- here::here("cohhio_test_dbox.csv")
dest <- "/ODH/Ketchbrook Analytics/test_file.csv"

req <- httr::POST(
  url = "https://content.dropboxapi.com/2/files/upload",
  httr::add_headers(
    Authorization = glue::glue("Bearer {Sys.getenv('DBOX_TOKEN')}"),
    `Dropbox-API-Arg` = jsonlite::toJSON(
      list(
        path = dest
      ),
      auto_unbox = TRUE
    )
  ),
  httr::content_type("application/octet-stream"),
  body = httr::upload_file(path = file)
)
