
library(glue)
library(httr)

# You can get a new (temporary) access token here:
# https://dropbox.github.io/dropbox-api-v2-explorer/#files_list_folder


# List Files in a Folder on DropBox ---------------------------------------

path <- "/ODH/Data/Grantee Data Reports/YWCA"
recursive <- "false"

req <- httr::POST(
  url = "https://api.dropboxapi.com/2/file_requests/list_v2",
  httr::add_headers(
    Authorization = glue::glue("Bearer {Sys.getenv('DBOX_TEMP_TOKEN')}")
  ),
  httr::content_type("application/json"),
  body = glue::glue('{{"path":"{path}","recursive":{recursive}}}')
)

files <- httr::content(req)$entries

files[[2]]$path_display


# Upload a File to a DropBox Folder ---------------------------------------

file <- here::here("cohhio_test_dbox.csv")
dest <- "/ODH/Ketchbrook Analytics/ODH CSVs/hudx-111_1651250747"

# TODO // Could not get this approach to work:
# If using App Key + Secret approach, must first base64encode "key:secret"
# key_sec <- glue::glue("{Sys.getenv('DBOX_KEY')}:{Sys.getenv('DBOX_SEC')}") |>
#   charToRaw() |>
#   base64enc::base64encode()

# See Authentication Types here:
# https://www.dropbox.com/developers/reference/auth-types

req <- httr::POST(
  url = "https://content.dropboxapi.com/2/files/upload",
  httr::add_headers(
    # Authorization = glue::glue("Basic {key_sec}"),
    Authorization = glue::glue("Bearer {Sys.getenv('DBOX_TEMP_TOKEN')}"),   # OAuth
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



# Download a File from DropBox -------------------------------------------



# file <- here::here("cohhio_test_dbox.csv")
# dest <- "/ODH/Ketchbrook Analytics/test_file.csv"

from <- "/ODH/Ketchbrook Analytics/ODH CSVs/hudx-111_1651250747"

# TODO // Could not get this approach to work:
# If using App Key + Secret approach, must first base64encode "key:secret"
# key_sec <- glue::glue("{Sys.getenv('DBOX_KEY')}:{Sys.getenv('DBOX_SEC')}") |>
#   charToRaw() |>
#   base64enc::base64encode()

# See Authentication Types here:
# https://www.dropbox.com/developers/reference/auth-types

req <- httr::POST(
  url = "https://content.dropboxapi.com/2/files/download_zip",
  httr::add_headers(
    # Authorization = glue::glue("Basic {key_sec}"),
    Authorization = glue::glue("Bearer {Sys.getenv('DBOX_TEMP_TOKEN')}"),   # OAuth
    `Dropbox-API-Arg` = jsonlite::toJSON(
      list(
        path = from
      ),
      auto_unbox = TRUE
    )
  )
)
