SexCodes <- tibble::tribble(
    ~Code, ~Description,
    0L, "Female",
    1L, "Male",
    8L, "Client doesn't know",
    9L, "Client prefers not to answer",
    99L, "Data not collected"
)

usethis::use_data(SexCodes, overwrite = TRUE)
