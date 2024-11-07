## code to prepare `CountyCodes` dataset goes here

# Source: https://www.hud.gov/sites/dfiles/CPD/documents/CoC/FY2024-GeoCodes-Report.pdf

# NOTE: City and county data is created separately because VS Code couldn't
# handle as many rows in tibble::tribble()

# Create city data
# County for each city was manually added
city_data <- tibble::tribble(
  ~geocode, ~city, ~county,
  "390042", "Akron", "Summit",
  "390066", "Alliance", "Stark",
  "390294", "Barberton", "Summit",
  "390600", "Bowling Green", "Wood",
  "390858", "Canton", "Stark",
  "391062", "Cincinnati", "Hamilton",
  "391104", "Cleveland", "Cuyahoga",
  "391110", "Cleveland Heights", "Summit",
  "391176", "Columbus", "Clark",
  "391320", "Cuyahoga Falls", "Summit",
  "391362", "Dayton", "Montgomery",
  "391500", "East Cleveland", "Cuyahoga",
  "391602", "Elyria", "Lorain",
  "391626", "Euclid", "Cuyahoga",
  "391638", "Fairborn", "Greene",
  "392118", "Hamilton City", "Butler",
  "392508", "Kent", "Portage",
  "392526", "Kettering", "Montgomery",
  "392628", "Lakewood", "Cuyahoga",
  "392634", "Lancaster", "Fairfield",
  "392730", "Lima", "Allen",
  "392820", "Lorain", "Lorain",
  "393012", "Mansfield", "Richland",
  "393054", "Marietta", "Washington",
  "393114", "Massillon", "Stark",
  "393168", "Mentor", "Lake",
  "393222", "Middletown", "Butler",
  "393558", "Newark", "Licking",
  "394098", "Parma", "Cuyahoga",
  "394680", "Sandusky", "Erie",
  "394998", "Springfield", "Clark",
  "395016", "Steubenville", "Jefferson",
  "395214", "Toledo", "Lucas",
  "395454", "Warren", "Trumbull",
  "395874", "Youngstown", "Mahoning"
) |>
  # Remove city column
  dplyr::select(-city)

# Create county data
county_data <- tibble::tribble(
  ~geocode, ~county,
  "399001", "Adams County",
  "399003", "Allen County",
  "399005", "Ashland County",
  "399007", "Ashtabula County",
  "399009", "Athens County",
  "399011", "Auglaize County",
  "399013", "Belmont County",
  "399015", "Brown County",
  "399017", "Butler County",
  "399019", "Carroll County",
  "399021", "Champaign County",
  "399023", "Clark County",
  "399025", "Clermont County",
  "399027", "Clinton County",
  "399029", "Columbiana County",
  "399031", "Coshocton County",
  "399033", "Crawford County",
  "399035", "Cuyahoga County",
  "399037", "Darke County",
  "399039", "Defiance County",
  "399041", "Delaware County",
  "399043", "Erie County",
  "399045", "Fairfield County",
  "399047", "Fayette County",
  "399049", "Franklin County",
  "399051", "Fulton County",
  "399053", "Gallia County",
  "399055", "Geauga County",
  "399057", "Greene County",
  "399059", "Guernsey County",
  "399061", "Hamilton County",
  "399063", "Hancock County",
  "399065", "Hardin County",
  "399067", "Harrison County",
  "399069", "Henry County",
  "399071", "Highland County",
  "399073", "Hocking County",
  "399075", "Holmes County",
  "399077", "Huron County",
  "399079", "Jackson County",
  "399081", "Jefferson County",
  "399083", "Knox County",
  "399085", "Lake County",
  "399087", "Lawrence County",
  "399089", "Licking County",
  "399091", "Logan County",
  "399093", "Lorain County",
  "399095", "Lucas County",
  "399097", "Madison County",
  "399099", "Mahoning County",
  "399101", "Marion County",
  "399103", "Medina County",
  "399105", "Meigs County",
  "399107", "Mercer County",
  "399109", "Miami County",
  "399111", "Monroe County",
  "399113", "Montgomery County",
  "399115", "Morgan County",
  "399117", "Morrow County",
  "399119", "Muskingum County",
  "399121", "Noble County",
  "399123", "Ottawa County",
  "399125", "Paulding County",
  "399127", "Perry County",
  "399129", "Pickaway County",
  "399131", "Pike County",
  "399133", "Portage County",
  "399135", "Preble County",
  "399137", "Putnam County",
  "399139", "Richland County",
  "399141", "Ross County",
  "399143", "Sandusky County",
  "399145", "Scioto County",
  "399147", "Seneca County",
  "399149", "Shelby County",
  "399151", "Stark County",
  "399153", "Summit County",
  "399155", "Trumbull County",
  "399157", "Tuscarawas County",
  "399159", "Union County",
  "399161", "Van Wert County",
  "399163", "Vinton County",
  "399165", "Warren County",
  "399167", "Washington County",
  "399169", "Wayne County",
  "399171", "Williams County",
  "399173", "Wood County",
  "399175", "Wyandot County"
) |>
  dplyr::mutate(
    # Remove "County" from county name
    county = stringr::str_replace(
      string = county,
      pattern = " County",
      replacement = ""
    )
  )

CountyCodes <- dplyr::bind_rows(
  city_data,
  county_data
) |>
  dplyr::arrange(county, geocode)

usethis::use_data(CountyCodes, overwrite = TRUE)
