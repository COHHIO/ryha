#' upload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_upload_ui <- function(id){
  ns <- NS(id)
  tagList(

    shiny::fluidRow(

      shiny::column(
        width = 3,

        shiny::fileInput(
          inputId = "upload_zip",
          label = "Upload HMIS .Zip File",
          accept = ".zip"
        )

      )

    )

  )
}

#' upload Server Functions
#'
#' @noRd
mod_upload_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    uploaded_data <- shiny::reactive({

      file <- input$upload_zip

      # Require that the uploaded file is indeed a .zip file
      ext <- tools::file_ext(file$datapath)

      shiny::req(file)

      shiny::validate(
        shiny::need(ext == "csv", "Please upload a .zip file")
      )

    })

    shiny::modalDialog(
      title = "Files Missing from Upload",
      "We could not find the following files in the .zip file you uploaded:",
      shiny::br(),
      vec_to_ul(vec = c("File1", "File2"))
    ) |>
      shiny::showModal()


  })
}

## To be copied in the UI
# mod_upload_ui("upload_1")

## To be copied in the server
# mod_upload_server("upload_1")
