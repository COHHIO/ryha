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
          inputId = ns("choose_zip"),
          label = "Choose HMIS .Zip File",
          accept = ".zip"
        ),

        shiny::actionButton(
          inputId = ns("upload_btn"),
          label = "Upload"
        )

      )

    )

  )
}

#' upload Server Functions
#'
#' @noRd
mod_upload_server <- function(id, conn){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    w <- waiter::Waiter$new(
      html = shiny::tagList(
        waiter::spin_fading_circles(),
        "Please Wait..."
      )
    )

    # Process the data in the uploaded .zip & write to Postgres
    shiny::observeEvent(input$upload_btn, {

      shiny::req(input$choose_zip$datapath)

      w$show()

      Sys.sleep(1)

      process_data(file = input$choose_zip$datapath) |>
        prep_tables(conn = conn) |>
        send_to_db(conn = conn)

      w$hide()

    })



    # shiny::modalDialog(
    #   title = "Files Missing from Upload",
    #   "We could not find the following files in the .zip file you uploaded:",
    #   shiny::br(),
    #   vec_to_ul(vec = c("File1", "File2"))
    # ) |>
    #   shiny::showModal()


  })
}

## To be copied in the UI
# mod_upload_ui("upload_1")

## To be copied in the server
# mod_upload_server("upload_1")
