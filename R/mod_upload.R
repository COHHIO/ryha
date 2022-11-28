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
mod_upload_server <- function(id){
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

      # Establish connection to PostgreSQL database
      con <- DBI::dbConnect(
        drv = RPostgres::Postgres(),
        dbname = Sys.getenv("AWS_POSTGRES_DBNAME"),
        host = Sys.getenv("AWS_POSTGRES_HOST"),
        port = Sys.getenv("AWS_POSTGRES_PORT"),
        user = Sys.getenv("AWS_POSTGRES_USER"),
        password = Sys.getenv("AWS_POSTGRES_PWD")
      )

      Sys.sleep(0.5)

      out <- process_data(file = input$choose_zip$datapath)

      if ("character" %in% class(out)) {

        shiny::modalDialog(
          title = "There was an issue with the upload",
          out
        ) |>
          shiny::showModal()

      } else {

        data <- out |>
          prep_tables(conn = con)

        delete_from_db(data = data, conn = con)

        send_to_db(data = data, conn = con)

        shiny::modalDialog(
          title = "Data uploaded successfully!",
          "Please refresh the app to see your data populate in the charts."
        ) |>
          shiny::showModal()

      }

      DBI::dbDisconnect(conn = con)

      w$hide()

    })

  })
}

## To be copied in the UI
# mod_upload_ui("upload_1")

## To be copied in the server
# mod_upload_server("upload_1")
