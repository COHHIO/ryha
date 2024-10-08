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
        width = 8,
        offset = 2,

        bs4Dash::box(
          width = 12,
          title = "Instructions for Uploading:",
          collapsible = FALSE,
          status = "primary",

          shiny::HTML(
            "
            <ol>
              <li> Click the <strong>Browse...</strong> button and open the HMIS export .zip file on your local computer
              <li> Enter the password needed to upload the .zip file to the app database
              <li> If the correct password has been entered, the <strong>Upload</strong> button will be enabled. Click the button to upload the file
            </ol>
            If you encounter an error screen during the process, please copy and paste the error and email <a href='mailto:amandawilson@cohhio.org'>amandawilson@cohhio.org</a>
            "
          )
        ) |>
          shiny::tagAppendAttributes(class = "custom-box"),

        shiny::hr(),

        shiny::div(
          align = "center",

          bs4Dash::box(
            width = 6,
            collapsible = FALSE,

            shiny::fluidRow(

              shiny::fileInput(
                inputId = ns("choose_zip"),
                label = "Choose HMIS .Zip File",
                accept = ".zip",
                width = "100%"
              ),

              shiny::passwordInput(
                inputId = ns("upload_pwd"),
                label = "Enter Upload Password",
                placeholder = "Enter Password...",
                width = "100%"
              ),

              shinyWidgets::actionBttn(
                inputId = ns("upload_btn"),
                label = "Upload",
                color = "success",
                block = TRUE,
                style = "material-flat"
              ) |>
                shiny::tagAppendAttributes(style = "margin-top: 2rem")

            )

          ) |>
            shiny::tagAppendAttributes(class = "custom-box")
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

    # Create the {waiter} loading screen
    w <- waiter::Waiter$new()

    shiny::observe({

      if (is.null(input$upload_pwd) || input$upload_pwd != Sys.getenv("UPLOAD_PWD") || is.null(input$choose_zip$datapath)) {

        shinyjs::disable(id = "upload_btn")

      } else {

        shinyjs::enable(id = "upload_btn")

      }

    })

    # Process the data in the uploaded .zip & write to Postgres
    shiny::observeEvent(input$upload_btn, {

      shiny::req(input$choose_zip$datapath)

      # Initialize the waiter
      w$show()

      # Update waiter message
      w$update(spinner_message("Step 1/5: Connecting to database..."))

      # Establish connection to PostgreSQL database
      con <- connect_to_db(env = Sys.getenv("APP_BACKEND"))

      Sys.sleep(0.5)

      # Update waiter message
      w$update(spinner_message("Step 2/5: Processing data..."))

      data <- process_data_safely(file = input$choose_zip$datapath)

      if (!is.null(data$error)) {

        shiny::modalDialog(
          title = "Upload Failed",
          shiny::HTML(data$error$message),
          shiny::br(), shiny::br(),
          shiny::em("Failed during stage: `process_data()`")
        ) |>
          shiny::tagAppendAttributes(class = "error-modal") |>
          shiny::showModal()

      } else {

        # Update waiter message
        w$update(spinner_message("Step 3/5: Preparing tables..."))

        data <- data$result |>
          prep_tables_safely(conn = con)

        if (!is.null(data$error)) {

          shiny::modalDialog(
            title = "Upload Failed",
            data$error$message,
            shiny::br(), shiny::br(),
            shiny::em("Failed during stage: `prep_tables()`")
          ) |>
            shiny::tagAppendAttributes(class = "error-modal") |>
            shiny::showModal()

        } else {

          # Update waiter message
          w$update(spinner_message("Step 4/5: Deleting matching records..."))

          out <- data$result |>
            delete_from_db_safely(conn = con)

          if (!is.null(out$error)) {

            shiny::modalDialog(
              title = "Upload Failed",
              out$error$message,
              shiny::br(), shiny::br(),
              shiny::em("Failed during stage: `delete_from_db()`")
            ) |>
              shiny::tagAppendAttributes(class = "error-modal") |>
              shiny::showModal()

          } else {

            out <- data$result |>
              send_to_db_safely(conn = con, waiter = w)

            if (!is.null(out$error)) {

              data$result |>
                delete_from_db(conn = con)

              shiny::modalDialog(
                title = "Upload Failed",
                out$error$message,
                shiny::br(), shiny::br(),
                shiny::em("Failed during stage: `send_to_db()`")
              ) |>
                shiny::tagAppendAttributes(class = "error-modal") |>
                shiny::showModal()

            } else {

              shiny::modalDialog(
                title = "Data uploaded successfully!",
                "Please refresh the app to see your data populate in the charts."
              ) |>
                shiny::showModal()

            }

          }

        }

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
