source("./R/helpers.R")

# UI Module
qualityControlUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(
      width = FALSE,
      title = "Quality Assessment",
      status = "primary",
      solidHeader = TRUE,
      actionButton(ns("help"), " Help ", icon("circle-question"), style = "color: #ffffff; background-color: #0092AC; border-color: #2e6da4"),
      hr(),
      fluidRow(column(
        6,
        box(
          width = NULL,
          solidHeader = TRUE,
          status = "info",
          selectInput(
            ns("select_fastq_file"),
            "Select FASTQ files",
            choices = c("None"),
            multiple = TRUE
          )
        )
      ), column(
        6,
        box(
          width = NULL,
          solidHeader = TRUE,
          status = "info",
          selectInput(ns("data_type"), "Available Graphics and data Tables", choices = (
            c(
              "None",
              "Average Quality",
              "Cycle-specific Average Quality",
              "Cycle-specific Base Call Proportion",
              "Cycle-specific GC Content",
              "Cycle-specific Quality Distribution",
              "Cycle-specific Quality Distribution - Boxplot",
              "PCA Biplot (cycle-specific read average quality)",
              "Per Read Mean Quality Distribution of Files",
              "Read Frequency",
              "Read Length Distribution"
            )
          ))
        )
      )),
      actionButton(
        ns('quality_report'),
        " Upload FASTQ Files",
        icon("upload"),
        style = "color: #ffffff; background-color: #0092AC; border-color: #2e6da4"
      ),
      textOutput(ns("qualityReport")),
      br(),
      tabBox(
        width = 12,
        tabPanel(
          title = "Graphics",
          icon = icon("upload"),
          value = "tab-graphics",
          plotOutput(ns("quality_plot")),
          downloadButton(ns("download_plot"), "Download Plot", style = "color: #ffffff; background-color: #0092AC; border-color: #2e6da4")
        ),
        tabPanel(
          title = "Data",
          icon = icon("upload"),
          value = "tab-data",
          box(
            width = NULL,
            status = "danger",
            solidHeader = FALSE,
            title = "The data description of the selected plot",
            DT::dataTableOutput(ns("quality_data")),
          ),
          br(),
          downloadButton(ns("download_data"), "Download Data as CSV", style = "color: #ffffff; background-color: #0092AC; border-color: #2e6da4")
        )
      )
    )
  )
}

# Server Module
qualityControlServer <- function(id, importedData) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Check if data is imported
    observeEvent(input$quality_report, {
      if (is.null(importedData())) {
        showModal(
          modalDialog(
            title = "Data Import Required",
            "Please import your FASTQ files before proceeding.",
            easyClose = TRUE,
            footer = NULL
          )
        )
      }
    })

    select_files(session, "select_fastq_file", importedData)

    # Render the quality plot
    output$quality_plot <- renderPlot({

      # Check if no plot is selected or data is not provided
      if (is.null(input$select_fastq_file) || "None" %in% input$select_fastq_file || input$data_type == "None" || is.null(importedData())) {
        blank_plot()
        return()
      }
      # Check specific requirements for the PCA Biplot
      if (input$data_type == "PCA Biplot (cycle-specific read average quality)" && length(input$select_fastq_file) <= 1) {
        showModal(
          modalDialog(
            title = "Multiple Files Required",
            "Please select at least two FASTQ files to proceed.",
            easyClose = TRUE,
            footer = NULL
          )
        )
        blank_plot()
        return()
      }
      plots(input$data_type, input$select_fastq_file,importedData)
      })


    # Render the quality data table
    output$quality_data <- DT::renderDataTable({

      if (is.null(input$select_fastq_file) || "None" %in% input$select_fastq_file|| input$data_type == "None" || is.null(importedData())) {
        return(as.matrix("Must have at least one file selected and a type of data chosen."))
      }

      if (input$data_type == "Cycle-specific Base Call Proportion") {
        return(as.matrix("No availabe data for Cycle-specific Base Call Proportion"))
      }

      datatable(input$data_type, input$select_fastq_file,importedData)
    })

    # Download plot handler
    output$download_plot <- downloadHandler(
      filename = function() {
        paste("quality_plot_", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        png(file)
        if (!is.null(input$select_fastq_file) && input$data_type != "None" && !is.null(importedData())) {
          plots(input$data_type, input$select_fastq_file,importedData)
        }
        dev.off()
      }
    )

    # Download data handler
    output$download_data <- downloadHandler(
      filename = function() {
        paste("quality_data_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        if (!is.null(input$select_fastq_file) && input$data_type != "None" && !is.null(importedData())) {
          resultList <- lapply(input$select_fastq_file, function(file) {
            data_functions[[input$data_type]](importedData()[file])
          })
          resultData <- do.call(rbind, resultList)
          write.csv(resultData, file, row.names = FALSE)
        }
      }
    )
  })
}
