
# Quality control UI
qualityControlUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(
      width = NULL,
      title = span(icon("check-circle"), " Quality Control Assessment"),
      status = "primary",
      solidHeader = TRUE,
      style = "background-color: #EAF2F8; border-radius: 10px; padding: 15px;",

      sidebarLayout(
        sidebarPanel(
          style = "background-color: #F8F9FA; border-radius: 10px; padding: 15px;",

          # 📂 File Selection
          h4("📂 Select FASTQ Files", style = "color: #0092AC;"),
          selectInput(ns("select_fastq_file"),
                      label = span(icon("file-upload"), " Choose FASTQ Files:"),
                      choices = c("None"), multiple = TRUE),

          # 📊 Metrics Selection
          h4("📊 Choose Quality Control Metric", style = "color: #0092AC;"),
          selectInput(ns("data_type"),
                      label = span(icon("chart-line"), " Available QC Metrics:"),
                      choices = c(
                        "None",
                        "Average Quality",
                        "Cycle-specific Average Quality",
                        "Cycle-specific Base Call Proportion",
                        "Cycle-specific GC Content",
                        "Cycle-specific Quality Distribution",
                        "Cycle-specific Quality Distribution - Boxplot",
                        "Per Read Mean Quality Distribution of Files",
                        "Read Frequency",
                        "Read Length Distribution"
                      )),

          hr(),

          # 📥 Download Report Button (Small)
          div(
            style = "display: flex; justify-content: left;",
            downloadButton(ns("download_data"),
                           "Download QC Report",
                           style = "color: #ffffff; background-color: #0092AC;
                           border-color: #007B9E; padding: 6px 12px; font-size: 14px;
                           border-radius: 5px; width: auto;")
          )

        ),

        mainPanel(
          tabBox(
            width = 12,

           # 📈 Graphics Tab
                        tabPanel(
                          title = span(icon("chart-bar"), " QC Visualization"),
                          value = "tab-graphics",
                            plotOutput(ns("quality_plot"), height = "400px"),

                          br(),

                          # 📥 Download Plot Button (Small)
                          div(
                            style = "display: flex; justify-content: left;",
                            downloadButton(ns("download_plot"),
                                           "Download Plot",
                                           style = "color: #ffffff; background-color: #0092AC;
                                           border-color: #007B9E; padding: 6px 12px; font-size: 14px;
                                           border-radius: 5px; width: auto;")
                          )
                        ),
           # 📊 Data Summary Tab
                       tabPanel(
                         title = span(icon("table"), " QC Data Summary"),
                         value = "tab-data",

                         box(
                           title = span(icon("database"), " Data Summary of QC Analysis"),
                           width = NULL,
                           status = "info",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           style = "background-color: #ffffff; border-radius: 10px; padding: 15px;",
                           DT::dataTableOutput(ns("quality_data"))
                         ),

                         br(),

                         # 📥 Download Data Button (Small)
                         div(
                           style = "display: flex; justify-content: left;",
                           downloadButton(ns("download_data_csv"),
                                          "Download CSV",
                                          style = "color: #ffffff; background-color: #0092AC;
                                          border-color: #007B9E; padding: 6px 12px; font-size: 14px;
                                          border-radius: 5px; width: auto;")
                         )
                       )
          ),

          br(),

          # ❓ Help Button (Small)
          div(
            style = "display: flex; justify-content: left;",
            actionButton(ns("help"),
                         "Help",
                         icon = icon("info-circle"),
                         style = "color: #ffffff; background-color: #F39C12;
                         border-color: #E67E22; padding: 6px 12px; font-size: 14px;
                         border-radius: 5px; width: auto;")
          )
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

    select_files(session, "select_fastq_file", importedData, "Select Files")

    # Render the quality plot
    output$quality_plot <- renderPlot({

      # Check if no plot is selected or data is not provided
      if (is.null(input$select_fastq_file) || "None" %in% input$select_fastq_file || input$data_type == "None" || is.null(importedData())) {
        blank_plot()
        return()
      }

      quality_plots(input$data_type, input$select_fastq_file,importedData)
      })


    # Render the quality data table
    output$quality_data <- DT::renderDataTable({

      if (is.null(input$select_fastq_file) || "None" %in% input$select_fastq_file|| input$data_type == "None" || is.null(importedData())) {
        return(as.matrix("Must have at least one file selected and a type of data chosen."))
      }

      df <- datatable(input$data_type, input$select_fastq_file,importedData)

      DT::datatable(df,options = list(scrollX = TRUE, pageLength = 5))
    })

    # Download plot handler
    output$download_plot <- downloadHandler(
      filename = function() {
        paste("quality_plot_", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        if (!is.null(input$select_fastq_file) && input$data_type != "None" && !is.null(importedData())) {
          # Open a PNG device
          png(file, width = 700, height = 300)

          # Generate the plot
          plot <- quality_plots(input$data_type, input$select_fastq_file, importedData)
          print(plot)  # Ensure the plot is actually printed

          # Close the device
          dev.off()
        }
      }
    )


    output$download_data_csv <- downloadHandler(
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





