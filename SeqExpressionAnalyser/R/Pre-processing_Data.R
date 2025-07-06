

Filter_TrimUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(
      width = NULL,
      title = span(icon("filter"), " Filtering & Trimming Reads"),
      status = "primary",
      solidHeader = TRUE,
      style = "background-color: #EAF2F8; border-radius: 10px; padding: 15px;",

      sidebarLayout(
        sidebarPanel(
          style = "background-color: #F8F9FA; border-radius: 10px; padding: 15px;",

          # 🔬 Set Filtering Parameters
                    h4("🔬 Set Filtering Parameters", style = "color: #0092AC;"),
                    checkboxInput(ns("pair_single"), "Paired-End Sequencing", FALSE),
                    textInput(ns("truncateStartBases"), "Bases to Remove (Start):", placeholder = "Start Bases"),
                    textInput(ns("truncateEndBases"), "Bases to Remove (End):", placeholder = "End Bases"),
                    textInput(ns("minLength"), "Minimum Sequence Length:", placeholder = "Min Length"),
                    uiOutput(ns("adapters_remover")),
                    textInput(ns("nBases"), "Max 'N' Bases Allowed:", placeholder = "Max Ns"),
                    textInput(ns("complexity"), "Minimum Sequence Complexity:", placeholder = "Complexity"),
                    hr(),

                    # 🚀 Filter & Trim Button (Small)
                    actionButton(ns("btn_filter"), "Run Filtering & Trimming", icon = icon("play"),
                                 style = "color: #ffffff; background-color: #0092AC;
                                 border-color: #007B9E; padding: 6px 12px; font-size: 14px;
                                 border-radius: 5px; width: auto;")
        ),

        mainPanel(
          tabBox(
            width = 12,
            # 📊 Filtered Data Summary
                        tabPanel(
                          title = span(icon("chart-bar"), " Preprocessing Results"),
                          value = "tab-preprocessing",
                          h4("📂 Select FASTQ Files", style = "color: #0092AC;"),
                          selectInput(ns("inputfile"), "Choose FASTQ Files:", choices = c(), multiple = TRUE),
                          uiOutput(ns("filemate")),
                          helpText("Multiple files can be processed, but all sequence file vectors must have identical lengths."),

                          hr(),
                          box(
                            title = span(icon("database"), " Filtered & Trimmed Data Summary"),
                            width = NULL,
                            status = "primary",
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            style = "background-color: #ffffff; border-radius: 10px; padding: 15px;",
                            DT::dataTableOutput(ns("tb_filter_trim"))
                          ),
                          # 📥 Download Filtering report Button (Small)
                          div(
                            style = "display: flex; justify-content: left;",
                            downloadButton(ns("dwn_filter_report"),
                                           "Download",
                                           style = "color: #ffffff; background-color: #0092AC;
                                           border-color: #007B9E; padding: 6px 12px; font-size: 14px;
                                           border-radius: 5px; width: auto;")))),

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


Filter_TrimServer <- function(id, importedData) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- NS(id)

      #Reactive value to store the Filtered data.
      filtered_data <- reactiveVal(NULL)

      select_files(session, "inputfile", importedData, "Select Files")

      observeEvent(input$pair_single, {
        if (input$pair_single) {
          output$filemate <- renderUI({
            selectInput(ns("filenameMate"), "Sequence pairs from paired-end experiments", choices = c(), multiple = TRUE)
          })
          select_files(session, "filenameMate", importedData,"Sequence pairs from paired-end experiments")
          output$adapters_remover <- renderUI({ NULL })
        } else {
          output$adapters_remover <- renderUI({
            list(textInput(ns("Lpattern"), "Left Adapter (5'-end):", placeholder = "Left Adapter"),
                 textInput(ns("Rpattern"), "Right Adapter (3'-end):", placeholder = "Right Adapter"))
            })
          output$filemate <- renderUI({NULL})
        }
      })

      filter <- reactive({
        tryCatch({
          input_file <- input$inputfile

          if (is.null(input_file) || length(input_file) == 0) {
            showModal(modalDialog(title = "Error", "No input files provided.", easyClose = TRUE))
            return(NULL)
          }

          path_info <- perFileInformation(importedData())$path
          if (is.null(path_info) || length(path_info) == 0) {
            showModal(modalDialog(title = "Error", "Invalid file path information.", easyClose = TRUE))
            return(NULL)
          }

          path <- path_info[1]

          # Generate file paths
          input_files_path <- vapply(input_file, function(file) file.path(path, file), FUN.VALUE = character(1))

          # Handle paired-end data only if enabled
          if (!is.null(input$pair_single) && input$pair_single) {
            filenameMate <- input$filenameMate
            if (is.null(filenameMate) || length(filenameMate) == 0) {
              showModal(modalDialog(title = "Error", "Paired-end mode selected, but no mate files provided.", easyClose = TRUE))
              return(NULL)
            }
            filesnameMates_path <- vapply(filenameMate, function(file) file.path(path, file), FUN.VALUE = character(1))
          } else {
            filesnameMates_path <- character(0)  # Ensure it's empty if not used
          }

          full_extension <- tools::file_ext(input_file[1])  # Extract file extension safely
          print(full_extension)

          # Create output directory
          outputDir <- file.path(path, "FilteredOutput")
          if (!dir.exists(outputDir)) {
            dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)
            print(paste("Created output directory:", outputDir))
          }

          # Generate output file names
          files_names <- tools::file_path_sans_ext(basename(input_files_path))
          output_files <- file.path(outputDir, paste0(files_names, "-Filtered.", full_extension))

          if (length(filesnameMates_path) > 0) {
            files_mate_names <- tools::file_path_sans_ext(basename(filesnameMates_path))
            output_mate_files <- file.path(outputDir, paste0(files_mate_names, "-FilteredMate.", full_extension))
          } else {
            output_mate_files <- character(0)  # Empty if not used
          }

          print("Running preprocessReads...")

          # Call preprocessReads with the correct arguments
          if (!is.null(input$pair_single) && input$pair_single) {
            results <- preprocessReads(
              filename = input_files_path,
              outputFilename = output_files,
              filenameMate = filesnameMates_path,
              outputFilenameMate = output_mate_files,
              nBases = as.numeric(input$nBases),
              truncateStartBases = as.numeric(input$truncateStartBases),
              truncateEndBases = as.numeric(input$truncateEndBases),
              Lpattern = as.character(input$Lpattern),
              Rpattern = as.character(input$Rpattern),
              complexity = as.numeric(input$complexity),
              minLength = as.numeric(input$minLength)
            )
          } else {
            results <- preprocessReads(
              filename = input_files_path,
              outputFilename = output_files,
              nBases = as.numeric(input$nBases),
              truncateStartBases = as.numeric(input$truncateStartBases),
              truncateEndBases = as.numeric(input$truncateEndBases),
              Lpattern = as.character(input$Lpattern),
              Rpattern = as.character(input$Rpattern),
              complexity = as.numeric(input$complexity),
              minLength = as.numeric(input$minLength)
            )
          }

          print("Results processed successfully.")
          return(as.data.frame(results))
          print("Done")

        }, warning = function(w) {
          showModal(modalDialog(title = "Warning", paste("Warning:", conditionMessage(w)), easyClose = TRUE))
          NULL
        }, error = function(e) {
          showModal(modalDialog(title = "Error", paste("Error:", conditionMessage(e)), easyClose = TRUE))
          NULL
        })
      })

      # Event reactive for button click to process uploaded data
      observeEvent(input$btn_filter, {
        if (is.null(importedData())) {
          showModal(
            modalDialog(
              title = "Data Import Required",
              "Please import your FASTQ files before proceeding.",
              easyClose = TRUE
            )
          )
          filtered_data(NULL)
        } else {
          withProgress(message = 'Processing data', value = 0, {
            data <- filter()
            filtered_data(data)
          })
        }
      })

      # Render the Data Table using the reactive value
      output$tb_filter_trim <- DT::renderDataTable({
        if (is.null(importedData())) {
          return(data.frame(Message = "Import your FASTQ files"))
        }
        if (!is.null(filtered_data())) {
          DT::datatable(filtered_data(), options = list(scrollX = TRUE))
        }
      })

      # Download handler for the count matrix
      output$dwn_filter_report <- downloadHandler(
        filename = function() {
          paste("filter_report", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
          write.csv(filtered_data() , file, row.names = FALSE)
        }
      )
    }
  )
}

