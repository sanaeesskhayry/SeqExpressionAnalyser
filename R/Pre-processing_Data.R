# UI function for the Filter and Trim section
Filter_TrimUI <- function(id) {
  ns <- NS(id)
  tagList(
    tabBox(
      width = FALSE,
      tabPanel(
        title = "Filtering and trimming reads",
        icon = icon("filter"),
        value = "tab-readData",
        actionButton(ns("help"), " Help ", icon("circle-question"), style = "color: #ffffff; background-color: #0092AC; border-color: #2e6da4"),
        hr(),
        fluidRow(column(
          6,
          box(
            width = FALSE,
            selectInput(ns("inputfile"),"Select one or more FASTQ files to process using the parameters defined below.", choices = c(), multiple = TRUE),
            helpText("Multiple files can be processed; in such cases, all sequence file vectors must have identical lengths.")
          )
        ),
        column(
          6,
          box(
            width = FALSE,
            selectInput(ns("filenameMate"),"Sequence pairs from paired-end experiments can be processed by specifying pairs of input files", choices = c(), multiple = TRUE)
          )
        )),
        h2("Set Filtering Parameters"),
        splitLayout(
          box(
            width = FALSE,
            solidHeader = FALSE,
            textInput(ns("truncateStartBases"), "The number of bases to be truncated (removed) from the beginning of each sequence.", placeholder = "truncate Start Bases"),
            textInput(ns("truncateEndBases"), "The number of bases to be truncated (removed) from the end of each sequence.", placeholder = "truncate End Bases"),
            textInput(ns("Lpattern"), "The left (5'-end) adapter sequence.", placeholder = "Left pattern"),
            textInput(ns("Rpattern"), "The right (3'-end) adapter sequence.", placeholder = "Right pattern")
          ),
          box(
            width = FALSE,
            solidHeader = FALSE,
            textInput(ns("minLength"), "The minimal allowed sequence length.", placeholder = "min Length"),
            textInput(ns("nBases"), "The maximal number of Ns allowed per sequence.", placeholder = "N Bases"),
            textInput(ns("complexity"), "The minimal sequence complexity", placeholder = "complexity")
          )
        ),
        actionButton(
          ns("btn_filter"),
          "Filter & Trim Data",
          icon("play"),
          style = "color: #ffffff; background-color: #0092AC; border-color: #2e6da4"
        ),
        helpText("Check your filtered data in the same directory that contains non-filtered fastq input file(s) "),


      ),
      # Data Overview Panel
      tabPanel(
        title = "Data Summary and Insights",
        icon = icon("file-contract"),
        value = "tab-dataOverview",
        actionButton(ns("help"), " Help ", icon("circle-question"), style = "color: #ffffff; background-color: #0092AC; border-color: #2e6da4"),
        hr(),
        box(
          width = NULL,
          status = "danger",
          solidHeader = FALSE,
          title = "Results of preprocessing",
          DT::dataTableOutput(ns("tb_filter_trim"))
        )
      )
    )
  )
}




Filter_TrimServer <- function(id, importedData) {
  moduleServer(
    id,
    function(input, output, session) {

      select_files(session, "inputfile", importedData)
      select_files(session, "filenameMate", importedData)


      filter <- reactive({
        tryCatch({
          print("filter")

          input_file <- input$inputfile
          filenameMate <- input$filenameMate

          if (length(input_file) == 0) stop("No input files provided.")
          if (length(filenameMate) == 0) stop("No filename mates provided.")

          # Get the file path from imported data
          path <- perFileInformation(importedData())$path

          # Generate file paths
          input_files_path <- unname(unlist(sapply(input_file, function(file) paste0(path[1], "/", file))))
          filesnameMates_path <- unname(unlist(sapply(filenameMate, function(file) paste0(path[1], "/", file))))

          print(class(input_files_path))
          print(filesnameMates_path)

          full_extension <- sub("^[^.]*\\.", "", input_file[1])  # Extract extension
          print(full_extension)

          # Create output directory
          outputDir <- file.path(path[1], "FilteredOutput")
          if (!dir.exists(outputDir)) {
            dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)
            print(paste("Created output directory:", outputDir))
          }
          outputDirectory(outputDir)

          # Generate file names
          files_names <- sub(paste0("\\.", full_extension, "$"), "", basename(input_files_path))
          files_mate_names <- sub(paste0("\\.", full_extension, "$"), "", basename(filesnameMates_path))

          # Construct output file paths
          output_files <- paste0(outputDir, "/", files_names, "-Filtered.", full_extension)
          output_mate_files <- paste0(outputDir, "/", files_mate_names, "-FilteredMate.", full_extension)

          # Check for existing files
          existing_files <- output_files[file.exists(output_files)]
          if (length(existing_files) > 0) {
            warning_message <- paste(
              "Existing output files detected. Please remove these files or use unique names:\n",
              paste(existing_files, collapse = "\n")
            )
            warning(warning_message)
            stop("Please remove existing output files or use unique file names to proceed.")
          }

          # Run preprocessReads function
          print("Running preprocessReads...")
          results <- preprocessReads(
            filename=input_files_path,
            outputFilename=output_files,
            filenameMate=filesnameMates_path,
            outputFilenameMate=output_mate_files,
            nBases = as.numeric(input$nBases),
            truncateStartBases = as.numeric(input$truncateStartBases),
            truncateEndBases = as.numeric(input$truncateEndBases),
            Lpattern = as.character(input$Lpattern),
            Rpattern = as.character(input$Rpattern),
            complexity = as.numeric(input$complexity),
            minLength = as.numeric(input$minLength)
          )

          # Check results
          if (is.null(results) || nrow(results) == 0) {
            stop("No results were returned by preprocessReads.")
          }

          print("Results processed successfully.")

          return(as.data.frame(results))
        }, warning = function(w) {
          # Display warning in a modal
          showModal(
            modalDialog(
              title = "Warning",
              paste("Warning:", conditionMessage(w)),
              easyClose = TRUE,
              footer = NULL
            )
          )
          NULL
        }, error = function(e) {
          # Display error in a modal
          showModal(
            modalDialog(
              title = "Error",
              paste("Error:", conditionMessage(e)),
              easyClose = TRUE,
              footer = NULL
            )
          )
          NULL
        })
      })





      filtered_data <- reactiveVal(NULL)

      # Event reactive for button click to process uploaded data
      observeEvent(input$btn_filter,  {
        if (is.null(importedData())) {
          showModal(
            modalDialog(
              title = "Data Import Required",
              "Please import your FASTQ files before proceeding.",
              easyClose = TRUE,
              footer = NULL
            )
          )
          filtered_data(NULL)
        }
        else{
          data <- filter()
          filtered_data(data)
        }

      })



      # Render the Data Table using the reactive value
      output$tb_filter_trim <- DT::renderDataTable({
        if (is.null(importedData())) {
          return(data.frame(Message = "Import your FASTQ files"))
        }
        if(!is.null(importedData())){
          DT::datatable(filtered_data(), options = list(scrollX = TRUE))
        }
      })

    }
  )
}
