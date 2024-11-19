# UI function for the Filter and Trim section
Filter_TrimUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(
      width = FALSE,
      status = "primary",
      solidHeader = TRUE,
      title = "Filtering and trimming reads",
      actionButton(ns("help"), " Help ", icon("circle-question"), style = "color: #ffffff; background-color: #0092AC; border-color: #2e6da4"),
      br(),
      textInput(ns("truncateEndBases"), "The number of bases to be truncated (removed) from the end of each sequence.", placeholder = "truncate End Bases"),
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
      helpText("Check your filtered data in the same directory that contains non-filtered fastq input file(s) "),
      actionButton(
        ns("btn_filter"),
        " Upload FASTQ Files",
        icon("upload"),
        style = "color: #ffffff; background-color: #0092AC; border-color: #2e6da4"
      ),
      br(), br(),
      box( width = FALSE,
           tags$b(span(style="color:gray", "The result of the filtering and trimming")),br(),br(),
           DT::dataTableOutput(ns("tb_filter_trim"))
      )

    )
  )
}


Filter_TrimServer <- function(id, importedData) {
  moduleServer(
    id,
    function(input, output, session) {
      # Define a reactive value to store the filtered data
      filteredData <- reactive({
        folder <- perFileInformation(importedData())$path
        fastqFiles <- list.files(full.names = TRUE, path = folder)
        files_names <- stringr::str_remove(basename(fastqFiles), "\\.fastq\\.gz$")
        file.format <- ".fastq.gz"

        # Create output directory if it doesn't exist
        outputDir <- file.path(folder, "FilteredOutput")
        if (!dir.exists(outputDir)) {
          dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)
        }

        # Run preprocessReads function
        results <- preprocessReads(
          fastqFiles,
          paste(outputDir, "/", files_names, "-Filtered", file.format, sep = ""),
          nBases = as.numeric(input$nBases),
          truncateStartBases = as.numeric(input$truncateStartBases),
          truncateEndBases = as.numeric(input$truncateEndBases),
          Lpattern = as.character(input$Lpattern),
          Rpattern = as.character(input$Rpattern),
          complexity = as.numeric(input$complexity),
          minLength = as.numeric(input$minLength)
        )



        # Set the filtered data as a reactive value
        return(as.data.frame(results))
      })

      # Event Reactive for the Filter and Trim button
      run_filter_trim <- eventReactive(input$btn_filter, {
        filteredData()
      })



      # Render the Data Table using the reactive value
      output$tb_filter_trim <- DT::renderDataTable({
        if (is.null(importedData())) {
          return(data.frame(Message = "Import your FASTQ files"))
        }
        req(input$nBases,input$truncateStartBases,input$truncateEndBases,input$Lpattern,input$Rpattern, input$complexity,input$minLength  )
        if(!is.null(importedData())){
          DT::datatable(run_filter_trim(), options = list(scrollX = TRUE))
        }
      })

    }
  )
}
