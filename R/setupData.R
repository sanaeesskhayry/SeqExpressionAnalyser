# Define the UI function for importing data
importDataUI <- function(id) {
  ns <- NS(id)
  tagList(tabBox(
    width = FALSE,
    # Read FASTQ data Panel
    tabPanel(
      title = "Data Setup",
      icon = icon("file-csv"),
      value = "tab-readData",

      # Main application title
      titlePanel("Set Up Your Data to Start"),
      hr(),
      # Sidebar layout with a help button and other inputs
      sidebarLayout(
        sidebarPanel(
          # File input for data (example)
          fileInput(ns("input_metadata"), "Upload the metadata of the study (.csv)"),
          # Selection inputs
          selectInput(ns("select_group_column"), "Select Group Column", choices = NULL),
          selectInput(
            ns("select_pair_single"),
            "Select Data Type",
            choices = c("paired-end", "single-end")
          ),
          # Other inputs needed for the analysis
          checkboxInput(ns("select_sample"), "Read a random sample from files", value = FALSE),
          numericInput(
            ns("numberOfTopReads"),
            "Number of top over-represented reads",
            value = 10,
            min = 1
          ),
          numericInput(
            ns("numberOfSequences"),
            "Number of sequences to read from each input file",
            value = 10,
            min = 1
          ),
          numericInput(
            ns("numberOfWorkers"),
            "Number of parallel workers",
            value = 2,
            min = 1
          )
        ),

        mainPanel(
          # Output panel for displaying results (placeholder)
          # Help button
          actionButton(ns("help"), " Help ", icon("circle-question"), style = "color: #ffffff; background-color: #0092AC; border-color: #2e6da4"),
          hr(),
          box(
            width = NULL,
            status = "primary",
            solidHeader = TRUE,
            title = "The experimental design of the study",
            collapsible = TRUE,
            collapsed = TRUE,
            DT::dataTableOutput(ns("metadata_description"))
          ),
          hr(),
          box(
            width = NULL,
            status = "danger",
            textInputIcon(
              ns("input_data"),
              "Directory Path (FASTQ Files Only)",
              placeholder = "e.g., /path/to/your/data",
              icon = icon("folder-open")
            ),
            tags$span(
              style = "color:blue",
              "Please verify that the selected directory contains only valid FASTQ files. Any non-FASTQ files should be removed to ensure accurate processing."
            ),
            br(),
            br(),
            actionButton(
              ns("btn_import_data"),
              " Upload FASTQ Files",
              icon("upload"),
              style = "color: #ffffff; background-color: #0092AC; border-color: #2e6da4"
            ),
            br(),
            hr(),
            h4("Uploaded FASTQ Files"),
            box(width = NULL,
                status = "primary",
                solidHeader = TRUE,
                title = "The uploaded data",
                collapsible = TRUE,
                collapsed = TRUE,
                DT::dataTableOutput(ns("data_description")))
          )
        )
      )
    ),
    # Data Overview Panel
    tabPanel(
      title = "Data Summary and Insights",
      icon = icon("file-contract"),
      value = "tab-dataOverview",
      # Sidebar layout with a help button and other inputs
      sidebarLayout(
        sidebarPanel(
          # Dropdown for File Selection
          selectInput(ns("select_file"), "Select a File", choices = c("None")),
          hr(),
          # Select a Plot:
          selectInput(
            ns("select_representation"),
            "Available Representations of the Data",
            choices = (
              c(
                "Most Frequent Sequencing Reads and Counts",
                "Frequency distribution of read width",
                "Number of unique reads, duplicated reads, etc",
                "Frequency distribution of mean quality of reads",
                "Frequency distribution of cycle-specific quality",
                "Frequency distribution of cycle-specific base call"
              )
            )
          )
        ),

        mainPanel(
          # Help button
          actionButton(ns("help"), " Help ", icon("circle-question"), style = "color: #ffffff; background-color: #0092AC; border-color: #2e6da4"),
          hr(),
          box(
            width = NULL,
            solidHeader = FALSE,
            title = "Data Overview",
            status = "danger",
            DT::dataTableOutput(ns("presentation"))
          )

        )
      )
    )
  ))

}

# Define the server function for uploading data
importDataServer <- function(input, output, session) {
  ns <- session$ns

  # Reactive function to retrieve metadata
  get_metadata <- reactive({
    if (is.null(input$input_metadata)) {
      return(NULL)
    }
    return(read.csv(input$input_metadata$datapath, header = TRUE))
  })

  # Render metadata description in a Data Table
  output$metadata_description <- DT::renderDataTable({
    if (is.null(get_metadata())) {
      return(data.frame(Message = "Enter the experimental design of your study."))
    }
    data <- get_metadata()
    DT::datatable(data, options = list(scrollX = TRUE, pageLength = 5))
  })

  # Updating select group column:
  observe({
    updateSelectInput(
      session,
      "select_group_column",
      label = "Select the group column from the metadata",
      choices = colnames(get_metadata()),
      selected = colnames(get_metadata())[1]
    )
  })

  # Reactive function to handle data upload and processing
  upload_data <- reactive({
    # Check if input data is provided
    if (input$input_data == "" || is.null(input$input_data)) {
      showModal(
        modalDialog(
          title = "Error",
          "No input data provided. Please upload the input data.",
          easyClose = TRUE,
          footer = actionButton(ns("retry"), "Retry")  # Retry button to reload
        )
      )
      return(NULL)
    }

    # Get the directory path of the data:
    folder <- gsub("\\\\", "/", input$input_data)

    # List FASTQ files in the directory
    files <- list.files(
      path = folder,
      pattern = "\\.(fq|fastq)(\\.gz)?$",
      full.names = TRUE,
      ignore.case = TRUE
    )

    if (length(files) == 0) {
      showModal(
        modalDialog(
          title = "Error",
          "No FASTQ files found in the directory. Ensure the correct folder.",
          easyClose = TRUE,
          footer = actionButton(ns("retry"), "Retry")
        )
      )
      return(NULL)
    }
    print(length(files))

    # Extract group column data from metadata
    if (is.null(get_metadata())) {
      showModal(
        modalDialog(
          title = "Error",
          "No metadata found. Please check that metadata is available.",
          easyClose = TRUE,
          footer = actionButton(ns("retry"), "Retry")
        )
      )
      return(NULL)
    }

    group_column <- input$select_group_column
    group <- factor(get_metadata()[[group_column]])
    print(group)
    print(length(group))

    # Ensure group length matches the number of files
    if (input$select_pair_single == "paired-end") {
      if (length(files) %% 2 != 0) {
        showModal(
          modalDialog(
            title = "Error",
            "Paired-end selected but an odd number of FASTQ files were found.",
            easyClose = TRUE,
            footer = actionButton(ns("retry"), "Retry")
          )
        )
        return(NULL)
      }

      pair <- rep(1:(length(files) / 2), each = 2)

    } else {
      pair <- 1:length(files)
    }

    # Sample or not
    sample <- as.logical(input$select_sample)
    print(sample)

    # Number of top reads
    topReads <- input$numberOfTopReads

    # Number of workers
    workers <- input$numberOfWorkers
    print(workers)

    # Perform quality analysis (e.g., using rqc)
    data <- rqcQA(
      files,
      pair = pair,
      group = group,
      sample = sample,
      top = topReads,
      workers = workers
    )
    print("Done")
    return(data)
  })


  # Reload the app when the "Retry" button is clicked
  observeEvent(input$retry, {
    session$reload()
  })

  reactive_result <- reactiveVal(NULL)

  # Event reactive for button click to process uploaded data
  observeEvent(input$btn_import_data, {
    data <- upload_data()
    reactive_result(data)
  })

  # Render data description (after processing)
  output$data_description <- DT::renderDataTable({
    if (is.null(get_metadata()) || is.null(reactive_result())) {
      return(data.frame(Message = " Please upload your FASTQ files to begin the analysis."))
    }
    datatable(as.data.frame(perFileInformation(reactive_result())),options = list(scroll = TRUE, autoWidth = TRUE))
  })

  fileChoices <- reactive({
    if (is.null(reactive_result())) {
      return(c("None"))
    }
    files <- names(reactive_result())
    choices <- c("None")
    for (filename in files) {
      choices <- c(choices, filename)
    }
    return(choices)
  })

  observe({
    updateSelectInput(session,
                      "select_file",
                      label = "Select a File",
                      choices = fileChoices())
  })

  output$presentation <- DT::renderDataTable({
    # Check if selected_file is not NULL or empty
    if (is.null(input$select_file) || input$select_file == "None") {
      return(data.frame(Message = " Setup your data for the analysis"))
    }

    selectedPresentation <- input$select_representation
    selectedFile <- input$select_file

    presentation_functions <- list(
      "Most Frequent Sequencing Reads and Counts" = perFileTopReads,
      "Frequency distribution of read width" = perReadWidth,
      "Number of unique reads, duplicated reads, etc" = perReadFrequency,
      "Frequency distribution of mean quality of reads" = perReadQuality,
      "Frequency distribution of cycle-specific quality" =  perCycleQuality,
      "Frequency distribution of cycle-specific base call" = perCycleBasecall
    )

    if (!(selectedPresentation %in% names(presentation_functions))) {
      return(data.frame(Message = " Setup your data for the analysis"))
    }

    result <- presentation_functions[[selectedPresentation]](reactive_result()[selectedFile])
    DT::datatable(as.data.frame(result), options = list(scroll = TRUE))
  })

  return(reactive_result)
}
