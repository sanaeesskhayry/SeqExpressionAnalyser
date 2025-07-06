# Data setup UI

importDataUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(
      title = span(icon("file-upload"), " Import & Prepare Data"),
      width = NULL,
      status = "primary",
      solidHeader = TRUE,
      collapsible = TRUE,
      collapsed = FALSE,
      style = "background-color: #EAF2F8; border-radius: 10px; padding: 15px;",

      sidebarLayout(

        sidebarPanel(
          style = "background-color: #F8F9FA; border-radius: 10px; padding: 15px;",

          h4("🧪 Load Demo Data", style = "color: #0092AC;"),
          actionButton(ns("btn_demo_data"), "Use Demo Data", icon = icon("play"),
                       style = "color: #ffffff; background-color: #F39C12;
                         border-color: #E67E22; padding: 6px 12px; font-size: 14px;
                                   border-radius: 5px; width: auto;"),
          hr(),


          box( title = span(icon("database"), " Upload Custom Data"),
               width = NULL,
               status = "primary",
               solidHeader = TRUE,
               collapsible = TRUE,
               collapsed = TRUE,  # << this collapses it on load
               style = "background-color: #ffffff; border-radius: 10px; padding: 15px;",
               #📂 Upload Metadata File
               h4("📋 Upload Metadata", style = "color: #0092AC;"),
               fileInput(ns("input_metadata"), "Upload Study Metadata (.csv)"),

               # 🏷 Select Group Column
               selectInput(ns("select_group_column"), "Choose Group Column:", choices = NULL),

               # 🔍 Data Type Selection
               h4("⚙️ Configuration", style = "color: #0092AC;"),
               selectInput(ns("select_pair_single"), "Choose Type:", choices = c("paired-end", "single-end")),

               # 🔢 Numeric Inputs
               numericInput(ns("numberOfWorkers"), "Parallel Workers:", value = 2, min = 1),

               # 📂 File Path Input
               textInputIcon(ns("input_data"), "Directory Path (FASTQ Files Only):",
                             placeholder = "e.g., /path/to/your/data", icon = icon("folder-open")),

               # 📄 Display detected files
               textOutput(ns("file_count")),

               br(),
               # 🚀 Upload Data Button (Small)
               div(
                 style = "display: flex; justify-content: left;",
                 actionButton(ns("btn_import_data"), "Upload FASTQ Files", icon = icon("upload"),
                              style = "color: #ffffff; background-color: #0092AC;
                                   border-color: #007B9E; padding: 6px 12px; font-size: 14px;
                                   border-radius: 5px; width: auto;")
               )
          )


        ),

        mainPanel(
          tabBox(
            width = 12,

            # 📊 Metadata Table Tab
            tabPanel(
              title = span(icon("table"), " Experimental Design"),
              value = "tab-experiment",
              box(
                title = span(icon("database"), " Experimental Design Data"),
                width = NULL,
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                style = "background-color: #ffffff; border-radius: 10px; padding: 15px;",
                DT::dataTableOutput(ns("metadata_description"))
              )
            ),

            # 📂 Uploaded FASTQ Files Tab
            tabPanel(
              title = span(icon("file"), " Uploaded FASTQ Data"),
              value = "tab-uploadedData",
              box(
                title = span(icon("file"), " Uploaded Data Details"),
                width = NULL,
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                style = "background-color: #ffffff; border-radius: 10px; padding: 15px;",
                DT::dataTableOutput(ns("data_description"))
              )
            ),

            # 📌 Data Summary & Insights Panel
            tabPanel(
              title = span(icon("chart-bar"), " Data Summary & Insights"),
              value = "tab-uploadedData",
              style = "background-color: #F8F9FA; border-radius: 10px; padding: 15px;",

              # 📂 File Selection
              h4("📂 Select Data File", style = "color: #0092AC;"),
              selectInput(ns("select_file"), "Choose a File:", choices = c("None")),

              # 📊 Select Data Representation
              h4("📊 Choose Visualisation", style = "color: #0092AC;"),
              selectInput(ns("select_representation"), "Select Data Representation:", choices = c(
                "Most Frequent Reads",
                "Read Width Distribution",
                "Unique/Duplicated Reads",
                "Mean Quality Distribution",
                "Cycle-Specific Quality",
                "Cycle-Specific Base Call"
              )),
              box(
                title = span(icon("chart-line"), " Data Overview"),
                width = NULL,
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                style = "background-color: #ffffff; border-radius: 10px; padding: 15px;",
                DT::dataTableOutput(ns("data_overview"))
              )
            )),

          br(),

          # ❓ Help Button (Small)
          div(
            style = "display: flex; justify-content: left;",
            actionButton(ns("btn_tutorial"),
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




#Data setup server

importDataServer <- function(input, output, session) {
    ns <- session$ns

    # Show help modal
    observeEvent(input$btn_tutorial, {
      showModal(modalDialog(
        title = "How to Use This Module",
        HTML("1. Upload metadata or use demo.<br>
              2. Provide FASTQ directory or use demo data.<br>
              3. Click Upload.<br>
              4. View summary."),
        easyClose = TRUE
      ))
    })

    # Reactive values to store metadata and QC result
    metadata <- reactiveVal(NULL)
    result <- reactiveVal(NULL)

    # Load metadata from file
    observeEvent(input$input_metadata, {
      req(input$input_metadata)
      df <- read.csv(input$input_metadata$datapath, header = TRUE)
      metadata(df)
    })

    # Load demo metadata on button click
    observeEvent(input$btn_demo_data, {
      demo_path <- system.file("extdata", "SraRunTable.csv", package = "SeqExpressionAnalyser")
      df <- read.csv(demo_path, header = TRUE)
      metadata(df)
    })

    # Update selectInput for group column once metadata is available
    observe({
      meta <- metadata()
      if (!is.null(meta)) {
        updateSelectInput(session, "select_group_column",
                          choices = names(meta),
                          selected = names(meta)[1])
      }
    })

    # Display metadata
    output$metadata_description <- DT::renderDataTable({
      meta <- metadata()
      if (is.null(meta)) {
        DT::datatable(data.frame(Message = "No metadata loaded yet."))
      } else {
        DT::datatable(meta, options = list(scrollX = TRUE))
      }
    })

    # Demo FASTQ file detection
    demo_fastq_files <- reactive({
      path <- system.file("extdata", "Data", package = "SeqExpressionAnalyser")
      list.files(path, pattern = "\\.(fq|fastq)(\\.gz)?$", full.names = TRUE)
    })

    # User FASTQ file detection
    custom_fastq_files <- reactive({
      req(input$input_data)
      path <- gsub("\\\\", "/", input$input_data)
      list.files(path, pattern = "\\.(fq|fastq)(\\.gz)?$", full.names = TRUE)
    })

    # Show number of FASTQ files found
    output$file_count <- renderText({
      files <- tryCatch(custom_fastq_files(), error = function(e) NULL)
      paste("Files detected:", length(files))
    })

    # Process user FASTQ files when "Upload My Data" is clicked
    observeEvent(input$btn_import_data, {
      req(metadata())
      files <- custom_fastq_files()
      group <- factor(metadata()[[input$select_group_column]])
      workers <- input$numberOfWorkers

      res <- Rqc::rqcQA(files, group = group, workers = workers)
      result(res)
    })

    # Process demo FASTQ files when "Load Demo Data" is clicked
    observeEvent(input$btn_demo_data, {
      files <- demo_fastq_files()
      meta <- metadata()
      req(meta)
      group <- factor(meta[["Developmental_Stage"]])
      res <- Rqc::rqcQA(files, group = group, workers = 2)
      result(res)
    })

    # Show QC summary of loaded FASTQ files
    output$data_description <- DT::renderDataTable({
      res <- result()
      if (is.null(res)) {
        DT::datatable(data.frame(Message = "No FASTQ files processed yet."))
      } else {
        DT::datatable(as.data.frame(perFileInformation(res)), options = list(scrollX = TRUE))
      }
    })



    select_files(session, "select_file", result, "Select Files")

    output$data_overview <- DT::renderDataTable({
      # Check if selected_file is not NULL or empty
      if (is.null(input$select_file) || input$select_file == "None") {
        return(DT::datatable(data.frame(Message = " Setup your data for the analysis")))
      }

      selectedPresentation <- input$select_representation
      selectedFile <- input$select_file

      presentation_functions <- list(
        "Most Frequent Reads" = perFileTopReads,
        "Read Width Distribution" = perReadWidth,
        "Unique/Duplicated Reads" = perReadFrequency,
        "Mean Quality Distribution" = perReadQuality,
        "Cycle-Specific Quality" =  perCycleQuality,
        "Cycle-Specific Base Call" = perCycleBasecall
      )

      if (!(selectedPresentation %in% names(presentation_functions))) {
        return(data.frame(Message = " Setup your data for the analysis"))
      }

      res <- presentation_functions[[selectedPresentation]](result()[selectedFile])
      DT::datatable(as.data.frame(res), options = list(scrollX = TRUE, pageLength = 5))
    })

    # Return final result for downstream use
    return(result)
  }


