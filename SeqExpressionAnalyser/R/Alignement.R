
alignmentUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(
      width = NULL,
      title = span(icon("dna"), " Mapping Reads to Reference Genome"),
      status = "primary",
      solidHeader = TRUE,
      style = "background-color: #EAF2F8; border-radius: 10px; padding: 15px;",

      sidebarLayout(
        sidebarPanel(
          style = "background-color: #F8F9FA; border-radius: 10px; padding: 15px;",

          # 📂 Upload Reference File
          h4("📂 Upload Reference Genome", style = "color: #0092AC;"),
          fileInput(ns("reference_file"), "Upload a reference file (FASTA format)"),

          # 📂 Read Files
          h4("📂 Select Read Files", style = "color: #0092AC;"),
          textInputIcon(
            ns("readfile1"),
            "Directory Path (FASTQ Files Only):",
            placeholder = "e.g., /path/to/your/data",
            icon = icon("folder-open")
          ),

          # 🔀 Paired-End Checkbox
          checkboxInput(ns("pair_single"), "Paired-End Sequencing", FALSE),
          uiOutput(ns("read2")),

          # 🔍 Output Format
          h4("📁 Output File Format", style = "color: #0092AC;"),
          selectInput(
            ns("outputFormat"),
            "Choose the Output Format:",
            choices = c("BAM", "SAM")
          ),
          hr(),

          # 🚀 Align Reads Button (Small)
          div(
            style = "display: flex; justify-content: left;",
            actionButton(
              ns("btn_lunch_alignement"),
              "Align Reads",
              icon = icon("play"),
              style = "color: #ffffff; background-color: #0092AC;
              border-color: #007B9E; padding: 6px 12px; font-size: 14px;
              border-radius: 5px; width: auto;"
            )
          )
        ),

        mainPanel(
          tabBox(
            width = 12,

            # 📄 Reference File Tab
            tabPanel(
              title = span(icon("file"), " Reference File"),
              value = "tab-reference",
              box(
                title = span(icon("database"), " Your Reference File"),
                width = NULL,
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                style = "background-color: #ffffff; border-radius: 10px; padding: 15px;",
                DT::dataTableOutput(ns("ref_file"))
              )
            ),

            # 📊 Alignment Results Tab
            tabPanel(
              title = span(icon("chart-line"), " Alignment Results"),
              value = "tab-alignment",
              box(
                title = span(icon("chart-bar"), " Alignment Results"),
                width = NULL,
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                style = "background-color: #ffffff; border-radius: 10px; padding: 15px;",
                DT::dataTableOutput(ns("alignement_results"))
              )
            )
          ),

          br(),

          # ❓ Help Button (Small)
          div(
            style = "display: flex; justify-content: left;",
            actionButton(ns("help"), "Help", icon = icon("info-circle"),
                         style = "color: #ffffff; background-color: #F39C12;
                         border-color: #E67E22; padding: 6px 12px; font-size: 14px;
                         border-radius: 5px; width: auto;")
          )
        )
      )
    )
  )
}


alignmentServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- NS(id)


      observeEvent(input$pair_single,{
        if(input$pair_single == TRUE){
          print(input$pair_single )
          output$read2 <- renderUI({

              textInputIcon(
                ns("readfile2"),
                "Directory for second reads in paired-end data (FASTQ Files Only):",
                placeholder = "e.g., /path/to/your/data",
                icon = icon("folder-open")
                      )
          })
        }
        else{
          output$read2 <- renderUI({
            NULL
          })
        }

      }
      )

      run_alignment <- reactive({
        # Ensure all required inputs are provided:
        if (!is.null(input$reference_file$datapath) &&
            input$readfile1 != ""
            #&&
            #input$readfile2 != ""
            ) {

          # Normalize directory paths:
          readfolder1 <- gsub("\\\\", "/", input$readfile1)
          #readfolder2 <- gsub("\\\\", "/", input$readfile2)

          # List FASTQ files in each directory:
          readfiles1 <- list.files(
            path = readfolder1,
            pattern = "\\.(fq|fastq)(\\.gz)?$",
            full.names = TRUE,
            ignore.case = TRUE
          )
         # readfiles2 <- list.files(
          #  path = readfolder2,
           # pattern = "\\.(fq|fastq)(\\.gz)?$",
            #full.names = TRUE,
            #ignore.case = TRUE
          #)
          print(readfiles1)
         # print(readfiles2)

         # if (length(readfiles1) != length(readfiles2)) {
          #  showNotification("Error: The number of FASTQ files in both directories do not match.", type = "error")
           # return(NULL)
          #}

          print("Building index...")
          ref <- input$reference_file$datapath
          # Build the index; note that the index is built in the working directory
          buildindex(basename = "reference_index", reference = ref)

          output_format <- input$outputFormat

          # Create output directory if it doesn't exist
          outputDir <- file.path(input$readfile1, "alignementOutput")
          if (!dir.exists(outputDir)) {
            dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)
          }
          print(paste("Output directory:", outputDir))

          # Determine the input file extension from the first FASTQ file
          full_extension <- sub("^[^.]*\\.", "", basename(readfiles1[1]))
          print(paste("Input file extension:", full_extension))

          # Use only readfiles1 to generate one output per paired sample
          readfile1_path <- readfiles1
         # readfile2_path <- readfiles2
          files_names <- basename( readfile1_path)
          print(files_names)

          # Call the alignment function from Rsubread
          align_stat <- align(
            index = "reference_index",
            readfile1 = readfile1_path,
          #  readfile2 = readfile2_path,
            input_format = full_extension,
            output_format = paste0(".", tolower(output_format)),
          output_file = paste0(outputDir, "/", tools::file_path_sans_ext(files_names), ".", tolower(output_format)),
          phredOffset = 33
          )
          return(align_stat)
        } else {
          return(NULL)
        }
      })

      # Reactive expression to read the reference FASTA file
      refData <- reactive({
        req(input$reference_file)
        fasta <- Biostrings::readDNAStringSet(input$reference_file$datapath)
        data.frame(
          File = input$reference_file$name,
          "Sequence Name" = names(fasta),
          Sequence = as.character(fasta),
          stringsAsFactors = FALSE
        )
      })

      output$ref_file <- DT::renderDataTable({
        if (is.null(input$reference_file)) {
          data.frame(Message = "Please import the reference file (FASTA format).")
        } else {
          DT::datatable(
            refData(),
            options = list(pageLength = 5, scrollX = TRUE),
            caption = "Reference File Details"
          )
        }
      })

      reactive_align <- reactiveVal(NULL)

      observeEvent(input$btn_lunch_alignement, {
        align_result <- run_alignment()
        if (is.null(align_result)) {
          showNotification("Alignment did not run. Please check that all inputs are correctly provided and the directories contain FASTQ files.", type = "error")
        } else {
          showNotification("Alignment completed successfully.", type = "message")
        }
        reactive_align(align_result)
      })

      output$alignement_results <- DT::renderDataTable({
        if (is.null(reactive_align())) {
          data.frame(Message = "Alignment results will appear here once the process is complete.")
        } else {
          DT::datatable(
            reactive_align(),
            options = list(pageLength = 15, scrollX = TRUE),
            caption = "Alignment Summary Results"
          )
        }
      })

    }
  )
}
