quantificationUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(
      width = NULL,
      title = span(icon("calculator"), " Feature Counting with featureCounts"),
      status = "primary",
      solidHeader = TRUE,
      style = "background-color: #EAF2F8; border-radius: 10px; padding: 15px;",

      sidebarLayout(
        sidebarPanel(
          style = "background-color: #F8F9FA; border-radius: 10px; padding: 15px;",

          # 📂 Upload Annotation File
          h4("📂 Upload Annotation File", style = "color: #0092AC;"),
          fileInput(ns("dw_GTF"),
                    "Select an Annotation File in gtf format",
                    accept = c(".gtf", ".gtf.gz")),

          # # 🔍 Feature & Attribute Selection
          # h4("🔍 Select Feature & Attribute", style = "color: #0092AC;"),
          # selectInput(ns("select_feature_type"), "Select Feature Type:", choices = NULL),
          # selectInput(ns("select_attribute_type"), "Select Attribute Type:", choices = NULL),

          # 📂 BAM File Directory
          h4("📁 Mapping Files Directory", style = "color: #0092AC;"),
          textInputIcon(
            ns("BAMFiles"),
            label = "Folder Path for Mapping Files (BAM/SAM)",
            placeholder = "e.g., C:/path/to/mapping_files",
            icon = icon("folder-open")
          ),
          tags$p("Note: The specified folder should contain only mapping files.",
                 style = "color:blue; font-weight:bold;"),
          hr(),

          # 🚀 Run Read Counting Button (Small)
          div(
            style = "display: flex; justify-content: left;",
            actionButton(ns("btn_count"),
                         "Run Read Counting",
                         icon = icon("play"),
                         style = "color: #ffffff; background-color: #0092AC;
                         border-color: #007B9E; padding: 6px 12px; font-size: 14px;
                         border-radius: 5px; width: auto;")
          )
        ),

        mainPanel(
          tabBox(
            width = 12,

            # 📄 Annotation File Preview Tab
            tabPanel(
              title = span(icon("file"), " Annotation File Preview"),
              value = "tab-annotation",
              box(
                title = span(icon("database"), " Annotation File Overview"),
                width = NULL,
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                style = "background-color: #ffffff; border-radius: 10px; padding: 15px;",
                DT::dataTableOutput(ns("AnnotationFile"))
              )
            ),

            # 📊 Flattened Annotation Features Tab
            tabPanel(
              title = span(icon("align-left"), " Flattened Annotation Features"),
              value = "tab-flatten",
              box(
                title = span(icon("list"), " Processed Annotation Features"),
                width = NULL,
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                style = "background-color: #ffffff; border-radius: 10px; padding: 15px;",
                DT::dataTableOutput(ns("flatten_features"))
              )
            ),

            # 📊 FeatureCounts Results Tab
            tabPanel(
              title = span(icon("table"), " FeatureCounts Results"),
              value = "tab-featurecounts",
              box(
                title = span(icon("chart-bar"), " Count Matrix"),
                width = NULL,
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                style = "background-color: #ffffff; border-radius: 10px; padding: 15px;",
                DT::dataTableOutput(ns("featureCount"))
              ),
              # 📥 Download Count Matrix Button (Small)
              div(
                style = "display: flex; justify-content: left;",
                downloadButton(ns("dwn_count_matrix"),
                               "Download Count Matrix",
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


# Quantification Server module
quantificationServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # Help modal for detailed instructions
      observeEvent(input$help, {
        showModal(modalDialog(
          title = "Quantification Module Help",
          HTML(
            "<ol>
              <li><b>Upload Annotation:</b> Upload your annotation file (.gtf, .gtf.gz, or .gff3). Ensure it contains the 'gene_id' attribute in the attributes column.</li>
              <li><b>Mapping Files Folder:</b> Enter the folder path that contains your mapping files (BAM/SAM). The folder should contain only mapping files.</li>
              <li><b>Run Read Counting:</b> Click 'Run Read Counting' to perform read quantification using featureCounts.</li>
              <li><b>Results & Download:</b> Once complete, the count matrix will be displayed below. You can download it using the 'Download Count Matrix' button.</li>
            </ol>"
          ),
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
      })

      dataGTF <- reactive({
        if (is.null(input$dw_GTF$datapath)) {
          return(NULL)
        }
        GTF_File <- gsub("\\\\", "/", input$dw_GTF$datapath)
        # Import as GFF3 (change format to "gtf" if appropriate)
        Annotation <- rtracklayer::import(GTF_File, format = "gtf")
        return( as.data.frame(Annotation))
      })

      # Updating select feature column:
      observe({
        updateSelectInput(
          session,
          "select_feature_type",
          label = "Select the feature column from the GTF/GFF file",
          choices = c("None",colnames(dataGTF())),
          selected = "None"
        )
      })

      # Updating select attribute column:
      observe({
        updateSelectInput(
          session,
          "select_attribute_type",
          label = "Select the attribute column from the GTF/GFF file",
          choices = c("None",colnames(dataGTF())),
          selected = "None"
        )
      })

      # Render the annotation file preview
      output$AnnotationFile <- DT::renderDataTable({
        if (is.null(input$dw_GTF$datapath) || input$dw_GTF$datapath == "" ) {
          return(as.matrix("Please import an annotation file."))
        }
        DT::datatable(dataGTF(),
                      options = list(scrollX = TRUE, pageLength = 5),
                      caption = "Annotation File Details")
      })

      # Render the flattened annotation features table with error checking
      output$flatten_features <- DT::renderDataTable({
        if (is.null(input$dw_GTF$datapath) || input$dw_GTF$datapath == "") {
          return(as.matrix("Please import an annotation file."))
        }
        file <- gsub("\\\\", "/", input$dw_GTF$datapath)

          flattened <- flattenGTF(GTFfile = file)

          DT::datatable(as.data.frame(flattened),
                        options = list(scrollX = TRUE, pageLength = 5),
                        caption = "Flattened Features from Annotation")
      })

      # Run featureCounts when "Run Read Counting" is clicked
      runCount <- reactive({
        req(input$BAMFiles, input$dw_GTF$datapath)
        BAM_folder <- input$BAMFiles
        print(BAM_folder)
        # List only BAM files in the provided folder:
        BAM_files <- list.files(path = BAM_folder,
                                pattern = "\\.bam$",
                                full.names = TRUE,
                                ignore.case = TRUE)
        if (length(BAM_files) == 0) {
          showNotification("Error: No BAM files found in the specified folder.", type = "error")
          return(NULL)
        }
        AnnotationFile <- input$dw_GTF$datapath

        showNotification("Running read counting...", type = "message")
        fc_SE <- featureCounts(BAM_files,
                              isGTFAnnotationFile = TRUE,
                               annot.ext = AnnotationFile,
                              isPairedEnd = TRUE
                              )
        countMatrix <- as.data.frame(fc_SE$counts)
        colnames(countMatrix) <- stringr::str_remove(colnames(countMatrix), "-subread.bam")
        showNotification("Read counting completed successfully.", type = "message")
        return(countMatrix)
      })

      reactive_count <- reactiveVal(NULL)

      observeEvent(input$btn_count, {
        count_result <- runCount()
        if (is.null(runCount)) {
          showNotification("Feature count did not run. Please check that all inputs are correctly provided and the directories contain FASTQ files.", type = "error")
        } else {
          showNotification("Feature count completed successfully.", type = "message")
        }
        reactive_count(count_result)
      })


      # Render the featureCounts count matrix table, limited to 15 rows per page
      output$featureCount <- DT::renderDataTable({
        if (input$BAMFiles == "" || is.null(input$BAMFiles)) {
          return(as.matrix("Please specify a folder path that contains your BAM file(s)."))
        }
        req(runCount())
        DT::datatable(reactive_count(),
                      options = list(pageLength = 5, scrollX = TRUE),
                      caption = "FeatureCounts: Count Matrix of Mapped Reads")
      })

      # Download handler for the count matrix
      output$dwn_count_matrix <- downloadHandler(
        filename = function() {
          paste("count-Matrix-", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
          write.csv(runCount(), file, row.names = FALSE)
        }
      )
    }
  )
}
