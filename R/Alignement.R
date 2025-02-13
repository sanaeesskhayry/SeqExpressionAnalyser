alignmentUI <- function(id) {
  ns <- NS(id)
  tagList(
    box( width= FALSE, title = "Mapping your reads to a reference genome", status= "primary", solidHeader = TRUE,


         # Sidebar layout with a help button and other inputs
         sidebarLayout(
           sidebarPanel(
             fileInput(ns("reference_file"),"Download a reference file:fasta file", multiple = TRUE),
             fileInput(ns("readfile1"),"The list of files including first reads in each library", multiple = TRUE),
             fileInput(ns("readfile2"),"Files that include second reads in paired-end read data", multiple = TRUE),
             selectInput(
               ns("outputFormat"),
               "specify the format of output file",
               choices = c("BAM", "SAM")
             ),
             selectInput(
               ns("data_type"),
               "The type of sequencing data",
               choices = c("rna", "dna")
             ),
             hr(),
             actionButton(
               ns("btn_lunch_alignment"),
               "Align Reads ",
               icon("circle-play"),
               style = "color: #ffffff; background-color: #0092AC; border-color: #2e6da4"
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
               DT::dataTableOutput(ns("Dwn_finish"))
             ),
             hr(),
             box(
               width = NULL,
               status = "danger",
               h4("Uploaded FASTQ Files"),
               box(width = NULL,
                   status = "primary",
                   solidHeader = TRUE,
                   title = "The uploaded data",
                   collapsible = TRUE,
                   collapsed = TRUE,
                   DT::dataTableOutput(ns("alignment_results")))
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

      # Reactive function to run the alignment
      run_alignment <- reactive({
        if (!is.null(input$reference_file$datapath)) {
          print("build index")
          print(input$readfile1$name[1])
          # Generate file paths
          readfile1_path <- list.files(input$readfile1$datapath, include.dirs = TRUE)
          readfile2_path <- list.files(input$readfile2$datapath, include.dirs = TRUE)
          print(readfile1_path)
          print(readfile2_path)

          real_path <- file.choose()
          cat("Selected file path:", real_path)


          ref <- input$reference_file$datapath
          output_index <- input$readfile1$datapath[1]
          buildindex(basename = paste0(dirname(output_index),"/indexFiles/reference_index"), reference = ref)


          output_format = input$outputFormat

          # Create output directory if it doesn't exist
          outputDir <- file.path( dirname(dirname(input$readfile1$datapath[1])), "alignementOutput")
          if (!dir.exists(outputDir)) {
            dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)
          }
          print(outputDir)


          files_names <- basename(c(readfile1_path,readfile2_path))
          print(files_names)

          full_extension <- sub("^[^.]*\\.", "", input$readfile1$datapath[1])  # Extract extension
          print(full_extension)

          align.stat <- align(
            index = "reference_index",
            readfile1 = readfile1_path,
            readfile2 = readfile2_path,
            input_format = full_extension,
            output_format = paste0(".",input$outputFormat),
            output_file = paste(outputDir, "/", files_names, "-subread.", output_format, sep = ""),
            phredOffset = 33
          )
          return(align.stat)
        } else {
          return(NULL)  # Return NULL when reference file is not selected
        }
      })

      # Render table to display a message or the reference file information
      output$Dwn_finish <- renderTable({
        if (is.null(input$reference_file$datapath)) {
          return(data.frame(Message = "Import the reference file with the FASTA format"))
        } else {
          return(input$reference_file)
        }
      })

      observeEvent(input$btn_lunch_alignment,  {
        run_alignment()

      })



    }
  )
}
