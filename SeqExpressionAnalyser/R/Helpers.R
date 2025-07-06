





footer <- function() {
  tags$div(
    class = "panel-footer",
    style = "text-align:center",
    tags$div(
      class = "foot-inner",
      list(
        # hr(),
        "SeqExpressionAnalyser is a project developed by Sanae Esskhayry in department of Medical Genetics and Oncogenetics, Mohammed VI University Hospital Center",
        tags$a(href = "https://fmpt.ac.ma/website/", "FMPT"),
        "-  Faculty of Medicine and Pharmacy, Abdelmalek Essaadi University, Tangier, Morocco", br(),
        "License: ", tags$a(href = "https://opensource.org/licenses/MIT", "GPL-3.0"), br(),

        "Development of the SeqExpressionAnalyzer package is on ",
        tags$a(href = "https://github.com/sanaeesskhayry/SeqExpressionAnalyser", "GitHub")
      )
    )
  )
}


select_files <- function(session,inputID,inputData, message){
  fileChoices <- reactive({
    if (is.null(inputData())) {
      return(c())
      print(inputID)
    }

      files <- names(inputData())
      choices <- c()
      for (filename in files) {
        choices <- c(choices, filename)
      }
      return(choices)
  })
    observe({
      updateSelectInput(session, inputID, label = message, choices = fileChoices())
    })

}



blank_plot <- function(main){
  # Create an empty plot frame with axes and labels
  p <- plot(
    x = 0,
    y = 0,
    xlim = c(0, 100),
    ylim = c(0, 100),
    type = "n",
    xlab = "X-axis",
    ylab = "Y-axis",
    main = "Plot Will Appear Here Once Data is Selected"
  )
  return(p)
}

# Generate the selected plot for all other options
quality_plots <- function(selectedData, selectedFiles, importedData){
  plot_functions <- list(
    "Average Quality" = rqcReadQualityPlot,
    "Cycle-specific Average Quality" = rqcCycleAverageQualityPlot,
    "Cycle-specific Base Call Proportion" = rqcCycleBaseCallsLinePlot,
    "Cycle-specific GC Content" = rqcCycleGCPlot,
    "Cycle-specific Quality Distribution" = rqcCycleQualityPlot,
    "Cycle-specific Quality Distribution - Boxplot" = rqcCycleQualityBoxPlot,
    "Per Read Mean Quality Distribution of Files" = rqcReadQualityBoxPlot,
    "Read Frequency" = rqcReadFrequencyPlot,
    "Read Length Distribution" = rqcReadWidthPlot
  )
  # Render the selected plot using the appropriate function
  p <- plot_functions[[selectedData]](importedData()[selectedFiles])

  return(p)
}


datatable <- function(selectedData, selectedFiles, importedData){

  # Define the data functions
  data_functions <- list(
    "Average Quality" = rqcReadQualityCalc,
    "Cycle-specific Average Quality" = rqcCycleAverageQualityCalc,
    "Cycle-specific Base Call Proportion" = rqcCycleBaseCallsCalc,
    "Cycle-specific GC Content" = rqcCycleGCCalc,
    "Cycle-specific Quality Distribution" = rqcCycleQualityCalc,
    "Cycle-specific Quality Distribution - Boxplot" = rqcCycleQualityBoxCalc,
    "Per Read Mean Quality Distribution of Files" = rqcReadQualityBoxCalc,
    "Read Frequency" = rqcReadFrequencyCalc,
    "Read Length Distribution" = rqcReadWidthCalc
  )
  resultList <- lapply(selectedFiles, function(file) {
    data_functions[[selectedData]](importedData()[file])
  })

  resultData <- do.call(rbind, resultList)
  return(resultData)
}



