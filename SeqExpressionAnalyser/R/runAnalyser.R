

## upload max 300mb files - can be changed if necessary
options(shiny.maxRequestSize = 1000000000 * 1024^2)
options(shiny.launch.browser = FALSE)



#' SeqExpressionAnalyser: Sequence Expression Analyser
#'
#' SeqExpressionAnalyser enables comprehensive differential expression analysis of RNA-Seq data. It is interactive, easy to use, and reproducible.
#' This function launches the main application included in the package.
#'
#' @name runAnalyser
#' @return A Shiny App is launched for interactive
#' and comprehensive differential gene expression analysis of RNA-Seq data
#' @export
#'
#' @examples
#' \dontrun{
#'   runAnalyser()
#' }



runAnalyser <- function(host = '127.0.0.1', port = NULL) {

  # Define the UI (User Interface) -----------------------------------
  ui <- shinydashboard::dashboardPage(
    skin = "black",
    title = "SeqExpressionAnalyser",

    # Dashboard Header -----------------------------------------------
   shinydashboard::dashboardHeader(
      title = tags$span(
        img(src = "SeqExpressionAnalyser/app_logo.png", height = "40px"),
        paste0( "SeqExpressionAnalyser ")),
      titleWidth = 350), #end dashboard header


    # Dashboard Sidebar -----------------------------------------------

   shinydashboard::dashboardSidebar(
      width = 350,
      shinydashboard::sidebarMenu(
        menuItem("Welcome !", tabName = "welcomePage", icon = icon("home", lib = "font-awesome")),
        menuItem("Data Setup", tabName = "collected_data", icon = icon("download", lib = "font-awesome")),
        menuItem("Quality Assessment", tabName = "quality", icon = icon("vial-circle-check")),
        menuItem("Filtering and Trimming", tabName = "filtering", icon = icon("filter")),
        menuItem("Alignment to Reference Genome", tabName = "alignment", icon = icon("indent")),
        menuItem("Read Counting", tabName = "quantification", icon = icon("table-list")),
        menuItem("Differential Expression Analysis", tabName = "DESeq2", icon = icon("dna")),
        menuItem("About", tabName = "aboutPage", icon = icon("university", lib = "font-awesome"))
      )
    ),#end Dashboard Sidebar


    # Dashboard Body -----------------------------------------------

   shinydashboard::dashboardBody(
     introjsUI(),
      tags$head(
        tags$style(HTML("
                        #myScrollBox{
                        overflow-y: scroll;
                        } "))),

      tabItems(
        tabItem(tabName = "welcomePage",box(
          width = FALSE,
          solidHeader = FALSE,
          includeMarkdown( system.file("extdata", "welcome.md", package = "SeqExpressionAnalyser"))
        )),
        tabItem(tabName = "collected_data", importDataUI("importData")),
        tabItem(tabName = "quality", qualityControlUI("qualityControl")),
        tabItem(tabName = "filtering", Filter_TrimUI("filter_trim")),
        tabItem(tabName = "alignment", alignmentUI("alignment")),
        tabItem(tabName = "quantification", quantificationUI("quantification")),
        tabItem(tabName = "DESeq2", dif_gene_ex_analysisUI("DGEA")),
        tabItem(tabName = "aboutPage",box(
          width = FALSE,
          solidHeader = FALSE,
          includeMarkdown(system.file("extdata", "about.md", package = "SeqExpressionAnalyser")),
          verbatimTextOutput("sessioninfo")
          ))
      ),
      footer()
    ) #end dashboardBody
  )

  # Define the server logic
  server <- function(input, output, session) {


    # Import Data server:

    importedData <- callModule(importDataServer, "importData")

    # Quality Control server:
    qualityControlServer("qualityControl", importedData)

    # Filter and Trimming server:
    Filter_TrimServer("filter_trim", importedData)

    # Alignment server:
    alignmentServer("alignment")

    # Quantification server:
    quantificationServer("quantification")

    # Differential expression analysis server:
    dif_gene_ex_analysisServer("DGEA")

    #Functional Analysis server:
    output$sessioninfo <- renderPrint({
      sessionInfo()
    })

  }

  # Run the Shiny app
  runApp(list(ui = ui, server = server), host = host, port = port)
}



