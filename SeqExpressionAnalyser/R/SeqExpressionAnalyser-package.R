#' @keywords internal
"_PACKAGE"

## usethis namespace: start

# Data manipulation
#' @importFrom dplyr filter mutate select rename
#' @importFrom tidyr gather
#' @importFrom utils read.csv write.csv sessionInfo

# Data tables and rendering
#' @importFrom DT datatable renderDataTable dataTableOutput

# Shiny framework (split by purpose for clarity)
# Basic shiny
#' @importFrom shiny NS icon fluidPage tags tagList
# Inputs
#' @importFrom shiny fileInput selectInput textInput numericInput radioButtons checkboxInput actionButton downloadButton
# Outputs
#' @importFrom shiny uiOutput helpText textOutput plotOutput verbatimTextOutput renderText renderPrint
# Layout
#' @importFrom shiny sidebarLayout sidebarPanel mainPanel tabPanel hr h4 br span div
# Logic
#' @importFrom shiny reactive observeEvent observe reactiveVal req showModal callModule runApp HTML updateSelectInput includeMarkdown img

# Shinydashboard layout
#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardSidebar dashboardBody
#' @importFrom shinydashboard menuItem sidebarMenu tabItems tabItem box tabBox

# Widgets (shinyWidgets, shinycssloaders, alerts)
#' @importFrom shinyWidgets textInputIcon
#' @import shinyalert
#' @import shinycssloaders

# Tutorials and help
#' @import rintrojs
#' @importFrom rintrojs introjsUI

# RNA-seq related packages
#' @import Rqc
#' @import QuasR
#' @import Rsubread
#' @import DESeq2
#' @import apeglm

# Plotting
#' @import ggplot2
#' @import ggplotify
#' @import EnhancedVolcano
#' @import pheatmap
#' @importFrom RColorBrewer brewer.pal

# Other
#' @importFrom tools file_path_sans_ext

## usethis namespace: end

NULL
