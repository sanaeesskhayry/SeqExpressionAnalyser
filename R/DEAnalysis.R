source('./R/cal_z_scores.R')
dif_gene_ex_analysisUI <- function(id) {
  ns <- NS(id)
  tagList(div(
    id = "myScrollBox", # To have the y direction scrollable

    tabBox(
      width = 12,

      # upload the metadata of the study-------------------------------------

      tabPanel(
        title = "Read in Experimental design info",
        icon = icon("upload"),
        value = "tab-metadata",
        box(
          width = FALSE,
          title = "Step 1",
          status = "danger",
          solidHeader = TRUE,
          h2("Upload the info on the experimental design"),
          hr(),
          fileInput(
            ns("phenotype_data"),
            "Import the metadata of the study as a .csv file"
          ),
          box(
            width = FALSE,
            h3("Experimental design preview"),
            hr(),
            #tags$h4(tags$b(tags$tspan("Description of the metadata"))),
            DT::dataTableOutput(ns("desc_phenotype_data"))
          ),
          uiOutput(ns("desc_columns"))
        )
      ),


      #-----------------------------------------------------------------------
      #FOR THE GENE EXPRESSION DATA :
      #-----------------------------------------------------------------------
      tabPanel(
        title = "Read in counts data",
        icon = icon("upload"),
        value = "tab-countData",
        box(
          width = FALSE,
          title = "Step 2",
          status = "danger",
          solidHeader = TRUE,
          h2("Upload your count matrix"),
          hr(),
          fileInput(ns("gene_expression_data"), "Read in counts data as .csv file"),
          h3("Count matrix preview"),
          hr(),
          tabBox(
            width = 12,
            tabPanel(
              title = "Data Table",
              icon = icon("table"),
              DT::dataTableOutput(ns("count_data")),
              value = "tab_countMatrix"
            ),
            tabPanel(
              title = "Count Data with Experimental design",
              icon = icon("table"),
              value = "tab_exp_count_data",
              DT::dataTableOutput(ns("desc_expression_data"))
            ),
            tabPanel(
              title = "Count matrix dimension",
              icon = icon("list-alt"),
              value = "tab_count_dim",
              tableOutput(ns("dim"))
            ),
            tabPanel(
              title = "Visualize gene expression data",
              icon = icon("image"),
              value = "tab_visual_count",
              uiOutput(ns("visualization"))
            )
          )
        )
      ),
      #-----------------------------------------------------------------------
      #Run Differential Gene Expression Analysis
      #-----------------------------------------------------------------------
      tabPanel(
        title = "Run Differential Gene Expression Analysis",
        icon = icon("play"),
        value = "tab-DE",
        box(
          width = FALSE,
          title = "Step 3",
          status = "danger",
          solidHeader = TRUE,
          tags$h4(tags$b(
            tags$tspan("pre-filtering counts data: removing rows with low gene counts")
          )),
          textInput(ns("filtering_factor"), "Enter the minimum count threshold"),
          textInput(
            ns("input_alpha"),
            "Enter the The statistical significance parameter alpha"
          ),
          actionButton(
            ns("btn_run_DFEXR"),
            "Run Differential Expression Analysis",
            icon("play"),
            style = "color: #ffffff; background-color: #0092AC; border-color: #2e6da4"
          ),
          br(),
          br(),
          box(
            width = FALSE,
            h3("Differential Expression Analysis Results"),
            DT::dataTableOutput(ns("results")),
            br(),
            downloadButton(ns("btn_dwn_Degs"), "Export the results", class = "btn btn-success"),
            br()
          )
        )
      ),

      #-----------------------------------------------------------------------
      #Summary plots
      #-----------------------------------------------------------------------
      tabPanel(
        title = "Summary plots",
        icon = icon("image"),
        value = "tab-plots",
        box(
          width = FALSE,
          title = "Step4:Interactive graphical exploration of the results",
          status = "danger",
          solidHeader = TRUE,
          #Setect a Plot :
          sidebarLayout(# Sidebar with a slider input
            sidebarPanel(
              #Setect a Plot :
              selectInput(
                ns("select_plot_type"),
                "Select the visualization plot",
                choices = (
                  c(
                    "None",
                    "MA plot",
                    "Shrunken LFC MA Plot",
                    "Dispersion plot",
                    "PCA plot",
                    "Volcano plot",
                    "Heatmap of Pairwise Sample Distances",
                    "Heatmap of Z Scores for the Top 20 Genes"
                  )
                )
              )
            ), # Show a plot of the generated distribution :
            mainPanel(fluidPage(
              tabsetPanel(tabPanel("Plot Description", textOutput(ns(
                "plot_desc"
              ))))
            ))),
          h2("generated plot"),
          hr(),
          plotOutput(ns("data_plot"))
        )

      ),



    )
  ))
}


dif_gene_ex_analysisServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #-------------------------------------------------------------------------
    # Function to read the metadata
    #-------------------------------------------------------------------------
    get_metadata <- reactive({
      req(input$phenotype_data$datapath)  # Use req() to check if the file path is provided
      pheno_data <- read.csv(input$phenotype_data$datapath, header = TRUE)
      return(pheno_data)
    })
    #-------------------------------------------------------------------------
    # Render metadata table
    #-------------------------------------------------------------------------
    output$desc_phenotype_data <- DT::renderDataTable({
      if (is.null(input$phenotype_data)) {
        return(data.frame(Message = "Upload the info on the experimental design as a .csv file"))
      }
      pheno_data <- cbind(samples = rownames(get_metadata()), get_metadata())
      rownames(pheno_data) <- NULL
      DT::datatable(pheno_data, options = list(scrollX = TRUE))
    })

    #-------------------------------------------------------------------------
    # Render UI for selecting metadata columns
    #-------------------------------------------------------------------------
    output$desc_columns <- renderUI({
      req(input$phenotype_data$datapath)  # Check if phenotype data is selected
      pheno_data <- cbind(samples = rownames(get_metadata()), get_metadata())
      rownames(pheno_data) <- NULL
      box(
        width = FALSE,
        tags$h4(tags$b(
          tags$tspan("Select the targeted columns for the analysis")
        )),
        selectInput(
          ns("description_column"),
          "Choose the description column: The column links the experimental data to the specific treatment condition of each sample",
          choices = c("None", colnames(pheno_data))
        ),
        selectInput(
          ns("tissue_column"),
          "Choose the tissue column: The \"tissue\" column in a metadata table is a list of unique identifiers that serve as labels for each individual sample in the study",
          choices = c("None", colnames(pheno_data))
        )
      )
    })

    #-------------------------------------------------------------------------
    # Modified metadata based on selected columns
    #-------------------------------------------------------------------------
    modified_metadata <- reactive({
      req(input$description_column,
          input$tissue_column,
          get_metadata())
      pheno_data <- cbind(samples = rownames(get_metadata()), get_metadata())
      rownames(pheno_data) <- NULL
      metadata <- pheno_data %>%
        select(input$description_column, input$tissue_column) %>%
        rename(tissue = colnames(.[2]), description = colnames(.[1]))
      return(metadata)
    })




    #-------------------------------------------------------------------------
    # Function to read count matrix data
    #-------------------------------------------------------------------------
    get_gene_data <- reactive({
      req(input$gene_expression_data$datapath)  # Check if gene expression data is selected
      gene_data <- read.csv(input$gene_expression_data$datapath, header = TRUE)
      return(gene_data)
    })
    #-------------------------------------------------------------------------
    # Render count matrix table
    #-------------------------------------------------------------------------
    output$count_data <-  DT::renderDataTable({
      if (is.null(input$gene_expression_data)) {
        return(data.frame(Message = "Upload your count matrix as a .csv file"))
      }
      DT::datatable(get_gene_data(), options = list(scrollX = TRUE))
    })

    #-------------------------------------------------------------------------
    # Join gene expression data with modified metadata
    #-------------------------------------------------------------------------
    join_data <- reactive({
      req(get_gene_data(), modified_metadata())  # Ensure data availability
      gene_data <- cbind(geneID = rownames(get_gene_data()), get_gene_data())
      rownames(gene_data) <- NULL
      gene_data <- gene_data %>%
        gather(key = 'samples', value = 'count', -geneID) %>%
        left_join(., modified_metadata(), by = c("samples" = "tissue"))
      return(gene_data)
    })
    #-------------------------------------------------------------------------
    # Render data dimension table
    #-------------------------------------------------------------------------
    output$dim <- renderTable({
      if (is.null(input$gene_expression_data)) {
        return(data.frame(Message = "Upload your metadata file"))
      }
      dim_data <- cbind(t(dim(get_gene_data())))
      colnames(dim_data) <- c("Number of rows(genes)", "Number of columns(samples)")
      dim_data
    })

    #-------------------------------------------------------------------------
    # Render count matrix data table with the metadata
    #-------------------------------------------------------------------------
    output$desc_expression_data <- DT::renderDataTable({
      if (is.null(input$gene_expression_data) ||
          is.null(input$phenotype_data) ||
          input$description_column == 'None' ||
          input$tissue_column == "None" || is.null(get_metadata())) {
        return(
          data.frame(Message = "Check your metadata file or your count matrix or select the targeted columns")
        )
      }
      DT::datatable(join_data(), options = list(scrollX = TRUE))
    })

    #-------------------------------------------------------------------------
    # Render UI for count matrix data visualization
    #-------------------------------------------------------------------------
    output$visualization <- renderUI({
      box(
        width = FALSE,
        textInput(ns("geneName"), "Enter the name of gene"),
        radioButtons(
          ns("choose_plot_type"),
          "Choose the type of visualization plot",
          choices = c("Bar plot", "Density")
        ),
        plotOutput(ns("visual_data"))
      )
    })

    #-------------------------------------------------------------------------
    # Render selected gene expression data visualization
    #-------------------------------------------------------------------------
    output$visual_data <- renderPlot({
      # Check required inputs
      if (is.null(input$gene_expression_data) ||
          is.null(input$phenotype_data) ||
          is.null(input$choose_plot_type) ||
          is.null(input$geneName) || is.null(join_data())) {
        return(plot(
          1,
          type = "n",
          xlab = "",
          ylab = "",
          xlim = c(0, 5),
          ylim = c(0, 5)
        ))
      }
      plot_type <- input$choose_plot_type
      data <- join_data()
      switch(
        plot_type,
        "Bar plot" = data %>%
          filter(geneID == input$geneName) %>%
          ggplot(., aes(
            x = samples, y = count, fill = description
          )) + geom_col(fill = "blue"),
        "Density" = data %>%
          filter(geneID == input$geneName) %>%
          ggplot(., aes(x = count, fill = description)) + geom_density(alpha = 0.3, fill = "blue")# Add density plot code here if needed
      )
    })


    #-------------------------------------------------------------------------
    # Run DESq2: Differential expression analysis
    #-------------------------------------------------------------------------
    run_DGEA <- reactive({
      #  construct a DESeqDataSet object
      refColumn <- input$description_column
      data <- get_gene_data()
      metadata <- get_metadata()
      dds <- DESeqDataSetFromMatrix(
        countData = data,
        colData = metadata,
        design = as.formula(paste("~", " ", refColumn))
      )
      # pre-filtering: removing rows with low gene counts
      # keeping rows that have at least 10 reads total
      keep <- rowSums(counts(dds)) >= as.numeric(input$filtering_factor)
      dds <- dds[keep, ]
      # Run DESeq
      dds <- DESeq(dds)
      res <- results(dds, alpha = as.numeric(input$input_alpha))
      # remove nulls
      res <- res[complete.cases(res), ]
      # Assuming 'res' is your DESeq2 results object
      numeric_cols <- sapply(res, is.numeric)
      res[, numeric_cols] <- lapply(res[, numeric_cols], round, 4)
      # Convert numeric columns to character
      #res[, numeric_cols] <- apply(res[, numeric_cols], 2, as.character)
      return(list(res, dds))
    })

    DGEA_results <- eventReactive(input$btn_run_DFEXR, {
      run_DGEA()
    })

    #-------------------------------------------------------------------------
    #Render the table results
    #-------------------------------------------------------------------------
    output$results <- DT::renderDataTable({
      if (is.null(input$gene_expression_data)) {
        return(
          data.frame(Message = " Setup your data for the analysis (Experimental design/count matrix")
        )
      }
      resultData <- DGEA_results()
      DT::datatable(as.data.frame(resultData[[1]]) , options = list(scrollX = TRUE))
    })

    #-------------------------------------------------------------------------
    #Render summary plots
    #-------------------------------------------------------------------------
    output$data_plot <- renderPlot({
      # Check if selected_file is not NULL or empty
      selectedPlot <- input$select_plot_type

      if (is.null(input$gene_expression_data) ||
          is.null(input$phenotype_data) || selectedPlot == "None") {
        return(plot(
          1,
          type = "n",
          xlab = "",
          ylab = "",
          xlim = c(0, 5),
          ylim = c(0, 5)
        ))
      }
      resultData <- DGEA_results()
      colors <- colorRampPalette(rev(brewer.pal(9, "Blues")))(255)
      switch(
        selectedPlot,
        "MA plot" = plotMA(resultData[[1]], cex = 0.7, ylim = c(-10, 10)) %>%
          abline(
            h = c(-1, 1),
            col = "red",
            lwd = 3
          ),
        "Shrunken LFC MA Plot" =  lfcShrink(
          resultData[[2]],
          coef = resultsNames(resultData[[2]])[[2]],
          type = "apeglm"
        ) %>%
          plotMA(., cex = 0.7, ylim = c(-10, 10)) %>%
          abline(
            h = c(-1, 1),
            col = "red",
            lwd = 3
          ),
        "Dispersion plot" = plotDispEsts(resultData[[2]], main = "Dispersion Plot"),
        "PCA plot" = rlogTransformation(resultData[[2]], blind = FALSE) %>%
          plotPCA(., intgroup = input$description_column) +
          geom_text(aes(label = name), size = 2.5) + ggtitle("PCA Plot"),
        "Volcano plot" = EnhancedVolcano(
          resultData[[1]],
          lab = rownames(resultData[[1]]),
          x = "log2FoldChange",
          y = "pvalue",
          title = resultsNames(resultData[[2]])[[2]],
          pCutoff = 10e-4,
          FCcutoff = 0.5,
          pointSize = 3.0,
          labSize = 6.0
        ),
        "Heatmap of Pairwise Sample Distances" =  as.matrix(dist(t(
          assay(rlogTransformation(resultData[[2]], blind = FALSE))
        ))) %>%
          pheatmap(
            .,
            clustering_distance_rows =
              dist(t(assay(
                rlogTransformation(resultData[[2]], blind = FALSE)
              ))),
            clustering_distance_cols =
              dist(t(assay(
                rlogTransformation(resultData[[2]], blind = FALSE)
              ))),
            col = colors
          ),
        "Heatmap of Z Scores for the Top 20 Genes" = t(apply(
          counts(resultData[[2]], normalized = TRUE), 1, cal_z_scores
        ))[rownames(resultData[[1]][order(resultData[[1]]$padj), ][1:20, ]), ]  %>%
          pheatmap (
            .,
            cluster_rows = TRUE,
            show_rownames = TRUE,
            cluster_cols = TRUE,
            annotation_col = get_metadata(),
            fontsize_row = 8
          )
      )
    })
    output$plot_desc <- renderText({
      selectedPlot <- input$select_plot_type
      if (selectedPlot == "None") {
        return("Setup your input data!")
      }
      switch(
        selectedPlot,
        "MA plot" = "it visualizes and indentifies gene expression changes from two different conditions (e.g.untreated vs. treated) in terms of log fold change (M)
                       on Y-axis and log of the mean of normalized expression counts of two conditions on X-axis.
                       Generally, genes with lower mean expression values will have higher variable log fold changes.
                       The blue dots are representing the differentially expressed genes and they have adjusted p-values of less than alpha parameter.
                       The triangles in the edge of the plot are genes with higher fold changes and the directions of the triangle are the direction of the fold change.
                       Genes on the right part of the plot means that these genes have high mean of normalized count
                       and high fold changes (They are very interesting to look into). " ,
        "Shrunken LFC MA Plot" = "The shrunken MA plot is similar to the MA plot, but it visualizes shrunken log2 fold changes, which remove noise associated
                               with log2 fold changes from low-count genes without requiring arbitrary filtering thresholds.",
        "Dispersion plot" = "Displays the dispersion estimates for the differential expression analysis.",
        "PCA plot" = "Generates a PCA plot based on the rlog-transformed counts, highlighting differences between conditions." ,
        "Volcano plot" = "Displays a volcano plot for differential expression analysis, highlighting genes with significant log2 fold changes and p-values.",
        "Heatmap of Pairwise Sample Distances" = "Displays a heatmap of the sample distances and clustering based on the log-transformed normalized counts.",
        "Heatmap of Z Scores for the Top 20 Genes" = "Displays a heatmap of Z scores for the top 20 genes"
      )

    })
    #-------------------------------------------------------------------------
    # DOWNLOAD THE RESULT TABLE :
    #-------------------------------------------------------------------------
    output$btn_dwn_Degs <- downloadHandler(
      filename = function() {
        paste("DEGs-Analysis-", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        resultData <- DGEA_results()
        write.csv(as.matrix(resultData[[1]]),
                  file,
                  quote = F,
                  row.names = T)
      }
    )


  })
}
