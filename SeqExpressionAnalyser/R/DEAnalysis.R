source('./R/cal_z_scores.R')
dif_gene_ex_analysisUI <- function(id) {
  ns <- NS(id)
  tagList(


    # 🔹 Main UI Box
    box(
      width = NULL,
      title = span(icon("dna"), " Differential Gene Expression Analysis"),
      status = "primary",
      solidHeader = TRUE,
      style = "background-color: #EAF2F8; border-radius: 10px; padding: 15px;",

      tabBox(
        width = 12,

        # 📂 Step 1: Upload Experimental Design
        tabPanel(
          title = span(icon("upload"), " Upload Experimental Design"),
          value = "tab-metadata",
          sidebarLayout(
            sidebarPanel(
              style = "background-color: #F8F9FA; border-radius: 10px; padding: 15px;",
              h4("📋 Upload the metadata of your study"),
              fileInput(ns("phenotype_data"), "Import metadata (.csv)"),
              uiOutput(ns("desc_columns"))
            ),
            mainPanel(
              box(
                title = span(icon("table"), " Experimental Design Preview"),
                width = NULL,
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                style = "background-color: #ffffff; border-radius: 10px; padding: 15px;",
                DT::dataTableOutput(ns("desc_phenotype_data"))
              )
            )
          )
        ),

        # 📊 Step 2: Upload Count Matrix
        tabPanel(
          title = span(icon("upload"), " Upload Count Matrix"),
          value = "tab-countData",
          sidebarLayout(
            sidebarPanel(
              style = "background-color: #F8F9FA; border-radius: 10px; padding: 15px;",
              h4("📊 Upload your count matrix"),
              fileInput(ns("gene_expression_data"), "Import count data (.csv)")
            ),
            mainPanel(
              tabBox(
                width = 12,
                tabPanel(
                  title = span(icon("table"), " Data Table"),
                  value = "tab_countMatrix",
                  box(
                    title = span(icon("table"), " Count Matrix Preview"),
                    width = NULL,
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    style = "background-color: #ffffff; border-radius: 10px; padding: 15px;",
                    DT::dataTableOutput(ns("count_data"))
                  )
                ),
                tabPanel(
                  title = span(icon("table"), " Count Data with Experimental Design"),
                  value = "tab_exp_count_data",
                  box(
                    title = span(icon("table"), " Experimental Design Data"),
                    width = NULL,
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    style = "background-color: #ffffff; border-radius: 10px; padding: 15px;",
                    DT::dataTableOutput(ns("desc_expression_data"))
                  )
                ),

                tabPanel(
                  title = span(icon("image"), " Visualize Gene Expression"),
                  value = "tab_visual_count",
                  uiOutput(ns("visual_gene_expression"))
                )
              )
            )
          )
        ),

        # 🔬 Step 3: Run Differential Expression Analysis
        tabPanel(
          title = span(icon("play"), " Run Differential Expression Analysis"),
          value = "tab-DE",
          sidebarLayout(
            sidebarPanel(
              style = "background-color: #F8F9FA; border-radius: 10px; padding: 15px;",
              h4("🧪 Pre-filtering: Removing genes with low counts"),
              textInput(ns("filtering_factor"), "Enter the minimum count threshold"),
              textInput(ns("input_alpha"), "Enter statistical significance parameter (alpha)"),
              hr(),
              actionButton(ns("btn_run_DFEXR"), "Run Analysis", icon = icon("play"),
                           style = "color: #ffffff; background-color: #0092AC;
                           border-color: #007B9E; padding: 6px 12px; font-size: 14px;
                           border-radius: 5px; width: auto;")
            ),
            mainPanel(
              box(
                title = span(icon("table"), " Differential Expression Results"),
                width = NULL,
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                style = "background-color: #ffffff; border-radius: 10px; padding: 15px;",
                DT::dataTableOutput(ns("results"))
              ),
              br(),
              div(
                style = "display: flex; justify-content: left;",
                downloadButton(ns("btn_dwn_Degs"),
                               "Download Results",
                               style = "color: #ffffff; background-color: #0092AC;
               border-color: #007B9E; padding: 6px 12px; font-size: 14px;
               border-radius: 5px; width: auto;")

              )
              # downloadButton(ns("btn_dwn_Degs"), "Download Results",
              #                style = "padding: 6px 12px; font-size: 14px; border-radius: 5px; width: auto;")
            )
          )
        ),

        # 📈 Step 4: Summary Plots
        tabPanel(
          title = span(icon("image"), " Summary Plots"),
          value = "tab-plots",
          sidebarLayout(
            sidebarPanel(
              style = "background-color: #F8F9FA; border-radius: 10px; padding: 15px;",
              h4("📊 Select Plot Type", style = "color: #0092AC;"),
              selectInput(ns("select_plot_type"), "Choose a visualization:", choices = c(
                "None",
                "MA plot",
                "Shrunken LFC MA Plot",
                "Dispersion plot",
                "PCA plot",
                "Volcano plot",
                "Heatmap of Pairwise Sample Distances",
                "Heatmap of Z Scores for the Top 20 Genes"
              )),
              textOutput(ns("plot_desc"))
            ),
            mainPanel(

                    plotOutput(ns("plot_summary")),
                  # 📥 Download Filtering report Button (Small)
                  div(
                    style = "display: flex; justify-content: left;",
                    downloadButton(ns("download_plot"), "Download Plot",
                                   style = "color: #ffffff; background-color: #0092AC;
                             border-color: #007B9E; padding: 6px 12px; font-size: 14px;
                             border-radius: 5px; width: auto;"))

            )
          )
        )
      )
    )
  )
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
      rownames(pheno_data) <- NULL
      print(rownames(pheno_data))
      return(pheno_data)
    })
    #-------------------------------------------------------------------------
    # Render metadata table
    #-------------------------------------------------------------------------
    output$desc_phenotype_data <- DT::renderDataTable({
      if (is.null(input$phenotype_data)) {
        return(data.frame(Message = "Upload the info on the experimental design as a .csv file"))
      }
      pheno_data <- get_metadata()
      rownames(pheno_data) <- NULL
      DT::datatable(pheno_data, options = list(scrollX = TRUE))
    })

    #-------------------------------------------------------------------------
    # Render UI for selecting metadata columns
    #-------------------------------------------------------------------------
    output$desc_columns <- renderUI({
      req(input$phenotype_data$datapath)  # Check if phenotype data is selected
      pheno_data <-  get_metadata()
      tagList(
        h4("Select the targeted columns for the analysis"),
        selectInput(
          ns("conditions_column"),
          "Choose the conditions column",
          choices = c("None", colnames(pheno_data))
        ),
        selectInput(
          ns("samples_column"),
          "Choose the samples names column",
          choices = c("None", colnames(pheno_data))
        )
      )

    })

    #-------------------------------------------------------------------------
    # Modified metadata based on selected columns
    #-------------------------------------------------------------------------
    modified_metadata <- reactive({
      req(input$conditions_column,
          input$samples_column,
          get_metadata())
      pheno_data <-  get_metadata()
      rownames(pheno_data) <- NULL
      metadata <- pheno_data %>%
        select(input$conditions_column, input$samples_column) %>%
        dplyr::rename(conditions = colnames(.[1]), samples = colnames(.[2]))
      print(metadata)
      return(metadata)
    })




    #-------------------------------------------------------------------------
    # Function to read count matrix data
    #-------------------------------------------------------------------------
    get_gene_data <- reactive({
      req(input$gene_expression_data$datapath)  # Check if gene expression data is selected
      gene_data <- read.csv(input$gene_expression_data$datapath, header = TRUE)
      colnames(gene_data)[1] <- "geneID"
      colnames(gene_data) <- gsub(".bam", "", colnames(gene_data))
      return(gene_data)
    })
    #-------------------------------------------------------------------------
    # Render count matrix table
    #-------------------------------------------------------------------------
    output$count_data <- DT::renderDataTable({
      if (is.null(input$gene_expression_data)) {
        return(data.frame(Message = "Upload your count matrix as a .csv file"))
      }

      gene_data <- get_gene_data()


      DT::datatable(gene_data, options = list(scrollX = TRUE))
    })


    #-------------------------------------------------------------------------
    # Join gene expression data with modified metadata
    #-------------------------------------------------------------------------
    join_data <- reactive({
      req(get_gene_data())  # Ensure gene data exists
      req(modified_metadata())  # Ensure metadata exists

      gene_data <- get_gene_data()

      # Check if gene_data has at least one row
      req(nrow(gene_data) > 0)

      gene_data <- gene_data %>%
        gather(key = 'samples', value = 'count', -geneID) %>%
        left_join(., modified_metadata(), by = c("samples" = "samples"))

      return(gene_data)
    })



    #-------------------------------------------------------------------------
    # Render count matrix data table with the metadata
    #-------------------------------------------------------------------------
    output$desc_expression_data <- DT::renderDataTable({
      if (is.null(input$gene_expression_data) ||
          is.null(input$phenotype_data) ||
          input$conditions_column == 'None' ||
          input$samples_column == "None" || is.null(get_metadata())) {
        return(
          data.frame(Message = "Check your metadata file or your count matrix or select the targeted columns")
        )
      }
      DT::datatable(join_data(), options = list(scrollX = TRUE))
    })

    #-------------------------------------------------------------------------
    # Render UI for count matrix data visualization
    #-------------------------------------------------------------------------
    output$visual_gene_expression <- renderUI({
      list(
        fluidRow(
          column(6,textInput(ns("geneName"), "Enter the name of gene")),
          column(6,radioButtons(
            ns("choose_plot_type"),
            "Choose the type of visualization plot",
            choices = c("Bar plot", "Density")
          ))
        ),
           plotOutput(ns("visualization")))


    })

    #-------------------------------------------------------------------------
    # Render selected gene expression data visualization
    #-------------------------------------------------------------------------
    output$visualization <- renderPlot({
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
          ylim = c(0, 5),
          main = "Plot Will Appear Here Once Data is Selected"
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
      # Get user input
      refColumn <- input$conditions_column
      data <- get_gene_data()
      metadata <- get_metadata()

      print(data)
      print(rownames(data))

      # Ensure first column is set as row names and remove it
      rownames(data) <- data[[1]]
      data <- data[, -1, drop = FALSE]

      rownames(metadata) <- metadata[[1]]
      metadata <- metadata[, -1, drop = FALSE]

      # Convert count data to a matrix of integers
      data <- as.matrix(data)
      storage.mode(data) <- "integer"

      # Convert metadata reference column to factor
      if (!is.factor(metadata[[refColumn]])) {
        metadata[[refColumn]] <- as.factor(metadata[[refColumn]])
      }

      # Ensure metadata is a data frame
      metadata <- as.data.frame(metadata)

      # Construct DESeq2 dataset
      dds <- DESeqDataSetFromMatrix(
        countData = data,
        colData = metadata,
        design = as.formula(paste("~", refColumn))
      )

      # Filter genes with at least X reads
      keep <- rowSums(counts(dds)) >= as.numeric(input$filtering_factor)
      dds <- dds[keep, ]

      # Run DESeq2 analysis
      dds <- DESeq(dds)
      res <- results(dds, alpha = as.numeric(input$input_alpha))

      # Convert S4 results object to a data frame before filtering
      res_df <- as.data.frame(res)

      # Ensure complete cases before filtering
      res_df <- res_df[complete.cases(res_df), ]

      # Round numeric values for better readability
      numeric_cols <- sapply(res_df, is.numeric)
      res_df[, numeric_cols] <- round(res_df[, numeric_cols], 4)

      return(list(res_df, dds))
    })

    reactive_DGE <- reactiveVal(NULL)


    observeEvent(input$btn_run_DFEXR, {
      results <- run_DGEA()

      if (is.null(results)) {
        showNotification("Differential gene expression analysis did not run. Please check that all inputs are correctly provided.", type = "error")
      } else {
        showNotification("Differential gene expression analysis completed successfully.", type = "message")
      }
      reactive_DGE(results)
    })

    library(DESeq2)
    library(DT)
    library(shiny)
    library(ggplot2)
    library(EnhancedVolcano)
    library(pheatmap)
    library(RColorBrewer)
    library(apeglm)

    cal_z_scores <- function(x) {
      (x - mean(x)) / sd(x)
    }

    run_DGEA <- reactive({
      refColumn <- input$conditions_column
      data <- get_gene_data()
      metadata <- get_metadata()

      rownames(data) <- data[[1]]
      data <- data[, -1, drop = FALSE]

      rownames(metadata) <- metadata[[1]]
      metadata <- metadata[, -1, drop = FALSE]

      data <- as.matrix(data)
      storage.mode(data) <- "integer"

      if (!is.factor(metadata[[refColumn]])) {
        metadata[[refColumn]] <- as.factor(metadata[[refColumn]])
      }

      metadata <- as.data.frame(metadata)

      dds <- DESeqDataSetFromMatrix(
        countData = data,
        colData = metadata,
        design = as.formula(paste("~", refColumn))
      )

      keep <- rowSums(counts(dds)) >= as.numeric(input$filtering_factor)
      dds <- dds[keep, ]

      dds <- DESeq(dds)
      res <- results(dds, alpha = as.numeric(input$input_alpha))

      res_df <- as.data.frame(res)
      res_df <- res_df[complete.cases(res_df), ]
      numeric_cols <- sapply(res_df, is.numeric)
      res_df[, numeric_cols] <- round(res_df[, numeric_cols], 4)

      return(list(res_df, dds))
    })

    reactive_DGE <- reactiveVal(NULL)

    observeEvent(input$btn_run_DFEXR, {
      results <- run_DGEA()
      if (is.null(results)) {
        showNotification("Differential gene expression analysis did not run.", type = "error")
      } else {
        showNotification("DGEA completed successfully.", type = "message")
      }
      reactive_DGE(results)
    })

    output$results <- DT::renderDataTable({
      resultData <- reactive_DGE()
      DT::datatable(as.data.frame(resultData[[1]]), options = list(scrollX = TRUE))
    })



    #-------------------------------------------------------------------------
    #Render the table results
    #-------------------------------------------------------------------------
    output$results <- DT::renderDataTable({
      if (is.null(input$gene_expression_data)) {
        return(
          data.frame(Message = " Setup your data for the analysis (Experimental design/count matrix)")
        )
      }
      resultData <- reactive_DGE()
      DT::datatable(as.data.frame(resultData[[1]]) , options = list(scrollX = TRUE))
    })

    #-------------------------------------------------------------------------
    #Render summary plots
    #-------------------------------------------------------------------------
    output$plot_summary <- renderPlot({
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
          ylim = c(0, 5),
          main = "Plot Will Appear Here Once Data is Selected"
        ))
      }
      resultData <- reactive_DGE()
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
          plotPCA(., intgroup = input$conditions_column) +
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
