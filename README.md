# `SeqExpressionAnalyser` - An interactive R/Bioconductor package for comprehensive differential gene expression analysis of RNA-Seq data
 
`SeqExpressionAnalyser`is an advanced software package that serves as a web application for interactive and reproducible differential gene expression analysis of RNA-Seq data. This comprehensive tool allows users to easily go through each step of the differential expression analysis workflow: `Reading FASTQ/BAM data`, performing `quality control`, `trimming and filtering`, `alignment`, `read counting`, `differential expression analysis`, `annotation, and functional analysis`. All in a user-friendly and supported manner. Each module generates a diverse range of outputs, including detailed data tables, HTML reports, and summary visualizations.

The `SeqExpressionAnalyser` package is written in the `R` programming language and the `Shiny framework`. This comprehensive tool provides an accessible solution for performing interactive and reproducible analysis of RNA-seq data for researchers with many different profiles (life scientists, clinicians, and experienced bioinformaticians) with optimal time and minimal effort. The software systematically applies Bioconductor's three main concepts: `transparency`, through the development of free and open-source software; `efficiency`, using infrastructure defined by core-maintained packages integrated into a single development ecosystem; and `reproducibility`, by guaranteeing that the software runs on any operating system supported by the R software and produces the same results.

The `SeqExpressionAnalyser` R/Bioconductor package offers a web application that ensures ease of use and reproducibility in studying key steps of differential expression analysis in RNA-Seq. Its interactive and dynamic user interface guarantees accessibility for users with limited scripting experience. The package provides a wide range of features, along with guided workflows and comprehensive, time-saving summarization capabilities.

## Features Overview
The `SeqExpressionAnalyser` package is written in R/Shiny framework and integrates features from several popular Bioconductor packages. Calling the `SeqExpressionAnalyser()` function activates the web application, leveraging the Shiny reactive programming model to efficiently generate and update its components. The user interface is designed using the shiny dashboard package, featuring a sidebar organized into different tabs that represent the various steps necessary for comprehensive differential expression analysis, from data setup to functional analysis, with a main panel dedicated to processing data for each step.

`SeqExpressionAnalyser` encompasses six main sections:
1. **Data Setup**: Upload FASTQ/BAM data.
2. **Quality Control (QC) and Pre-processing**: Assess read quality using the [Rqc](https://bioconductor.org/packages/release/bioc/html/Rqc.html) package. Then, using the [QuasR](https://bioconductor.org/packages/release/bioc/html/QuasR.html) package from Bioconductor, we can trim and filter low-quality reads.
3. **Read Mapping**: Align reads to a reference genome using [Rsubread](https://bioconductor.org/packages/release/bioc/html/Rsubread.html) Bioconductor.
4. **Read Counting**: Utilize `featureCounts` to summarize reads.
5. **Differential Expression Analysis**: Implement [DESeq2](https://bioconductor.org/packages/release/bioc/html/DESeq2.html) for robust statistical analysis.
6. **Functional Analysis**: Perform gene set enrichment using ClusterProfiler.

## Installation

To install SeqExpressionAnalyser, follow these steps:

1. **Install R**: Ensure you have R (version >= 4.0) installed on your system.
2. **Install Bioconductor**: Check for the BiocManager package and install it if it's missing, then use it to install the BiocUpgrade package for updating Bioconductor packages.

    ```R
    if (!requireNamespace("BiocManager", quietly = TRUE))
        install.packages("BiocManager")
    BiocManager::install("BiocUpgrade")
    ```

3. **Install SeqExpressionAnalyzer**: Install via GitHub, development vertion

    ```R
    install.packages("devtools")
    library(devtools)
    devtools::install_github("sanaeesskhayry/SeqExpressionAnalyser")
    ```

## Usage

To start using SeqExpressionAnalyser:

1. Load the package:

    ```R
    library(SeqExpressionAnalyser)
    ```

2. Launch the application:

    ```R
    SeqExpressionAnalyser()
    ```

3. Follow the user-friendly interface to upload your data and navigate through the analysis workflow.

## Example Workflow

1. Upload your RNA-Seq FASTQ or BAM files.
2. Perform quality control and visualize the results, then you can trim and filter low-quality reads.
3. Map your reads to a reference genome.
4. Count reads per gene.
5. Conduct differential expression analysis.
6. Interpret and visualize results with various plots (e.g., volcano plots, heatmaps).

## Contributing

We welcome contributions! To contribute:

1. Fork the repository.
2. Create a new branch (`git checkout -b feature-branch`).
3. Make your changes and commit them (`git commit -m 'Add new feature'`).
4. Push to the branch (`git push origin feature-branch`).
5. Create a pull request.

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

## Contact

For questions or support, please contact:  
**Sanae Esskhayry**  
Email: [sanaeesskhayry01@gmail.com](mailto:sanaeesskhayry01@gmail.com)  
GitHub: [sanaeesskhayry](https://github.com/sanaeesskhayry)

---
