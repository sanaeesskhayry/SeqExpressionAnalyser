# `SeqExpressionAnalyser` - An interactive R package for comprehensive differential gene expression analysis of RNA-Seq data
![SeqExpressionAnalyser Logo](inst/www/app_logo.png)

 
`SeqExpressionAnalyser`is an advanced software package that serves as a web application for interactive and reproducible differential gene expression analysis of RNA-Seq data. This comprehensive tool allows users to easily go through each step of the differential expression analysis workflow: `Reading FASTQ data`, performing `quality control`, `trimming and filtering`, `alignment`, `read counting`, and `differential expression analysis`. All in a user-friendly and supported manner. Each module generates a diverse range of outputs, including detailed data tables, HTML reports, and summary visualizations.

The `SeqExpressionAnalyser` package is written in the `R` programming language and the `Shiny framework`. This comprehensive tool provides an accessible solution for performing interactive and reproducible analysis of RNA-seq data for researchers with many different profiles (life scientists, clinicians, and experienced bioinformaticians) with optimal time and minimal effort.



## Features Overview
The `SeqExpressionAnalyser` package is written in R/Shiny framework and integrates features from several popular Bioconductor packages. Calling the `runAnalyser()` function activates the web application, leveraging the Shiny reactive programming model to efficiently generate and update its components. The user interface is designed using the shiny dashboard package, featuring a sidebar organized into different tabs that represent the various steps necessary for comprehensive differential expression analysis, from data setup to functional analysis, with a main panel dedicated to processing data for each step.

`SeqExpressionAnalyser` encompasses five main sections:
1. **Data Setup**: Upload FASTQ/BAM data.
2. **Quality Control (QC) and Pre-processing**: Assess read quality using the [Rqc](https://bioconductor.org/packages/release/bioc/html/Rqc.html) package. Then, using the [QuasR](https://bioconductor.org/packages/release/bioc/html/QuasR.html) package from Bioconductor, we can trim and filter low-quality reads.
3. **Read Mapping**: Align reads to a reference genome using [Rsubread](https://bioconductor.org/packages/release/bioc/html/Rsubread.html) Bioconductor.
4. **Read Counting**: Utilize `featureCounts` to summarize reads.
5. **Differential Expression Analysis**: Implement [DESeq2](https://bioconductor.org/packages/release/bioc/html/DESeq2.html) for robust statistical analysis.

## Installation

Here is a complete user guide for downloading and installing the **SeqExpressionAnalyser** R package from GitHub using the **devtools** package.

## Prerequisites
Before installing the **SeqExpressionAnalyser** package from GitHub, ensure that:
- You have **R** installed on your system. Download it from [CRAN](https://cran.r-project.org/).
- You have **RStudio** (optional but recommended) installed. Get it from [RStudio](https://posit.co/download/rstudio-desktop/).
## Step 1: Install `devtools`
The `devtools` package allows you to install R packages from GitHub. First, install it from CRAN:
```r
install.packages("devtools")
```
Then, load the package into your R session:
```r
library(devtools)
```
## Step 2: Install the SeqExpressionAnalyser Package from GitHub
To install a package from GitHub, use the `install_github()` function. The syntax is:
```r
install_github("sanaeesskhayry/SeqExpressionAnalyser")
```
## Step 3: Load the Installed Package
Once installed, load the package into R using:
```r
library(SeqExpressionAnalyser)
```
## Step 4: Verify the Installation
To check if the package is installed correctly, you can:
- List installed packages:
  ```r
  installed.packages()
  ```
- Check if the package is available:
  ```r
  find.package("SeqExpressionAnalyser")
  ```
- Check the package version:
  ```r
  packageVersion("SeqExpressionAnalyser")
  ```
## Usage

To start using SeqExpressionAnalyser:
1. Load the package:
    ```R
    library(SeqExpressionAnalyser)
    ```
2. Launch the application:
    ```R
    runAnalyser()
    ```
3. Follow the user-friendly interface to upload your data and navigate through the analysis workflow.

## Example Workflow

1. Upload your RNA-Seq FASTQ files.
2. Perform quality control and visualize the results, then you can trim and filter low-quality reads.
3. Map your reads to a reference genome.
4. Count reads per gene.
5. Conduct differential expression analysis.
6. Interpret and visualize results with various plots (e.g., volcano plots, heatmaps).

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

## Contact

For questions or support, please contact:  
**Sanae Esskhayry**  
Email: [sanaeesskhayry@gmail.com](mailto:sanaeesskhayry@gmail.com)  
GitHub: [sanaeesskhayry](https://github.com/sanaeesskhayry)

---
