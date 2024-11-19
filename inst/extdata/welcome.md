# Welcome to `SeqExpressionAnalyzer`!


`SeqExpressionAnalyzer`is a Bioconductor package containing a Shiny application for analyzing RNA-Seq data from       quality control to differential expression. This comprehensive tool provides an accessible solution for performing interactive and reproducible analysis of RNA-seq data for researchers with many different profiles (life scientists, clinicians, and experienced bioinformaticians) with optimal time and minimal effort.

`SeqExpressionAnalyzer` is a web-based application that allows users to analyze RNA-Seq data in an interactive and reproducible manner. A call to the `RunAnalyser()` method provides the web application with all of its features by completely using the Shiny reactive programming model to efficiently regenerate the rendered components.  
The user interface layout is based on the shiny dashboard package, with a sidebar structured into different tabs that represent the various steps required to perform a comprehensive differential expression analysis, from data setup to functional analysis, and a main panel with full data processing for each step.


# Feature Overview

The `SeqExpressionAnalyzer` application includes six features: `(1) data setup`, `(2) quality
control and data pre-processing`, `(3) read mapping`, `(4) read counting`, `(5) differential
gene expression analysis`, and `(6) functional analysis` are described as follows:


1- **Data setup:** SeqExpressionAnalyzer requires uploading fastq (or.fastq.gz) format RNA-Seq data from sequencing studies, typically obtained from online databases or collaboration with sequencing facilities.

2- **Quality Control (QC) and Pre-processing:** SeqExpressionAnalyzer uses the `Rqc` Bioconductor package for quality control and pre-processing of high-throughput sequencing data. It supports both FASTQ and BAM files as input. It uses parallel computing to handle large datasets and provides visualization tools for identifying patterns. The software provides also reports that integrates seamlessly to the R/Bioconductor environment, including publication-ready images.The `QuasR` package from Bioconductor is used to trim and filter low-quality reads, with parameters including base truncation, adapter sequence, sequence length, and complexity.

3- **Read mapping:** `Rsubread` software, a Bioconductor tool, efficiently aligns clean reads to
reference genomes, offering faster speeds, lower memory usage, and more accurate
read count summaries compared to competitors like TopHat2, STAR, and HTSeq.

4- **Read Counting:** The SeqExpressionAnalyzer package uses featureCounts to assign reads
to genes based on GTF/GFF file annotation, obtaining raw read counts in a CSV file for
subsequent analysis.

5- **Normalization and Differential Expression Analysis:** SeqExpressionAnalyzer is based on the DESeq2 package. It is a popular tool in the scientific community because of its strong statistical methods and consistent results in differential expression analysis of many types of high-throughput sequencing data. DESeq2 overcomes difficulties such as small replicate numbers, discreteness, and outliers by employing shrinkage estimation to produce more consistent and interpretable findings. Furthermore, the package DESeq2 normalizes the dataset by computing a size factor for each sample.

6- **Annotation and Biological Interpretation:** SeqExpressionAnalyzer uses gene set
enrichment analysis (GSEA) to predict functional outcomes after differential gene
expression analysis. It connects multiple biological databases, facilitating relationships
between pathways and differentially expressed genes. The Benjamini and Hochberg
method applies false discovery rate correction, reducing false positives and improving
reliability.

7- **Visualization and Interpretation:** SeqExpressionAnalyzer offers various visualizations, including volcano plots (a), MA plots (b), heat maps (c), etc., for analyzing RNA-Seq data in various analysis steps.


</br></br>
