# Welcome to `SeqExpressionAnalyser`!

`SeqExpressionAnalyser` is an advanced R package designed as a web-based application for interactive RNA-Seq data analysis, covering the entire workflow from quality control to differential expression analysis. It provides an intuitive and efficient solution for researchers from diverse backgrounds—including life scientists, clinicians, and bioinformaticians—enabling comprehensive RNA-Seq analysis with minimal effort.

The application is launched via the `RunAnalyser()` function, which initiates a fully interactive web interface powered by Shiny’s reactive programming model, ensuring dynamic and efficient data rendering.

The user interface is built with the `Shiny Dashboard package` and consists of a structured sidebar with tabs representing different stages of analysis, from data preprocessing to differential gene expression analysis. The main panel provides interactive tools for data processing and visualisation, making RNA-Seq analysis streamlined and accessible.

---

# SeqExpressionAnalyser: Comprehensive Features Overview 

`SeqExpressionAnalyser` is an intuitive application designed to streamline RNA-Seq data analysis. It offers a complete end-to-end workflow, covering everything from raw data preparation to result interpretation and visualisation. With six core features, the software ensures a seamless and efficient analysis pipeline suitable for researchers of all backgrounds.

---

### **1. Data Setup**  
SeqExpressionAnalyser begins by requiring users to provide a **directory path** containing only the relevant **FASTQ** or **FASTQ.GZ** files. Additionally, users must upload a **study metadata file** in **CSV format**, which contains essential study design and experimental details. The column names from the metadata will be displayed in a select list, allowing users to select the appropriate grouping column to organise the uploaded data according to the study design.  

Users are also required to specify:  
- Whether the sequencing data is **paired-end or single-end**.  
- The **number of parallel workers** for sequence reading to optimise processing speed.  

Once all parameters are set, users can click the **"Upload FASTQ Files"** button. Upon successful upload, a **summary table** will display key details, including:  
- File name and base directory.  
- Number of sampled reads and total reads for each file.  

The application also provides **streamlined data representations**, highlighting:  
- The most abundant sequencing reads and their counts.  
- The frequency distribution of read lengths.  
- The mean quality of reads.  
- Cycle-specific quality metrics and base calls.  
- Insights into **unique and duplicated reads**, improving data quality assessment.  

---  

### **2. Quality Control and Pre-processing**  
Maintaining high data quality is essential for accurate results. SeqExpressionAnalyser incorporates robust tools to evaluate and refine sequencing data.  

#### **Quality Control**  
- Utilises the [`Rqc`](https://www.bioconductor.org/packages/release/bioc/html/Rqc.html) Bioconductor package to assess sequencing quality.  
- Implements **parallel computing** for enhanced processing speed when handling large datasets.  
- Provides interactive **visualisation tools** to detect common sequencing issues, such as low-quality bases, GC-content variations, and adapter contamination.  
- Generates **publication-ready quality report**. 

#### **Pre-processing**  
SeqExpressionAnalyser ensures data is clean and ready for analysis using the [`QuasR`](https://www.bioconductor.org/packages/release/bioc/html/QuasR.html) package. The following types of filtering tasks can be performed in sequence:
- **Truncate Reads:** Removes nucleotides from both the 5′ and 3′ ends of each read to eliminate low-quality regions.
- **Trim Adapters:** Removes any adapter sequences present at the beginning or end of reads.
- **Filter Low-Quality Reads:** Users can specify thresholds to filter out reads that do not meet certain quality criteria, such as:
  - ***N Bases:*** Restricting the number of ambiguous 'N' calls within a read.
  - ***Min Length:*** Filtering out reads shorter than the specified length.
  - ***Dinucleotide Complexity:*** Excluding low-complexity reads with a dinucleotide entropy below a specified threshold. This complexity is calculated as Shannon entropy.

These steps guarantee high-quality input data for subsequent analyses.  

---  

### **3. Read Mapping**  
Accurate read alignment is a critical step in RNA-Seq analysis. SeqExpressionAnalyser employs the [`Rsubread`](https://bioconductor.org/packages/release/bioc/html/Rsubread.html) Bioconductor package for high-performance read mapping:  
- **Optimised Speed & Memory Usage** – Efficiently processes large sequencing datasets.  
- **High-accuracy Alignment** – Effectively maps reads, even in complex transcriptomes.  
- **BAM (or SAM) File Output** – Generates alignment files used for read counting and downstream analyses.  

This step ensures precise genome or transcriptome mapping, setting the stage for gene expression quantification.  

---  

### **4. Read Counting or Quantification**  
SeqExpressionAnalyser leverages [`featureCounts`](https://subread.sourceforge.net/featureCounts.html) to quantify gene expression by assigning reads to annotated genomic features:  
- **Supports GTF/GFF Annotations** – Reads are mapped to genes based on standard genome annotation formats.  
- **Efficient & Scalable** – Designed to handle large datasets while maintaining accuracy.  
- **Structured Output** – Produces raw read counts in CSV format, where rows represent genes, columns correspond to samples, and values indicate the number of reads per gene in each sample.  

This step generates essential expression data for comparative analysis.  

---  

### **5. Differential Expression Analysis**  
At the core of SeqExpressionAnalyser is its ability to detect differentially expressed genes (DEGs) using [`DESeq2`](https://bioconductor.org/packages/release/bioc/html/DESeq2.html):  

- **Robust Statistical Modelling** – Accounts for variability in gene expression and experimental conditions.  
- **Shrinkage Estimation** – Mitigates the effects of small sample sizes and outliers, improving result consistency.  
- **Normalisation** – Adjusts for differences in sequencing depth, ensuring comparability across samples.  
- **Comprehensive Output** – Generates a detailed list of DEGs, including log-fold changes, p-values, and adjusted p-values for biological interpretation. 
- **Visualisation and Interpretation** – SeqExpressionAnalyser includes an array of visualisation tools to facilitate data interpretation and exploration:  
  - **Volcano Plots** – Highlight significant DEGs based on fold-change and statistical thresholds.  
  - **MA Plots** – Display gene expression changes across different conditions.  
  - **Heatmaps** – Reveal gene expression patterns and clustering relationships across samples. 
--- 

## **Conclusion**  
SeqExpressionAnalyser is a comprehensive, user-friendly solution for RNA-Seq analysis. By integrating advanced computational tools with an intuitive interface, it empowers researchers to efficiently process, analyse, and visualise sequencing data. From raw data processing to publication-ready visualisations, SeqExpressionAnalyser ensures reproducibility, accuracy, and accessibility in transcriptomics research.  

Get started today and unlock the full potential of your RNA-Seq data!  

---


</br></br>
