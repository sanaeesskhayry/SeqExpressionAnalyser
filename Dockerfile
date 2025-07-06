FROM rocker/r-ver:4.3.3

# Set CRAN repo to RSPM (for faster binary installs)
RUN mkdir -p /etc/R && \
    echo 'options(repos = c(CRAN = "https://packagemanager.posit.co/all/__linux__/focal/latest"))' > /etc/R/Rprofile.site

# Install system libraries
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libbz2-dev \
    liblzma-dev \
    libpng-dev \
    libjpeg-dev \
    libtiff5-dev \
    libcairo2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libglpk-dev \
    zlib1g-dev \
    build-essential \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install remotes and BiocManager
RUN Rscript -e "install.packages(c('remotes', 'BiocManager'))"

# Copy the package source
COPY SeqExpressionAnalyser /home/app/SeqExpressionAnalyser
WORKDIR /home/app/SeqExpressionAnalyser

# Install CRAN + Bioconductor dependencies
RUN Rscript -e "remotes::install_deps(dependencies = TRUE, repos = BiocManager::repositories())"

# Install the package
RUN R CMD INSTALL .

# Expose port for Shiny
EXPOSE 3838

# Launch the app
CMD ["R", "-e", "SeqExpressionAnalyser::runAnalyser(host = '0.0.0.0', port = 3838)"]
