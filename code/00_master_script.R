### Packages
# List required packages
packages <- c("data.table","tidyverse","this.path",
              "haven", "kableExtra", "openxlsx",
              "rmarkdown", "stringi", "stringr", "Hmisc",
              "ggrepel", "gridExtra", "fastDummies")

# Install missing packages
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) {
  install.packages(packages[!installed], repos = "https://cran.rstudio.com/")
}

# Load all packages
invisible(lapply(packages, library, character.only = TRUE))

### Working dir
current_dir <- this.dir()
setwd(file.path(current_dir,"../"))

lapply(c("results"),
       \(x) {if (!dir.exists(x)) {dir.create(x)}})

# Download prepared dumps (we will use only rds files)
# 2018: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/C2OTH9
# 2021: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/SGRQTI
# 2024: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/WMO7Y2

# Write way to load files 
# Cross-sections
rcvs24 <- as.data.table(readRDS("data/rcvs_rdd_2024_2026-03-14.Rds"))
rcvs21 <- as.data.table(readRDS("data/rcvs_2021_dataset_2026-03-14.Rds"))
rcvs18 <- as.data.table(readRDS("data/rcvs_2018_dataset_2026-03-14.Rds"))
# Panel
cohort24 <- as.data.table(readRDS("data/rcvs_panel_2024_2026-03-14.Rds"))

# (4) Run scripts
eval(parse("code/01a_gather_repeated_crosssections.R", encoding = "UTF-8"))
eval(parse("code/01b_gather_panel.R", encoding = "UTF-8"))
eval(parse("code/01c_attach_deflators.R", encoding = "UTF-8"))
eval(parse("code/01d_attach_panel_weights.R", encoding = "UTF-8"))
eval(parse("code/01e_export_data.R", encoding = "UTF-8"))
eval(parse("code/01f_prepare_english_version.R", encoding = "UTF-8"))
eval(parse("code/01g_rendering_codebooks.R", encoding = "UTF-8"))
eval(parse("code/02_produce_figures.R", encoding = "UTF-8"))
eval(parse("code/03_power_analysis.R", encoding = "UTF-8"))