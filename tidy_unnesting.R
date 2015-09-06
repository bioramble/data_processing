############################################################################
# Bioramble
# Tidy Unnesting
# by Jesse Lipp
# created: Sep 5, 2015
############################################################################

# --------------------------------------------------------------------------
# Set up environment
# --------------------------------------------------------------------------
# clean-up
rm(list = ls())

# libraries
if (!require(tidyr)) {
  install.packages("tidyr")
  library(tidyr)
}
if (!require(stringr)) {
  install.packages("stringr")
  library(stringr)
}

# --------------------------------------------------------------------------
# A toy example
# --------------------------------------------------------------------------
### create some example data
data <- data.frame("ID" = c("A", "B", "C"), 
                   "value" = c("34", "23;45;434", NA), 
                   stringsAsFactors = FALSE)

### untidy method
# split "value" into lists
values <- str_split(data$value, ";")
# count number of elements for each list
n <- sapply(values, length)
# replicate rownames of data based on elements for each list
row_rep <- unlist(mapply(rep, rownames(data), n))
# replicate rows of original data
data_tidy <- data[row_rep, ]
# replace nested measurements with unnested measurements
data_tidy$value <- unlist(values)
# reformat row names
rownames(data_tidy) <- seq(nrow(data_tidy))

### tidy method
# use dplyr/magrittr style piping
data_tidy2 <- data %>% 
  # split "value" into lists
  transform(value = str_split(value, ";")) %>%
  # unnest magic
  unnest(value)

### test if results are equivalent
all.equal(data_tidy, data_tidy2)

# --------------------------------------------------------------------------
# S. pombe name mapping
# --------------------------------------------------------------------------
# download data
if (!file.exists("sysID2product.tsv")) {
  download.file(url = "ftp://ftp.ebi.ac.uk/pub/databases/pombase/pombe/Mappings/sysID2product.tsv", 
                destfile = "sysID2product.tsv", 
                method = "curl")
}
# read data
raw <- read.delim("sysID2product.tsv", header = FALSE, stringsAsFactors = FALSE)
names(raw) <- c("orf", "symbol", "synonyms", "protein")
# unnest
data <- raw %>% 
  transform(synonyms = str_split(synonyms, ",")) %>% 
  unnest(synonyms)
