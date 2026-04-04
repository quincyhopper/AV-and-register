# =============================================================================
# The purpose of this script is to convert the enron_cleaned files into a 
# quanteda corpus of the same format as training_Original. 
# enron_cleaned contains two directories: Enron_Test_cleaned and Enron_Training_cleaned.
# We use Enron_Training_cleaned to create the corpus used for all further analysis.
# =============================================================================

library(quanteda)
library(tidyverse)

enron_path <- "data/enron_cleaned/Enron_Training_cleaned/"

all_files <- list.files(enron_path, pattern = "\\.txt$", 
                        recursive = TRUE, full.names = TRUE)

# Deduplicate by basename - keep only first occurrence of each filename
all_files <- all_files[!duplicated(basename(all_files))]

# Read texts named by filename only
texts <- map_chr(all_files, read_file)
names(texts) <- basename(all_files) |> str_trim()

# Build corpus
enron_corp <- corpus(texts)

# Parse docvars from filename
docvars(enron_corp, "texttype") <- str_extract(names(texts), "known|unknown")
docvars(enron_corp, "author")   <- str_extract(names(texts), "(?<=\\[)[^-]+(?= -)") |> str_trim()
docvars(enron_corp, "corpus")   <- "Enron"