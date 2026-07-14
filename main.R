# =============================================================================
# This script manipulates the corpora and creates the variables necessary for 
# use in the quarto document, namely:
# (1) formality data
# (2) the AV experiment results
# (3) unique overlapping ngrams
#
# Load training_Original and training_problems into the global environment
# before starting.
# Run prepare_enron.R to load the cleaned enron_corp into the global environment.
# =============================================================================

library(idiolect)
library(quanteda)
library(quanteda.textstats) 
library(glue)
library(spacyr)
library(ggplot2)
library(ggeffects)
library(dplyr)
library(pbapply)
library(tidyr)
library(reshape2)
library(viridis)
library(knitr)
library(MASS)
library(pscl)
library(stringr)

# Extract corpora (enron_corp should be loaded already)
acl_corp <- corpus_subset(training_Original, corpus == 'ACL')
blog_corp <- corpus_subset(training_Original, corpus == "Koppel's Blogs")
amazon_corp <- corpus_subset(training_Original, corpus == "Amazon")
stack_corp <- corpus_subset(training_Original, corpus == "StackExchange")
perv_corp <- corpus_subset(training_Original, corpus == "Perverted Justice")

# Get summary of datasets (docs, authors, avg_Q, etc) for Table 1
corpora_summary <- list(
  corpus_summary(acl_corp, training_problems, "ACL", "ACL"),
  corpus_summary(stack_corp, training_problems, "StackExchange", "Stack"),
  corpus_summary(amazon_corp, training_problems, "Amazon", "Amazon"),
  corpus_summary(enron_corp, enron_training_problems, "Enron", "Enron"),
  corpus_summary(blog_corp, training_problems, "Koppel's Blogs", "Blog"),
  corpus_summary(perv_corp, training_problems, "Perverted Justice", "PJ")
) %>%
  dplyr::bind_rows()

saveRDS(corpora_summary, file="data/corpora_summary.rds")

# =============================================================================
# Section 1: Calculating Formality
# =============================================================================

# 1. Take 50 samples of 1000 tokens from each corpus
acl_samples <- formality_sample(acl_corp, sample_length = 1000, num_of_samples = 50)
amazon_samples <- formality_sample(amazon_corp, sample_length = 1000, num_of_samples = 50)
blog_samples <- formality_sample(blog_corp, sample_length = 1000, num_of_samples = 50)
enron_samples <- formality_sample(enron_corp, sample_length = 1000, num_of_samples = 50)
stack_samples <- formality_sample(stack_corp, sample_length = 1000, num_of_samples = 50)
perv_samples <- formality_sample(perv_corp, sample_length = 1000, num_of_samples = 50)

# 2. Calculate formality
acl_formality <- measure_formality(acl_samples, 'ACL')
amazon_formality <- measure_formality(amazon_samples, 'Amazon')
blog_formality <- measure_formality(blog_samples, 'Blog')
enron_formality <- measure_formality(enron_samples, 'Enron')
stack_formality <- measure_formality(stack_samples, 'Stack')
perv_formality <- measure_formality(perv_samples, "PJ")

# 3. Combine formality results into one data frame 
combined_formality <- bind_rows(acl_formality,
                                amazon_formality,
                                blog_formality,
                                enron_formality,
                                stack_formality,
                                perv_formality)

# 4. Save data
saveRDS(combined_formality, file = "data/combined_formality.rds")

# =============================================================================
# Section 2: Authorship verification experiment
# =============================================================================

# Extract training problems for each corpus
training_problems <- filter(training_problems, corpus %in% c("ACL", "Koppel's Blogs", "Amazon", "Enron", "StackExchange", "Perverted Justice"))

# Extract training_problems for enron corpus; some authors have been removed
enron_authors <- docvars(enron_corp, 'author')
enron_training_problems <- training_problems |>
  filter(corpus == "Enron") |>
  filter(unknown_author %in% enron_authors & known_author %in% enron_authors)

# 1. For each corpus, run AV for the training problems
acl_authorship <- verify_authorship(acl_corp, 
                                       problems = training_problems, 
                                       corp_name = "ACL")
amazon_authorship <- verify_authorship(amazon_corp, 
                                          problems = training_problems, 
                                          corp_name = "Amazon")
blog_authorship <- verify_authorship(blog_corp, 
                                        problems = training_problems, 
                                        corp_name = "Koppel's Blogs")
enron_authorship <- verify_authorship(enron_corp, 
                                         problems = enron_training_problems, 
                                         corp_name = "Enron")
stack_authorship <- verify_authorship(stack_corp, 
                                         problems = training_problems, 
                                         corp_name = "StackExchange")
perv_authorship <- verify_authorship(perv_corp, 
                                        problems = training_problems, 
                                        corp_name = "Perverted Justice")

# 2. Calculate AV metrics (accuracy, precision, etc.) and organise in one data
# frame
av_results <- performance_table(acl_authorship,
                                stack_authorship,
                                amazon_authorship,
                                enron_authorship,
                                blog_authorship,
                                perv_authorship)
saveRDS(av_results, file = "data/av_results.rds")

# 3. Calculate the confidence intervals for the AV results
problem_counts <- corpora_summary |>
  dplyr::select(Corpus, N = Verification_cases)

CI_table <- av_results %>%
  dplyr::left_join(problem_counts, by = "Corpus") %>%
  dplyr::mutate(
    p = Accuracy,
    SE = sqrt(p * (1 - p) / N),
    lower_ci = p - qnorm(0.975) * SE,
    upper_ci = p + qnorm(0.975) * SE
  )

saveRDS(CI_table, "data/CI_table.rds")

# =============================================================================
# Section 3: Find unique overlapping ngrams
# =============================================================================

acl_overlaps <- extract_unique_overlaps(acl_corp, 
                                        problems = training_problems, 
                                        corp_name = "ACL")
amazon_overlaps <- extract_unique_overlaps(amazon_corp, 
                                           problems = training_problems, 
                                           corp_name = "Amazon")
blog_overlaps <- extract_unique_overlaps(blog_corp, 
                                              problems = training_problems, 
                                              corp_name = "Koppel's Blogs")
enron_overlaps <- extract_unique_overlaps(enron_corp, 
                                          problems = enron_training_problems, 
                                          corp_name = "Enron")
stack_overlaps <- extract_unique_overlaps(stack_corp, 
                                          problems = training_problems, 
                                          corp_name = "StackExchange")
perv_overlaps <- extract_unique_overlaps(perv_corp, 
                                         problems = training_problems, 
                                         corp_name = "Perverted Justice")

overlap_datasets <- list(
  "ACL"    = acl_overlaps,
  "Enron"  = enron_overlaps,
  "Amazon" = amazon_overlaps,
  "Blog"   = blog_overlaps,
  "Stack"  = stack_overlaps,
  "PJ"     = perv_overlaps
)

# Counts of unique overlaps per corpus (just to check things look normal)
same_author_overlaps <- summarise_overlaps(overlap_datasets, pair_type = 'same')
different_author_overlaps <- summarise_overlaps(overlap_datasets, pair_type = 'different')

# Sample 5 ngrams per corpus for n=1,3,6
unique_ngram_examples <- sample_raw_overlaps(overlap_datasets)
saveRDS(unique_ngram_examples, file='data/unique_ngram_examples.rds')