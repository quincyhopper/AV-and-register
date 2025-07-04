# =============================================================================
# This script manipulates the corpora and creates the variables necessary for 
# use in the quarto document, namely:
# (1) formulaicity data
# (2) formality data
# (3) the top 10 most common ngrams in each corpora
# (4) the model
# (5) the AV experiment results
#
# Note: some functions get masked by some of the packages, so I tend to only
# run up to viridis until I calculate the model.
#
# Load training_Origininal and training_problems into the global environment
# before starting.
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

# Extract corpora
acl_corp <- corpus_subset(training_Original, corpus == 'ACL')
blog_corp <- corpus_subset(training_Original, corpus == "Koppel's Blogs")
amazon_corp <- corpus_subset(training_Original, corpus == "Amazon")
enron_corp <- corpus_subset(training_Original, corpus == "Enron")
stack_corp <- corpus_subset(training_Original, corpus == "StackExchange")
perv_corp <- corpus_subset(training_Original, corpus == "Perverted Justice")

# Extract training problems for each corpus
training_problems <- filter(training_problems, corpus %in% c("ACL", "Koppel's Blogs", "Amazon", "Enron", "StackExchange", "Perverted Justice"))

# =============================================================================
# Section 1: Calculating Jaccard and formality
# =============================================================================

# 1. Take 50 samples of 1000 tokens from each corpus
acl_samples <- jaccard_sample(acl_corp, sample_length = 1000, num_of_samples = 50)
amazon_samples <- jaccard_sample(amazon_corp, sample_length = 1000, num_of_samples = 50)
blog_samples <- jaccard_sample(blog_corp, sample_length = 1000, num_of_samples = 50)
enron_samples <- jaccard_sample(enron_corp, sample_length = 1000, num_of_samples = 50)
stack_samples <- jaccard_sample(stack_corp, sample_length = 1000, num_of_samples = 50)
perv_samples <- jaccard_sample(perv_corp, sample_length = 1000, num_of_samples = 50)

# 2. Calculate Jaccard coefficient for the samples 
acl_word_jaccard <- jaccard(acl_samples, feature = 'word', 'ACL')
amazon_word_jaccard <- jaccard(amazon_samples, feature = 'word', 'Amazon')
blog_word_jaccard <- jaccard(blog_samples, feature = 'word', 'Blog')
enron_word_jaccard <- jaccard(enron_samples, feature = 'word', 'Enron')
stack_word_jaccard <- jaccard(stack_samples, feature = 'word', 'Stack')
perv_word_jaccard <- jaccard(perv_samples, feature = 'word', "PJ")

# 3. Combine Jaccard results into one data frame
combined_jaccard <- bind_rows(acl_word_jaccard,
                           amazon_word_jaccard,
                           blog_word_jaccard,
                           enron_word_jaccard,
                           stack_word_jaccard,
                           perv_word_jaccard)

# 4. Calculate formality and ld for each corpus
acl_formality <- measure_formality(acl_samples, 'ACL')
amazon_formality <- measure_formality(amazon_samples, 'Amazon')
blog_formality <- measure_formality(blog_samples, 'Blog')
enron_formality <- measure_formality(enron_samples, 'Enron')
stack_formality <- measure_formality(stack_samples, 'Stack')
perv_formality <- measure_formality(perv_samples, "PJ")

# 5. Combine formality results into one data frame 
combined_formality <- bind_rows(acl_formality,
                                amazon_formality,
                                blog_formality,
                                enron_formality,
                                stack_formality,
                                perv_formality)

# 5. Combine Jaccard results and formality results into one data frame
main_word <- combined_jaccard |>
  left_join(dplyr::select(combined_formality, doc_id, f), by = c("A" = "doc_id")) |>
  rename(f_A = f) |>
  left_join(dplyr::select(combined_formality, doc_id, f), by = c("B" = "doc_id")) |>
  rename(f_B = f) |>
  mutate(
    mean_f = (f_A + f_B)/2,
    overlap = as.numeric(overlap),
    union = as.numeric(union),
    jaccard = as.numeric(jaccard),
    f_A = as.numeric(f_A),
    f_B = as.numeric(f_B),
    mean_f = as.numeric(mean_f),
    n = as.numeric(n)
  )

# 6. Save data
saveRDS(combined_formality, file = "combined_formality.rds")
saveRDS(main_word, file = "main_word.rds")

# 7. Create data frame of mean Jaccard for each n for each corpus (optional)
options(scipen = 999)
jaccard_summary <- combined_jaccard %>%
  group_by(n, corpus) %>%
  summarise(mean_jaccard = mean(jaccard, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = corpus, values_from = mean_jaccard) %>%
  mutate(across(where(is.numeric), as.numeric)) %>%
  data.frame()

# =============================================================================
# Section 2: Extracting top 10 most common words from each corpus
# =============================================================================

# 1. Create data frame of top ten most common words for all corpora
topwords <- top_word_table(acl_samples, 
                           stack_samples, 
                           amazon_samples, 
                           enron_samples,
                           blog_samples,
                           perv_samples)

saveRDS(topwords, file =  "topwords.rds")

# =============================================================================
# Section 3: Negative binomial regression model
# =============================================================================

# 1. Model data
nb_model<- glm.nb(overlap ~ mean_f + I(n^2) + offset(log(union)),
                  data = filter(main_word, n < 4))
saveRDS(nb_model, "nb_model.rds")

# =============================================================================
# Section 4: Authorship verification experiment
# =============================================================================

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
                                         problems = training_problems, 
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
saveRDS(av_results, file = "av_results.rds")

# 3. Calculate the confidence intervals for the AV results
CI_table <- av_results %>%
  # Add the number of verification cases per corpus using recode
  mutate(
    N = recode(Corpus,
               "ACL"    = 186,
               "Amazon" = 1600,
               "Blog"   = 1200,
               "Enron"  = 64,
               "Stack"  = 150,
               "PJ" = 208)
  ) %>%
  # Compute the standard error and confidence intervals
  mutate(
    p = Accuracy,
    SE = sqrt(p * (1 - p) / N),
    lower_ci = p - qnorm(0.975) * SE,
    upper_ci = p + qnorm(0.975) * SE
  )
saveRDS(CI_table, "CI_table.rds")
