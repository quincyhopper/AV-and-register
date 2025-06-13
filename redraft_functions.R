# =============================================================================
# This script contains the function that are used in the main script to create
# all of the necessary variables. At the end of this script are some optional 
# functions that are useful for exploring the data.
# =============================================================================

# Runs ngram tracing on all the training problems (n = 1-6)
attribute_authorship <- function(corpus, problems, corp_name) {
  
  # Extract known/unknown texts
  doc_names <- docnames(corpus)
  Q_data <- corpus_subset(corpus, grepl("^unknown", doc_names))
  K_data <- corpus_subset(corpus, grepl("^known", doc_names))
  
  # Filter out other corpora from problem set
  problems <- filter(problems, corpus == corp_name)
  
  # Process each problem in the problems dataframe
  combos <- lapply(seq_len(nrow(problems)), function(i) {
    
    unknown_author <- problems$unknown_author[i]
    known_author <- problems$known_author[i]
    
    # Extract problem combo from corpus for the current row
    Q <- corpus_subset(Q_data, author == unknown_author)
    K <- corpus_subset(K_data, author == known_author)
    
    # For each n-gram length (from 1 to 6), run n-gram tracing
    results_list <- lapply(1:6, function(n){
      
      result <- ngram_tracing(Q, K,
                              tokens = 'word',
                              remove_punct = FALSE,
                              remove_symbols = TRUE,
                              remove_numbers = TRUE,
                              lowercase = FALSE,
                              n = n,
                              coefficient = 'simpson',
                              features = FALSE,
                              cores = 4) |> 
        dplyr::mutate(n = n,
                      problem_index = i)  # optional: track which problem this is from
      
      return(result)
    })
    
    # Combine all n-gram results for the current problem
    combined_results <- dplyr::bind_rows(results_list)
    return(combined_results)
  })
  
  # Combine the results from all problems into one dataframe
  final_results <- dplyr::bind_rows(combos)
  
  return(final_results)
}

# Calculates the metrics for the AV results and returns a data frame
performance_table <- function(acl_authorship, stack_authorship, amazon_authorship, enron_authorship, blog_authorship, perv_authorship) {
  # Create a named list of the corpora in the desired order:
  corpora <- list(
    ACL = acl_authorship,
    Stack = stack_authorship,
    Amazon = amazon_authorship,
    Enron = enron_authorship,
    Blog = blog_authorship,
    PJ = perv_authorship
  )
  
  # Define the n-gram lengths (1 to 6)
  n_values <- 1:6
  
  # Initialize an empty list to store results for each corpus and n-gram length
  results_list <- list()
  
  # Loop through each corpus and each n-gram length
  for (corpus_name in names(corpora)) {
    for (n_val in n_values) {
      # Filter the data frame for the current n-gram length
      df_n <- dplyr::filter(corpora[[corpus_name]], n == n_val)
      
      # Compute performance; assume performance() returns a list with evaluation and roc elements.
      perf <- performance(df_n)
      
      # Extract the desired metrics.
      # Note: We assume the names in perf$evaluation are:
      # "Balanced Accuracy", "AUC", "F1", "Precision", and "Recall".
      # You can adjust these if the actual names differ.
      acc       <- perf$evaluation$`Balanced Accuracy`
      auc       <- perf$evaluation$AUC
      f1        <- perf$evaluation$F1
      precision <- perf$evaluation$Precision
      recall    <- perf$evaluation$Recall
      
      # Create a temporary data frame for this corpus and n-gram length
      temp_df <- data.frame(
        Corpus = corpus_name,
        n = n_val,
        Accuracy = acc,
        AUC = auc,
        F1 = f1,
        Precision = precision,
        Recall = recall,
        stringsAsFactors = FALSE
      )
      
      # Append to our list
      results_list[[length(results_list) + 1]] <- temp_df
    }
  }
  
  # Combine all rows into a single data frame
  final_df <- do.call(rbind, results_list)
  return(final_df)
}

# Calculates the top ten ngrams (1-3) for each corpus
top_word_table <- function(acl, stack, amazon, enron, blog, PJ) {
  # Explicitly name the corpora in the desired order:
  corpora <- list(acl = acl,
                  stack = stack,
                  amazon = amazon,
                  enron = enron,
                  blog = blog,
                  PJ = PJ)
  
  n_values <- c(1, 2, 3)
  
  result <- matrix("",
                   nrow = length(n_values),
                   ncol = length(corpora),
                   dimnames = list(as.character(n_values), names(corpora)))
  
  for (corpus_name in names(corpora)) {
    for (n in n_values) {
      topwords <- tokens(corpora[[corpus_name]],
                         what = 'word',
                         remove_punct = TRUE,
                         remove_symbols = TRUE,
                         remove_numbers = TRUE,
                         remove_url = TRUE,
                         remove_separators = TRUE,
                         concatenator = " ") |>
        tokens_ngrams(n = n) |>
        tokens_remove(stopwords("english")) |>
        dfm() |>
        topfeatures(n = 10)
      
      cell_text <- paste(names(topwords), collapse = "\n")
      
      result[as.character(n), corpus_name] <- cell_text
    }
  }
  
  as.data.frame(result)
}

# Calculates the mean number of tokens in a corpus' 'known' texts and 'unknown'
# texts
tokens_per_QK <- function(corpus) {
  
  doc_names <- docnames(corpus)
  
  K_corpus <- corpus_subset(corpus, author %in% training_problems$known_author)
  if (ndoc(K_corpus) == 0) {
    stop("K corpus is empty. Check if 'author' matches any 'known_author' in training_problems.")
  }
  
  K_corpus <- corpus_subset(K_corpus, grepl("^known", doc_names))
  if (ndoc(K_corpus) == 0) {
    stop("K corpus is empty after filtering for 'known' doc names.")
  }
  
  K <- K_corpus |>
    ntoken() |>
    mean() |>
    round(0)
  
  Q_corpus <- corpus_subset(corpus, author %in% training_problems$unknown_author)
  if (ndoc(Q_corpus) == 0) {
    stop("Q corpus is empty. Check if 'author' matches any 'unknown_author' in training_problems.")
  }
  
  Q_corpus <- corpus_subset(Q_corpus, grepl("^unknown", doc_names))
  if (ndoc(Q_corpus) == 0) {
    stop("Q corpus is empty after filtering for 'unknown' doc names.")
  }
  
  Q <- Q_corpus |>
    ntoken() |>
    mean() |>
    round(0)
  
  cat("K: ", K, "\n")
  cat("Q: ", Q, "\n")
}