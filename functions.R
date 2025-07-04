# =============================================================================
# This script contains the function that are used in the main script to create
# all of the necessary variables. At the end of this script are some optional 
# functions that are useful for exploring the data.
# =============================================================================

# Runs ngram tracing on all the training problems (n = 1-6)
verify_authorship <- function(corpus, problems, corp_name) {
  
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
      
      # Compute performance
      perf <- performance(df_n)
      
      # Extract the desired metrics.
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

# Takes 50 samples of 1000 tokens from corpus
jaccard_sample <- function(corpus, sample_length, num_of_samples) {
  
  samples <- corpus_group(corpus, groups = author) |>
    chunk_texts(size = sample_length) |>
    corpus_sample(size = num_of_samples)
  
  return(samples)
}

# Returns data frame of Jaccard coefficient for every pairwise combo of texts
jaccard <- function(samples, feature, corpus_name) {
  
  max_n <- ifelse(feature == 'character', 20, 10)
  
  results_df <- pblapply(1:max_n, function(n) {
    # Create weighted document-feature matrix
    dfm_matrix <- tokens(samples, what = feature) |>
      tokens_ngrams(n = n) |>
      dfm() |>
      dfm_weight(scheme = 'boolean')
    
    # Get document names
    doc_names <- rownames(dfm_matrix)
    
    # Compute pairwise overlap and union
    overlaps <- tcrossprod(as.matrix(dfm_matrix)) # Intersection counts
    row_sums <- rowSums(as.matrix(dfm_matrix))
    unions <- outer(row_sums, row_sums, "+") - overlaps # Union counts
    
    # Jaccard similarity
    jaccard_matrix <- overlaps / unions
    
    # Convert to long format
    pairwise_data <- expand.grid(A = doc_names, B = doc_names) |>
      mutate(
        A = as.character(A),
        B = as.character(B),
        overlap = as.vector(overlaps),
        union = as.vector(unions),
        jaccard = as.vector(jaccard_matrix)
      ) |>
      filter(A < B) |>  # Remove duplicate comparisons
      mutate(
        author_A = sub("\\.[0-9]+$", "", as.character(A)),
        author_B = sub("\\.[0-9]+$", "", as.character(B))
      ) |>
      filter(author_A != author_B) |>  # Exclude same-author pairs
      mutate(n = n, corpus = corpus_name)
    
    return(pairwise_data)
  })
  
  # Combine results from all n-gram sizes
  results_df <- do.call(bind_rows, results_df)
  
  return(results_df)
}

# Returns data frame of the relative frequencies of all relevant POS, as well 
# as the computed F measure
measure_formality <- function(samples, corpus_name) {
  
  df <- spacy_parse(samples,
                    pos = TRUE,
                    tag = FALSE,
                    lemma = FALSE,
                    entity = FALSE,
                    dependency = FALSE,
                    nounphrase = FALSE,
                    multithread = FALSE,
                    additional_attributes = NULL) |>
    filter(pos != "SPACE") |> # Remove the "SPACE" pos tag
    group_by(doc_id) |>
    reframe(
      total_pos = n(),  # Total number of POS tags (excluding SPACE)
      noun_freq = sum(pos == "NOUN", na.rm = TRUE) / total_pos,
      adj_freq = sum(pos == "ADJ", na.rm = TRUE) / total_pos,
      adp_freq = sum(pos == "ADP", na.rm = TRUE) / total_pos,
      det_freq = sum(pos == "DET", na.rm = TRUE) / total_pos,
      pron_freq = sum(pos == "PRON", na.rm = TRUE) / total_pos,
      verb_freq = sum(pos == "VERB", na.rm = TRUE) / total_pos,
      adv_freq = sum(pos == "ADV", na.rm = TRUE) / total_pos,
      intj_freq = sum(pos == "INTJ", na.rm = TRUE) / total_pos,
      f = ((noun_freq + adj_freq + adp_freq + det_freq - 
              pron_freq - verb_freq - adv_freq - intj_freq + 1) * 50)  # Scale to 100 and divide by 2
    ) |>
    mutate(corpus = corpus_name) |> # Add corpus column
    ungroup()  # Ensure the result is ungrouped
  
  return(df)
}

# Measures formality and lexical density
formality_ld <- function(samples, corpus_name) {
  
  # Tokenise
  toks <- tokens(samples, 
                 what = "word",
                 remove_punct = TRUE,
                 remove_symbols = TRUE,
                 remove_numbers = TRUE,
                 remove_url = TRUE)
  
  # Calculate total tokens and content tokens
  total_tokens <- ntoken(toks)
  content_tokens <- tokens_remove(toks, stopwords("english")) |> ntoken()
  
  # Calculate lexical density
  lexical_density <- content_tokens / total_tokens
  
  # Make data frame of formality and LD
  df <- spacy_parse(samples,
                    pos = TRUE,
                    tag = FALSE,
                    lemma = FALSE,
                    entity = FALSE,
                    dependency = FALSE,
                    nounphrase = FALSE,
                    multithread = FALSE,
                    additional_attributes = NULL) |>
    filter(pos != "SPACE") |> # Remove the "SPACE" pos tag
    group_by(doc_id) |>
    reframe(
      total_pos = n(),  # Total number of POS tags (excluding SPACE)
      noun_freq = sum(pos == "NOUN", na.rm = TRUE) / total_pos,
      adj_freq = sum(pos == "ADJ", na.rm = TRUE) / total_pos,
      adp_freq = sum(pos == "ADP", na.rm = TRUE) / total_pos,
      det_freq = sum(pos == "DET", na.rm = TRUE) / total_pos,
      pron_freq = sum(pos == "PRON", na.rm = TRUE) / total_pos,
      verb_freq = sum(pos == "VERB", na.rm = TRUE) / total_pos,
      adv_freq = sum(pos == "ADV", na.rm = TRUE) / total_pos,
      intj_freq = sum(pos == "INTJ", na.rm = TRUE) / total_pos,
      f = ((noun_freq + adj_freq + adp_freq + det_freq - 
              pron_freq - verb_freq - adv_freq - intj_freq + 1) * 50)  # Scale to 100 and divide by 2
    ) |>
    mutate(
      corpus = corpus_name,  # Add corpus column
      LD = lexical_density[doc_id]  # Add lexical density by matching doc_id
    ) |> 
    ungroup()  # Ensure the result is ungrouped
  
  return(df)
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