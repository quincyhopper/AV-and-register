# =============================================================================
# This script contains the function that are used in the main script to create
# all of the necessary variables.
# =============================================================================

# Get the corpus stats for Table
corpus_summary <- function(corpus, probs, corp_name, display_name) {
  
  # Get document names and valid authors
  doc_names <- quanteda::docnames(corpus)
  valid_authors <- unique(quanteda::docvars(corpus, 'author'))
  
  # Filter problems to only include valid authors
  valid_probs <- probs %>% 
    dplyr::filter(corpus == corp_name) %>%
    dplyr::filter((known_author %in% valid_authors) & (unknown_author %in% valid_authors))
  
  # Calculate average K tokens
  K <- quanteda::corpus_subset(corpus, grepl("^known", doc_names))
  avg_K <- round(mean(quanteda::ntoken(K)), 0)
  
  # Calculate average Q tokens
  Q <- quanteda::corpus_subset(corpus, grepl("^unknown", doc_names))
  avg_Q <- round(mean(quanteda::ntoken(Q)), 0)
  
  # Return dataframe with all features
  dplyr::tibble(
    Corpus = display_name,
    Documents = quanteda::ndoc(corpus),
    Authors = length(valid_authors),
    Verification_cases = nrow(valid_probs),
    `avg(K)` = avg_K,
    `avg(Q)` = avg_Q
  )
}

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
      
      # Skip if an author from this problem has been removed by duplicate cleaning
      if (ndoc(Q) == 0 || ndoc(K) == 0) {
        return(NULL)
      }
      
      result <- ngram_tracing(Q, K,
                              tokens = 'word',
                              remove_punct = FALSE,
                              remove_symbols = TRUE,
                              remove_numbers = TRUE,
                              lowercase = FALSE,
                              n = n,
                              coefficient = 'simpson',
                              features = TRUE,
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

# Takes 50 samples of 1000 tokens from corpus
formality_sample <- function(corpus, sample_length, num_of_samples) {
  
  samples <- corpus_group(corpus, groups = author) |>
    chunk_texts(size = sample_length) |>
    corpus_sample(size = num_of_samples)
  
  return(samples)
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

extract_unique_overlaps <- function(corpus, problems, corp_name, n_sizes = 1:6) {
  
  # Filter to current corpus
  corp_problems <- problems %>% 
    dplyr::filter(corpus == corp_name)
  
  if (nrow(corp_problems) == 0) {
    stop(paste("No verification problems found in the dataset for:", corp_name))
  }
  
  # Get authors in problems
  active_q_authors <- unique(corp_problems$unknown_author)
  active_k_authors <- unique(corp_problems$known_author)
  
  doc_names <- quanteda::docnames(corpus)
  doc_authors <- quanteda::docvars(corpus, "author")
  
  # Get documents in these problems
  active_q_docs <- doc_names[grepl("^unknown", doc_names) & doc_authors %in% active_q_authors]
  active_k_docs <- doc_names[grepl("^known", doc_names) & doc_authors %in% active_k_authors]
  active_doc_set <- c(active_q_docs, active_k_docs)
  
  sub_corpus <- corpus[active_doc_set]
  
  # Re-grab names and metadata from the subsetted corpus
  sub_doc_names <- quanteda::docnames(sub_corpus)
  sub_doc_authors <- quanteda::docvars(sub_corpus, "author")
  
  all_ngram_results <- list()
  
  for (n_val in n_sizes) {
    cat(sprintf("\n--- Extracting Unique %d-grams for %s ---\n", n_val, corp_name))
    
    toks <- quanteda::tokens(sub_corpus, 
                             what = "word",
                             remove_punct = FALSE,
                             remove_symbols = TRUE,
                             remove_numbers = TRUE) %>% 
      quanteda::tokens_ngrams(n = n_val)
    
    # Build boolean DFM for fast presence/absence checks
    dfm_sub <- quanteda::dfm(toks) %>% 
      quanteda::dfm_weight(scheme = "boolean")
    
    # Doc frequencies
    total_df <- quanteda::docfreq(dfm_sub)
    feat_names <- quanteda::featnames(dfm_sub)
    
    problem_results <- pbapply::pblapply(seq_len(nrow(corp_problems)), function(i) {
      prob <- corp_problems[i, ]
      q_doc <- sub_doc_names[grepl("^unknown", sub_doc_names) & sub_doc_authors == prob$unknown_author]
      k_docs <- sub_doc_names[grepl("^known", sub_doc_names) & sub_doc_authors == prob$known_author]
      
      # Fallback for missing files
      if (length(q_doc) == 0 || length(k_docs) == 0) {
        return(dplyr::tibble(
          problem_index = i,
          n = n_val,
          unknown_author = prob$unknown_author,
          known_author = prob$known_author,
          unique_overlap = NA_character_
        ))
      }
      
      q_doc <- q_doc[1] # Guarantee Q is a single text
      qk_dfm <- dfm_sub[c(q_doc, k_docs), ]
      qk_df <- colSums(qk_dfm)
      
      # Check feature presence in Q (row 1) and K (rows 2 to end)
      q_has <- colSums(qk_dfm[1, , drop = FALSE]) > 0
      k_has <- colSums(qk_dfm[2:nrow(qk_dfm), , drop = FALSE]) > 0
      
      # Candidate features must be present in both Q and K
      candidates <- q_has & k_has
      
      if (!any(candidates)) {
        return(dplyr::tibble(
          problem_index = i,
          n = n_val,
          unknown_author = prob$unknown_author,
          known_author = prob$known_author,
          unique_overlap = ""
        ))
      }
      
      is_unique <- candidates & (total_df == qk_df)
      
      true_unique_feats <- feat_names[is_unique]
      
      collapsed_features <- if (length(true_unique_feats) > 0) {
        paste(true_unique_feats, collapse = "|")
      } else {
        ""
      }
      
      dplyr::tibble(
        problem_index = i,
        n = n_val,
        unknown_author = prob$unknown_author,
        known_author = prob$known_author,
        unique_overlap = collapsed_features
      )
    })
    
    all_ngram_results[[as.character(n_val)]] <- problem_results
  }
  
  dplyr::bind_rows(all_ngram_results)
}

summarise_overlaps <- function(overlap_list, pair_type = "different") {
  if (!pair_type %in% c("same", "different")) {
    stop("pair_type must be either 'same' or 'different'")
  }
  
  # Combine corpora
  combined <- dplyr::bind_rows(overlap_list, .id = "Corpus")
  
  if (pair_type == "same") {
    filtered_data <- combined %>% dplyr::filter(unknown_author == known_author)
  } else {
    filtered_data <- combined %>% dplyr::filter(unknown_author != known_author)
  }
  
  # Calculate overlap
  summary_table <- filtered_data %>%
    dplyr::mutate(
      num_shared_ngrams = dplyr::if_else(
        unique_overlap == "" | is.na(unique_overlap), 
        0, 
        stringr::str_count(unique_overlap, "\\|") + 1
      )
    ) %>%
    # Group by Corpus and n-gram length
    dplyr::group_by(Corpus, n) %>%
    dplyr::summarise(
      # Count of pairs that share at least 1 unique n-gram
      pairs_sharing = sum(num_shared_ngrams > 0),
      # Total count of unique n-grams shared across all these pairs
      total_ngrams_shared = sum(num_shared_ngrams),
      .groups = "drop"
    )
  
  return(summary_table)
}

sample_raw_overlaps <- function(overlap_list, seed = 42) {
  set.seed(seed)
  
  target_ns <- c(1, 3, 6)
  corpus_order <- c("ACL", "Stack", "Amazon", "Enron", "Blog", "PJ")
  
  # Combine lists and divide lists of ngrams into singles
  raw_tokens <- dplyr::bind_rows(overlap_list, .id = "Corpus") %>%
    dplyr::filter(n %in% target_ns) %>%
    dplyr::filter(unique_overlap != "" & !is.na(unique_overlap)) %>%
    tidyr::separate_longer_delim(unique_overlap, delim = "|") %>%
    # Prevent duplicate identical strings from occupying the same cell sample
    dplyr::distinct(Corpus, n, unique_overlap)
  
  sampled_tokens <- raw_tokens %>%
    dplyr::group_by(Corpus, n) %>%
    dplyr::slice_sample(n = 5, replace = FALSE) %>%
    dplyr::ungroup()
  
  formatted_tokens <- sampled_tokens %>%
    dplyr::group_by(Corpus, n) %>%
    dplyr::summarise(
      examples = paste(unique_overlap, collapse = "\n"),
      .groups = "drop"
    )
  
  all_combos <- expand.grid(
    Corpus = corpus_order,
    n = target_ns,
    stringsAsFactors = FALSE
  )
  
  final_table <- all_combos %>%
    dplyr::left_join(formatted_tokens, by = c("Corpus", "n")) %>%
    dplyr::mutate(examples = tidyr::replace_na(examples, "[No matches]")) %>%
    tidyr::pivot_wider(names_from = Corpus, values_from = examples) %>%
    dplyr::arrange(n) %>%
    dplyr::mutate(n = dplyr::case_when(
      n == 1 ~ "Unigrams (1-grams)",
      n == 3 ~ "Trigrams (3-grams)",
      n == 6 ~ "Six-grams (6-grams)"
    ))
  
  final_df <- as.data.frame(final_table)
  rownames(final_df) <- final_df$n
  final_df$n <- NULL
  final_df <- final_df[, corpus_order]
  
  return(final_df)
}