library(quanteda)
library(dplyr)
library(stringr)

find_duplicates <- function(corpus) {
  df <- tibble::tibble(
    doc_id = quanteda::docnames(corpus),
    author = quanteda::docvars(corpus, "author"),
    text = as.character(corpus)
  )
  
  # Normalise whitespace
  df <- df %>%
    mutate(
      normalized_text = stringr::str_squish(text),
      char_count = nchar(normalized_text)
    )
  
  duplicates <- df %>%
    group_by(normalized_text) %>%
    filter(n() > 1) %>%
    ungroup()
  
  if (nrow(duplicates) == 0) {
    message("No duplicates found!")
    return(NULL)
  }
  
  # Compile to summary
  duplicate_summary <- duplicates %>%
    group_by(normalized_text) %>%
    summarise(
      copies_found = n(),
      file_names = paste(doc_id, collapse = " | "),
      authors = paste(unique(author), collapse = " & "),
      character_length = first(char_count),
      # Extract a preview snippet
      text_snippet = substr(first(text), 1, 150),
      .groups = "drop"
    ) %>%
    # Sort by the largest duplicate groups first
    arrange(desc(copies_found)) %>%
    select(-normalized_text)
  
  return(duplicate_summary)
}

get_duped_authors <- function(dupes_df) {
  
  # Extract author names
  authors_to_drop <- dupes_df$authors %>%
    stringr::str_split(" & ") %>%
    unlist() %>% # Flatten to single vector of names
    stringr::str_trim() %>% # Trim whitespace just in case 
    unique()
  
  return(authors_to_drop)
}

### Clean up the corpora (only ACL, Blogs and PJ are duplicated)
acl_drops <- get_duped_authors(find_duplicates(acl_corp))
acl_corp <- acl_corp %>%
  corpus_subset(!(author %in% acl_drops))

blog_drops <- get_duped_authors(find_duplicates(blog_corp))
blog_corp <- blog_corp %>%
  corpus_subset(!(author %in% blog_drops))

perv_drops <- get_duped_authors(find_duplicates(perv_corp))
perv_corp <- perv_corp %>% 
  corpus_subset(!(author %in% perv_drops))