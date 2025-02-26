identify_common_members <- function(data) {
  # Collect all EINs for each name within the same year
  ein_mapping <- data %>%
    group_by(name_cleaned, TaxYr) %>%
    summarise(all_eins = list(unique(Filer.EIN)), .groups = "drop")
  
  # Merge back and correctly compute other EINs within the same year
  df_with_others <- data %>%
    left_join(ein_mapping, by = c("name_cleaned", "TaxYr")) %>%
    rowwise() %>%
    mutate(other_eins = paste(setdiff(unlist(all_eins), Filer.EIN), collapse = ", ")) %>%
    ungroup() %>%
    select(-all_eins)  # Drop intermediate column
  
  return(df_with_others)
}

