# Function to standardize names within firms
standardize_names <- function(data, max_dist) {
  data <- data %>%
    dplyr::group_by(Filer.EIN) %>% 
    mutate(name_id = row_number())  # Assign a unique ID for each row
  
  # Self-join on names within the same firm based on string distance
  matches <- stringdist_inner_join(
    data, data, by = "name_cleaned", 
    max_dist = max_dist, 
    method = "osa" # Optimal string alignment distance
  ) %>%
    filter(Filer.EIN.x == Filer.EIN.y) %>%  # Only consider matches within the same firm
    rename(Filer.EIN = Filer.EIN.x) %>%
    filter(name_id.x < name_id.y) %>%  # Avoid duplicate pairs
    dplyr::group_by(Filer.EIN, name_cleaned.x, name_cleaned.y) %>%
    summarise(count_x = n(), count_y = n(), .groups = "drop") %>%
    arrange(Filer.EIN, desc(count_x))  # Prioritize the most frequent name
  
  # Create a mapping of misspelled names to the most frequent version
  name_mapping <- matches %>%
    dplyr::group_by(Filer.EIN, name_cleaned.y) %>%
    summarise(correct_name = first(name_cleaned.x), .groups = "drop")
  
  # Replace incorrect names with the correct ones
  df_corrected <- data %>%
    left_join(name_mapping, by = c("Filer.EIN", "name_cleaned" = "name_cleaned.y")) %>%
    mutate(name_cleaned = coalesce(correct_name, name_cleaned)) %>%
    select(-correct_name, -name_id)
  
  return(df_corrected)
}

standardize_names_state <- function(data, max_dist) {
  data <- data %>%
    dplyr::group_by(state) %>% 
    mutate(name_id = row_number())  # Assign a unique ID for each row
  
  # Self-join on names within the same state based on string distance
  matches <- stringdist_inner_join(
    data, data, by = "name_cleaned", 
    max_dist = max_dist, 
    method = "osa" # Optimal string alignment distance
  ) %>%
    filter(state.x == state.y) %>%  # Only consider matches within the same state
    rename(state = state.x) %>%
    filter(name_id.x < name_id.y) %>%  # Avoid duplicate pairs
    dplyr::group_by(state, name_cleaned.x, name_cleaned.y) %>%
    summarise(count_x = n(), count_y = n(), .groups = "drop") %>%
    arrange(state, desc(count_x))  # Prioritize the most frequent name
  
  # Create a mapping of misspelled names to the most frequent version
  name_mapping <- matches %>%
    dplyr::group_by(state, name_cleaned.y) %>%
    summarise(correct_name = first(name_cleaned.x), .groups = "drop")
  
  # Replace incorrect names with the correct ones
  df_corrected <- data %>%
    left_join(name_mapping, by = c("state", "name_cleaned" = "name_cleaned.y")) %>%
    mutate(name_cleaned = coalesce(correct_name, name_cleaned)) %>%
    select(-correct_name, -name_id)
  
  return(df_corrected)
}

standardize_names_hrr <- function(data, max_dist) {
  data <- data %>%
    dplyr::group_by(hrrcode) %>% 
    mutate(name_id = row_number())  # Assign a unique ID for each row
  
  # Self-join on names within the same state based on string distance
  matches <- stringdist_inner_join(
    data, data, by = "name_cleaned", 
    max_dist = max_dist, 
    method = "osa" # Optimal string alignment distance
  ) %>%
    filter(hrrcode.x == hrrcode.y) %>%  # Only consider matches within the same state
    rename(hrrcode = hrrcode.x) %>%
    filter(name_id.x < name_id.y) %>%  # Avoid duplicate pairs
    dplyr::group_by(hrrcode, name_cleaned.x, name_cleaned.y) %>%
    summarise(count_x = n(), count_y = n(), .groups = "drop") %>%
    arrange(hrrcode, desc(count_x))  # Prioritize the most frequent name
  
  # Create a mapping of misspelled names to the most frequent version
  name_mapping <- matches %>%
    dplyr::group_by(hrrcode, name_cleaned.y) %>%
    summarise(correct_name = first(name_cleaned.x), .groups = "drop")
  
  # Replace incorrect names with the correct ones
  df_corrected <- data %>%
    left_join(name_mapping, by = c("hrrcode", "name_cleaned" = "name_cleaned.y")) %>%
    mutate(name_cleaned = coalesce(correct_name, name_cleaned)) %>%
    select(-correct_name, -name_id)
  
  return(df_corrected)
}