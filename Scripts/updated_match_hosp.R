## Match AHA IDs to tax identifiers

# Read in EIN data from XMLs
all_ein_data <- readRDS("CreatedData/all_ein_data_scheduleH.rds")

# Combine the variables that end with "BusinessNameLine1Txt"
all_ein_data <- all_ein_data %>%
  mutate(BusinessName1 = Filer.BusinessName.BusinessNameLine1Txt) %>%
  mutate(BusinessName1 = ifelse(is.na(BusinessName1), Filer.BusinessNameLine1Txt, BusinessName1))

# Combine Line2 of business name
all_ein_data <- all_ein_data %>%
  mutate(BusinessName2 = Filer.BusinessName.BusinessNameLine2Txt)

# Change businessname to all caps
all_ein_data <- all_ein_data %>%
  mutate(BusinessName1 = toupper(BusinessName1),
         BusinessName2 = toupper(BusinessName2))

# Select and rename relevant variables
# keep all variables with USAddress in the variable name
ein_data <- all_ein_data %>%
  select(TaxYr, Filer.EIN, BusinessName1, BusinessName2, Filer.USAddress.AddressLine1Txt, Filer.USAddress.StateAbbreviationCd,
         Filer.USAddress.ZIPCd)

# If BusinessName2 starts with DBA (doing business as) then make that the official business name
ein_data <- ein_data %>%
  mutate(BusinessName = ifelse(is.na(BusinessName2), BusinessName1, NA))
ein_data <- ein_data %>%
  mutate(BusinessName = ifelse(grepl("^DBA|^D/B/A", BusinessName2), BusinessName2, BusinessName)) %>%
  mutate(BusinessName = str_remove(BusinessName, "^DBA|^D/B/A"))
ein_data <- ein_data %>%
  mutate(BusinessName = ifelse(is.na(BusinessName) & str_detect(BusinessName1, "DBA$"), BusinessName2, BusinessName))
ein_data <- ein_data %>%
  mutate(BusinessName = ifelse(is.na(BusinessName) & BusinessName2=="GROUP RETURN", BusinessName1, BusinessName))
ein_data <- ein_data %>%
  mutate(BusinessName = ifelse(is.na(BusinessName) & (str_detect(BusinessName1, "AND$|OF$|\\&$|-$")|str_detect(BusinessName2, "^AND|^OF|^\\&|^-")), paste(BusinessName1, BusinessName2, sep=" "), BusinessName))
ein_data <- ein_data %>%
  mutate(BusinessName = ifelse(is.na(BusinessName) & !str_detect(BusinessName2, "\\s"), paste(BusinessName1, BusinessName2, sep=" "), BusinessName))

# look at missing business names
observe <- ein_data %>%
  filter(is.na(BusinessName))



