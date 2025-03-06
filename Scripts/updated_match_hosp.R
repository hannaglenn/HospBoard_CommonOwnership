## Match AHA IDs to tax identifiers

library(dplyr)
library(stringr)
library(readr)
library(cdlTools)

# Read in EIN data from XMLs
all_ein_data <- readRDS("CreatedData/all_ein_data_scheduleH.rds")

# Get rid of EINs that don't appear from at least 2018-2021
all_ein_data <- all_ein_data %>%
  mutate(present = ifelse(TaxYr>=2018 & TaxYr<=2021, 1, 0)) %>%
  group_by(Filer.EIN) %>%
  filter(sum(present)>3) %>%
  ungroup()

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
         Filer.USAddress.ZIPCd, Filer.USAddress.CityNm)

# Work through any missing business names to determine most accurate name
ein_data <- ein_data %>%
  mutate(BusinessName = ifelse(is.na(BusinessName2), BusinessName1, NA))
ein_data <- ein_data %>%
  mutate(BusinessName = ifelse(grepl("^DBA|^D/B/A", BusinessName2), BusinessName2, BusinessName)) %>%
  mutate(BusinessName = str_remove(BusinessName, "^DBA|^D/B/A"))
ein_data <- ein_data %>%
  mutate(BusinessName = ifelse(is.na(BusinessName) & str_detect(BusinessName1, "DBA$"), BusinessName2, BusinessName))
ein_data <- ein_data %>%
  mutate(BusinessName = ifelse(is.na(BusinessName) & (BusinessName2=="GROUP RETURN"|BusinessName2=="LETTER RULING"), BusinessName1, BusinessName))
ein_data <- ein_data %>%
  mutate(BusinessName = ifelse(is.na(BusinessName) & (str_detect(BusinessName1, "AND$|OF$|\\&$|-$")|str_detect(BusinessName2, "^AND|^OF|^\\&|^-")), paste(BusinessName1, BusinessName2, sep=" "), BusinessName))
ein_data <- ein_data %>%
  mutate(BusinessName = ifelse(is.na(BusinessName) & !str_detect(BusinessName2, "\\s"), paste(BusinessName1, BusinessName2, sep=" "), BusinessName))
ein_data <- ein_data %>%
  mutate(BusinessName = ifelse(is.na(BusinessName) & str_detect(BusinessName1, "EASTERN MAINE HEALTHCARE SYSTEMS"), BusinessName2, BusinessName))
ein_data <- ein_data %>%
  mutate(BusinessName = ifelse(is.na(BusinessName) & str_detect(BusinessName2, "^C/O"), BusinessName2, BusinessName)) %>%
  mutate(BusinessName = str_remove(BusinessName, "^C/O"))
ein_data <- ein_data %>%
  mutate(BusinessName = ifelse(is.na(BusinessName) & str_detect(BusinessName2, "^F/K/A|^FKA|^\\(FKA|^\\(F/K/A|^FORMERLY KNOWN AS"), BusinessName1, BusinessName))
ein_data <- ein_data %>%
  mutate(BusinessName = ifelse(is.na(BusinessName) & BusinessName1==BusinessName2, BusinessName1, BusinessName))
ein_data <- ein_data %>%
  mutate(BusinessName = ifelse(is.na(BusinessName) & str_detect(BusinessName1, "OZARK HEALTH"), BusinessName1, BusinessName))
ein_data <- ein_data %>%
  mutate(BusinessName = ifelse(is.na(BusinessName) & BusinessName2=="ADMINISTERING WILLS EYE HOSPITAL", "WILLS EYE HOSPITAL", BusinessName))
ein_data <- ein_data %>%
  mutate(BusinessName = ifelse(is.na(BusinessName) & str_detect(BusinessName1, "WINDBER HOSPITAL INC"), BusinessName2, BusinessName))
ein_data <- ein_data %>%
  mutate(BusinessName = ifelse(is.na(BusinessName) & str_detect(BusinessName2, "BELLIN HEALTH OCONTO HOSPITAL"), BusinessName2, BusinessName))
ein_data <- ein_data %>%
  mutate(BusinessName = ifelse(is.na(BusinessName) & str_detect(BusinessName1, "POMERENE HOSPITAL"), BusinessName1, BusinessName))
ein_data <- ein_data %>%
  mutate(BusinessName = ifelse(is.na(BusinessName) & str_detect(BusinessName2, "DBA\\s"), str_extract(BusinessName2, "DBA.+"), BusinessName)) %>%
  mutate(BusinessName = str_remove(BusinessName, "DBA\\s"))
ein_data <- ein_data %>%
  mutate(BusinessName = ifelse(is.na(BusinessName) & str_detect(BusinessName2, "PERRY COUNTY MEMORIAL HOSPITAL"), BusinessName2, BusinessName))
ein_data <- ein_data %>%
  mutate(BusinessName = ifelse(is.na(BusinessName) & str_detect(BusinessName2, "NORTHERN LIGHT INLAND HOSPITAL"), BusinessName2, BusinessName))
ein_data <- ein_data %>%
  mutate(BusinessName = ifelse(is.na(BusinessName) & str_detect(BusinessName2, "NORTHERN LIGHT ACADIA HOSPITAL"), BusinessName2, BusinessName))
ein_data <- ein_data %>%
  mutate(BusinessName = ifelse(is.na(BusinessName) & str_detect(BusinessName2, "MCKENZIE HEALTH SYSTEM"), BusinessName2, BusinessName))
ein_data <- ein_data %>%
  mutate(BusinessName = ifelse(is.na(BusinessName) & str_detect(BusinessName2, "NORTHERN LIGHT"), BusinessName2, BusinessName))
ein_data <- ein_data %>%
  mutate(BusinessName = ifelse(is.na(BusinessName) & str_detect(BusinessName1, "ALEGENT HEALTH-COMMUNITY MEMORIAL"), BusinessName1, BusinessName))
ein_data <- ein_data %>%
  mutate(BusinessName = ifelse(is.na(BusinessName) & str_detect(BusinessName2, "DISTRICT HEALTH FACILITIES CORP"), BusinessName1, BusinessName))
ein_data <- ein_data %>%
  mutate(BusinessName = ifelse(is.na(BusinessName) & str_detect(BusinessName1, "\\(FKA|FKA"), str_remove(BusinessName1, "FKA.+"), BusinessName))

# For any still missing observations, paste the two business names together
ein_data <- ein_data %>%
  mutate(BusinessName = ifelse(is.na(BusinessName), paste(BusinessName1, BusinessName2, sep=" "), BusinessName))

# Keep relevant variables
ein_data <- ein_data %>%
  select(TaxYr, Filer.EIN, BusinessName, Filer.USAddress.AddressLine1Txt, Filer.USAddress.StateAbbreviationCd, Filer.USAddress.ZIPCd,
         Filer.USAddress.CityNm) %>%
  mutate(BusinessName = str_trim(BusinessName))


# Read in the AHA data
AHA <- read_csv("RawData/AHAdata_20052023.csv")

# Keep relevant variables
AHA <- AHA %>%
  select(YEAR, ID, MNAME, MLOCADDR, FSTCD, MLOCZIP, MLOCCITY) 

# Standardize the variables in both data sets
# Convert AHA hospital name to standard characters
AHA <- AHA %>%
  mutate(MNAME = iconv(MNAME, "latin1", "ASCII", sub=""))
# Fix common abbreviations in the AHA data
AHA <- AHA %>%
  mutate(MNAME = str_replace(MNAME, "Med\\s", "MEDICAL"),
         MNAME = str_replace(MNAME, "Ctr\\s", "CENTER"))
AHA <- AHA %>%
  mutate(MNAME = toupper(MNAME)) %>%
  mutate(MNAME = str_remove_all(MNAME, "-|\\s\\.")) %>%
  mutate(MLOCADDR = toupper(MLOCADDR),
         MLOCCITY = toupper(MLOCCITY))

# Convert zip codes into first five digits
AHA <- AHA %>%
  mutate(MLOCZIP = str_sub(MLOCZIP, 1, 5)) %>%
  mutate(MLOCZIP = ifelse(nchar(MLOCZIP)==4, paste("0", MLOCZIP, sep=""), MLOCZIP))


ein_data <- ein_data %>%
  mutate(BusinessName = str_remove_all(BusinessName, "-|\\s\\."),
         Filer.USAddress.AddressLine1Txt = toupper(Filer.USAddress.AddressLine1Txt),
         Filer.USAddress.CityNm = toupper(Filer.USAddress.CityNm))

ein_data <- ein_data %>%
  mutate(Filer.USAddress.ZIPCd = str_sub(Filer.USAddress.ZIPCd, 1, 5))

# Convert tax state abbreviation to FIPS code
ein_data <- ein_data %>%
  mutate(Filer.Stfips = fips(Filer.USAddress.StateAbbreviationCd))

# Join the data sets using stringdist_inner_join
joined_data <- ein_data %>%
  fuzzyjoin::stringdist_left_join(AHA, 
                                   by=c("BusinessName"="MNAME"), 
                                   method="jw", 
                                   max_dist=0.1,
                                   distance_col = "jw_dist")


# remove the YEAR element
joined_data <- joined_data %>%
  select(-YEAR) %>%
  distinct()

# Only keep matches in the same state
joined_data <- joined_data %>%
  filter(Filer.Stfips==FSTCD | is.na(FSTCD)) %>%
  distinct()

joined_data <- joined_data %>%
  distinct(TaxYr, Filer.EIN, ID, .keep_all = TRUE)

# For those that have multiple matches, create an index of things that indicate a good match
joined_data <- joined_data %>%
  mutate(index = 0) %>%
  mutate(index = ifelse(Filer.USAddress.CityNm==MLOCCITY, index+1, index)) %>%
  mutate(index = ifelse(Filer.USAddress.ZIPCd==MLOCZIP, index+1, index)) %>%
  mutate(index = ifelse(Filer.USAddress.AddressLine1Txt==MLOCADDR, index+5, index)) %>%
  mutate(index = ifelse(str_detect(BusinessName, MLOCCITY), index+1, index)) %>%
  group_by(Filer.EIN, TaxYr) %>%
  mutate(index = ifelse(jw_dist==min(jw_dist), index+1, index)) %>%
  ungroup()

# Within a Filer.EIN, year: pick the match with the highest index
joined_data <- joined_data %>%
  group_by(Filer.EIN, TaxYr) %>%
  filter(index==max(index)) %>%
  ungroup()
  

# observe any Filer.EINs that have multiple matches in the same year
observe <- joined_data %>%
  group_by(Filer.EIN, TaxYr) %>%
  filter(n()>1)

# if there are multiple matches in the same year and one of them has a matching address, keep that one
joined_data <- joined_data %>%
  group_by(Filer.EIN, TaxYr) %>%
  mutate(address_match = ifelse(n()>1 & Filer.USAddress.AddressLine1Txt==MLOCADDR, 1, NA)) %>%
  tidyr::fill(address_match, .direction="updown") %>%
  filter(!(address_match==1 & Filer.USAddress.AddressLine1Txt!=MLOCADDR)) %>%
  select(-address_match)

# drop any Filer.EINs that have duplicates in the same year
joined_data <- joined_data %>%
  group_by(Filer.EIN, TaxYr) %>%
  filter(n()==1) %>%
  ungroup()

# Merge the joined data with the original EIN data
ein_data <- ein_data %>%
  left_join(joined_data)
ein_data <- ein_data %>%
  distinct()

# Are any EINs associated with multiple AHAs?
observe <- ein_data %>%
  filter(!is.na(ID)) %>%
  group_by(Filer.EIN, ID, TaxYr) %>%
  filter(n()>1)
  # None

# How many EINs have a match?
ein_data %>%
  filter(!is.na(ID)) %>%
  distinct(Filer.EIN) %>%
  nrow()
  # 609 matches  

# Fill in missing ID values
ein_data <- ein_data %>%
  group_by(Filer.EIN) %>%
  tidyr::fill(ID, .direction="updown") %>%
  ungroup()

# look at observations that did not have a match
no_match <- ein_data %>%
  filter(is.na(ID))

