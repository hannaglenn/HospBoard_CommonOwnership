library(readr)

created_data_path <- "CreatedData/"

# Read in people and EIN data from XMLs
all_ein_data <- readRDS(paste0(created_data_path, "all_ein_data_scheduleH.rds"))

# Combine the variables that end with "BusinessNameLine1Txt"
all_ein_data <- all_ein_data %>%
  mutate(BusinessName = Filer.BusinessName.BusinessNameLine1Txt) %>%
  mutate(BusinessName = ifelse(is.na(BusinessName), Filer.BusinessNameLine1Txt, BusinessName))

# Change businessname to all caps
all_ein_data <- all_ein_data %>%
  mutate(BusinessName = toupper(BusinessName))

head(all_ein_data$BusinessName)

# observe NA values for business name
all_ein_data %>%
  select(Filer.EIN, TaxYr, BusinessName) %>%
  filter(is.na(BusinessName))
  # Every observation has a name

# Only keep EINs that are in at least years 2018-2021
all_ein_data <- all_ein_data %>%
  mutate(count=ifelse(TaxYr>=2018 & TaxYr<=2021,1,0)) %>%
  group_by(Filer.EIN) %>%
  mutate(sum=sum(count)) %>%
  ungroup() %>%
  filter(sum>3) 

# How many distinct EINs are there?
length(unique(all_ein_data$Filer.EIN))
  # 2096

# Combine Line2 of business name
all_ein_data <- all_ein_data %>%
  mutate(BusinessName2 = Filer.BusinessName.BusinessNameLine2Txt) %>%
  mutate(BusinessName2 = ifelse(is.na(BusinessName), Filer.BusinessNameLine2Txt, BusinessName))


# MATCH TO AHA HOSPITALS #######################

# Read in AHA data that includes hospital name and AHAID
AHA <- read_csv("RawData/AHAdata_20052023.csv") %>%
  filter(YEAR>=2016 & YEAR<=2023)

# Get rid of symbols in hosp name
AHA$MNAME <- iconv(AHA$MNAME,from="ISO-8859-1")

# Limit to nonprofit hospitals
AHA <- AHA %>%
  filter(FSTCD<=56 & nchar(MLOCZIP)>4 & (CNTRL==23 | (CNTRL>=12 & CNTRL<=16))) %>%
  dplyr::rename(name.AHA=MNAME) %>%
  mutate(state=cdlTools::fips(FSTCD, to='Abbreviation')) %>%
  mutate(name.AHA=tolower(name.AHA)) %>%
  mutate(SYSNAME=tolower(SYSNAME))

AHA <- tidyr::separate(AHA, MLOCZIP, into=c('zip','extra'), sep="-", extra='merge', remove=FALSE) %>%
  select(-extra) %>%
  mutate(zip=as.double(zip))

# get rid of any AHAID that wasn't present from 2018-2021 at least
AHA <- AHA %>%
  mutate(count=ifelse(YEAR>=2018 & YEAR<=2021,1,0)) %>%
  group_by(ID) %>%
  mutate(sum=sum(count)) %>%
  ungroup() %>%
  filter(sum>3) %>%
  filter(!str_detect(name.AHA, "cancer"))


# look at hospitals that have changes in ownership or system name
AHA <- AHA %>%
  group_by(ID) %>%
  mutate(lag_sys=lag(SYSID)) %>%
  ungroup() %>%
  mutate(change=ifelse(lag_sys!=SYSID,1,NA)) %>%
  group_by(ID) %>%
  tidyr::fill(change, .direction="downup") %>%
  ungroup() 

sys_changes <- AHA %>%
  filter(change==1)
no_sys_changes <- AHA %>%
  filter(is.na(change))

num_hosp <- no_sys_changes %>%
  distinct(ID)
  #3341

observe <- all_ein_data %>%
  select(TaxYr, Filer.EIN, BusinessName)

# Do any EINs change their tax name over time?
observe <- all_ein_data %>%
  distinct(Filer.EIN, BusinessName) %>%
  mutate(count=1) %>%
  group_by(Filer.EIN) %>%
  mutate(sum=sum(count)) %>%
  ungroup() %>%
  filter(sum>1)
  # 0

# create data where each AHAID has one row that collects all names, zip codes, and system names (wide format)
data_AHA_name <- no_sys_changes %>%
  distinct(ID, name.AHA, state)
data_AHA_name <- data_AHA_name %>%
  group_by(ID) %>%
  mutate(num=row_number()) %>%
  ungroup()
data_AHA_name <- tidyr::pivot_wider(data=data_AHA_name,
                             names_from=num, 
                             values_from=c(name.AHA),
                             id_cols=c(ID, state)) %>%
  rename(name.AHA_1=`1`, name.AHA_2=`2`, name.AHA_3=`3`) 

data_AHA_zip <- no_sys_changes %>%
  distinct(ID, zip, state)
data_AHA_zip <- data_AHA_zip %>%
  group_by(ID) %>%
  mutate(num=row_number()) %>%
  ungroup()
data_AHA_zip <- tidyr::pivot_wider(data=data_AHA_zip,
                            names_from=num, 
                            values_from=c(zip),
                            id_cols=c(ID, state)) %>%
  rename(zip_1=`1`, zip_2=`2`) 

data_AHA_sys <- no_sys_changes %>%
  distinct(ID, SYSNAME, state)
data_AHA_sys <- data_AHA_sys %>%
  group_by(ID) %>%
  filter(!is.na(SYSNAME)) %>%
  mutate(num=row_number()) %>%
  ungroup()
data_AHA_sys <- tidyr::pivot_wider(data=data_AHA_sys,
                            names_from=num, 
                            values_from=c(SYSNAME),
                            id_cols=c(ID, state)) %>%
  rename(sys_1=`1`, sys_2=`2`) 

data_AHA <- data_AHA_name %>%
  left_join(data_AHA_zip, by=c("ID","state")) %>%
  left_join(data_AHA_sys, by=c("ID","state"))

rm(data_AHA_name, data_AHA_sys, data_AHA_zip, dups, num_hosp, observe, sys_changes, AHA, no_sys_changes)

# Clean Tax Data ####

tax <- all_ein_data %>%
  rename(name.tax=BusinessName) %>%
  mutate(name.tax=tolower(name.tax)) %>%
  rename(name.tax2 = BusinessName2) %>%
  mutate(name.tax2=tolower(name.tax2)) 

# get rid of foundations and auxiliaries in tax data
tax <- tax %>%
  filter(!str_detect(name.tax,"foundation|auxiliary|auxilliary|cancer|hospice|mental|geriatric|cardiology|associates|nurse|eye|veterinary|aux|dental")) %>%
  filter(!str_detect(name.tax2,"foundation|auxiliary|auxilliary|cancer|hospice|mental|geriatric|cardiology|associates|nurse|eye|veterinary|aux|dental") | is.na(name.tax2)) %>%
  mutate(clinic=ifelse(str_detect(name.tax,"clinic"),1,0),
         hospital=ifelse(str_detect(name.tax,"hospital"),1,0)) %>%
  filter(!(clinic==1 & hospital==0)) %>%
  select(-clinic, -hospital)
# got rid of around 400 observations

# clean up zip code variable by taking only first 5 characters
tax <- tax %>%
  mutate(zipcode=ifelse(nchar(Filer.USAddress.ZIPCd)>5, substr(Filer.USAddress.ZIPCd,1,5), Filer.USAddress.ZIPCd)) 

# look at NAs ofstate and zipcode
observe <- tax %>%
  filter(is.na(Filer.USAddress.StateAbbreviationCd) | is.na(zipcode))
  # 28 observations
# get rid of those 28 observations
tax <- tax %>%
  filter(!is.na(Filer.USAddress.StateAbbreviationCd) & !is.na(zipcode))

# Create smaller tax data with no duplicates in name, state 
tax_nodups <- tax %>%
  dplyr::rename(state=Filer.USAddress.StateAbbreviationCd,
         ein = Filer.EIN) %>%
  distinct(ein, name.tax, state, name.tax2, zipcode) %>%
  mutate(count=1) %>%
  group_by(name.tax, name.tax2, state) %>%
  mutate(sum=sum(count)) %>%
  ungroup() %>%
  filter(sum==1) %>%
  select(-sum, -count)

# are there any duplicates when also incorporating zip code?
observe <- tax %>%
  dplyr::rename(state=Filer.USAddress.StateAbbreviationCd,
                ein = Filer.EIN) %>%
  distinct(ein, name.tax, state, name.tax2, zipcode) %>%
  mutate(count=1) %>%
  group_by(name.tax, name.tax2, state, zipcode) %>%
  mutate(sum=sum(count)) %>%
  ungroup() %>%
  filter(sum>1) %>%
  select(-sum, -count)



# Create a data set that will be used to store matches
match_data <- data_AHA %>%
  mutate(name.tax_hosp=NA,
         name.tax_sys=NA,
         ein_hosp=NA,
         ein_sys=NA,
         zip.tax_hosp=NA,
         zip.tax_sys=NA)

# Find any exact matches of hospital names ####
# get rid of common differences 
match_data <- match_data %>%
  mutate(name.AHA1_spaceless=name.AHA_1,
         name.AHA2_spaceless=name.AHA_2,
         name.AHA3_spaceless=name.AHA_3)
match_data$name.AHA1_spaceless=str_remove_all(match_data$name.AHA1_spaceless,"\\s|,|inc|\\.|'|&|-")
match_data$name.AHA2_spaceless=str_remove_all(match_data$name.AHA2_spaceless,"\\s|,|inc|\\.|'|&|-")
match_data$name.AHA3_spaceless=str_remove_all(match_data$name.AHA3_spaceless,"\\s|,|inc|\\.|'|&|-")
tax_nodups <- tax_nodups %>%
  mutate(name.tax1_spaceless=name.tax,
         name.tax2_spaceless=name.tax2)
tax_nodups$name.tax1_spaceless <- str_remove_all(tax_nodups$name.tax1_spaceless,"\\s|,|inc|\\.|&|incorporated|associates|corporation|association")
tax_nodups$name.tax2_spaceless <- str_remove_all(tax_nodups$name.tax2_spaceless,"\\s|,|inc|\\.|&|incorporated|associates|corporation|association")

# Finding exact matches in any of the names from AHA or tax (in the same state)
for (i in 1:dim(match_data)[1]){
  st <- match_data$state[[i]]
  
  list_tax <- tax_nodups %>%
    filter(state==st)
  list_tax1 <- paste(as.list(list_tax)[["name.tax1_spaceless"]], collapse="|")
  list_tax2 <- paste(as.list(list_tax)[["name.tax2_spaceless"]], collapse="|")
  list_tax <- paste0("|", list_tax1,"|", list_tax2, "|", collapse="|")
  
  match <- str_extract(list_tax, paste0("(?<=\\|)(",match_data$name.AHA1_spaceless[[i]],")(?=\\|)"))
  if (is.na(match) & !is.na(match_data$name.AHA2_spaceless[[i]])) {
    match <- str_extract(list_tax, paste0("(?<=\\|)(",match_data$name.AHA2_spaceless[[i]],")(?=\\|)"))
  }
  if (is.na(match) & !is.na(match_data$name.AHA3_spaceless[[i]])) {
    match <- str_extract(list_tax, paste0("(?<=\\|)(",match_data$name.AHA3_spaceless[[i]],")(?=\\|)"))
  }
  
  if (!is.na(match)) {
    match_info <- tax_nodups %>%
      filter((name.tax1_spaceless==match | name.tax2_spaceless==match) & state==st) %>%
      mutate(one=ifelse(name.tax1_spaceless==match,1,0))
    
    if (match_info$one[1]==1){
      match_data$name.tax_hosp[i] <- match_info$name.tax[1]
    }
    if (match_info$one[1]==0){
      match_data$name.tax_hosp[i] <- match_info$name.tax2[1]
    }
    
    match_data$ein_hosp[i] <- match_info$ein[1]
    match_data$zip.tax_hosp[i] <- match_info$zipcode[1]
  }
}

# How many hospitals aren't NA in ein_hosp?
observe <- match_data %>%
  filter(!is.na(ein_hosp))
  # 608 hospitals

# now lets get rid of common words and look for exact matches again
match_data$name.AHA1_spaceless=str_remove_all(match_data$name.AHA_1,"\\bincorporated\\b|signature\\b|healthcare\\b|\\bhealth center\\b|\\bregional\\b|\\bhospital\\b|\\bhealth systems\\b|\\bthe\\b|\\bhealth network\\b|\\bmedical center\\b|\\bhcsr\\b|\\band\\b|\\bctr\\b|,|inc|\\.|'|&|-|\\bhealth\\b")
match_data$name.AHA1_spaceless=str_remove_all(match_data$name.AHA1_spaceless,"\\s")
match_data$name.AHA2_spaceless=str_remove_all(match_data$name.AHA_2,"\\bincorporated\\b|signature\\b|healthcare\\b|\\bhealth center\\b|\\bregional\\b|\\bhospital\\b|\\bhealth systems\\b|\\bthe\\b|\\bhealth network\\b|\\bmedical center\\b|\\bhcsr\\b|\\band\\b|\\bctr\\b|,|inc|\\.|'|&|-|\\bhealth\\b")
match_data$name.AHA2_spaceless=str_remove_all(match_data$name.AHA2_spaceless,"\\s")
match_data$name.AHA3_spaceless=str_remove_all(match_data$name.AHA_3,"\\bincorporated\\b|signature\\b|healthcare\\b|\\bhealth center\\b|\\bregional\\b|\\bhospital\\b|\\bhealth systems\\b|\\bthe\\b|\\bhealth network\\b|\\bmedical center\\b|\\bhcsr\\b|\\band\\b|\\bctr\\b|,|inc|\\.|'|&|-|\\bhealth\\b")
match_data$name.AHA3_spaceless=str_remove_all(match_data$name.AHA3_spaceless,"\\s")

tax_nodups$name.tax1_spaceless=str_remove_all(tax_nodups$name.tax,"\\bincorporated\\b|signature\\b|healthcare\\b|\\bhealth center\\b|\\bassociation\\b|\\bcorporation\\b|\\bassociates\\b|\\bregional\\b|\\bhospital\\b|\\bhealth systems\\b|\\bthe\\b|\\bhealth network\\b|\\bmedical center\\b|\\bhcsr\\b|\\band\\b|\\bctr\\b|,|inc|\\.|'|&|-|\\bhealth\\b")
tax_nodups$name.tax1_spaceless=str_remove_all(tax_nodups$name.tax1_spaceless,"\\s")
tax_nodups$name.tax2_spaceless=str_remove_all(tax_nodups$name.tax2,"\\bincorporated\\b|signature\\b|healthcare\\b|\\bhealth center\\b|\\bassociation\\b|\\bcorporation\\b|\\bassociates\\b|\\bregional\\b|\\bhospital\\b|\\bhealth systems\\b|\\bthe\\b|\\bhealth network\\b|\\bmedical center\\b|\\bhcsr\\b|\\band\\b|\\bctr\\b|,|inc|\\.|'|&|-|\\bhealth\\b")
tax_nodups$name.tax2_spaceless=str_remove_all(tax_nodups$name.tax2_spaceless,"\\s")

# Finding exact matches in any of the names from AHA or tax (in the same state)
for (i in 1:dim(match_data)[1]){
  if (is.na(match_data$name.tax_hosp[[i]])){
    st <- match_data$state[[i]]
    
    list_tax <- tax_nodups %>%
      filter(state==st)
    list_tax1 <- paste(as.list(list_tax)[["name.tax1_spaceless"]], collapse="|")
    list_tax2 <- paste(as.list(list_tax)[["name.tax2_spaceless"]], collapse="|")
    list_tax <- paste0("|", list_tax1,"|", list_tax2, "|", collapse="|")
    
    match <- str_extract(list_tax, paste0("(?<=\\|)(",match_data$name.AHA1_spaceless[[i]],")(?=\\|)"))
    if (is.na(match) & !is.na(match_data$name.AHA2_spaceless[[i]])) {
      match <- str_extract(list_tax, paste0("(?<=\\|)(",match_data$name.AHA2_spaceless[[i]],")(?=\\|)"))
    }
    if (is.na(match) & !is.na(match_data$name.AHA3_spaceless[[i]])) {
      match <- str_extract(list_tax, paste0("(?<=\\|)(",match_data$name.AHA3_spaceless[[i]],")(?=\\|)"))
    }
    
    if (!is.na(match)) {
      match_info <- tax_nodups %>%
        filter((name.tax1_spaceless==match | name.tax2_spaceless==match) & state==st) %>%
        mutate(one=ifelse(name.tax1_spaceless==match,1,0))
      
      if (match_info$one[1]==1){
        match_data$name.tax_hosp[i] <- match_info$name.tax[1]
      }
      if (match_info$one[1]==0){
        match_data$name.tax_hosp[i] <- match_info$name.tax2[1]
      }
      
      match_data$ein_hosp[i] <- match_info$ein[1]
      match_data$zip.tax_hosp[i] <- match_info$zipcode[1]
    }
  }
}

# How many hospitals aren't NA in ein_hosp?
observe <- match_data %>%
  filter(!is.na(ein_hosp))
  # 759 hospitals

# Now match system names
# Find any exact matches of system names ####
match_data <- match_data %>%
  mutate(sys1_spaceless=tolower(sys_1),
         sys2_spaceless=tolower(sys_2))
match_data$sys1_spaceless=str_remove_all(match_data$sys1_spaceless,"\\s|,|inc|\\.|'|&|-")
match_data$sys2_spaceless=str_remove_all(match_data$sys2_spaceless,"\\s|,|inc|\\.|'|&|-")

tax_nodups <- tax_nodups %>%
  mutate(name.tax1_spaceless=name.tax,
         name.tax2_spaceless=name.tax2)
tax_nodups$name.tax1_spaceless <- str_remove_all(tax_nodups$name.tax1_spaceless,"\\s|,|inc|\\.|&|incorporated|associates|corporation|association")
tax_nodups$name.tax2_spaceless <- str_remove_all(tax_nodups$name.tax2_spaceless,"\\s|,|inc|\\.|&|incorporated|associates|corporation|association")


for (i in 1:dim(match_data)[1]){
  if (is.na(match_data$name.tax_sys[[i]])){
    st <- match_data$state[[i]]
    
    list_tax <- tax_nodups %>%
      filter(state==st)
    list_tax1 <- paste(as.list(list_tax)[["name.tax1_spaceless"]], collapse="|")
    list_tax2 <- paste(as.list(list_tax)[["name.tax2_spaceless"]], collapse="|")
    list_tax <- paste0("|", list_tax1,"|", list_tax2, "|", collapse="|")
    
    match <- str_extract(list_tax, paste0("(?<=\\|)(",match_data$sys1_spaceless[[i]],")(?=\\|)"))
    if (is.na(match) & !is.na(match_data$name.AHA2_spaceless[[i]])) {
      match <- str_extract(list_tax, paste0("(?<=\\|)(",match_data$sys2_spaceless[[i]],")(?=\\|)"))
    }
    
    if (!is.na(match)) {
      match_info <- tax_nodups %>%
        filter((name.tax1_spaceless==match | name.tax2_spaceless==match) & state==st) %>%
        mutate(one=ifelse(name.tax1_spaceless==match,1,0))
      
      if (match_info$one[1]==1){
        match_data$name.tax_sys[i] <- match_info$name.tax[1]
      }
      if (match_info$one[1]==0){
        match_data$name.tax_sys[i] <- match_info$name.tax2[1]
      }
      
      match_data$ein_sys[i] <- match_info$ein[1]
      match_data$zip.tax_sys[i] <- match_info$zipcode[1]
    }
  }
}

summary(match_data$ein_sys)

# How many hospitals aren't NA in ein_hosp or ein_sys?
observe <- match_data %>%
  filter(!(is.na(ein_hosp) & is.na(ein_sys)))
# 918 hospitals

no_matches <- match_data %>%
  filter(is.na(ein_hosp) & is.na(ein_sys)) %>%
  select(ID, state, name.AHA_1, name.AHA_2, sys_1, zip_1)

# Find manual matches based on state and google searches when possible ####
match_data <- match_data %>%
  # maine
  mutate(ein_hosp = ifelse(ID==6110150, 010227195, ein_hosp)) %>%
  mutate(ein_sys = ifelse(ID==6110167, 010211494, ein_sys)) %>%
  mutate(ein_hosp = ifelse(ID==6110410, 010263628, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6110555, 320265031, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6110582, 010217211, ein_hosp)) %>%
  # new hampshire
  mutate(ein_hosp = ifelse(ID==6120130, 222594672, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6120170, 020222140, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6120240, 020232673, ein_hosp)) %>%
  # vermont
  mutate(ein_hosp = ifelse(ID==6130015, 030219309, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6130225, 036013761, ein_hosp)) %>%
  # massachusetts
  mutate(ein_sys = ifelse(ID==6140008, 912155626, ein_sys)) %>%
  mutate(ein_hosp = ifelse(ID==6140065, 042126583, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6140690, 042704686, ein_hosp)) %>%
  mutate(ein_sys = ifelse(ID==6140923, 900054984, ein_sys)) %>%
  mutate(ein_hosp = ifelse(ID==6140980, 042103581, ein_hosp)) %>%
  mutate(ein_sys = ifelse(ID==6141095, 912155626, ein_sys)) %>%
  mutate(ein_sys = ifelse(ID==6141130, 900054984, ein_sys)) %>%
  # rhode island
  # connecticut
  mutate(ein_hosp = ifelse(ID==6160810, 060646966, ein_hosp)) %>%
  # New york
  mutate(ein_hosp = ifelse(ID==6210001, 141772971, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6210025, 743177454, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6210030, 473869194, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6211350, 160393490, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6211490, 141364513, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6212040, 160743226, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6212130, 141338470, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6212350, 150346515, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6212370, 150552726, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6212655, 131624096, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6213710, 150532180, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6213923, 111704595, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6213990, 141338471, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6214240, 160743037, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6214280, 237221763, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6214670, 900909506, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6214990, 141338544, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6215165, 150524324, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6215460, 131740127, ein_hosp)) %>%
  # new jersey
  mutate(ein_sys = ifelse(ID==6220125, 210634484, ein_sys)) %>%
  mutate(ein_hosp = ifelse(ID==6220190, 210634462, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6220217, 210662542, ein_hosp)) %>%
  mutate(ein_sys = ifelse(ID==6220275, 210634484, ein_sys)) %>%
  mutate(ein_hosp = ifelse(ID==6220547, 210635001, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6221145, 221494454, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6221330, 210660835, ein_hosp)) %>%
  mutate(ein_sys = ifelse(ID==6221430|ID==6221500, 223548695, ein_sys)) %>%
  # pennsylvania
  mutate(ein_hosp = ifelse(ID==6230510, 250987222, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6230783, 251002937, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6231375, 231529076, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6232575, 611663540, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6232725, 231996150, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6232980, 240795959, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6233520, 251244202, ein_hosp)) %>%
  # maryland
  mutate(ein_hosp = ifelse(ID==6320120, 520608007, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6320127, 520591607, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6320290, 520491660, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6320310, 520591685, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6320416, 460726303, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6320500, 520607945, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6320690, 520646893, ein_hosp)) %>%
  # dc
  mutate(ein_hosp = ifelse(ID==6330120, 521272129, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6330130, 522218584, ein_hosp)) %>%
  # virginia
  mutate(ein_hosp = ifelse(ID==6340005, 541453954, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6340290, 201106426, ein_hosp)) %>%
  mutate(ein_sys = ifelse(ID==6340425|ID==6340500|ID==6340520|ID==6340521, 901000718, ein_sys)) %>%
  # west virginia
  mutate(ein_hosp = ifelse(ID==6350270, 550375433, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6350420, 550420956, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6350600, 311524546, ein_hosp)) %>%
  # north carolina
  mutate(ein_hosp = ifelse(ID==6360625, 561484844, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6360840, 566000674, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6360895, 560554202, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6360950, 560530233, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6361210, 561509260, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6361403, 560525657, ein_hosp)) %>%
  # south carolina
  mutate(ein_hosp = ifelse(ID==6370009, 570370242, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6370330, 571067254, ein_hosp)) %>%
  # georgia
  mutate(ein_hosp = ifelse(ID==6380024, 582322328, ein_hosp)) %>%
  mutate(ein_sys = ifelse(ID==6380037|ID==6380080|ID==6380200|ID==6380400|ID==6380488, 900790361, ein_sys)) %>%
  mutate(ein_sys = ifelse(ID==6380385, 800785570, ein_sys)) %>%
  mutate(ein_hosp = ifelse(ID==6380655, 582513901, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6380775, 582032904, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6381130, 581734026, ein_hosp)) %>%
  mutate(ein_sys = ifelse(ID==6388093, 711045290, ein_sys)) %>%
  # florida
  mutate(ein_sys = ifelse(ID==6390034|ID==6390765, 590973502, ein_sys)) %>%
  mutate(ein_sys = ifelse(ID==6390308|ID==6390411, 590657322, ein_sys)) %>%
  mutate(ein_hosp = ifelse(ID==6390404, 264019868, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6390530, 590624371, ein_hosp)) %>%
  # ohio
  mutate(ein_sys = ifelse(ID==6410019, 300752920, ein_sys)) %>%
  mutate(ein_sys = ifelse(ID==6410110|ID==6410199|ID==6410335|ID==6410435|ID==6410515|ID==6410805|ID==6410920|ID==6411120|ID==6411290|ID==6419020|ID==6411376|ID==6411930,900059117, ein_sys)) %>%
  mutate(ein_hosp = ifelse(ID==6410265, 314391798, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6411585, 311657206, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6411900, 344441792, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6411955, 341623770, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6411959, 341041385, ein_hosp)) %>%
  # indiana
  mutate(ein_hosp = ifelse(ID==6420100, 356067049, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6420197, 351967665, ein_hosp)) %>%
  mutate(ein_sys = ifelse(ID==6420320, 350593390, ein_sys)) %>%
  mutate(ein_hosp = ifelse(ID==6420590, 351970706, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6420760, 352087092, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6420830, 202401676, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6420910, 350895832, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6421190, 350892672, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6421300, 350868157, ein_hosp)) %>%
  # illinois
  mutate(ein_hosp = ifelse(ID==6430016, 362739299, ein_hosp)) %>%
  mutate(ein_sys = ifelse(ID==6430019|ID==6430545|ID==6431060|ID==6431420|ID==6431765|ID==6432060|ID==6432127|ID==6432910, 364724966, ein_sys)) %>%
  mutate(ein_hosp = ifelse(ID==6430430, 366000085, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6431782, 370647938, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6432300, 362170155, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6432445, 371363001, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6432810, 362222696, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6432980, 370661238, ein_hosp)) %>%
  # michigan
  # wisonsin
  mutate(ein_sys = ifelse(ID==6450017|ID==6450019|ID==6450032|ID==6450175|ID==6452165|ID==6450200|ID==6450239|ID==6450360|ID==6450470|ID==6450865|ID==6451740|ID==6451920, 611649250, ein_sys)) %>%
  mutate(ein_hosp = ifelse(ID==6450037, 061745397, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6450261, 390831153, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6450340, 390819992, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6450515, 390804125, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6450610, 390816845, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6451795, 390892183, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6451891, 411811073, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6452115, 396105970, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6452170, 390806302, ein_hosp)) %>%
  # kentucky
  mutate(ein_hosp = ifelse(ID==6510280, 610510934, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6510473, 061705652, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6510488, 610703799, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6510874, 610432526, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6511000, 610965365, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6519070, 610518022, ein_hosp)) %>%
  # tennessee
  mutate(ein_hosp = ifelse(ID==6520466, 540544705, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6520575, 311626179, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6520576, 620988604, ein_hosp)) %>%
  # alabama
  mutate(ein_sys = ifelse(ID==6530100|ID==6530116|ID==6530417, 203713023, ein_sys)) %>%
  # mississippi
  mutate(ein_hosp = ifelse(ID==6540470, 640324402, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6540509, 870741588, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6540575, 640362400, ein_hosp)) %>%
  # minnesota
  mutate(ein_sys = ifelse(ID==6610085|ID==6610200|ID==6610240|ID==6610455|ID==6610815|ID==6611229|ID==6611300, 363261413, ein_sys)) %>%
  mutate(ein_sys = ifelse(ID==6610130|ID==6610365|ID==6610380|ID==6610390|ID==6610410|ID==6610520|ID==6610550|ID==6611148|ID==6611742|ID==6611930, 411813221, ein_sys)) %>%
  mutate(ein_hosp = ifelse(ID==6610365, 410844574, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6610380, 411620386, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6610390, 410714079, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6610410, 410695604, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6610530, 411949230, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6610550, 410726173, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6610620, 410307617, ein_hosp)) %>%
  mutate(ein_sys = ifelse(ID==6610720|ID==6611135|ID==6611325|ID==6611440|ID==6611750|ID==6612050, 411813221, ein_sys)) %>%
  mutate(ein_hosp = ifelse(ID==6610840, 455023260, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6610950, 421707837, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6611750, 452438973, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6611915, 410786046, ein_hosp)) %>%
  # iowa
  mutate(ein_hosp = ifelse(ID==6620070, 421487967, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6620490, 420680354, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6620585, 421009175, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6620652, 420933383, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6620805, 420710268, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6620830, 421087612, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6621360, 420698265, ein_hosp)) %>%
  # missouri
  mutate(ein_hosp = ifelse(ID==6630009, 832249459, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6630380, 440661018, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6631165, 430653493, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6639015, 430715842, ein_hosp)) %>%
  # north dakota
  mutate(ein_hosp = ifelse(ID==6640060, 450227311, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6640310, 450306787, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6640455, 770637498, ein_hosp)) %>%
  # south dakota
  mutate(ein_hosp = ifelse(ID==6650250, 460228038, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6650411, 466015787, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6650455, 460260288, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6650470, 460226283, ein_hosp)) %>%
  # nebraska
  mutate(ein_hosp = ifelse(ID==6660005, 911858433, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6660120, 470426530, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6660140, 470482234, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6660192, 470468078, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6660300, 470378779, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6661010, 470379039, ein_hosp)) %>%
  # kansas
  mutate(ein_hosp = ifelse(ID==6670787, 486099245, ein_hosp)) %>%
  # arkansas
  mutate(ein_hosp = ifelse(ID==6710029, 710772737, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6710130, 716044543, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6710470, 352414105, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6710482, 710561765, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6710525, 273970823, ein_hosp)) %>%
  # louisiana
  mutate(ein_hosp = ifelse(ID==6720027, 251925187, ein_hosp)) %>%
  mutate(ein_sys = ifelse(ID==6720122, 831605004, ein_sys)) %>%
  mutate(ein_hosp = ifelse(ID==6720399, 720500565, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6720878, 721479692, ein_hosp)) %>%
  # oklahoma
  mutate(ein_hosp = ifelse(ID==6730530, 736617937, ein_hosp)) %>%
  # texas
  mutate(ein_sys = ifelse(ID==6741018|ID==6741035, 352410801, ein_sys)) %>%
  mutate(ein_hosp = ifelse(ID==6741018, 750800661, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6741545, 750703337, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6742360, 752765566, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6743145, 752663904, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6749090, 273993262, ein_hosp)) %>%
  # montana
  mutate(ein_hosp = ifelse(ID==6810085, 810515463, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6810135, 264230898, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6810505, 816016920, ein_hosp)) %>%
  # idaho
  mutate(ein_hosp = ifelse(ID==6820411, 821162805, ein_hosp)) %>%
  # wyoming
  mutate(ein_hosp = ifelse(ID==6830262, 836000182, ein_hosp)) %>%
  # colorado
  mutate(ein_hosp = ifelse(ID==6840033, 043730045, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6840915, 840398876, ein_hosp)) %>%
  # new mexico
  mutate(ein_hosp = ifelse(ID==6850235, 850313268, ein_hosp)) %>%
  # arizona
  mutate(ein_hosp = ifelse(ID==6860320, 860098923, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6860410, 043651340, ein_hosp)) %>%
  # utah
  # nevada
  # washington
  mutate(ein_sys = ifelse(ID==6910190, 810463482, ein_sys)) %>%
  # oregon
  mutate(ein_hosp = ifelse(ID==6920743, 930415219, ein_hosp)) %>%
  # california
  mutate(ein_hosp = ifelse(ID==6930385, 940760193, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6930403, 941458282, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6930706, 274658935, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6930850, 951816017, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6933370, 942823538, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6933488, 952821104, ein_hosp)) %>%
  mutate(ein_hosp = ifelse(ID==6933505, 956064971, ein_hosp)) %>%
  # alaska
  mutate(ein_hosp = ifelse(ID==6940215, 920077523, ein_hosp)) 

# manual matches from JMP ####  
match_data <- match_data %>%
  mutate(ein_hosp = ifelse(ID=='6120080', 510491062, ein_hosp),
         ein_hosp = ifelse(ID=='6120190', 20222150, ein_hosp),
         ein_hosp = ifelse(ID=='6120380', 20222131, ein_hosp),
         ein_hosp = ifelse(ID=='6140465', 237000827, ein_hosp),
         ein_hosp = ifelse(ID=='6141890', 42103577, ein_hosp),
         ein_sys = ifelse(ID=='6141300', 912155626, ein_sys),
         ein_hosp = ifelse(ID=='6150001', 50259004, ein_hosp),
         ein_hosp = ifelse(ID=='6160013', 60646768, ein_hosp),
         ein_hosp = ifelse(ID=='6160650', 60646696, ein_hosp),
         ein_hosp = ifelse(ID=='6160750', 60665979, ein_hosp),
         ein_hosp = ifelse(ID=='6210058', 146049030, ein_hosp),
         ein_hosp = ifelse(ID=='6210400', 131740110, ein_hosp),
         ein_hosp = ifelse(ID=='6210740', 111986351, ein_hosp),
         ein_hosp = ifelse(ID=='6211180', 160743024, ein_hosp),
         ein_hosp = ifelse(ID=='6211290', 160743966, ein_hosp),
         ein_hosp = ifelse(ID=='6211470', 160743301, ein_hosp),
         ein_hosp = ifelse(ID=='6211500', 134111638, ein_hosp),
         ein_hosp = ifelse(ID=='6211545', 161165049, ein_hosp),
         ein_hosp = ifelse(ID=='6211690', 141338465, ein_hosp),
         ein_hosp = ifelse(ID=='6213830', 150539039, ein_hosp),
         ein_hosp = ifelse(ID=='6213870', 131725076, ein_hosp),
         ein_hosp = ifelse(ID=='6215260', 222807681, ein_hosp),
         ein_hosp = ifelse(ID=='6215330', 131740130, ein_hosp),
         ein_hosp = ifelse(ID=='6220280', 221487173, ein_hosp),
         ein_hosp = ifelse(ID=='6220322', 221750190, ein_hosp),
         ein_hosp = ifelse(ID=='6220425', 223693169, ein_hosp),
         ein_hosp = ifelse(ID=='6230019', 230794160, ein_hosp),
         ein_hosp = ifelse(ID=='6230022', 311538725, ein_hosp),
         ein_hosp = ifelse(ID=='6230040', 231352208, ein_hosp),
         ein_hosp = ifelse(ID=='6230041', 251054206, ein_hosp),
         ein_hosp = ifelse(ID=='6230270', 250965274, ein_hosp),
         ein_hosp = ifelse(ID=='6230297', 251865142, ein_hosp),
         ein_hosp = ifelse(ID=='6231345', 231534300, ein_hosp),
         ein_hosp = ifelse(ID=='6231460', 232720289, ein_hosp),
         ein_hosp = ifelse(ID=='6233100', 251801532, ein_hosp),
         ein_hosp = ifelse(ID=='6310120', 510103684, ein_hosp),
         ein_hosp = ifelse(ID=='6320020', 521169362, ein_hosp),
         ein_hosp = ifelse(ID=='6320420', 520679694, ein_hosp),
         ein_hosp = ifelse(ID=='6320670', 520619006, ein_hosp),
         ein_hosp = ifelse(ID=='6320735', 521372665, ein_hosp),
         ein_sys = ifelse(ID=='6340020'|ID=='6340030', 540620889, ein_sys),
         ein_hosp = ifelse(ID=='6340040', 540505989, ein_hosp),
         ein_hosp = ifelse(ID=='6340145', 237424835, ein_hosp),
         ein_hosp = ifelse(ID=='6340330', 521271901, ein_hosp),
         ein_hosp = ifelse(ID=='6340360', 540505913, ein_hosp),
         ein_hosp = ifelse(ID=='6340454', 540696355, ein_hosp),
         ein_hosp = ifelse(ID=='6350555', 237441353, ein_hosp),
         ein_hosp = ifelse(ID=='6350710', 550404900, ein_hosp),
         ein_hosp = ifelse(ID=='6350800', 550422958, ein_hosp),
         ein_hosp = ifelse(ID=='6360001', 566017737, ein_hosp),
         ein_hosp = ifelse(ID=='6360012', 561376368, ein_hosp),
         ein_hosp = ifelse(ID=='6360021', 562112733, ein_hosp),
         ein_hosp = ifelse(ID=='6360355', 562070036, ein_hosp),
         ein_hosp = ifelse(ID=='6360920', 560543238, ein_hosp),
         ein_hosp = ifelse(ID=='6361340', 560547479, ein_hosp),
         ein_hosp = ifelse(ID=='6361375', 560642846, ein_hosp),
         ein_hosp = ifelse(ID=='6361686', 561340424, ein_hosp),
         ein_hosp = ifelse(ID=='6370375', 570873845, ein_hosp),
         ein_hosp = ifelse(ID=='6370640', 570343398, ein_hosp),
         ein_hosp = ifelse(ID=='6380020', 582224545, ein_hosp),
         ein_hosp = ifelse(ID=='6380360', 262037695, ein_hosp),
         ein_hosp = ifelse(ID=='638080A', 580593388, ein_hosp),
         ein_hosp = ifelse(ID=='6380885', 582510435, ein_hosp),
         ein_hosp = ifelse(ID=='6380950', 581973570, ein_hosp),
         ein_hosp = ifelse(ID=='6381170', 586001667, ein_hosp),
         ein_hosp = ifelse(ID=='6381215', 581790149, ein_hosp),
         ein_hosp = ifelse(ID=='6381225', 711045290, ein_hosp),
         ein_hosp = ifelse(ID=='6389065', 586025393, ein_hosp),
         ein_hosp = ifelse(ID=='6390050', 592447554, ein_hosp),
         ein_hosp = ifelse(ID=='6390200', 593234721, ein_hosp),
         ein_hosp = ifelse(ID=='6390499', 592319288, ein_hosp),
         ein_hosp = ifelse(ID=='6390640', 590624424, ein_hosp),
         ein_hosp = ifelse(ID=='6390669', 592314655, ein_hosp),
         ein_hosp = ifelse(ID=='6410012', 341887844, ein_hosp),
         ein_hosp = ifelse(ID=='6410015', 344428218, ein_hosp),
         ein_hosp = ifelse(ID=='6410012', 341887844, ein_hosp),
         ein_hosp = ifelse(ID=='6410015', 344428218, ein_hosp), 
         ein_hosp = ifelse(ID=='6410217', 344440884, ein_hosp),
         ein_hosp = ifelse(ID=='6410382', 310537122, ein_hosp),
         ein_hosp = ifelse(ID=='6411370', 311156690, ein_hosp),
         ein_hosp = ifelse(ID=='6411375', 341408846, ein_hosp),
         ein_hosp = ifelse(ID=='6411400', 344428598, ein_hosp),
         ein_hosp = ifelse(ID=='6411440', 311765550, ein_hosp),
         ein_hosp = ifelse(ID=='6411540', 341883284, ein_hosp),
         ein_hosp = ifelse(ID=='6411579', 311458827, ein_hosp),
         ein_hosp = ifelse(ID=='6411870', 341425870, ein_hosp),
         ein_hosp = ifelse(ID=='351955872', 351955872, ein_hosp),
         ein_hosp = ifelse(ID=='6420130', 351720796, ein_hosp),
         ein_hosp = ifelse(ID=='6420210', 350900741, ein_hosp),
         ein_hosp = ifelse(ID=='6420605', 350983617, ein_hosp),
         ein_hosp = ifelse(ID=='6420728', 351088640, ein_hosp),
         ein_hosp = ifelse(ID=='6420870', 474673365, ein_hosp),
         ein_hosp = ifelse(ID=='6430337', 370645239, ein_hosp),
         ein_hosp = ifelse(ID=='6431927', 363637465, ein_hosp),
         ein_hosp = ifelse(ID=='6431970', 370661230, ein_hosp),
         ein_hosp = ifelse(ID=='6432000', 362174832, ein_hosp),
         ein_hosp = ifelse(ID=='6432615', 370681540, ein_hosp),
         ein_hosp = ifelse(ID=='6432640', 371396010, ein_hosp),
         ein_hosp = ifelse(ID=='6440015', 383236977, ein_hosp),
         ein_hosp = ifelse(ID=='6441011', 381426919, ein_hosp),
         ein_hosp = ifelse(ID=='6441231', 380593405, ein_hosp),
         ein_hosp = ifelse(ID=='6441350', 381619577, ein_hosp),
         ein_hosp = ifelse(ID=='6441360', 382800065, ein_hosp),
         ein_hosp = ifelse(ID=='6441450', 382908586, ein_hosp),
         ein_hosp = ifelse(ID=='6441595', 381360584, ein_hosp),
         ein_hosp = ifelse(ID=='6441830', 382947657, ein_hosp),
         ein_hosp = ifelse(ID=='6442327', 381738615, ein_hosp),
         ein_hosp = ifelse(ID=='6442410', 382317300, ein_hosp),
         ein_hosp = ifelse(ID=='6450330', 390807060, ein_hosp),
         ein_hosp = ifelse(ID=='6451300', 390806828, ein_hosp),
         ein_hosp = ifelse(ID=='6451370', 390848401, ein_hosp),
         ein_hosp = ifelse(ID=='6451630', 390837206, ein_hosp),
         ein_hosp = ifelse(ID=='6451840', 390832914, ein_hosp),
         ein_hosp = ifelse(ID=='6452020', 390910727, ein_hosp),
         ein_hosp = ifelse(ID=='6510049', 610601267, ein_hosp),
         ein_hosp = ifelse(ID=='6510255', 204474637, ein_hosp),
         ein_hosp = ifelse(ID=='6510266', 610525158, ein_hosp),
         ein_hosp = ifelse(ID=='6510350', 452696517, ein_hosp),
         ein_hosp = ifelse(ID=='6510550', 611293786, ein_hosp),
         ein_hosp = ifelse(ID=='6510745', 610523304, ein_hosp),
         ein_hosp = ifelse(ID=='6520785', 620479367, ein_hosp),
         ein_hosp = ifelse(ID=='6520895', 620545814, ein_hosp),
         ein_hosp = ifelse(ID=='6530142', 630754793, ein_hosp),
         ein_hosp = ifelse(ID=='6540170', 640926753, ein_hosp),
         ein_hosp = ifelse(ID=='6540235', 640770155, ein_hosp),
         ein_hosp = ifelse(ID=='6540580', 640655993, ein_hosp),
         ein_hosp = ifelse(ID=='6610097', 410841441, ein_hosp),
         ein_hosp = ifelse(ID=='6610400', 410714079, ein_hosp),
         ein_hosp = ifelse(ID=='6610590', 410724034, ein_hosp),
         ein_hosp = ifelse(ID=='6610810', 411865315, ein_hosp),
         ein_hosp = ifelse(ID=='6611585', 270052697, ein_hosp),
         ein_hosp = ifelse(ID=='6611860', 205617275, ein_hosp),
         ein_hosp = ifelse(ID=='6612090', 410713914, ein_hosp),
         ein_hosp = ifelse(ID=='6620300', 420680355, ein_hosp),
         ein_hosp = ifelse(ID=='6620670', 420738969, ein_hosp),
         ein_hosp = ifelse(ID=='6621202', 420932564, ein_hosp),
         ein_hosp = ifelse(ID=='6629060', 420710268, ein_hosp),
         ein_hosp = ifelse(ID=='6630230', 430662495, ein_hosp),
         ein_hosp = ifelse(ID=='6630234', 436004544, ein_hosp),
         ein_hosp = ifelse(ID=='6630650', 440655986, ein_hosp),
         ein_hosp = ifelse(ID=='6630788', 431741457, ein_hosp),
         ein_hosp = ifelse(ID=='6631320', 440577118, ein_hosp),
         ein_hosp = ifelse(ID=='6631378', 436005776, ein_hosp),
         ein_hosp = ifelse(ID=='6640003', 261175213, ein_hosp),
         ein_hosp = ifelse(ID=='6640004', 331007002, ein_hosp),
         ein_hosp = ifelse(ID=='6640055', 450458242, ein_hosp),
         ein_hosp = ifelse(ID=='6640062', 456013474, ein_hosp),
         ein_hosp = ifelse(ID=='6640267', 450340688, ein_hosp),
         ein_hosp = ifelse(ID=='6640383', 450232743, ein_hosp),
         ein_hosp = ifelse(ID=='6640475', 450358986, ein_hosp),
         ein_hosp = ifelse(ID=='6650080', 460246437, ein_hosp),
         ein_hosp = ifelse(ID=='6650160', 460450523, ein_hosp),
         ein_hosp = ifelse(ID=='6650345', 460239781, ein_hosp),
         ein_hosp = ifelse(ID=='6659005', 460380552, ein_hosp),
         ein_hosp = ifelse(ID=='6660112', 470426285, ein_hosp),
         ein_hosp = ifelse(ID=='6670013', 481140505, ein_hosp),
         ein_hosp = ifelse(ID=='6670050', 480561974, ein_hosp),
         ein_hosp = ifelse(ID=='6670325', 481226833, ein_hosp),
         ein_hosp = ifelse(ID=='6670366', 480577658, ein_hosp),
         ein_hosp = ifelse(ID=='6670515', 480761700, ein_hosp),
         ein_hosp = ifelse(ID=='6670692', 481226856, ein_hosp),
         ein_hosp = ifelse(ID=='6670913', 481226830, ein_hosp),
         ein_hosp = ifelse(ID=='6671210', 486005089, ein_hosp),
         ein_hosp = ifelse(ID=='6710040', 710411459, ein_hosp),
         ein_hosp = ifelse(ID=='6710045', 710772959, ein_hosp),
         ein_hosp = ifelse(ID=='6710100', 710403278, ein_hosp),
         ein_hosp = ifelse(ID=='6710157', 954896822, ein_hosp),
         ein_hosp = ifelse(ID=='6720217', 720491106, ein_hosp),
         ein_hosp = ifelse(ID=='6730106', 10603214, ein_hosp),
         ein_hosp = ifelse(ID=='6730110', 731506316, ein_hosp),
         ein_hosp = ifelse(ID=='6730187', 263778478, ein_hosp),
         ein_hosp = ifelse(ID=='6741690', 741548089, ein_hosp),
         ein_hosp = ifelse(ID=='6742045', 203069241, ein_hosp),
         ein_hosp = ifelse(ID=='6742135', 742557820, ein_hosp),
         ein_hosp = ifelse(ID=='6743189', 272814620, ein_hosp),
         ein_hosp = ifelse(ID=='6810040', 810232121, ein_hosp),
         ein_hosp = ifelse(ID=='6810100', 810286525, ein_hosp),
         ein_hosp = ifelse(ID=='6810123', 810373589, ein_hosp),
         ein_hosp = ifelse(ID=='6810129', 810469886, ein_hosp),
         ein_hosp = ifelse(ID=='6810150', 810264548, ein_hosp),
         ein_hosp = ifelse(ID=='6810160', 810405434, ein_hosp),
         ein_hosp = ifelse(ID=='6810380', 237169043, ein_hosp),
         ein_hosp = ifelse(ID=='6810405', 816016152, ein_hosp),
         ein_hosp = ifelse(ID=='6810481', 810221486, ein_hosp),
         ein_hosp = ifelse(ID=='6820111', 814065632, ein_hosp),
         ein_hosp = ifelse(ID=='6820212', 273311774, ein_hosp),
         ein_hosp = ifelse(ID=='6840018', 841262971, ein_hosp),
         ein_hosp = ifelse(ID=='6840940', 840586742, ein_hosp),
         ein_hosp = ifelse(ID=='6840950', 261167922, ein_hosp),
         ein_hosp = ifelse(ID=='6850140', 850442957, ein_hosp),
         ein_hosp = ifelse(ID=='6860374', 860171900, ein_hosp),
         ein_hosp = ifelse(ID=='6864000', 860334996, ein_hosp),
         ein_hosp = ifelse(ID=='6880050', 880252723, ein_hosp),
         ein_hosp = ifelse(ID=='6910669', 910637400, ein_hosp),
         ein_hosp = ifelse(ID=='6920070', 930602940, ein_hosp),
         ein_hosp = ifelse(ID=='6930119', 953782169, ein_hosp),
         ein_hosp = ifelse(ID=='6930120', 952477294, ein_hosp),
         ein_hosp = ifelse(ID=='6932010', 951816005, ein_hosp),
         ein_hosp = ifelse(ID=='6933310', 942637032, ein_hosp),
         ein_hosp = ifelse(ID=='6940010', 920162721, ein_hosp),
         ein_hosp = ifelse(ID=='6940040', 920041414, ein_hosp))



# after the manual matches, I have 1316 matches in either hospital or system

# create crosswalk of Eins and AHA IDs
EIN_AHA_cw <- match_data %>%
  select(ID, ein_hosp, ein_sys) %>%
  mutate(ein_hosp = as.character(ein_hosp),
         ein_sys = as.character(ein_sys)) %>%
  filter(!is.na(ein_hosp)|!is.na(ein_sys)) %>%
  distinct()

# save to created data
write_csv(EIN_AHA_cw, paste0(created_data_path, 'EIN_AHA_cw.csv'))
