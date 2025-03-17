library(dplyr)
library(fuzzyjoin)
library(stringdist)
library(tidyr)
library(stringr)
library(ggplot2)
library(patchwork)
library(readr)
library(data.table)
library(kableExtra)
library(purrr)
library(ggpubr)

created_data_path <- "CreatedData/"

## Compare connected independent hospitals to unconnected independent hospitals


###### PART 1: CLEAN DATA ###############################################################################################################################

# Read in cleaned up version of names found in 990 tax forms
people <- read_rds(paste0(created_data_path, "cleaned_people_data2.rds"))

# Read is crosswalk of AHA ID - EIN, only keeping those who match to a standalone hospital
cw <- readRDS(paste0(created_data_path, "updated_ein_aha_cw.rds")) 

# Clean up the people data 
# only keep people whose EIN is in the crosswalk as a standalone hospital
people <- people %>% filter(Filer.EIN %in% cw$Filer.EIN)

# only keep people who are board members
board_people <- people %>%
  filter(board_member==1) %>%
  select(TaxYr, Filer.EIN, name_cleaned)

# Only keep EINs present in 2017-2021
ein_keep <- board_people %>%
  distinct(TaxYr, Filer.EIN) %>%
  mutate(count = ifelse(TaxYr %in% 2017:2021, 1, 0)) %>%
  group_by(Filer.EIN) %>%
  summarise(count = sum(count)) %>%
  filter(count>=5)

board_people <- board_people %>%
  filter(Filer.EIN %in% ein_keep$Filer.EIN)

# clean up extras in names
board_people$name_cleaned <- str_remove(board_people$name_cleaned, "deceased")
board_people$name_cleaned <- str_remove(board_people$name_cleaned, "\\seff$")
board_people$name_cleaned <- str_remove(board_people$name_cleaned, "\\scsjp$")
board_people$name_cleaned <- str_remove(board_people$name_cleaned, "\\scisa$")
board_people$name_cleaned <- str_remove(board_people$name_cleaned, "^mrs\\s")
board_people$name_cleaned <- str_remove(board_people$name_cleaned, "^reverend\\s")
board_people$name_cleaned <- str_remove(board_people$name_cleaned, "\\scns$")
board_people$name_cleaned <- str_remove_all(board_people$name_cleaned, "january|february|march|june|july|august|september|october|november|december|dec\\b")

# get rid of empty names
board_people <- board_people %>%
  filter(name_cleaned!="" & !str_detect(name_cleaned, "ceo|cfo"))

# look at random sample of 10 EINs
board_people %>%
  ungroup() %>%
  select(Filer.EIN) %>%
  distinct() %>%
  sample_n(10)

# define function to normalize the name columns to take care of different order of first, last
normalize_name <- function(name) {
  # Split by spaces
  parts <- unlist(strsplit(name, " "))
  
  # Remove single-letter initials (optional)
  parts <- parts[nchar(parts) > 1]
  
  # Sort non-initial parts alphabetically
  paste(sort(parts), collapse = " ")
}

board_people$name_cleaned <- sapply(board_people$name_cleaned, normalize_name)

# remove empty names and one-word names
board_people <- board_people %>%
  filter(str_detect(name_cleaned, "[a-zA-Z]")) %>%
  filter(str_count(name_cleaned, " ")>0)

# join AHA data to get HRR and state
AHA <- read_csv("RawData/AHAdata_20052023.csv") 

AHA_hrr <- AHA %>%
  select(ID, YEAR, HRRCODE, SYSID, MNAME, NETWRK, NETNAME) %>%
  filter(YEAR>=2015 & YEAR<=2023) %>%
  mutate(YEAR = as.character(YEAR))

board_people <- board_people %>%
  left_join(cw, by = c("Filer.EIN")) %>%
  left_join(AHA_hrr, by = c("ID", "TaxYr"="YEAR"))

# fill HRR code
board_people <- board_people %>%
  group_by(Filer.EIN) %>%
  fill(HRRCODE, .direction = "downup") %>%
  ungroup()

# how many IDs?
length(unique(board_people$ID))
  #1520

# run the script titled "function1_standardize_names"
source("Scripts//function1_standardize_names.R")

# combine names that have a slight misspelling using the standardize names function
# these have to be in the same EIN to be combined
board_people <- standardize_names_optimized(board_people, max_dist = 3)

# also standardize names that are a very close match within the same HRR (more strict matches)
board_people <- board_people %>%
  standardize_names_by_hrr(., max_dist = 2)


# run the script titled "function2_find_common_board_members"
source("Scripts//function2_identify_common_members.R")

# identify common members on the board of directors
board_people <- identify_common_members(board_people)

# remove variables we don't need
board_people <- board_people %>%
  select(Filer.EIN, TaxYr, name_cleaned, other_eins)

# separate other_eins into multiple columns
board_people <- board_people %>%
  separate_wider_delim(other_eins, delim = ",", names_sep = "", too_few="align_start")

# distinct
board_people <- board_people %>%
  distinct(Filer.EIN, TaxYr, name_cleaned, other_eins1, other_eins2, other_eins3)

# wide to long in other_eins
board_people <- board_people %>%
  pivot_longer(cols = starts_with("other_eins"), names_to = "num_board", values_to = "other_ein")

# remove NA values in other_ein
board_people <- board_people %>%
  filter(!(num_board=="other_eins2" & is.na(other_ein))) %>%
  filter(!(num_board=="other_eins3" & is.na(other_ein)))

# trim the white space on other_ein
board_people$other_ein <- str_trim(board_people$other_ein)

# Merge in AHA geographic information
AHA_geog <- AHA %>%
  select(ID, YEAR, SYSID, NETNAME, FSTCD, LONG, LAT, HRRCODE, MNAME) %>%
  filter(YEAR>=2016 & YEAR<=2022)

# Are there any duplicates in ID, YEAR?
AHA_geog %>% 
  group_by(ID, YEAR) %>%
  summarise(n = n()) %>%
  filter(n > 1)

# Merge AHA data to the AHA - EIN crosswalk
cw <- cw %>%
  left_join(AHA_geog, by = "ID") 

# Merge AHA data to the board_people data
board_people <- board_people %>%
  mutate(TaxYr = as.numeric(TaxYr)) %>%
  left_join(cw, by = c("Filer.EIN", "TaxYr"="YEAR"))

# Rename variables to show that they are the filer geographic information
board_people <- board_people %>%
  rename(filer_id=ID, filer_sysid = SYSID, filer_stcd = FSTCD, filer_long = LONG, filer_lat = LAT, filer_hrrcode = HRRCODE,
         filer_name = MNAME, filer_net = NETNAME)

# Merge AHA data to the other_ein data
board_people <- board_people %>%
  left_join(cw, by = c("other_ein" = "Filer.EIN", "TaxYr"="YEAR"))

# Rename variables to show that they are the other_ein geographic information
board_people <- board_people %>%
  rename(other_id=ID, other_sysid = SYSID, other_stcd = FSTCD, other_long = LONG, other_lat = LAT, other_hrrcode = HRRCODE,
         other_name = MNAME, other_net = NETNAME)

# Remove any observations with NA for ID at any point
board_people <- board_people %>%
  mutate(missing = ifelse(is.na(filer_id) & TaxYr %in% 2017:2021, 1, 0)) %>%
  group_by(Filer.EIN) %>%
  filter(sum(missing)==0) %>%
  filter(TaxYr!=2015)


# Create hospital-level connections data
hospital_connections <- board_people %>%
  distinct(TaxYr, Filer.EIN, other_ein, filer_id, filer_sysid, filer_stcd, filer_long, filer_lat, filer_hrrcode,filer_name, filer_net,
           other_id, other_sysid, other_stcd, other_long, other_lat, other_hrrcode, other_name, other_net) 

# only keep connections in the same HRR
hospital_connections <- hospital_connections %>%
  filter(filer_hrrcode==other_hrrcode | is.na(other_hrrcode))

# only keep connections with hospitals not affiliated by system
hospital_connections <- hospital_connections %>%
  filter(is.na(filer_sysid) | is.na(other_sysid) | filer_sysid!=other_sysid)

# get rid of connections where the name indicates system affiliation even if system id is missing
hospital_connections <- hospital_connections %>%
  mutate(name_dist = stringdist::stringdist(str_extract(filer_name,"[A-Za-z]+\\s"), str_extract(other_name,"[A-Za-z]+\\s"), method = "jw")) %>%
  filter(name_dist!=0 | is.na(name_dist))


# If a hospital has a connection in a given year, remove the empty other_ein row
hospital_connections <- hospital_connections %>%
  mutate(connected = ifelse(other_ein!="", 1, NA)) %>%
  group_by(TaxYr, Filer.EIN) %>%
  fill(connected, .direction = "downup") %>%
  ungroup() %>%
  mutate(connected = ifelse(is.na(connected), 0, connected)) %>%
  filter(!(other_ein=="" & connected==1)) %>%
  distinct() %>%
  select(-connected)


## PART 2: SUMMARIZE HOSPITAL CONNECTIONS #########################################################

# look at random sample of 10 EINs
hospital_connections %>%
  ungroup() %>%
  select(Filer.EIN) %>%
  distinct() %>%
  sample_n(10)
  
hospital_pairs <- hospital_connections %>%
  mutate(connected = ifelse(other_ein!="", 1, NA)) %>%
  group_by(TaxYr, Filer.EIN) %>%
  fill(connected, .direction = "downup") %>%
  ungroup() %>%
  mutate(connected = ifelse(is.na(connected), 0, connected))

# define connections within the same HRR
hospital_connections <- hospital_connections %>%
  mutate(connected = ifelse(other_ein!="", 1, NA)) %>%
  group_by(TaxYr, Filer.EIN) %>%
  fill(connected, .direction = "downup") %>%
  ungroup() %>%
  mutate(connected = ifelse(is.na(connected), 0, connected)) %>%
  distinct(Filer.EIN, filer_id, filer_hrrcode, TaxYr, connected)


# how many hospitals are connected in each year?
num_conn_list <- hospital_connections %>%
  group_by(TaxYr) %>%
  summarise(n_connected = sum(connected)) %>%
  mutate(n_connected = paste("n = ", n_connected))


# graph the percent of hospitals that are connected in each year
hospital_connections %>%
  group_by(TaxYr) %>%
  summarise(m_connected = mean(connected)) %>%
  ggplot(aes(x=TaxYr)) +
  geom_line(aes(y=m_connected)) +
  labs(title = "",
       x = "\nYear",
       y = "Percent of Hospitals\n") +
  theme_minimal() + xlim(2017,2021) + ylim(0,.5) +
  labs(color='') +
  # add labels for the raw number of hospitals at each point using num_conn_list
  geom_label(aes(x=TaxYr, y=m_connected, label=num_conn_list$n_connected), vjust=-1, size = 3)
  
ggsave("Objects//connected_percent.pdf", width=6, height=4)

# Graph the percent of HRRs that have connected hospitals within them

# how many HRRs? are connected in each year?
num_conn_list <- hospital_connections %>%
  distinct(TaxYr, filer_hrrcode, connected) %>%
  group_by(TaxYr) %>%
  summarise(n_connected = sum(connected)) %>%
  mutate(n_connected = paste("n = ", n_connected))


hospital_connections %>%
  group_by(TaxYr, filer_hrrcode) %>%
  summarise(connected = sum(connected)) %>%
  mutate(connected = ifelse(connected>0, 1, 0)) %>%
  group_by(TaxYr) %>%
  summarise(m_connected = mean(connected)) %>%
  ggplot(aes(x=TaxYr)) +
  geom_line(aes(y=m_connected)) +
  labs(title = "",
       x = "\nYear",
       y = "Percent of HRRs\n") +
  theme_minimal() + xlim(2017,2021) + ylim(0,.5) +
  labs(color='') +
  # add labels for the raw number of hospitals at each point using num_conn_list
  geom_label(aes(x=TaxYr, y=m_connected, label=num_conn_list$n_connected), vjust=-1, size = 3)

ggsave("Objects//connected_HRR_percent.pdf", width=6, height=4)


# plot geographically the hospitals that are connected
plot_map <- function(df, year) {
  df_year <- df %>% filter(TaxYr == year)
  
  # Identify hospitals with at least one connection
  connected_hospitals <- df_year %>%
    filter(connected==1) %>%
    select(Filer.EIN) %>%
    distinct()
  
  # Create nodes (all hospitals, even unconnected ones)
  nodes <- df_year %>%
    select(Filer.EIN, filer_lat, filer_long) %>%
    distinct(Filer.EIN, .keep_all = TRUE) %>%
    mutate(is_connected = ifelse(Filer.EIN %in% connected_hospitals$Filer.EIN, "Connected", "Unconnected"))
  
  
  # Filter hospitals with board connections
  df_filtered <- df_year %>%
    filter(connected==1)
  
  
  # Generate the plot
  ggplot() +
    # Draw U.S. state boundaries
    geom_polygon(data = us_states, aes(x = long, y = lat, group = group),
                 fill = "lightgray", color = "white") +
    # Draw connection lines
    geom_segment(data = df_filtered, 
                 aes(x = filer_long, y = filer_lat, xend = other_long, yend = other_lat),
                 color = "blue", alpha = 1, size = 0.65) +
    # Plot all hospitals as red dots
    geom_point(data = nodes, aes(x = filer_long, y = filer_lat, color = is_connected), 
               size = 0.05) +
    scale_color_manual(values = c("Connected" = "blue", "Unconnected" = "red")) + 
    theme_minimal() +
    labs(title = year) +
    coord_fixed(1.3)  +
    ylim(20,50) + xlim(-130,-60) +
    theme(legend.position = "none")
  
}

# read in map data
us_states <- map_data("state")


# 1. Geographic map of connection within the same HRR excluding systems

plot_map(hospital_pairs, 2017)

# Generate maps for each year (2017-2022)
plots <- lapply(2017:2022, function(year) plot_map(hospital_pairs, year))

# Combine plots into a single graphic
# combine legends into one 
combined_plot <- wrap_plots(plots, ncol = 2, guides = "collect", axis_titles = "collect")  

# save combined plot
ggsave("Objects//connected_maps.pdf", width = 8, height = 11, units = "in")


# Summarise hospital pairs
hospital_connected_pairs <- hospital_pairs %>%
  select(TaxYr, filer_id, other_id) %>%
  filter(!is.na(filer_id) & !is.na(other_id))

AHA_pair <- AHA %>%
  select(YEAR, ID, SERV, BDTOT, MNAME, SYSID) %>%
  filter(YEAR>=2016 & YEAR<=2022)

# join by filer_id
hospital_connected_pairs <- hospital_connected_pairs %>%
  left_join(AHA_pair, by=c("filer_id"="ID", "TaxYr"="YEAR")) %>%
  rename(filer_SERV = SERV, filer_BDTOT = BDTOT, filer_name = MNAME, filer_sysid = SYSID) %>%
  left_join(AHA_pair, by=c("other_id"="ID", "TaxYr"="YEAR")) %>%
  rename(other_SERV = SERV, other_BDTOT = BDTOT, other_name = MNAME, other_sysid = SYSID)

# create variables capturing type of relationship (general - general/ general - specialty/specialty - specialty)
hospital_connected_pairs <- hospital_connected_pairs %>%
  mutate(gen_gen = ifelse(filer_SERV==10 & other_SERV==10,1,0),
         gen_spec = ifelse((filer_SERV==10 & other_SERV %in% c(13,22,33,41,42,44,45,46,47,48,49)) | (other_SERV==10 & filer_SERV %in% c(13,22,33,41,42,44,45,46,47,48,49)),1,0),
         spec_spec = ifelse(filer_SERV %in% c(13,22,33,41,42,44,45,46,47,48,49) & other_SERV %in% c(13,22,33,41,42,44,45,46,47,48,49),1,0),
         adult_child = ifelse((filer_SERV %in% c(10,13,22,33,41,42,44,45,46,47,48,49) & other_SERV %in% 50:59) | (other_SERV %in% c(13,22,33,41,42,44,45,46,47,48,49) & filer_SERV %in% 50:59),1,0))

# create variables capturing ownership relationship (ind - ind/sys - sys/ind - sys)
hospital_connected_pairs <- hospital_connected_pairs %>%
  mutate(ind_ind = ifelse(is.na(filer_sysid) & is.na(other_sysid), 1, 0),
         sys_sys = ifelse(!is.na(filer_sysid) & !is.na(other_sysid), 1, 0),
         ind_sys = ifelse((is.na(filer_sysid) & !is.na(other_sysid)) | (!is.na(filer_sysid) & is.na(other_sysid)), 1, 0))

# put these values in a table 
pair_table <- hospital_connected_pairs %>%
  summarise("General - General" = mean(gen_gen),
            "General - Specialty" = mean(gen_spec),
            "Specialty - Specialty" = mean(spec_spec),
            "Adult - Childrens" = mean(adult_child),
            "Ind. - Ind." = mean(ind_ind),
            "Sys. - Sys." = mean(sys_sys),
            "Ind. - Sys." = mean(ind_sys))

# pivot longer
pair_table <- pair_table %>%
  pivot_longer(cols = `General - General`:`Ind. - Sys.`, names_to = "variable", values_to = "value")

# add row for total number of connected hospitals
pair_table <- pair_table %>%
  add_row(variable = "Total Connected Hospitals", value = nrow(distinct(hospital_connected_pairs, filer_id)))

knitr::kable(pair_table, format = "latex",
             col.names = c("Type of Connected Pair", "Percent"),
             caption = "Types of Hospital Connections",
             row.names = FALSE,
             table.envir="table",
             digits=2,
             booktabs=TRUE,
             escape=F,
             align=c("l","c"),
             position="ht!") %>%
  kable_styling(full_width=F) %>%
  pack_rows(" ",1,3) %>%
  pack_rows(" ",5,7) %>%
  write("Objects//hospital_pair_types.tex")

# Summarise connected general hospitals vs. unconnected general hospitals 
gen_connected <- hospital_connected_pairs %>%
  filter(gen_gen==1) %>%
  distinct(TaxYr, filer_id) %>%
  mutate(group = "General Connected to General")

gen_connected_spec <- hospital_connected_pairs %>%
  filter(gen_spec==1) %>%
  filter(filer_SERV==10) %>%
  distinct(TaxYr, filer_id) %>%
  mutate(group = "General Connected to Specialty")

spec_connected_gen <- hospital_connected_pairs %>%
  filter(gen_spec==1) %>%
  filter(other_SERV==10) %>%
  distinct(TaxYr, filer_id) %>%
  mutate(group = "Specialty Connected to General")

gen_unconnected <- hospital_pairs %>%
  filter(other_ein=="") %>%
  distinct(TaxYr, filer_id) %>%
  left_join(AHA_pair, by=c("filer_id"="ID", "TaxYr"="YEAR")) %>%
  filter(SERV==10) %>%
  mutate(group = "General Unconnected") %>%
  select(TaxYr, filer_id, group)

spec_unconnected <- hospital_pairs %>%
  filter(other_ein=="") %>%
  distinct(TaxYr, filer_id) %>%
  left_join(AHA_pair, by=c("filer_id"="ID", "TaxYr"="YEAR")) %>%
  filter(SERV %in% c(13,22,33,41,42,44,45,46,47,48,49)) %>%
  mutate(group = "Specialty Unconnected") %>%
  select(TaxYr, filer_id, group)

# combine the two
gen_hosp_connections <- bind_rows(gen_connected, gen_connected_spec, spec_connected_gen, gen_unconnected, spec_unconnected)

# get AHA variables I want to summarise in this table
AHA_gen <- AHA %>%
  select(YEAR, ID, GENBD, PEDBD, OBBD, MSICBD, CICBD, NICBD, NINTBD, PEDICBD, 
         BRNBD, SPCICBD, REHABBD, OTHICBD, ACULTBD, ALCHBD, PSYBD, SNBD88, ICFBD88,
         OTHLBD94, OTHBD94, HOSPBD, FTMT, FTRNTF, ICLABHOS, MCDDC, MCRDC, HRRCODE, MAPP5, MCRNUM, ACLABHOS)
  
gen_hosp_connections <- gen_hosp_connections %>%
  left_join(AHA_gen, by=c("filer_id"="ID", "TaxYr"="YEAR"))

gen_hosp_connections <- gen_hosp_connections %>%
  group_by(filer_id) %>%
  fill(MCRNUM, .direction="downup") %>%
  ungroup()

# how many non-NAs for MCRNUM? 
gen_hosp_connections %>%
  filter(!is.na(MCRNUM)) %>%
  distinct(filer_id) %>%
  nrow()

# Read in CMS provider patient utilization data
for (year in 2017:2021) {
  assign(paste0("util", year), read_csv(paste0("RawData/MedicareHospitalUtilization/Medicare_IP_Hospitals_by_Provider_",year,".csv")) %>% mutate(year=year))
}

util <- rbind(util2017, util2018, util2019, util2020, util2021) %>%
  select(year, Rndrng_Prvdr_CCN, Rndrng_Prvdr_RUCA_Desc, Tot_Benes:Bene_Avg_Age, Bene_Race_Wht_Cnt:Bene_Race_Othr_Cnt, Bene_CC_PH_Cancer6_V2_Pct, Bene_CC_PH_CKD_V2_Pct,
         Bene_CC_PH_COPD_V2_Pct, Bene_CC_PH_IschemicHeart_V2_Pct, Bene_Avg_Risk_Scre)
  
# join to gen_hosp_connections
gen_hosp_connections <- gen_hosp_connections %>%
  left_join(util, by=c("MCRNUM"="Rndrng_Prvdr_CCN", "TaxYr"="year"))

gen_hosp_connections <- gen_hosp_connections %>%
  filter(TaxYr %in% 2017:2021)

observe <- gen_hosp_connections %>%
  filter(!is.na(Rndrng_Prvdr_RUCA_Desc)) 

# fill missing variables when applicable
gen_hosp_connections <- gen_hosp_connections %>%
  group_by(filer_id) %>%
  fill(GENBD:OTHBD94, HRRCODE, .direction = "downup") %>%
  ungroup()

# create variable for how concentrated services are (hhi from AHA beds)
gen_hosp_connections <- gen_hosp_connections %>%
  mutate(hhi = (GENBD/HOSPBD)^2 + (PEDBD/HOSPBD)^2 + (OBBD/HOSPBD)^2 + (MSICBD/HOSPBD)^2 + (CICBD/HOSPBD)^2 + (NICBD/HOSPBD)^2 + (NINTBD/HOSPBD)^2 + 
           (PEDICBD/HOSPBD)^2 + (BRNBD/HOSPBD)^2 + (SPCICBD/HOSPBD)^2 + (OTHICBD/HOSPBD)^2 + (REHABBD/HOSPBD)^2 + (ALCHBD/HOSPBD)^2 + 
           (PSYBD/HOSPBD)^2 + (SNBD88/HOSPBD)^2 + (ICFBD88/HOSPBD)^2 + (ACULTBD/HOSPBD)^2 + (OTHLBD94/HOSPBD)^2 + (OTHBD94/HOSPBD)^2)

# create variable for how concentrated patients are in cancer, chronic kidney, COPD, and heart failure from the CMS data
gen_hosp_connections <- gen_hosp_connections %>%
  mutate(cancer = Bene_CC_PH_Cancer6_V2_Pct,
         kidney = Bene_CC_PH_CKD_V2_Pct,
         copd = Bene_CC_PH_COPD_V2_Pct,
         heart = Bene_CC_PH_IschemicHeart_V2_Pct) %>%
  mutate(hhi_cms = (cancer)^2 + (kidney)^2 + (copd)^2 + (heart)^2)


# Create variables measuring the overlap in services of hospitals in the same HRR

# create new variables for whether each service is offered or not
gen_hosp_connections <- gen_hosp_connections %>%
  mutate(GEN = ifelse(GENBD>0, 1, 0),
         PED = ifelse(PEDBD>0, 1, 0),
         OB = ifelse(OBBD>0, 1, 0),
         MSIC = ifelse(MSICBD>0, 1, 0),
         CIC = ifelse(CICBD>0, 1, 0),
         NIC = ifelse(NICBD>0, 1, 0),
         NINT = ifelse(NINTBD>0, 1, 0),
         PEDIC = ifelse(PEDICBD>0, 1, 0),
         BRN = ifelse(BRNBD>0, 1, 0),
         SPCIC = ifelse(SPCICBD>0, 1, 0),
         OTHIC = ifelse(OTHICBD>0, 1, 0),
         REHAB = ifelse(REHABBD>0, 1, 0),
         ALCH = ifelse(ALCHBD>0, 1, 0),
         PSY = ifelse(PSYBD>0, 1, 0),
         SN = ifelse(SNBD88>0, 1, 0),
         ICF = ifelse(ICFBD88>0, 1, 0),
         ACULT = ifelse(ACULTBD>0, 1, 0),
         OTHL = ifelse(OTHLBD94>0, 1, 0),
         OTH = ifelse(OTHBD94>0, 1, 0))

# same but for CMS
gen_hosp_connections <- gen_hosp_connections %>%
  mutate(cancer = ifelse(!is.na(cancer) & cancer>0, 1, 0),
         kidney = ifelse(!is.na(kidney) & kidney>0, 1, 0),
         copd = ifelse(!is.na(copd) & copd>0, 1, 0),
         heart = ifelse(!is.na(heart) & heart>0, 1, 0))

bed_cols <- c("GEN", "PED", "OB", "MSIC", "CIC", "NIC", "NINT", "PEDIC", "BRN", "SPCIC", "OTHIC", "REHAB", "ALCH", "PSY", "SN", "ICF", "ACULT", "OTHL", "OTH")

compute_jaccard_similarity <- function(df){
  df %>%
    group_by(TaxYr, HRRCODE) %>%
    mutate(
      jaccard_similarity = map_dbl(row_number(), function(i) {
        a <- as.numeric(df[i, bed_cols])  # Current hospital's services
        
        # Compute Jaccard similarity with all other hospitals in the same HRR
        similarities <- map_dbl(setdiff(row_number(), i), function(j) {
          b <- as.numeric(df[j, bed_cols])  # Other hospital's services
          
          if (sum(a | b) == 0) return(NA)  # Avoid division by zero
          
          sum(a & b) / sum(a | b)  # Jaccard similarity formula
        })
        
        mean(similarities, na.rm = TRUE)  # Average similarity across all comparisons
      })
    ) %>%
    ungroup()
}


gen_hosp_connections <- compute_jaccard_similarity(gen_hosp_connections) 

# rename jaccard variable
gen_hosp_connections <- gen_hosp_connections %>%
  rename(jaccard_similarity_aha = jaccard_similarity)

bed_cols <- c("cancer", "kidney", "copd", "heart")

gen_hosp_connections <- compute_jaccard_similarity(gen_hosp_connections) %>%
  rename(jaccard_similarity_cms = jaccard_similarity)

gen_hosp_connections <- gen_hosp_connections %>%
  mutate(academic = ifelse(MAPP5==1,1,0)) %>%
  mutate(metro = ifelse(str_detect(Rndrng_Prvdr_RUCA_Desc, "Metropolitan"),1,0)) 


# create summary stats table for connected vs. unconnected hospitals
table_data <- gen_hosp_connections %>%
  group_by(group) %>%
  summarise("Num Beds" = mean(HOSPBD, na.rm = TRUE),
            "Concentration of Services Offerered (AHA)" = mean(hhi, na.rm = TRUE),
            "Overlap within HRR (AHA)" = mean(jaccard_similarity_aha, na.rm = TRUE),
            "Concentration of Services Offered (CMS)" = mean(hhi_cms, na.rm=T),
            "Overlap within HRR (CMS)" = mean(jaccard_similarity_cms, na.rm=T),
            "Academic Med. Center" = mean(academic, na.rm=T),
            "Metropolitan Area" = mean(metro, na.rm=T),
            "Has a NICU" = mean(NIC, na.rm=T),
            "Has a Cath Lab" = mean(ACLABHOS, na.rm=T),
            "Total Benes" = mean(Tot_Benes, na.rm=T),
            "Average Patient Age" = mean(Bene_Avg_Age, na.rm=T)) %>%
  t() %>%
  as.data.frame() %>%
  # make row names into its own column
  tibble::rownames_to_column("Variable") 
colnames(table_data) <- table_data[1,]
table_data <- table_data %>%
  filter(group!="group") %>%
  mutate_at(vars(-group), as.numeric) 
  
  
kable(table_data, format = "latex",
        col.names = c("Variable", "General Connected to General", "General Connected to Specialty", "Specialty Connected to General", "General Unconnected", "Specialty Unconnected"),
        caption = "Summary Statistics",
        row.names = FALSE,
        table.envir="table",
        digits=2,
        booktabs=TRUE,
        escape=F,
        align=c("l","c","c","c","c","c"),
        position="ht!") %>%
  write("Objects//hospital_connected_summary.tex")

# graph concentration of services by connected vs. unconnected
gen_hosp_connections %>%
  filter(TaxYr %in% 2017:2021) %>%
  mutate(group = ifelse(gen_gen==1, "Gen - Gen Connected", "General Unconnected")) %>%
  group_by(TaxYr, group) %>%
  summarise("Concentration of Services" = mean(hhi, na.rm = TRUE)) %>%
  ggplot(aes(x=TaxYr, y=`Concentration of Services`, color=group)) +
  geom_line() +
  labs(title = "Concentration of Services by Connected vs. Unconnected Hospitals",
       x = "Year",
       y = "Concentration of Services") +
  theme_minimal() + xlim(2017,2021) + ylim(.2,.8) + 
  labs(color='')
ggsave("Objects//concentration_services_time.pdf", width=7, height=4)


# Make a summary stats table of observable features of connected vs. unconnected hospitals 

# create a variable for the first year a hospital pair is connected
minyr_connections <- ind_hosp_connections %>%
  filter(connected_ind_sameHRR==1) %>%
  group_by(Filer.EIN) %>%
  mutate(first_year_ind_connected = min(TaxYr)) %>%
  ungroup() %>%
  distinct(Filer.EIN, first_year_ind_connected)

ind_hosp_connections <- ind_hosp_connections %>%
  left_join(minyr_connections, by=c("Filer.EIN")) %>%
  mutate(first_year_ind_connected = ifelse(is.na(first_year_ind_connected), 0, first_year_ind_connected))

minyr_connections <- ind_hosp_connections %>%
  filter(connected_sys_sameHRR==1) %>%
  group_by(Filer.EIN) %>%
  mutate(first_year_sys_connected = min(TaxYr)) %>%
  ungroup() %>%
  distinct(Filer.EIN, first_year_sys_connected)

ind_hosp_connections <- ind_hosp_connections %>%
  left_join(minyr_connections, by=c("Filer.EIN")) %>%
  mutate(first_year_sys_connected = ifelse(is.na(first_year_sys_connected), 0, first_year_sys_connected))


ind_hosp_data <- ind_hosp_connections %>%
  distinct(Filer.EIN, filer_id, TaxYr, connected_ind_sameHRR, connected_sys_sameHRR, unconnected_in_HRR, 
           first_year_ind_connected, first_year_sys_connected) 


AHA_services <- AHA %>%
  select(YEAR, ID, GENBD, GENHOS, GENVEN, PEDBD, PEDHOS, PEDVEN, OBBD, OBHOS, OBVEN, MSICBD, MSICHOS, MSICVEN,
         CICBD, CICHOS, CICVEN, NICBD, NICHOS, NICVEN, NINTBD, NINTHOS, NINTVEN, PEDICBD, PEDICHOS, PEDICVEN, 
         BRNBD, BRNHOS, BRNVEN, SPCICBD, SPCICHOS, SPCICVEN, REHABBD, REHABHOS, REHABVEN, OTHICBD, ACULTBD,
         ALCHBD, ALCHHOS, ALCHVEN, PSYBD, PSYHOS, PSYVEN, SNBD88, SNHOS, SNVEN, ICFBD88, ICFHOS, ICFVEN,
         OTHLBD94, OTHBD94, HOSPBD, FTMT, FTRNTF, ICLABHOS, MCDDC, MCRDC, SERV) %>%
  filter(YEAR>=2016 & YEAR<=2022)

# create indicators for offering each type of service
AHA_services <- AHA_services %>%
  mutate(GEN = ifelse(GENBD>0, 1, 0),
         PED = ifelse(PEDBD>0, 1, 0),
         OB = ifelse(OBBD>0, 1, 0),
         MSIC = ifelse(MSICBD>0, 1, 0),
         CIC = ifelse(CICBD>0, 1, 0),
         NIC = ifelse(NICBD>0, 1, 0),
         NINT = ifelse(NINTBD>0, 1, 0),
         PEDIC = ifelse(PEDICBD>0, 1, 0),
         BRN = ifelse(BRNBD>0, 1, 0),
         SPCIC = ifelse(SPCICBD>0, 1, 0),
         OTHIC = ifelse(OTHICBD>0, 1, 0),
         REHAB = ifelse(REHABBD>0, 1, 0),
         ALCH = ifelse(ALCHBD>0, 1, 0),
         PSY = ifelse(PSYBD>0, 1, 0),
         SN = ifelse(SNBD88>0, 1, 0),
         ICF = ifelse(ICFBD88>0, 1, 0),
         ACULT = ifelse(ACULTBD>0, 1, 0),
         OTHL = ifelse(OTHLBD94>0, 1, 0),
         OTH = ifelse(OTHBD94>0, 1, 0))

# create variable for number of physicians/nurses per bed
AHA_services <- AHA_services %>%
  mutate(physicians_per_bed = FTMT/HOSPBD,
         nurses_per_bed = FTRNTF/HOSPBD)

# Create measure of concentration of services offered using HHI formula with number of beds in each service GENBD:OTHBD94
AHA_services <- AHA_services %>%
  mutate(hhi = (GENBD/HOSPBD)^2 + (PEDBD/HOSPBD)^2 + (OBBD/HOSPBD)^2 + (MSICBD/HOSPBD)^2 + (CICBD/HOSPBD)^2 + (NICBD/HOSPBD)^2 + (NINTBD/HOSPBD)^2 + 
           (PEDICBD/HOSPBD)^2 + (BRNBD/HOSPBD)^2 + (SPCICBD/HOSPBD)^2 + (OTHICBD/HOSPBD)^2 + (REHABBD/HOSPBD)^2 + (ALCHBD/HOSPBD)^2 + 
           (PSYBD/HOSPBD)^2 + (SNBD88/HOSPBD)^2 + (ICFBD88/HOSPBD)^2 + (ACULTBD/HOSPBD)^2 + (OTHLBD94/HOSPBD)^2 + (OTHBD94/HOSPBD)^2)

# create indicator for whether the hospital has a NICU and cath lab
AHA_services <- AHA_services %>%
  mutate(NICU = ifelse(NICBD>0, 1, 0),
         cath_lab = ICLABHOS)

# create variable for number of services offered
AHA_services <- AHA_services %>%
  mutate(num_services = rowSums(select(AHA_services, GEN:OTH), na.rm = TRUE))

# create variables for medicare/medicaid patients per bed
AHA_services <- AHA_services %>%
  mutate(MCDDC = MCDDC/HOSPBD,
         MCRDC = MCRDC/HOSPBD)

# keep only variables I need
AHA_services <- AHA_services %>%
  select(YEAR, ID, HOSPBD, physicians_per_bed, nurses_per_bed, num_services, hhi, NICU, cath_lab, MCDDC, MCRDC)

# join AHA data with hospital data
ind_hosp_data <- ind_hosp_data %>%
  left_join(AHA_services, by=c("filer_id"="ID", "TaxYr"="YEAR"))

# create summary stats table for connected vs. unconnected
ind_summary_table <- ind_hosp_data %>%
  filter(TaxYr %in% 2017:2021) %>%
  mutate(group = ifelse(connected_ind_sameHRR==1 & connected_sys_sameHRR==0, "Connected to Ind.", NA)) %>%
  mutate(group = ifelse(connected_ind_sameHRR==1 & connected_sys_sameHRR==1, "Connected to Both", group)) %>%
  mutate(group = ifelse(connected_ind_sameHRR==0 & connected_sys_sameHRR==1, "Connected to Sys.", group)) %>%
  mutate(group = ifelse(connected_ind_sameHRR==0 & connected_sys_sameHRR==0, "Unconnected", group)) %>%
  group_by(group) %>%
  summarise("Num Beds" = mean(HOSPBD, na.rm = TRUE),
            "Num Physicians/Bed" = mean(physicians_per_bed, na.rm = TRUE),
            "Num Nurses/Bed" = mean(nurses_per_bed, na.rm = TRUE),
            "Number of Services Offered (Max 16)" = mean(num_services, na.rm = TRUE),
            "Concentration of Services" = mean(hhi, na.rm = TRUE),
            "Has a NICU" = mean(NICU, na.rm = TRUE),
            "Has a Cath Lab" = mean(cath_lab, na.rm = TRUE),
            "Medicaid discharges/Bed" = mean(MCDDC, na.rm = TRUE),
            "Medicare discharges/Bed" = mean(MCRDC, na.rm = TRUE))

# transpose with variable names as additional columns
ind_summary_table <- ind_summary_table %>%
  pivot_longer(cols = `Num Beds`:`Medicare discharges/Bed`, names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = group, values_from = value) 

# add number of unique systems as variable labeled "n"
ind_summary_table <- ind_summary_table %>%
  add_row(variable = "n", `Connected to Both` = round(nrow(ind_hosp_data %>% filter(connected_ind_sameHRR==1 & connected_sys_sameHRR==1))/5,0),
                 `Connected to Ind.` = round(nrow(ind_hosp_data %>% filter(connected_ind_sameHRR==1 & connected_sys_sameHRR==0))/5,0),
                 `Connected to Sys.` = round(nrow(ind_hosp_data %>% filter(connected_ind_sameHRR==0 & connected_sys_sameHRR==1))/5,0),
                 `Unconnected` = round(nrow(ind_hosp_data %>% filter(connected_ind_sameHRR==0 & connected_sys_sameHRR==0))/5,0))


# round values to 2 digits
ind_summary_table <- ind_summary_table %>%
  mutate(across(where(is.numeric), ~round(., 2)))

# create a knitr table
table <- knitr::kable(ind_summary_table, format = "latex",
                      col.names = c("Variable", "Connected to Both", "Connected to Ind.", "Connected to Sys.", "Unconnected"),
                      caption = "Averages of Independent Hospitals",
                      row.names = FALSE,
                      table.envir="table",
                      digits=2,
                      booktabs=TRUE,
                      escape=F,
                      align=c("l","c","c","c","c"),
                      position="ht!") %>%
  kable_styling(full_width=F)
# save as a tex file
write(table, file="Objects//means_independent_table.tex")

# Create a table capturing summary statistics of HRRs in each category


# graph average concentration of services over time for unconnected vs. connected hospitals
ind_hosp_data %>%
  filter(TaxYr %in% 2017:2021) %>%
  mutate(group = ifelse(connected_ind_sameHRR==1 & connected_sys_sameHRR==0, "Connected to Ind.", NA)) %>%
  mutate(group = ifelse(connected_ind_sameHRR==1 & connected_sys_sameHRR==1, "Connected to Both", group)) %>%
  mutate(group = ifelse(connected_ind_sameHRR==0 & connected_sys_sameHRR==1, "Connected to Sys.", group)) %>%
  mutate(group = ifelse(connected_ind_sameHRR==0 & connected_sys_sameHRR==0, "Unconnected", group)) %>%
  group_by(group, TaxYr) %>%
  summarise(hhi = mean(hhi, na.rm = TRUE)) %>%
  ggplot(aes(x=TaxYr, y=hhi, color=group)) +
  geom_line() +
  geom_point() +
  labs(title = "Average concentration of services offered by hospitals",
       x = "Year",
       y = "HHI") +
  theme_minimal() + xlim(2017,2021) + ylim(0,1)
ggsave("Objects//connected_independent_hhi.pdf", width=7, height=4)

# graph average hhi, grouping by first_year_connected
ind_hosp_data %>%
  filter(first_year_ind_connected %in% c(0,2018,2019,2020)) %>%
  group_by(first_year_ind_connected, TaxYr) %>%
  summarise(hhi = mean(hhi, na.rm = TRUE)) %>%
  ggplot(aes(x=TaxYr, y=hhi, color=as.factor(first_year_ind_connected))) +
  geom_line() +
  geom_point() +
  labs(title = "Average concentration of services offered by hospitals",
       x = "Year",
       y = "HHI") +
  theme_minimal() + xlim(2016,2022) + ylim(0,1)
  # there's just such a small number of hospitals in each category

# graph using relative year
ind_hosp_data %>%
  mutate(rel_year = TaxYr - first_year_ind_connected) %>%
  group_by(rel_year) %>%
  filter(first_year_ind_connected %in% c(2019)) %>%
  summarise(hhi = mean(hhi, na.rm = TRUE)) %>%
  ggplot(aes(x=rel_year, y=hhi)) +
  geom_line() +
  geom_point() +
  labs(title = "Average concentration of services offered by hospitals",
       x = "Relative Year",
       y = "HHI") +
  theme_minimal() + xlim(-2,2) + ylim(0,1)



# Create a summary stats table looking at the difference of two hospitals in a pair

pairs <- hospital_connections %>%
  filter(connected_ind==1) %>%
  select(filer_id, other_id, TaxYr) %>%
  distinct()

# join AHA data to the filer and other
pairs <- pairs %>%
  left_join(AHA, by=c("filer_id"="ID", "TaxYr"="YEAR")) %>%
  rename(filer_HOSPBD = HOSPBD, filer_physicians_per_bed = physicians_per_bed, filer_nurses_per_bed = nurses_per_bed, 
         filer_num_services = num_services, filer_hhi = hhi, filer_NICU = NICU, filer_cath_lab = cath_lab, 
         filer_MCDDC = MCDDC, filer_MCRDC = MCRDC) %>%
  left_join(AHA, by=c("other_id"="ID", "TaxYr"="YEAR")) %>%
  rename(other_HOSPBD = HOSPBD, other_physicians_per_bed = physicians_per_bed, other_nurses_per_bed = nurses_per_bed, 
         other_num_services = num_services, other_hhi = hhi, other_NICU = NICU, other_cath_lab = cath_lab, 
         other_MCDDC = MCDDC, other_MCRDC = MCRDC)

# for each variable, calculate the max and min within the pair
pairs <- pairs %>%
  rowwise %>%
  mutate(min_hospbd = min(filer_HOSPBD, other_HOSPBD),
         max_hospbd = max(filer_HOSPBD, other_HOSPBD),
         min_physicians_per_bed = min(filer_physicians_per_bed, other_physicians_per_bed),
         max_physicians_per_bed = max(filer_physicians_per_bed, other_physicians_per_bed),
         min_nurses_per_bed = min(filer_nurses_per_bed, other_nurses_per_bed),
         max_nurses_per_bed = max(filer_nurses_per_bed, other_nurses_per_bed),
         min_num_services = min(filer_num_services, other_num_services),
         max_num_services = max(filer_num_services, other_num_services),
         min_hhi = min(filer_hhi, other_hhi),
         max_hhi = max(filer_hhi, other_hhi),
         min_NICU = min(filer_NICU, other_NICU),
         max_NICU = max(filer_NICU, other_NICU),
         min_cath_lab = min(filer_cath_lab, other_cath_lab),
         max_cath_lab = max(filer_cath_lab, other_cath_lab),
         min_MCDDC = min(filer_MCDDC, other_MCDDC),
         max_MCDDC = max(filer_MCDDC, other_MCDDC),
         min_MCRDC = min(filer_MCRDC, other_MCRDC),
         max_MCRDC = max(filer_MCRDC, other_MCRDC))

# calculate the average of the min and maxvariables in each year
pairs_avgs <- pairs %>%
  ungroup() %>%
  summarise(min_hospbd = mean(min_hospbd, na.rm = TRUE),
            max_hospbd = mean(max_hospbd, na.rm = TRUE),
            min_physicians_per_bed = mean(min_physicians_per_bed, na.rm = TRUE),
            max_physicians_per_bed = mean(max_physicians_per_bed, na.rm = TRUE),
            min_nurses_per_bed = mean(min_nurses_per_bed, na.rm = TRUE),
            max_nurses_per_bed = mean(max_nurses_per_bed, na.rm = TRUE),
            min_num_services = mean(min_num_services, na.rm = TRUE),
            max_num_services = mean(max_num_services, na.rm = TRUE),
            min_hhi = mean(min_hhi, na.rm = TRUE),
            max_hhi = mean(max_hhi, na.rm = TRUE),
            min_NICU = mean(min_NICU, na.rm = TRUE),
            max_NICU = mean(max_NICU, na.rm = TRUE),
            min_cath_lab = mean(min_cath_lab, na.rm = TRUE),
            max_cath_lab = mean(max_cath_lab, na.rm = TRUE),
            min_MCDDC = mean(min_MCDDC, na.rm = TRUE),
            max_MCDDC = mean(max_MCDDC, na.rm = TRUE),
            min_MCRDC = mean(min_MCRDC, na.rm = TRUE),
            max_MCRDC = mean(max_MCRDC, na.rm = TRUE))

# pivot data to where there is a column for variable, column for max, and column for min
pairs_avgs <- pairs_avgs %>%
  pivot_longer(cols = min_hospbd:max_MCRDC, names_to = "variable", values_to = "value") %>%
  mutate(type = ifelse(str_detect(variable, "min"), "Min", "Max")) %>%
  mutate(variable = str_remove(variable, "min_|max_")) %>%
  pivot_wider(names_from = type, values_from = value) %>%
  mutate(diff = Max - Min)

# rename variables to match the table
pairs_avgs <- pairs_avgs %>%
  mutate(variable = ifelse(variable=="hospbd", "Num Beds", variable),
         variable = ifelse(variable=="physicians_per_bed", "Num Physicians/Bed", variable),
         variable = ifelse(variable=="nurses_per_bed", "Num Nurses/Bed", variable),
         variable = ifelse(variable=="num_services", "Number of Services Offered (Max 16)", variable),
         variable = ifelse(variable=="hhi", "Concentration of Services", variable),
         variable = ifelse(variable=="NICU", "Has a NICU", variable),
         variable = ifelse(variable=="cath_lab", "Has a Cath Lab", variable),
         variable = ifelse(variable=="MCDDC", "Medicaid discharges/Bed", variable),
         variable = ifelse(variable=="MCRDC", "Medicare discharges/Bed", variable))

# calculate the regular averages for hospitals without any connections
uncon_avgs <- hosp_data %>%
  filter(connected_ind==0) %>%
  summarise("Num Beds" = mean(HOSPBD, na.rm = TRUE),
            "Num Physicians/Bed" = mean(physicians_per_bed, na.rm = TRUE),
            "Num Nurses/Bed" = mean(nurses_per_bed, na.rm = TRUE),
            "Number of Services Offered (Max 16)" = mean(num_services, na.rm = TRUE),
            "Concentration of Services" = mean(hhi, na.rm = TRUE),
            "Has a NICU" = mean(NICU, na.rm = TRUE),
            "Has a Cath Lab" = mean(cath_lab, na.rm = TRUE),
            "Medicaid discharges/Bed" = mean(MCDDC, na.rm = TRUE),
            "Medicare discharges/Bed" = mean(MCRDC, na.rm = TRUE))

# pivot data
uncon_avgs <- uncon_avgs %>%
  pivot_longer(cols = `Num Beds`:`Medicare discharges/Bed`, names_to = "variable", values_to = "unconnected average") 

# merge the data
pairs_avgs <- pairs_avgs %>%
  left_join(uncon_avgs, by="variable")

# round to 2 decimals
pairs_avgs <- pairs_avgs %>%
  mutate(across(where(is.numeric), ~round(., 2)))

# Make a knitr table
table <- knitr::kable(pairs_avgs, format = "latex",
                      col.names = c("Variable", "Connected Pair Avg Min", "Connected Pair Avg Max", "Range in Pair", "Unconnected Avg"),
                      caption = "Difference in Independent Hospitals with Common Board Members",
                      row.names = FALSE,
                      table.envir="table",
                      digits=2,
                      booktabs=TRUE,
                      escape=F,
                      align=c("l","c","c","c","c"),
                      position="ht!") %>%
  kable_styling(full_width=F)
write(table, file="Objects//connected_indpair_diff_table.tex")





# Do hospitals that become connected change their services offered?

