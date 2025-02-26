library(dplyr)
library(fuzzyjoin)
library(stringdist)
library(tidyr)
library(stringr)
library(ggplot2)
library(patchwork)
library(readr)

created_data_path <- "C:\\Users\\hkagele\\Downloads\\"

## Compare connected systems to unconnected systems 


###### PART 1: CLEAN DATA ###############################################################################################################################

# Read in cleaned up version of names found in 990 tax forms
people <- read_rds(paste0(created_data_path, "cleaned_people_data.rds"))

# Read is crosswalk of AHA ID - EIN, only keeping those who match to a standalone hospital
cw <- read_csv(paste0(created_data_path, "EIN_AHA_cw.csv")) %>%
  select(ID, ein_hosp) %>%
  filter(!is.na(ein_hosp))

# Clean up the people data 
# only keep people whose EIN is in the crosswalk as a standalone hospital
people <- people %>% filter(Filer.EIN %in% cw$ein_hosp)

# only keep people who are board members
board_people <- people %>%
  filter(board_member==1) %>%
  select(TaxYr, Filer.EIN, name_cleaned)

# remove empty names and one-word names
board_people <- board_people %>%
  filter(str_detect(name_cleaned, "[a-zA-Z]")) %>%
  filter(str_count(name_cleaned, " ")>0)

# join AHA data to get HRR and state
AHA <- read_csv(paste0(created_data_path, "AHAdata_20052023.csv")) %>%
  select(ID, YEAR, STCD, HRRCODE, SYSID) %>%
  filter(YEAR>=2015 & YEAR<=2023) %>%
  mutate(YEAR = as.character(YEAR))

board_people <- board_people %>%
  left_join(cw, by = c("Filer.EIN" = "ein_hosp")) %>%
  left_join(AHA, by = c("ID", "TaxYr"="YEAR"))

# only keep hospitals that belong to systems
board_people <- board_people %>%
  filter(!is.na(SYSID))

# run the script titled "function1_standardize_names"
source("Scripts//function1_standardize_names.R")

# combine names that have a slight misspelling using the standardize names function
# these have to be in the same EIN to be combined
board_people <- standardize_names(board_people, max_dist = 2)

# also standardize names that are a very close match within the same HRR and/or state
board_people <- board_people %>%
  rename(hrrcode = HRRCODE) %>%
  standardize_names_hrr(., max_dist = 1)

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

# wide to long in other_eins
board_people <- board_people %>%
  pivot_longer(cols = starts_with("other_eins"), names_to = "num_board", values_to = "other_ein")

# remove NA values in other_ein
board_people <- board_people %>%
  filter(!is.na(other_ein))

# trim the white space on other_ein
board_people$other_ein <- str_trim(board_people$other_ein)

# Bring in the AHA data to merge in system and geographic information on the hospitals
AHA <-  read_csv(paste0(created_data_path, "AHAdata_20052023.csv"))

# Keep AHA variables I want
AHA <- AHA %>%
  select(ID, YEAR, SYSID, STCD, LONG, LAT, HRRCODE) %>%
  filter(YEAR>=2016 & YEAR<=2022)

# Are there any duplicates in ID, YEAR?
AHA %>% 
  group_by(ID, YEAR) %>%
  summarise(n = n()) %>%
  filter(n > 1)

# Merge AHA data to the AHA - EIN crosswalk
cw <- cw %>%
  left_join(AHA, by = "ID") 

# Merge AHA data to the board_people data
board_people <- board_people %>%
  mutate(TaxYr = as.numeric(TaxYr)) %>%
  left_join(cw, by = c("Filer.EIN" = "ein_hosp", "TaxYr"="YEAR"))

# Rename variables to show that they are the filer geographic information
board_people <- board_people %>%
  rename(filer_id=ID, filer_sysid = SYSID, filer_stcd = STCD, filer_long = LONG, filer_lat = LAT, filer_hrrcode = HRRCODE)

# Merge AHA data to the other_ein data
board_people <- board_people %>%
  left_join(cw, by = c("other_ein" = "ein_hosp", "TaxYr"="YEAR"))

# Rename variables to show that they are the other_ein geographic information
board_people <- board_people %>%
  rename(other_id=ID, other_sysid = SYSID, other_stcd = STCD, other_long = LONG, other_lat = LAT, other_hrrcode = HRRCODE)

# Create hospital-level connections data
hospital_connections <- board_people %>%
  distinct(TaxYr, Filer.EIN, other_ein, filer_id, filer_sysid, filer_stcd, filer_long, filer_lat, filer_hrrcode,
           other_id, other_sysid, other_stcd, other_long, other_lat, other_hrrcode) 

# If a hospital has a connection in a given year, remove the empty other_ein row
hospital_connections <- hospital_connections %>%
  mutate(connected = ifelse(other_ein!="", 1, NA)) %>%
  group_by(TaxYr, Filer.EIN) %>%
  fill(connected, .direction = "downup") %>%
  ungroup() %>%
  mutate(connected = ifelse(is.na(connected), 0, connected)) %>%
  filter(!(other_ein=="" & connected==1))

# define connections within the same HRR
# 3 types of connections: unconnected, connected to hosp in the same system, connected to hosp in diff sys or no sys at all
hospital_connections <- hospital_connections %>%
  mutate(unconnected_in_HRR = ifelse(other_ein=="" | filer_hrrcode!=other_hrrcode, 1, 0)) %>%
  mutate(connected_samesys = ifelse(other_ein!="" & filer_hrrcode==other_hrrcode & filer_sysid==other_sysid, 1, 0)) %>%
  mutate(connected_diffsys = ifelse(other_ein!="" & filer_hrrcode==other_hrrcode & (filer_sysid!=other_sysid | is.na(other_sysid)), 1, 0))

observe <- hospital_connections %>%
  filter(unconnected_in_HRR==0 & connected_samesys==0 & connected_diffsys==0)

# how many hospitals are connected in same system each year?
hospital_connections %>%
  group_by(TaxYr) %>%
  summarise(n_connected = sum(connected_samesys),
            n_connected_diffsys = sum(connected_diffsys),
            n_unconnected = sum(unconnected_in_HRR))

# graph the percent of hospitals that are connected in the same system each year
hospital_connections %>%
  group_by(TaxYr) %>%
  summarise(n_connected = sum(connected_samesys),
            n_connected_diffsys = sum(connected_diffsys),
            n_unconnected = sum(unconnected_in_HRR)) %>%
  mutate(pct_connected = n_connected/(n_connected+n_connected_diffsys+n_unconnected)) %>%
  ggplot(aes(x=TaxYr, y=pct_connected)) +
  geom_line() +
  geom_point() +
  labs(title = "Percent of in-system hospitals that share a board member\nwithin their system and market",
       x = "Year",
       y = "Percent\n") +
  theme_minimal() + xlim(2016,2022) + ylim(-.1,1)
ggsave("Objects//connected_systems_percent.pdf", width=6, height=4)


# plot geographically the systems that are connected

plot_map <- function(df, year) {
  df_year <- df %>% filter(TaxYr == year)
  
  # Identify hospitals with at least one connection
  connected_hospitals <- df_year %>%
    filter(!is.na(other_ein) & other_ein != "") %>%
    filter(filer_hrrcode==other_hrrcode & filer_sysid==other_sysid) %>%
    select(Filer.EIN) %>%
    distinct()
  
  # Create nodes (all hospitals, even unconnected ones)
  nodes <- df_year %>%
    select(Filer.EIN, filer_lat, filer_long) %>%
    distinct(Filer.EIN, .keep_all = TRUE) %>%
    mutate(is_connected = ifelse(Filer.EIN %in% connected_hospitals$Filer.EIN, "Connected", "Unconnected"))
  
  
  # Filter hospitals with board connections
  df_filtered <- df_year %>%
    filter(!is.na(other_ein) & other_ein != "") %>%
    filter(filer_hrrcode==other_hrrcode & filer_sysid==other_sysid)
  
  
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
               size = 0.65) +
    scale_color_manual(values = c("Connected" = "blue", "Unconnected" = "red")) + 
    theme_minimal() +
    labs(title = paste("In-system hospitals that share a board member\nwithin their system and market -", year)) +
    coord_fixed(1.3)  +
    ylim(20,50) + xlim(-130,-60) +
    theme(legend.position = "none")
  
}

# read in map data
us_states <- map_data("state")


# 1. Geographic map of connection within the same HRR excluding systems

plot_map(hospital_connections, 2017)

# Generate maps for each year (2017-2022)
plots <- lapply(2017:2022, function(year) plot_map(hospital_connections, year))

# Combine plots into a single graphic
combined_plot <- wrap_plots(plots, ncol = 2)  

# save combined plot
ggsave("Objects//common_boards_insystem_maps.pdf", width = 8, height = 11, units = "in")



# Make a summary stats table of observable features of connected vs. unconnected hospitals ####

hosp_data <- hospital_connections %>%
  distinct(Filer.EIN, filer_id, TaxYr, connected_samesys, unconnected_in_HRR) 

# Read in AHA data and keep variables I need
AHA <- read_csv(paste0(created_data_path, "AHAdata_20052023.csv"))

AHA <- AHA %>%
  select(YEAR, ID, GENBD, PEDBD, OBBD, MSICBD, CICBD, NICBD, NINTBD, PEDICBD, BRNBD, SPCICBD, OTHICBD, REHABBD, ALCHBD, PSYBD, SNBD88, ICFBD88, ACULTBD, 
         OTHLBD94, OTHBD94, HOSPBD, FTMT, FTRNTF, ICLABHOS, MCDDC, MCRDC) %>%
  filter(YEAR>=2016 & YEAR<=2022)

# create indicators for offering each type of service
AHA <- AHA %>%
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
AHA <- AHA %>%
  mutate(physicians_per_bed = FTMT/HOSPBD,
         nurses_per_bed = FTRNTF/HOSPBD)

# Create measure of concentration of services offered using HHI formula with number of beds in each service GENBD:OTHBD94
AHA <- AHA %>%
  mutate(hhi = (GENBD/HOSPBD)^2 + (PEDBD/HOSPBD)^2 + (OBBD/HOSPBD)^2 + (MSICBD/HOSPBD)^2 + (CICBD/HOSPBD)^2 + (NICBD/HOSPBD)^2 + (NINTBD/HOSPBD)^2 + 
           (PEDICBD/HOSPBD)^2 + (BRNBD/HOSPBD)^2 + (SPCICBD/HOSPBD)^2 + (OTHICBD/HOSPBD)^2 + (REHABBD/HOSPBD)^2 + (ALCHBD/HOSPBD)^2 + 
           (PSYBD/HOSPBD)^2 + (SNBD88/HOSPBD)^2 + (ICFBD88/HOSPBD)^2 + (ACULTBD/HOSPBD)^2 + (OTHLBD94/HOSPBD)^2 + (OTHBD94/HOSPBD)^2)

# create indicator for whether the hospital has a NICU and cath lab
AHA <- AHA %>%
  mutate(NICU = ifelse(NICBD>0, 1, 0),
         cath_lab = ICLABHOS)

# create variable for number of services offered
AHA <- AHA %>%
  mutate(num_services = rowSums(select(AHA, GEN:OTH), na.rm = TRUE))

# create variables for medicare/medicaid patients per bed
AHA <- AHA %>%
  mutate(MCDDC = MCDDC/HOSPBD,
         MCRDC = MCRDC/HOSPBD)

# keep only variables I need
AHA <- AHA %>%
  select(YEAR, ID, HOSPBD, physicians_per_bed, nurses_per_bed, num_services, hhi, NICU, cath_lab, MCDDC, MCRDC)

# join AHA data with hospital data
hosp_data <- hosp_data %>%
  left_join(AHA, by=c("filer_id"="ID", "TaxYr"="YEAR"))

# create summary stats table for connected vs. unconnected
summary_table <- hosp_data %>%
  group_by(connected_samesys) %>%
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
summary_table <- summary_table %>%
  pivot_longer(cols = `Num Beds`:`Medicare discharges/Bed`, names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = connected_samesys, values_from = value) 

# add number of unique systems as variable labeled "n"
summary_table <- summary_table %>%
  add_row(variable = "n", `0` = round(nrow(hosp_data %>% filter(connected_samesys==0))/7,0),
          `1` = round(nrow(hosp_data %>% filter(connected_samesys==1))/7,0)) 

# round values to 2 digits
summary_table <- summary_table %>%
  mutate(across(where(is.numeric), ~round(., 2)))

# create a knitr table
table <- knitr::kable(summary_table, format = "latex",
      col.names = c("Variable", "Unconnected", "Connected in same system"),
      caption = "Means for in-system hospitals")
# save as a tex file
write(table, file="Objects//means_insystem_table.tex")


# Create a summary stats table looking at the difference of two hospitals in a pair

pairs <- hospital_connections %>%
  filter(connected_samesys==1) %>%
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

# create variable for medicare/medicaid discharges per bed
pairs <- pairs %>%
  mutate(filer_MCDDC = filer_MCDDC/filer_HOSPBD,
         filer_MCRDC = filer_MCRDC/filer_HOSPBD,
         other_MCDDC = other_MCDDC/other_HOSPBD,
         other_MCRDC = other_MCRDC/other_HOSPBD)

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
  filter(connected_samesys==0) %>%
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
      caption = "Difference in Hospitals with Common Board Members in the Same System")
write(table, file="Objects//connected_syspair_diff_table.tex")







