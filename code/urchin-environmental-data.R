# Urchin environmental data
# July 2023

# set up ------------------------------------------------------------------

# 1. set up ---------------------------------------------------------------

# ⊣ a. packages -----------------------------------------------------------

library(tidyverse) # general cleaning and use
library(here) # helps organize files
library(readxl) # reads in excel files
library(janitor) # helps clean up data frames

# ⊣ b. data ---------------------------------------------------------------

sst <- read_xlsx(here("data", "ZOE_RS_Data.xlsx"), sheet = "SST", na = c("NA", "NaN"))
no3 <- read_xlsx(here("data", "ZOE_RS_Data.xlsx"), sheet = "NO3", na = c("NA", "NaN"))
npp <- read_xlsx(here("data", "ZOE_RS_Data.xlsx"), sheet = "NPP", na = c("NA", "NaN"))
chl <- read_xlsx(here("data", "ZOE_RS_Data.xlsx"), sheet = "CHL", na = c("NA", "NaN"))


# 2. cleaning -------------------------------------------------------------

# ⊣ a. cleaning each environmental data set -------------------------------

# ⊣ ⊣ i. cleaning function ------------------------------------------------

cleaning_fxn <- function(type) {
  
  # function set up part 1: choose the right data frame based on the "type" ----
  df <- if(type == "sst") {
    sst
  } else if(type == "no3") {
    no3
  } else if(type == "npp") {
    npp
  } else if(type == "chl") {
    chl
  } else {
    warning("Did you provide a data type? Double check!")
    return(NA)
  }
  
  # function set up part 2: create a label for the column of values based on the data type ----
  label <- if(type == "sst") {
    "sst_c"
  } else if(type == "no3") {
    "no3_umol_l_1"
  } else if(type == "npp") {
    "npp_mg_c_m_2_d_1"
  } else if(type == "chl") {
    "chl_mg_m_3"
  } else {
    warning("Did you provide a data type? Double check!")
    return(NA)
  }
  
  # what the actual function does ----
  # takes the data frame name as an argument and then
  df %>% 
    # makes column names easier to parse using function from {janitor} and then
    clean_names() %>% 
    # removes the "units" column because the units are going into the value column label and then
    select(!(contains("units"))) %>% 
    # puts the data frame into "tidy" format, where each row is an observation and then
    pivot_longer(cols = f_10:g_100, names_to = "name", values_to = label) %>% 
    # separates the column with site_scale into two separate columns and then
    separate_wider_delim(cols = name, delim = "_", names = c("site", "scale_km")) %>% 
    # makes an "observation ID": some information that allows you to uniquely identify each observation
    unite("obs_ID", year, month_s, day_s, site, scale_km, remove = FALSE)
}


# ⊣ ⊣ ii. using the function ----------------------------------------------

sst_clean <- cleaning_fxn("sst")
no3_clean <- cleaning_fxn("no3")
npp_clean <- cleaning_fxn("npp")
chl_clean <- cleaning_fxn("chl")



# ⊣ b. combining data frames ----------------------------------------------

# create a data frame called env_full starting with sst_clean
env_full <- sst_clean %>% 
  # select only columns of interest
  select(obs_ID, sst_c) %>% 
  
  # joining ----
# selecting columns of interest from no3_clean (only observation ID and the value)
# joining with sst_clean
full_join(select(no3_clean, obs_ID, no3_umol_l_1), 
          # setting this relationship because of the multiple values thing
          relationship = "many-to-many", 
          # joining both data frames by the obs_ID column
          by = "obs_ID") %>% 
  # selecting columns of interest from npp_clean and joining
  full_join(select(npp_clean, obs_ID, npp_mg_c_m_2_d_1), 
            by = "obs_ID") %>% 
  # selecting columns of interest from chl_clean and joining
  full_join(select(chl_clean, obs_ID, chl_mg_m_3),
            by = "obs_ID") %>% 
  
  # getting the metadata ----
# separating the obs_ID column into the meaningful components
separate_wider_delim(cols = "obs_ID", delim = "_", 
                     names = c("year", "month_s", "day_s", "site", "scale_km")) %>% 
  # creating a column for date by combining year, month_s, day_s
  unite("date", year, month_s, day_s, sep = "-") %>% 
  
  # general cleaning ----
# making sure the date is actually read as a date
mutate(date = as_date(date),
       # replacing the site abbreviations with full site names
       site = case_when(
         site == "f" ~ "fort bragg",
         site == "g" ~ "gaviota"
       ),
       # making sure the scales are categories in numerical order
       scale_km = fct_relevel(as_factor(scale_km), "10", "25", "50", "100")) %>% 
  # arranging the whole data frame by date to make it easier to parse
  arrange(date)



# 3. getting the date window ----------------------------------------------

# function to get the date window ----
get_window <- function(date, size) {
  # choosing a start and end ----
  # start: just whatever the date is minus the size of the window you choose
  start <- as_date(date) - size
  # end: the date you choose
  end <- as_date(date)
  
  # filtering the data frame ----
  # use the full data frame
  env_full %>% 
    # filter the data frame to only include dates that are equal to or after the start and equal to or before the end
    filter(date >= start & date <= end)
}

# function to get the window summary ----
window_summary <- function(df) {
  
  df %>% 
    # grouping the data frame by site and scale_km
    # important! this function assumes the data frame has those two columns named these things
    # if not, it might not work!
    group_by(site, scale_km) %>% 
    
    # summarizing ----
  summarize(sst_mean = mean(sst_c, na.rm = TRUE),
            sst_var = var(sst_c, na.rm = TRUE), 
            sst_n = sum(!is.na(sst_c)),
            
            no3_mean = mean(no3_umol_l_1, na.rm = TRUE),
            no3_var = var(no3_umol_l_1, na.rm = TRUE),
            no3_n = sum(!is.na(no3_umol_l_1)),
            
            npp_mean = mean(npp_mg_c_m_2_d_1, na.rm = TRUE),
            npp_var = var(npp_mg_c_m_2_d_1, na.rm = TRUE),
            npp_n = sum(!is.na(npp_mg_c_m_2_d_1)),
            
            chl_mean = mean(chl_mg_m_3, na.rm = TRUE),
            chl_var = var(chl_mg_m_3, na.rm = TRUE),
            chl_n = sum(!is.na(chl_mg_m_3))
            
  ) 
}

get_window(date = "2019-03-15", size = 30) %>% 
  window_summary()



