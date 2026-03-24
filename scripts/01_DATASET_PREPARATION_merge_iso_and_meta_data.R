################################################################################
# Author: Flavian Tschurr
# Email: flavian.tschurr@usys.ethz.ch 
# Project: NiWUE data merging with meta
# Date: 20260104
# Script purpose: read in isotope data and link them to UID
################################################################################
# Load libraries
# list of required packages
packages <- c("readxl", "dplyr","tidyr")
# Loop through and install/load each one
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

library(readxl)
library(tidyr)
library(dplyr)

base_path <- "PATH/TO/YOUR/GIT/DIR"
################################################################################

# read in look up table to define treatment_levels
treatment_definer_lookup <- read.csv(file.path(base_path,"data",
                                              "treatment_definer.csv"),sep=";",
                                     encoding = "UTF-8")

names(treatment_definer_lookup)[1] <- "trial_name"

# retrieve meta data from zenodo
record_id <- "19201634"
isodata_file_name <- "winter_wheat_isotopes_management.csv" 

# Read the data directly into R
# Note: Zenodo might redirect, read_csv handles this automatically
iso_data <- read_csv(paste0("https://zenodo.org/api/records/", record_id,
                            "/files/",isodata_file_name, "/content"))

#clean cols to be filled in the next steps
meta_data <- iso_data[,-which(names(iso_data) %in% 
                          c("d13C","d15N","C_content","N_content","C_N_ratio"))]


# load preprocessed isotope data (extracted from .xlsx files to csv, and matched
# with current nameing conventions i.e. UID)
preprocessed_data_path <- file.path(base_path,"data","preprocessed")
#select all raw files
preprocessed_files <- list.files(preprocessed_data_path, pattern = ".csv")
remeasured_list <- readRDS(list.files(preprocessed_data_path, pattern = ".rds",
                                      full.names = T))

################################################################################

data_list <- list()

meta_data$yield_dt_ha <- as.numeric(meta_data$yield_dt_ha)
# homogenize yield data to dry matter, AS, QA, P24A, PROspective, DFG Wheat,
# FAST and Reload are already in DM
for(i in 1:length(meta_data$UID)){
  trial <- meta_data[i,"location_ID"]
  # browser()
  if(trial == "BL"){
    meta_data[i,"yield_dt_ha"]<- as.numeric(meta_data[i,"yield_dt_ha"])*0.86
  }else if( trial =="DOK"){ # DOK before 2019 is 90% DM for straw and 85% for grain
    if(as.numeric(meta_data[i,"year"]) < 2019){
      if(meta_data[i,"material"] =="straw"){
        meta_data[i,"yield_dt_ha"]<- as.numeric(meta_data[i,"yield_dt_ha"])*0.9
        
      }else{
        meta_data[i,"yield_dt_ha"]<- as.numeric(meta_data[i,"yield_dt_ha"])*0.85
        
      } 
      
    }else if(trial == "Rothamsted"){ 
      meta_data[i,"yield_dt_ha"]<- as.numeric(meta_data[i,"yield_dt_ha"])*0.85
      
    }
  }
}


#loop over traits in the preprocessed files
for(trait in  c("d15N","d13C","C_N_ratio")){
  files_of_interest <- grep(trait, preprocessed_files, value=T)
  data_list[[trait]] <- do.call("rbind",lapply(file.path(preprocessed_data_path,
                                                         files_of_interest),
                         read.csv))
  if(trait == "d15N"){
    content_namer = "N_content"
  }else if(trait == "d13C"){
    content_namer = "C_content"

  }else{
    content_namer="percentage"
  }
  names(data_list[[trait]]) <- gsub("percentage",content_namer,
                                    names(data_list[[trait]]))
  names(data_list[[trait]]) <- gsub("delta",trait, names( data_list[[trait]]))
  data_list[[trait]]$UID <- gsub("TU_Berlin","DFG",data_list[[trait]]$UID)
}

names(data_list[["C_N_ratio"]]) <- gsub("percentage_N","N_content",
                                        names(data_list[["C_N_ratio"]]))



# merge delta, concentrations and C:N ratio
merged <- left_join(meta_data,data_list[["d13C"]][,c("UID","d13C","C_content")],
                    by="UID")
merged <- left_join(merged,data_list[["d15N"]][,c("UID","d15N")], by="UID")
merged <- left_join(merged,data_list[["C_N_ratio"]][,c("UID","C_N_ratio",
                                                       "N_content")], by="UID")


# define a treatment_level accoridng to hte look up table
merged$treatment_level <- NA
for( i in 1:length(merged$location)){
  trial_n <- merged$trial_name[i]
  one_trial <- subset(treatment_definer_lookup, trial_name == trial_n)
  
  col_oI <- one_trial$defining_col[1]
  
  merged$treatment_level[i] <- one_trial$treatment_name[
    which(one_trial$defining_value == merged[[col_oI]][i])]
  
}


head(merged)
# covert yield from dt to t per ha
merged$yield_dt_ha <- as.numeric(merged$yield_dt_ha)/10
names(merged) <- gsub("yield_dt_ha","yield_t_ha",names(merged))
names(merged) <- gsub("location_ID","trial_ID",names(merged))


# rearrange the columns to a meaningful order
merged <- merged %>%
  relocate(treatment_level, .after = rep) %>%
  relocate(d13C, d15N,C_content,N_content,C_N_ratio, .after = variety)



# check d15N of Rothamsted --> enrichment experiment in 1980
rh <- subset(merged, trial_ID =="Rothamsted")
plot(rh$year,rh$d15N)

# define cut off threshild
threshold_before_enrichment <- round(max(subset(rh, year < 1978)$d15N, 
                                         na.rm = T))

check <- merged[which(merged$d15N > threshold_before_enrichment),]
# add values to the comments
merged$comments_  <- ifelse(merged$d15N > threshold_before_enrichment,paste0(
  "due to prior experiment with labelled d15N, this value 
  (above the max prior experiment threshold of 11) was excluded: "
  ,merged$d15N), merged$comments)

merged$d15N <- ifelse(merged$d15N > threshold_before_enrichment,NA, merged$d15N)

# some values must have been measured twice. 
# here we choose the correct measurement
for(uid in merged$UID){
  if(uid %in% names(remeasured_list)){
    remeasured_list[[uid]]
    d15N_value <- remeasured_list[[uid]][["N"]]$delta
    d13C_value <- remeasured_list[[uid]][["C"]]$delta
    C_N_ratio_value <- remeasured_list[[uid]][[
      "C"]]$percentage/remeasured_list[[uid]][["N"]]$percentage
    merged[which(merged$UID == uid),"d15N"] <- d15N_value
    merged[which(merged$UID == uid),"d13C"] <- d13C_value
    merged[which(merged$UID == uid),"C_N_ratio"] <- C_N_ratio_value
  }  
}


# check how many samples are available
sum(!is.na(merged$d13C))
sum(!is.na(merged$d15N))
sum(!is.na(merged$C_N_ratio))


################################################################################
# create files for publication
isotopes_management_df <- merged[,-which(names(merged) %in% c("yield_t_ha",
                                                        "comments_","...22" ))]

# management and iso data file
write.csv(isotopes_management_df, file.path(base_path,"data",
                        "winter_wheat_isotopes_management.csv"), row.names = F)


## yield data file 

yield_df <- merged[,which(names(merged) %in% c("location",
                                               "trial_ID",
                                               "trial_name",
                                               "year",
                                               "UID",
                                               "material", "yield_t_ha", 
                                               "responsible_person"  ))]

# we add the corresponding DOI of already published data

# broadbalk yields 1851-1925: "DOI: 10.23637/RBK1-1796346264-1"
# broadbalk yields 1968-2022: "DOI: 10.23637/RBK1-YLD6822-01"
# broadbalk yields 1926-1967: "DOI: 10.23637/RBK1-YLD2667-01"
  
# "DFG" "DOI:10.1093/jxb/eraf191"
yield_df$reference_to_cite <- NA

for(i in 1:length(yield_df$trial_ID)){
  if(yield_df$trial_ID[i] == "DFG"){
    yield_df$reference_to_cite[i] = "DOI:10.1093/jxb/eraf191"
  }else if(yield_df$trial_ID[i] == "Rothamsted"){
    
    if(yield_df$year[i] %in% c(1851:1925)){
      yield_df$reference_to_cite[i] = "DOI: 10.23637/RBK1-1796346264-1"
    }else if(yield_df$year[i] %in% c(1926:1967)){
      yield_df$reference_to_cite[i] = "DOI: 10.23637/RBK1-YLD2667-01"
      
    }else if(yield_df$year[i] %in% c(1968:2022)){
      yield_df$reference_to_cite[i] = "DOI: 10.23637/RBK1-YLD6822-01"
      
    }else{
      yield_df$reference_to_cite[i] = NA
    }
  }
  
}


yield_df <- yield_df %>%
  relocate(reference_to_cite, .after = responsible_person)


write.csv(yield_df, file.path(base_path,"data","winter_wheat_yield.csv"), 
          row.names = F)


na_entries_df <- merged[which(is.na(merged$d13C)),]
write.csv(na_entries_df, file.path(base_path,"data","not_measured_yet.csv"), 
          row.names = F)
