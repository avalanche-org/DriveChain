rm(list=ls())
library(readxl)
library(dplyr)
library(plyr)
library(stringr)
library(outbreaks)
library(visNetwork)
library(epicontacts)

CoVid_19_patient_V2 = read_xlsx("Input_ConstructionOfChains.xlsx") 

id_missing_index = setdiff(CoVid_19_patient_V2$index_ID, CoVid_19_patient_V2$contact_ID) # Get IDs that do not have an index 
nb = str_count(id_missing_index, pattern = "-C") # Get the number of times the letter C appears in the ID 

while (sum(nb, na.rm = TRUE) >=1 & length(id_missing_index)>=1){
  id_missing_index = setdiff(CoVid_19_patient_V2$index_ID, CoVid_19_patient_V2$contact_ID)
  nb = str_count(id_missing_index, pattern = "-C")
  if (length(id_missing_index)>=1){
    source("Function_recup_index.R")
    index1 = recup_index(data = id_missing_index, "-C")
    index1
    names(index1) = "index_ID"
    dataset_id_missing_index = data.frame(contact_ID = id_missing_index,
                                          region = NA,
                                          sanitaryDistrict = NA,
                                          sex = NA,
                                          age = NA,
                                          index_ID = index1,
                                          result = 1
    )
    CoVid_19_patient_V2 = unique(rbind(CoVid_19_patient_V2, dataset_id_missing_index))
  }
}

contact_V2 <- unique(CoVid_19_patient_V2[, c("index_ID", "contact_ID")])

CoVid_19_patient_V2$result[CoVid_19_patient_V2$contact_ID %in% contact_V2$index_ID] = 1
CoVid_19_patient_V2$result = ifelse(is.na(CoVid_19_patient_V2$result), NA,
                                    ifelse(CoVid_19_patient_V2$result == 1, "Infected",
                                           ifelse(CoVid_19_patient_V2$result == 0, "Non-Infected", NA)))

nb_prev_index = str_count(CoVid_19_patient_V2$contact_ID, pattern = "-C")
index = str_split_fixed(contact_V2$index_ID, pattern = "-", max(nb_prev_index, na.rm = TRUE)+1)[, 1]

# Draw chains with function make_epicontacts from "epicontacts" package 
x_V2_deep_spreader <- make_epicontacts(linelist = CoVid_19_patient_V2,
                                       contacts = contact_V2[index!=""& index == "SN/DKS/20/2502",],
                                       directed = TRUE)
x_V2_deep_spreader1 <- make_epicontacts(linelist = CoVid_19_patient_V2,
                                        contacts = contact_V2[index!=""& index == "SN/SED/20/5219",],
                                        directed = TRUE)
plot(x_V2_deep_spreader, "result", col_pal = spectral)
plot(x_V2_deep_spreader1, "result", col_pal = spectral)
