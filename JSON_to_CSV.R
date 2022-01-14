
# Converts and parses COCO cameratrap format JSON output from Microsoft MegaDetector 
# Allows counting of bounding boxes by category for comparison to manual classification
# ***Goal is automating human counting by counting human "boxes" detected per image ***


# Created: M Fennell, Feb 8, 2021
# mitchell.fennell@gmail.com

### 0. Build workspace ####
library(jsonlite)
library(plyr)
library(dplyr)
library(tidyr)
library(stringr)

setwd("D:/Mitch/MD-calibration/FennellBeirneBurton_2022/InputData/MD_input")

### 1. Load Data ####
# File directory organized by sites (Each folder is a camera trap station)
WD <- "D:/Mitch/Cathedral/3.Data/3.3 Image_Processing/3.3.2MegaDetector/SplitJSONs/" 
site.list <- list.dirs(path = WD, recursive = F, full.names = F)
file.list <- list.files(getwd())


for (s in 1:length(file.list)){
      file_dat <- fromJSON(paste0(file.list[s]), flatten = T, simplifyMatrix = T) # Assign correct JSON here #
      str(file_dat)
      file_img <- file_dat[["images"]]
      
      ### 2. Create empty dataframe with correct dimensions ####
      
      df_out <- data.frame(matrix(ncol = 3, nrow = 500000)) # Make sure nrow is considerably more than the number of detections
      cnames <- c("file","cat","conf")
      colnames(df_out) <- cnames
      
      
      ### 3. Iterate through original JSON and populate a row in new dataframe for each bounding box ####
      i = 1
      for (k in 1:length(file_img$file)){
        for (j in 1:length(file_img[[1]][[k]]$category)){
          df_out$file[i] <- file_img[[2]][[k]]
          if (!is.null(file_img[[1]][[k]]$category[[j]])){
            df_out$cat[i] <- file_img[[1]][[k]]$category[[j]]
            df_out$conf[i] <- file_img[[1]][[k]]$conf[[j]]}
          else {
            df_out$cat[i] <- 0
            df_out$conf[i] <- 0}
          if (i == 500000) break
          i = i+1
        }
      }
      
      MD_output <- filter(df_out, (is.na(df_out$file) == F))
      for (g in 1:nrow(MD_output)){
        if (MD_output$cat[g] == 2){
          MD_output$cat[g] <- "Human"
        }
        if (MD_output$cat[g] == 1){
          MD_output$cat[g] <- "Animal"
        }
        if (MD_output$cat[g] == 0){
          MD_output$cat[g] <- "Blank"
        }
        if (MD_output$cat[g] == 3){
          MD_output$cat[g] <- "Vehicle"
        }
      }
      write.csv(MD_output, paste0("D:/Mitch/MD-calibration/FennellBeirneBurton_2022/MD_output/",site.list[s],"_MD_output_long.csv"), row.names = F)
}
