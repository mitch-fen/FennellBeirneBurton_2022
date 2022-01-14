# MegaDetector validation testing
# Mitch Fennell 
# mitchfen@mail.ubc.ca

### 0. Initialize ####
library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)

### 1. Read in csv's ####
# REMEMBER -> ONLY LABELED IMAGES ARE EXPORTED FROM THE DATABASE...NOT BLANKS.
# All images were manually classified, so we assume anything without a classification to be blank!

## Image classifications ####
id <- read.csv("InputData/images_idents_clean.csv", header=T)

# Remove deleted image classifications
id <- id[id$deleted=="f",]

# Remove images which are misfires (label in a sequence as a species but actually blank)
table(id$latin_name, id$misfire)
id <- id[id$misfire!="t",]
id <- id[id$latin_name!="No animal",]

## Merge the mega detector output ####
file.list <- list.files("./InputData/MD_output")
mega <- do.call(rbind, lapply(list.files("./InputData/MD_output", full.names = T), read.csv))

head(mega)
table(mega$cat)

# remove the brackets from mega files to match DB output!
mega$file <- gsub("\\(|)","",mega$file)

# Make all the .JPG's into .jpg's
mega$file <- str_replace(mega$file, pattern = ".JPG", replacement = ".jpg")
id$orig_file <- str_replace(id$orig_file, pattern = ".JPG", replacement = ".jpg")


### 2. Explore data ####

# As we would expect - only a few stations have longer filenames
table(nchar(mega$file))

mega_sort_length <- mega %>%
  mutate(file = as.character(file)) %>%
  arrange(str_length(file),file)

# Number of photos in MD files
length(unique(mega$file))

# Number of NON-BLANK photos in manual DB output
length(unique(id$orig_file))

# Number of unique images per station in Megadetector
station.list <- unique(id$station_id)
images.by.station <- as.data.frame(matrix(nrow = length(station.list), ncol = 2))
colnames(images.by.station) <- c("Site","n.Imgs")
for (i in 1:nrow(images.by.station)){
  images.by.station$Site[i] <- station.list[i]
  tmp <- filter(mega, str_detect(file, station.list[i]))
  images.by.station$n.Imgs[i] <- length(unique(tmp$file))
}

# Number of images in MD output by station
images.by.station # <- Compare this to the number of images you expect (from tracking spreadsheet)

# Assign method to each set of classifications
mega$Method <- "mega"
id$Method   <- "human"

# All the files in ID should be represented in mega, as MD keeps blanks in the JSON

# Blanks are missing in the manual ID output...add them back in by using the image list from mega
missing <- mega[!mega$file %in% id$orig_file,]
mega_cut <- mega[mega$file %in% id$orig_file,]
id_cut <- id[id$orig_file %in% mega$file,]

# Matchy Matchy
length(unique(mega$file))

# Lots missing from ID - because you have removed misfires and blanks are excluded
length(unique(id_cut$orig_file))
length(unique(missing$file))

length(unique(id_cut$orig_file)) + length(unique(missing$file))

table(duplicated(id_cut)) 

# There are duplicates in MEGA but that is because they are id'ing multiple things (see below)
table(duplicated(mega))
mega_cut[duplicated(mega),]

# Do we ever multiple designations per image?
table(mega$file, mega$cat) # yes all the time!

## Filter by confidence threshold ####
#Improves things
table(mega$file[mega$conf>0.9], mega$cat[mega$conf>0.9])

# Mitch says filter to above 0.9 confidence
mega_cut <- mega[mega$conf>0.9,]


### 3. Add in missing (blank and misfire) files to manual ID ####
# Merge the dataframes
head(id_cut)
id_simple <- id_cut[,c("orig_file", "group_count", "latin_name", "behaviour") ]
colnames(id_simple) <- c("file", "count", "id", "type")
id_simple$method <- "human_id"
id_simple$Probability <- NA

table(id_simple$id)
head(id_simple)

# Simplify species ID's to "Human or Animal"
id_simple$id <- as.character(id_simple$id)
id_simple$id[id_simple$id=="Homo sapiens"] <- "Human"
id_simple$id[!id_simple$id=="Human"] <- "Animal"
table(id_simple$id)
head(id_simple)

# Add in the blanks to the HUMAN ID DATAFRAME
missing <- data.frame(file=missing$file, count=0, id="Blank", type=NA, method="human_id", "Probability"=NA)
missing<- missing[duplicated(missing)==FALSE,]

# Join
id_simple <- rbind(id_simple, missing)

# Check
table(duplicated(id_simple))

### 4. Add in cut classifications to MD ID's due to thresholding above ####

# Lots missing from mega (because I cut the thresholds)
length(unique(mega_cut$file))
# Which were not missing earlier
length(unique(mega$file))

table(mega_cut$cat)

# Add them in as blanks
missing <- id_simple[!id_simple$file %in% mega_cut$file,]
missing <- data.frame(file=missing$file, cat="Blank", conf=NA, Method="mega")
missing<- missing[duplicated(missing)==FALSE,]
mega_cut <-rbind(mega_cut, missing)

length(unique(mega_cut$file)) # This line and the next should be the same!
length(unique(id_simple$file))

# Simplify
mega_simple <- mega_cut %>% 
  group_by(file, cat) %>% 
  dplyr::summarise(count=n(), Probability = mean(conf, na.rm=TRUE)) 

mega_simple <- as.data.frame(mega_simple)
length(unique(mega_simple$file))

# Fix up column names
colnames(mega_simple) <- c("file", "id", "count", "Probability")

mega_simple$type <- NA
mega_simple$method <- "megadetector"
mega_simple <- mega_simple[, c("file","count", "id", "type", "method", "Probability")]

colnames(mega_simple)

# Check
rbind(id_simple, mega_simple)
colnames(id_simple)

str(id_simple)
tmp1 <- as.data.frame(table(as.character(id_simple$file)))
table(tmp1$Freq)

str(mega_simple)
tmp1 <- as.data.frame(table(as.character(mega_simple$file)))
table(tmp1$Freq)

## Create analysis dataframe ####
final <- rbind(id_simple, mega_simple)


### 5. Analyze human detections! ####

# Deal with Vehicles
# Sometimes vehicles in MD will also classify person (on a bike, in window of car, etc.)
table(final$type)
final$id[final$type=="mountain biking"] <- "Vehicle"
final$id[final$type=="truck/car"] <- "Vehicle"
final$id[final$type=="quad/motorbike"] <- "Vehicle"
final$id[final$type=="snowmobile"] <- "Vehicle"


# All datasets, when there is a human identified, is identified by the other methods
tmp <- as.data.frame(table(final$method, final$id))
tmp$Var1 <- relevel(tmp$Var1, "human_id")
barplot(tmp$Freq[tmp$Var2=="Human"][c(2,1,3)], las=2, names.arg=tmp$Var1[tmp$Var2=="Human"][c(2,1,3)])
abline(h=max(tmp$Freq), lty=2)

# Number ID'd by each
table(final$method)


## True positive ####
# If it is ID'd as human manually - does MD pick it up?
tmp1 <- final[final$method=="human_id" & final$id=="Human",]

tmp2 <- final[final$method=="megadetector" & final$id=="Human" & final$file %in% tmp1$file,]

true.pos <- data.frame("Method"=c("Human_id","Megadetector"), "Images"=c(length(unique(tmp1$file)),length(unique(tmp2$file)))) 

# Plot
par(mfrow=c(1,4), mar = c(7,4,4,2))
barplot(true.pos$Images, las=2, names.arg=true.pos$Method, main = "True.Pos")
abline(h=max(true.pos$Images), lty=2)

true.pos$Percentage <- round(true.pos$Images/max(true.pos$Images)*100,1)

## True negative ####
# Subset what was labeled as not human 
tmp1 <- final[final$method=="human_id" & (!final$id=="Human"),]
table(tmp1$id)

# What mega detector thought not human (blank or animal or vehicle) 
tmp2 <- final[final$method=="megadetector" & (!final$id=="Human") & final$file %in% tmp1$file,]
# But some of these may have "human too"
tmp4 <- final[final$method=="megadetector" & final$id=="Human" & final$file %in% tmp1$file,]
tmp2[tmp2$file %in% tmp4$file,] #YES ARGH

# Remove the files that actually had humans labeled too
tmp2 <- tmp2[!tmp2$file %in% tmp4$file,]

table(tmp2$id)

true.neg <- data.frame("Method"=c("Human_id","Megadetector"), "Images"=c(length(unique(tmp1$file)),length(unique(tmp2$file)))) 
true.neg$Percentage <- round(true.neg$Images/max(true.neg$Images)*100,1)

# Add to plot
barplot(true.neg$Images, las=2, names.arg=true.neg$Method, main = "True.Neg")
abline(h=max(true.neg$Images), lty=2)

## False positive ####
# Everything that is NOT HUMAN
tmp1 <- final[final$method=="human_id" & (!final$id=="Human"),]
table(tmp1$id)
# Images in that subset which HAVE been labeled human by MD
tmp2 <- final[final$method=="megadetector" & final$id=="Human" & final$file %in% tmp1$file,]
table(tmp2$id)

false.pos <- data.frame("Method"=c("Human_id","Megadetector"), "Images"=c(length(unique(tmp1$file)),length(unique(tmp2$file)))) 
false.pos$Percentage <- round(false.pos$Images/max(false.pos$Images)*100,1)

# Add to plot
barplot(false.pos$Images, las=2, names.arg=false.pos$Method, main = "False.Pos")
abline(h=max(false.pos$Images), lty=2)

## False negative ####
# Labelled as human, missed by MD
tmp1 <- final[final$method=="human_id" & final$id=="Human",]

tmp2 <- final[final$method=="megadetector" & (!final$id=="Human") & final$file %in% tmp1$file,]
# But some of these may have "human too"
tmp4 <- final[final$method=="megadetector" & final$id=="Human" & final$file %in% tmp1$file,]
tmp2 <- tmp2[!tmp2$file %in% tmp4$file,] #YES ARGH


false.neg <- data.frame("Method"=c("Human_id","Megadetector"), "Images"=c(length(unique(tmp1$file)),length(unique(tmp2$file)))) 
false.neg$Percentage <- round(false.neg$Images/max(false.neg$Images)*100,1)

# Plot
barplot(false.neg$Images, las=2, names.arg=false.neg$Method, main = "False.Neg")
abline(h=max(false.neg$Images), lty=2)

## Summary stats ####
true.pos #94.7
true.neg #99.3
false.neg #5.3
false.pos #0.7

#Accuracy
accu <- (true.pos$Images[2]+true.neg$Images[2])/
  (true.pos$Images[2]+true.neg$Images[2]+false.pos$Images[2]+false.neg$Images[2])

# Misclassification rate
misclass <- 1-accu

#Sensitivity (recall)
sens <- (true.pos$Images[2])/(true.pos$Images[2]+false.neg$Images[2])

#Specificity
spec <- (true.neg$Images[2])/(true.neg$Images[2]+false.pos$Images[2])

#Precision
prec <- (true.pos$Images[2])/(true.pos$Images[2]+false.pos$Images[2])

# F-score
F_score_hum <- (2*prec*sens)/(prec+sens)


head(final)

 ## plot by station ####
# Split by station
final$Deployment.Location.ID <- substr(final$file, 1,6)
library(dplyr)
library(tidyr)
CR_final <- final %>% 
  group_by(Deployment.Location.ID,method, id ) %>% 
  summarise(count=n()) 

par(mfrow=c(1,1), mar = c(5,6,3,1))
plot(CR_final$count[CR_final$method=="megadetector" & CR_final$id=="Human" & is.na(CR_final$id)==F]~
       CR_final$count[CR_final$method=="human_id" & CR_final$id=="Human"], las=1, pch=19,
       ylab="",xlab="Manual Identification", main="", cex = 1.5, cex.lab=1.3, cex.axis=1.2)
mtext(side=2, text="Megadetector Identification", line = 5, cex=1.3)
abline(a=0, b=1, lty=2)
text(x = -5000, y=32000, labels = "A", xpd = NA, font = 2, cex = 1.3)

CR_by_site <- CR_final %>%
  filter(id=="Human")%>%
  pivot_wider(names_from =  method, values_from = count)

abline(lm(megadetector~human_id, data = CR_by_site), lty = 1, col = "blue")

## Quick regression of human image performance ####
lm(megadetector~human_id, data = CR_by_site) #0.96 corr coef

### 6. Analyze animal detections ####

## True positive ####
# If it is ID'd as animal by manual ID, does MD pick it up?
tmp1 <- final[final$method=="human_id" & final$id=="Animal",]

tmp2 <- final[final$method=="megadetector" & final$id=="Animal" & final$file %in% tmp1$file,]

true.pos <- data.frame("Method"=c("Human_id","Megadetector"), "Images"=c(length(unique(tmp1$file)),length(unique(tmp2$file)))) 

par(mfrow=c(1,4))
barplot(true.pos$Images, las=1, names.arg=true.pos$Method)
abline(h=max(true.pos$Images), lty=2)

## True -negative ####
# Subset what was labeled as not animal 
tmp1 <- final[final$method=="human_id" & (!final$id=="Animal"),]
table(tmp1$id)

# What mega detector thought not Animal (blank or human)
tmp2 <- final[final$method=="megadetector" & (!final$id=="Animal") & final$file %in% tmp1$file,]
# But some of these may have "animal too"
tmp4 <- final[final$method=="megadetector" & final$id=="Animal" & final$file %in% tmp1$file,]
tmp2[tmp2$file %in% tmp4$file,] #YES ARGH

# Remove the files that actually had animals labeled too
tmp2 <- tmp2[!tmp2$file %in% tmp4$file,]

table(tmp2$id)

true.neg <- data.frame("Method"=c("Human_id","Megadetector"), "Images"=c(length(unique(tmp1$file)),length(unique(tmp2$file)))) 

barplot(true.neg$Images, las=1, names.arg=true.neg$Method)
abline(h=max(true.neg$Images), lty=2)

## False positive ####
# Everything that is NOT animal
tmp1 <- final[final$method=="human_id" & (!final$id=="Animal"),]
table(tmp1$id)
# Images in that subset which HAVE been  labelled animal
tmp2 <- final[final$method=="megadetector" & final$id=="Animal" & final$file %in% tmp1$file,]

table(tmp2$id)

false.pos <- data.frame("Method"=c("Human_id","Megadetector"), "Images"=c(length(unique(tmp1$file)),length(unique(tmp2$file)))) 

barplot(false.pos$Images, las=1, names.arg=false.pos$Method)
abline(h=max(false.pos$Images), lty=2)

## False negative ####
tmp1 <- final[final$method=="human_id" & final$id=="Animal",]

tmp2 <- final[final$method=="megadetector" & (!final$id=="Animal") & final$file %in% tmp1$file,]
# But some of these may have "animal too"
tmp4 <- final[final$method=="megadetector" & final$id=="Animal" & final$file %in% tmp1$file,]
tmp2 <- tmp2[!tmp2$file %in% tmp4$file,] #YES ARGH

false.neg <- data.frame("Method"=c("Human_id","Megadetector"), "Images"=c(length(unique(tmp1$file)),length(unique(tmp2$file)))) 

barplot(false.neg$Images, las=1, names.arg=false.neg$Method)
abline(h=max(false.neg$Images), lty=2)

## Summary stats ####

true.pos$Percentage <- round(true.pos$Images/max(true.pos$Images)*100,1)
true.pos #92.3

true.neg$Percentage <- round(true.neg$Images/max(true.neg$Images)*100,1)
true.neg #97.1

false.neg$Percentage <- round(false.neg$Images/max(false.neg$Images)*100,1)
false.neg #7.7
 
false.pos$Percentage <- round(false.pos$Images/max(false.pos$Images)*100,1)
false.pos #2.9

#Accuracy
accu <- (true.pos$Images[2]+true.neg$Images[2])/
  (true.pos$Images[2]+true.neg$Images[2]+false.pos$Images[2]+false.neg$Images[2])

# Misclassification rate
misclass <- 1-accu

#Sensitivity (Recall)
sens <- (true.pos$Images[2])/(true.pos$Images[2]+false.neg$Images[2])

#Specificity
spec <- (true.neg$Images[2])/(true.neg$Images[2]+false.pos$Images[2])

#Precision
prec <- (true.pos$Images[2])/(true.pos$Images[2]+false.pos$Images[2])

# F-score
F_score_animal <- (2*prec*sens)/(prec+sens)

# Split by station
final$Deployment.Location.ID <- substr(final$file, 1,6)
library(dplyr)
CR_final <- final %>% 
  group_by(Deployment.Location.ID,method, id ) %>% 
  summarise(count=n()) 

par(mfrow=c(1,1), mar = c(5,6,3,1))
animal.det.by.site <- plot(CR_final$count[CR_final$method=="megadetector" & CR_final$id=="Animal" & is.na(CR_final$id)==F]~
       CR_final$count[CR_final$method=="human_id" & CR_final$id=="Animal"], las=1, pch=19,
     ylab="",xlab="Manual ID", main="Animal Detections by Site")
mtext(side=2, text="Megadetector", line = 4)
abline(a=0, b=1, lty=2)

CR_by_site <- CR_final %>%
  filter(id=="Animal")%>%
  pivot_wider(names_from =  method, values_from = count)

abline(lm(megadetector~human_id, data = CR_by_site), lty = 1, col = "blue")

## Quick regression of animal image performance ####
lm(megadetector~human_id, data = CR_by_site) #0.89 corr coef


### 7. Independent Human Detections ####

# Create manual ID df of all human images
id_humans <- filter(id, common_names == "Human" & (behaviour == "hiking" | behaviour == "horseback riding" | behaviour == "mountain biking" | behaviour == "inspecting camera"| behaviour =="")) 
length(unique(id_humans$orig_file))

# Filter mega df to only humans, above 0.9 confidence
mega_humans <- filter(mega, cat == "Human" & conf > 0.9)

# Collapse by image
mega_humans <- mega_humans %>%
  group_by(file) %>%
  summarise(ind.count = n())
length(unique(mega_humans$file))

# MD images not in manual ID file
length(setdiff(mega_humans$file, id_humans$orig_file))  

# To investigate:
setdiff(mega_humans$file, id_humans$orig_file)

# Manual ID images not in MD files (shouldn't be any if all matched...wishful thinking)
length(setdiff(id_humans$orig_file, mega_humans$file)) 

# To investigate:
setdiff(id_humans$orig_file, mega_humans$file)


## Independence loop for manual ID ####
dat <- id_humans
independent <- 5


dat$Date_Time.Captured <- ymd_hms(dat$exif_timestamp)
dat$Deployment.Location.ID <- dat$station_id
dat$Species <- dat$latin_name
dat$Minimum.Group.Size <- dat$group_count

dat$Species <- as.character(dat$Species)
dat$Deployment.Location.ID <- as.character(dat$Deployment.Location.ID)

# Order the dataframe by Site, date
dat <- dat[order(dat$Deployment.Location.ID, dat$Date_Time.Captured),]

dat <- dat %>%
  arrange(Deployment.Location.ID) %>%
  group_by(Deployment.Location.ID, Species) %>%
  mutate(duration = int_length(Date_Time.Captured %--% lag(Date_Time.Captured)))

# loop that assigns event ID
dat$Event.ID <- 9999
mins <- independent
seq <- as.numeric(paste0(nrow(dat),0))
seq <- round(seq,-(nchar(seq)))
for (i in 2:nrow(dat)) {
  dat$Event.ID[i-1]  <- paste0("E",format(seq, scientific = F))
  if(is.na(dat$duration[i]) | abs(dat$duration[i]) > (mins * 60)){
    seq <- seq + 1
  }
}

# Update the information for the last row
# group ID  for the last row
if(dat$duration[nrow(dat)] < (mins * 60)|
   is.na(dat$duration[nrow(dat)])){
  dat$Event.ID[nrow(dat)] <- dat$Event.ID[nrow(dat)-1]
} else{
  dat$Event.ID[nrow(dat)] <- paste0("E",format(seq+1, scientific = F))
}

# If there is no minimum groupsize take number of animals
if(!"Minimum.Group.Size" %in% colnames(dat)) {dat$Minimum.Group.Size <- dat$Number.of.Animals}

# Calculate the event length and size

# find out the last and the first of the time in the group
top <- dat %>% group_by(Event.ID) %>% top_n(1,Date_Time.Captured) %>% select(Event.ID, Date_Time.Captured)
bot <- dat %>% group_by(Event.ID) %>% top_n(-1,Date_Time.Captured) %>% select(Event.ID, Date_Time.Captured)
names(bot)[2] <- c("Date_Time.Captured_end")
dec_no <- dat %>% group_by(Event.ID) %>% summarise(n())
event_grp <- dat %>% group_by(Event.ID) %>% summarise(max(Minimum.Group.Size))

# calculate the duration
diff <-  top %>% left_join(bot, by="Event.ID") %>%
  mutate(duration=abs(int_length(Date_Time.Captured %--% Date_Time.Captured_end))) %>%
  left_join(event_grp, by="Event.ID")%>%
  left_join(dec_no, by="Event.ID")

# Remove duplicates
diff <- diff[duplicated(diff)==FALSE,]

names(diff) <- c("Event.ID","Date_Time.end","Date_Time.start","Event.Duration","Event.Groupsize","Event.Observations")
diff$Date_Time.end<-NULL;diff$Date_Time.start<-NULL
dat$duration <-NULL
# Merge the data
dat <-  dat %>%
  left_join(diff,by="Event.ID")

# Subset to the first observation in each event
# Subset to independent observations using your chosen threshold
ind.dat <- dat[!duplicated(dat$Event.ID),]
ind.dat <- as.data.frame(ind.dat)
ind.dat$Species <-as.factor(ind.dat$Species)

# finalize as proper method
ind.dat.id <- ind.dat


## Independence loop for MegaDetector ID ####
library(stringr)

dat <- mega_humans
independent <- 5

table(nchar(mega_humans$file))

# Extract datetime from filenames
dat$Deployment.Location.ID <- str_extract(dat$file, pattern = "[^_]+")
dat$Date_Time.Captured <- str_extract(dat$file, pattern = "(?<=__).+")
dat$Date_Time.Captured <- str_sub(dat$Date_Time.Captured, start = 1, end = 20)
dat$Date_Time.Captured <- str_replace(dat$Date_Time.Captured, pattern = "__", replacement = " ")

dat$Date_Time.Captured <- ymd_hms(dat$Date_Time.Captured)

# Since all ID's should be human:
dat$Species <- "Human"

# Create a group count variable for ind. loop to use (minimum number of people in any one image)
dat$Minimum.Group.Size <- dat$ind.count

dat$Species <- as.character(dat$Species)
dat$Deployment.Location.ID <- as.character(dat$Deployment.Location.ID)


# Order the dataframe by Site, date
dat <- dat[order(dat$Deployment.Location.ID, dat$Date_Time.Captured),]

### NEW WAY
dat <- dat %>%
  #filter(Species == i) %>%
  arrange(Deployment.Location.ID) %>%
  group_by(Deployment.Location.ID, Species) %>%
  mutate(duration = int_length(Date_Time.Captured %--% lag(Date_Time.Captured)))

# loop that assigns event ID
dat$Event.ID <- 9999
mins <- independent
seq <- as.numeric(paste0(nrow(dat),0))
seq <- round(seq,-(nchar(seq)))
for (i in 2:nrow(dat)) {
  dat$Event.ID[i-1]  <- paste0("E",format(seq, scientific = F))
  if(is.na(dat$duration[i]) | abs(dat$duration[i]) > (mins * 60)){
    seq <- seq + 1
  }
}

# Update the information for the last row
# group ID  for the last row
if(dat$duration[nrow(dat)] < (mins * 60)|
   is.na(dat$duration[nrow(dat)])){
  dat$Event.ID[nrow(dat)] <- dat$Event.ID[nrow(dat)-1]
} else{
  dat$Event.ID[nrow(dat)] <- paste0("E",format(seq+1, scientific = F))
}

# If there is no minimum groupsize take number of animals
if(!"Minimum.Group.Size" %in% colnames(dat)) {dat$Minimum.Group.Size <- dat$Number.of.Animals}

# Calculate the event length and size

# find out the last and the first of the time in the group
top <- dat %>% group_by(Event.ID) %>% top_n(1,Date_Time.Captured) %>% select(Event.ID, Date_Time.Captured)
bot <- dat %>% group_by(Event.ID) %>% top_n(-1,Date_Time.Captured) %>% select(Event.ID, Date_Time.Captured)
names(bot)[2] <- c("Date_Time.Captured_end")
dec_no <- dat %>% group_by(Event.ID) %>% summarise(n())
event_grp <- dat %>% group_by(Event.ID) %>% summarise(max(Minimum.Group.Size))

# caculate the duration
diff <-  top %>% left_join(bot, by="Event.ID") %>%
  mutate(duration=abs(int_length(Date_Time.Captured %--% Date_Time.Captured_end))) %>%
  left_join(event_grp, by="Event.ID")%>%
  left_join(dec_no, by="Event.ID")

# Remove duplicates
diff <- diff[duplicated(diff)==FALSE,]

names(diff) <- c("Event.ID","Date_Time.end","Date_Time.start","Event.Duration","Event.Groupsize","Event.Observations")
diff$Date_Time.end<-NULL;diff$Date_Time.start<-NULL
dat$duration <-NULL
# Merge the data
dat <-  dat %>%
  left_join(diff,by="Event.ID")

# Subset to the first observation in each event
# Subset to independent observations using your chosen threshold
ind.dat <- dat[!duplicated(dat$Event.ID),]
ind.dat <- as.data.frame(ind.dat)
ind.dat$Species <-as.factor(ind.dat$Species)

# finalize as proper method
ind.dat.mega <- ind.dat


## Save both mega and human ID'd independent data to file ####
write.csv(ind.dat.mega, "D:/Mitch/MD-calibration/ind.dat.mega.csv", row.names = F)
write.csv(ind.dat.id, "D:/Mitch/MD-calibration/ind.dat.id.csv", row.names = F)

### 8. Compare independent detections ####

setdiff(ind.dat.mega$file, ind.dat.id$orig_file)
setdiff(ind.dat.id$orig_file, ind.dat.mega$file)

# Use data processing script to chunk data into site weeks
# -> Matrix chunk of exploration .RMD


eff <- read.csv("CATH_Deployments_Mar08.csv", header=T) # Read in deployment data
eff <- filter(eff, Deployment.Location.ID != "CATH33" & Deployment.Location.ID != "CATH08" & Deployment.Location.ID != "CATH25" & Deployment.Location.ID != "CATH22") # Match with sites from beginning

## Formatting ####
# Date formatting
tz <- "UTC"
as.Date(ymd_hms(eff$Camera.Deployment.Begin.Date[1], truncated=3))
table((strptime(eff$Camera.Deployment.End.Date, "%Y-%m-%d", tz="UTC")-strptime(eff$Camera.Deployment.Begin.Date, "%Y-%m-%d", tz="UTC"))>=0)

eff$Camera.Deployment.Begin.Date <- strptime(as.Date(ymd_hms(eff$Camera.Deployment.Begin.Date, truncated=3, tz=tz)), "%Y-%m-%d", tz=tz)
eff$Camera.Deployment.End.Date   <- strptime(as.Date(ymd_hms(eff$Camera.Deployment.End.Date, truncated=3, tz=tz)), "%Y-%m-%d", tz=tz)

ymd(eff$Camera.Deployment.Begin.Date[1])

eff$Days <- as.numeric(round(difftime(eff$Camera.Deployment.End.Date, eff$Camera.Deployment.Begin.Date, units="days"),1))

# Create daily lookup to show camera activity through time
tmp <- eff[is.na(eff$Camera.Deployment.End.Date)==F,]
daily.lookup <- list()
for(i in 1:nrow(tmp))
{
  if(as.Date(tmp$Camera.Deployment.Begin.Date[i])!=as.Date(tmp$Camera.Deployment.End.Date[i]))
  {
    daily.lookup[[i]] <- data.frame("Date"=seq(as.Date(tmp$Camera.Deployment.Begin.Date[i]), as.Date(tmp$Camera.Deployment.End.Date[i]), by="days"), "Deployment.Location.ID"=tmp$Deployment.Location.ID[i])
  }
}

## Weekly matrix of human classified data (human counts) ####
ind.dat.id$Species <- factor(ind.dat.id$Species)


tmp <- bind_rows(daily.lookup)
# Simplify the date to year-week
tmp$Date <- strftime(tmp$Date, format = "%Y-W%U")

# Calculate the number of days in each week  
week.obs <- tmp %>% 
  group_by(Deployment.Location.ID,Date ) %>%
  summarise(Effort = n())

# Convert to a data frame
week.obs <- as.data.frame(week.obs)
# Add species columns  
week.obs[, levels(ind.dat.id$Species)] <- NA
week.count <- week.obs

# For each week, count the number of individuals/observations
for(i in 1:nrow(week.obs))
{
  tmp <- ind.dat.id[ind.dat.id$Deployment.Location.ID==week.obs$Deployment.Location.ID[i] & strftime(ind.dat.id$Date_Time.Captured, format = "%Y-W%U")== week.obs$Date[i],]
  for(j in 1:length(levels(ind.dat.id$Species)))
  {
    week.obs[i,levels(ind.dat.id$Species)[j]] <- length(tmp$Species[tmp$Species==levels(ind.dat.id$Species)[j]])
    week.count[i,levels(ind.dat.id$Species)[j]] <- sum(tmp$Event.Groupsize[tmp$Species==levels(ind.dat.id$Species)[j]])
  }
}

# Save human ID'd weekly human use data
write.csv(week.obs, paste0("CATH_manual_Independent_weekly_observations.csv"), row.names = F) 
write.csv(week.count, paste0("CATH_manual_Independent_weekly_counts.csv"), row.names = F) 

## Weekly matrix of megadetector classified data (human counts) ####
ind.dat.mega$Species <- factor(ind.dat.mega$Species)
tmp <- bind_rows(daily.lookup)

# Simplify the date to year-week
tmp$Date <- strftime(tmp$Date, format = "%Y-W%U")

# Calculate the number of days in each week  
week.obs <- tmp %>% 
  group_by(Deployment.Location.ID,Date ) %>%
  summarise(Effort = n())

# Convert to a data frame
week.obs <- as.data.frame(week.obs)
# Add species columns  
week.obs[, levels(ind.dat.mega$Species)] <- NA
week.count <- week.obs

# For each week, count the number of individuals/observations
for(i in 1:nrow(week.obs))
{
  tmp <- ind.dat.mega[ind.dat.mega$Deployment.Location.ID==week.obs$Deployment.Location.ID[i] & strftime(ind.dat.mega$Date_Time.Captured, format = "%Y-W%U")== week.obs$Date[i],]
  for(j in 1:length(levels(ind.dat.mega$Species)))
  {
    week.obs[i,levels(ind.dat.mega$Species)[j]] <- length(tmp$Species[tmp$Species==levels(ind.dat.mega$Species)[j]])
    week.count[i,levels(ind.dat.mega$Species)[j]] <- sum(tmp$Event.Groupsize[tmp$Species==levels(ind.dat.mega$Species)[j]])
  }
}

# Save MD ID'd weekly human use data
write.csv(week.obs, paste0("CATH_megadetector_Independent_weekly_observations.csv"), row.names = F) 
write.csv(week.count, paste0("CATH_megadetector_Independent_weekly_counts.csv"), row.names = F) 

## Compare site-week output matrices ####

# Read in weekly detection counts from above
weekly.manual <- read.csv("CATH_manual_Independent_weekly_observations.csv", header = T)
weekly.megadetector <- read.csv("CATH_megadetector_Independent_weekly_observations.csv", header = T)

# Match column names
weekly.manual <- weekly.manual %>%
  rename(Human = Homo.sapiens)

# Differences
setdiff(weekly.manual, weekly.megadetector)
setdiff(weekly.megadetector, weekly.manual)

sum(weekly.manual$Human)
sum(weekly.megadetector$Human) 

sum(weekly.manual$Human) - sum(weekly.megadetector$Human) #-19 (19 more with MD)

(sum(weekly.megadetector$Human) - sum(weekly.manual$Human))/sum(weekly.manual$Human)*100 #0.002 or 0.2% more total detections with MD

# Number of detections per site
weekly.manual.site <- weekly.manual %>% 
  group_by(Deployment.Location.ID) %>%
  summarise(Count.manual = sum(Human)) 
  
weekly.megadetector.site <- weekly.megadetector %>% 
  group_by(Deployment.Location.ID) %>%
  summarise(Count.mega = sum(Human)) 

# Combine to compare methods side by side
count.by.site <- left_join(weekly.manual.site, weekly.megadetector.site)
count.by.site$diff <- count.by.site$Count.manual - count.by.site$Count.mega

# Combine weeks to compare directly 
weekly.manual <- weekly.manual %>%
  rename(Human.Manual = Human)

weekly.megadetector <- weekly.megadetector %>%
  rename(Human.MD = Human)


weekly.by.site <- full_join(weekly.manual, weekly.megadetector)
weekly.by.site$diff <- weekly.by.site$Human.Manual - weekly.by.site$Human.MD

weekly.by.site.summ <- weekly.by.site

weekly.by.site.summ$percent.diff <- (abs(weekly.by.site.summ$diff)/weekly.by.site.summ$Human.Manual)*100
weekly.by.site.summ$percent.diff[is.na(weekly.by.site.summ$percent.diff)] <- 0

weekly.by.site.summ <- weekly.by.site.summ %>% 
  filter_all(all_vars(!is.infinite(.))) # Remove site weeks where infinite error due to division by 0

# Summary stats ####
mean(weekly.by.site.summ$percent.diff) #0.45% mean difference

mean(weekly.by.site.summ$Human.Manual)
min(weekly.by.site.summ$Human.Manual)
max(weekly.by.site.summ$Human.Manual)

mean(weekly.by.site.summ$diff) #-0.0024
abs(min(weekly.by.site.summ$diff)) #4
max(weekly.by.site.summ$diff) #3






## Run a quick regression on the relationship between MD and Manual ####
mod1 <- lm(Human.Manual~Human.MD, data = weekly.by.site.summ)
mod1
summary(mod1)

## Summary plots (publication figures) ####

# Figure 1
final$Deployment.Location.ID <- substr(final$file, 1,6)
library(dplyr)
CR_final <- final %>% 
  group_by(Deployment.Location.ID,method, id ) %>% 
  summarise(count=n()) 

# Fig.1a
par(mfrow=c(1,2), mar = c(5,6,3,1))
plot(CR_final$count[CR_final$method=="megadetector" & CR_final$id=="Human" & is.na(CR_final$id)==F]~
       CR_final$count[CR_final$method=="human_id" & CR_final$id=="Human"], las=1, pch=19,
     ylab="",xlab="Manual Identification", main="", cex=1.5, cex.lab=1.3, cex.axis=1.2)
mtext(side=2, text="MegaDetector Identification", line = 4, cex=1.3)
abline(a=0, b=1, lty=2)
text(x = -5000, y=31500, labels = "A", xpd = NA, font = 2, cex = 1.3)

CR_by_site <- CR_final %>%
  filter(id=="Human")%>%
  pivot_wider(names_from =  method, values_from = count)

abline(lm(megadetector~human_id, data = CR_by_site), lty = 1, col = "blue")

#Fig.1b
animal.det.by.site <- plot(CR_final$count[CR_final$method=="megadetector" & CR_final$id=="Animal" & is.na(CR_final$id)==F]~
                             CR_final$count[CR_final$method=="human_id" & CR_final$id=="Animal"], las=1, pch=19,
                           ylab="",xlab="Manual Identification", main="", cex=1.5, cex.lab=1.3, cex.axis=1.2)
mtext(side=2, text="MegaDetector Identification", line = 4, cex=1.3)
abline(a=0, b=1, lty=2)
text(x = -450, y=2800, labels = "B", xpd = NA, font = 2, cex = 1.3)

CR_by_site <- CR_final %>%
  filter(id=="Animal")%>%
  pivot_wider(names_from =  method, values_from = count)

abline(lm(megadetector~human_id, data = CR_by_site), lty = 1, col = "blue")

# Figure 2
weekly.by.site.summ <- weekly.by.site
weekly.by.site.summ$site.week <- paste0(weekly.by.site.summ$Deployment.Location.ID,"-", weekly.by.site.summ$Date)

CR_final <- final %>% 
  group_by(Deployment.Location.ID,method, id ) %>% 
  summarise(count=n()) 

par(mfrow=c(1,1), mar = c(5,6,3,1))
plot(weekly.by.site.summ$Human.Manual~
       weekly.by.site.summ$Human.MD, las=1, pch=19,
     ylab="MegaDetector Identification",xlab="Manual Identification", main="")
abline(a=0, b=1, lty=2)
#abline(lm(Human.MD~Human.Manual, data = weekly.by.site.summ), col = "blue") # Too busy with overlapping lines


