



#Packages

{
library(lubridate)
library(stringr)
library(base)
library("zoo")
library("plyr")
library("readxl")
library(dplyr)
library("tidyverse")
library("data.table")
library(reshape2)
  library(plotly)
}

#Data Gathering
{
# Data_Frame = data.frame()
# Data_Frame = read.csv("./Recorded_Vs_Computed/DLR_Total_Generation_and_Demand_2017-2022-11-15_correct.csv")

############################################################################################################################################
############################################################################################################################################
############################################################################################################################################
#####################################################   MW Sheet data cleaning and plot   ##################################################
############################################################################################################################################
############################################################################################################################################
############################################################################################################################################

## Hourly Energy (Power) calculation 

# Directory path in which files are present
files_path = "./MAR-2022"

#Extracting  all files from
Complete_File_list = list.files(path = files_path, pattern = ".xl",recursive=TRUE)

# Complete_File_list <- Complete_File_list[-c(34,63:72,101,150,152,154,156,158,160,171,172,212,214)]
#deleted Hidden LS and DLR with prescript ~$ data for 2017 folder 
Complete_File_list = grep('~$', Complete_File_list, fixed = TRUE, value = TRUE, invert = TRUE)


# Just Initialize so that at the end all sheets will be bind on it.
All_Files_data_Frame = data.frame()



for(file_index in 1:length(Complete_File_list)){
  # file_index = 1
 
  # Reading Excel file and specific sheet
  DLR_Files_Main = read_excel(paste0(files_path, Complete_File_list[file_index]),  sheet="MW")
  
  # Removing rows from main file
  DLR_Files_Main = DLR_Files_Main[-c(1),]
  
  # Transposing the Matrix
  DLR_Files_Main_transpose = as.data.frame(t(as.matrix(DLR_Files_Main)))
  
  DLR_Files_Main_transpose = DLR_Files_Main_transpose [c(1:25),-c(1:2)]
  
  colnames( DLR_Files_Main_transpose) = paste(as.character(DLR_Files_Main_transpose[1,]))
  
  DLR_Files_Main_transpose = DLR_Files_Main_transpose[-1,]
  
  rownames(DLR_Files_Main_transpose) = NULL
  
  # Filter Total Generation and Total Demand Column
  DLR_Files_Main_transpose =  DLR_Files_Main_transpose[,c("TOTAL GENERATION","NPCC LOAD MNGMT.","REG. LOAD MNGMT.","TOTAL SYS.DEMAND")]
  
  #for dot splitting special care [[1]]=1st row of list and [1]=1st element of that list's string and than format
  first_hour = as.POSIXct(gsub("K", "0",strsplit(strsplit(Complete_File_list[file_index],
                                                          "/")[[1]][2], "\\.")[[1]][1]), format = "%d %B %Y")
  
  # for Single folder file
  #first_hour = as.POSIXct(gsub("K", "0",strsplit(Complete_File_list[file_index], 
                                                       #   "/")[[1]][1]), format = "%d %B %Y")
  
  # time variable, seq.= date-time class k lea, from=start, to=end, by=increment
  Time = seq.POSIXt(from = first_hour, to = first_hour + (23*60*60), by = "hour")
  
  Time = Time[c(2:24,1)]
  
  #  adding time row in files
  DLR_Files_Main_transpose = cbind(Time,DLR_Files_Main_transpose)
  
   
  # binding all rows in empty data-frame  
  All_Files_data_Frame = bind_rows(All_Files_data_Frame, DLR_Files_Main_transpose)
  
}


#All_Files_data_Frame = read.csv("./Recorded_Vs_Computed/DLR_Total_Generation_and Demand_till_May-2023.csv")
#All_Files_data_Frame$Time= as.POSIXct(All_Files_data_Frame$Time, format= "%Y-%m-%d %H:%M")



#Data_Frame$Time = as.POSIXct(Data_Frame$Time, format= "%m/%d/%Y %H:%M")

#Removing Nov 2022 data 
Data_Frame = Data_Frame[format(Data_Frame$Time, "%Y-%m-%d")<= "2022-10-31",]


Data_Frame <- bind_rows(Data_Frame,All_Files_data_Frame)

Data_Frame <- Data_Frame[order(Data_Frame$Time),]


write.csv(All_Files_data_Frame,"DLR_Total_Generation_and Demand_2017-May2023.csv",row.names = F)
}

#Time Issue Resolve
{
#   Data_Frame <- read.csv("DLR_Total_Generation_and Demand_2021-2022_Fiscal_Year.csv")
#   
#  # Data_Frame$Time <- as.POSIXct(Data_Frame$Time, format= "%Y-%m-%d %H:%M")
#   
#   # Data_Frame$Time <- as.POSIXct(Data_Frame$Time, format= "%m/%d/%Y %H:%M")
#   # 
#   # Data_Frame <- Data_Frame[order(Data_Frame$Time),]
#   # 
#   # 
#   # Data_Frame <- Data_Frame[format(Data_Frame$Time,"%Y")=="2021",]
# 
#   Unique_Date <- unique(format(Data_Frame$Time,"%Y-%m-%d"))
# 
# for(i in 1:length(Unique_Date)){
# 
#  temp_data_frame =  Data_Frame[format(Data_Frame$Time,"%Y-%m-%d %H:%M") >= paste(Unique_Date[i], "00:00") & format(Data_Frame$Time,"%Y-%m-%d %H:%M") <= paste(Unique_Date[i], "23:00") ,]
# 
#  temp1 <- temp_data_frame[1:23,2:3]
#  temp2 <- temp_data_frame[24,2:3]
# 
# 
#   Time <- temp_data_frame$Time[c(2:24,1)]
# 
#   temp_data_frame = rbind(temp1,temp2)
# 
#   temp_data_frame$Time <- Time
# 
#   temp_data_frame <- temp_data_frame[c(24,1:23),c(3,1:2)]
# 
# Data_Frame[format(Data_Frame$Time,"%Y-%m-%d %H:%M") >= paste(Unique_Date[i], "00:00") & format(Data_Frame$Time,"%Y-%m-%d %H:%M") <= paste(Unique_Date[i], "23:00") ,] = temp_data_frame
# 
# }
# 
# Data_Frame <- Data_Frame[order(Data_Frame$Time),]
# 
# write.csv(Data_Frame,"DLR_Total_Generation_and Demand_2021_2nd_Half_correct.csv",row.names = F)
}


