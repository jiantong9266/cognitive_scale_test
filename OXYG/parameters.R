# :::::::::MODEL DETAILS
#current_data = "19W010"
current_model = "V1.1_190205"
current_data = "19W017"
inputFilePath = "s3a://nbcu-ds-linear-forecasting/interim/data_layers/OXYG/"
inputFileName = "OXYG_DATA"

# # :::::::::DATA-RELATED PARAMETERS ::::::::::::::::::::::::::::::::::::::::::::
# 
# # ::::::: GENERAL PATH 
# 
# inputFilePath <- "/mnt/nbcu-ds-linear-forecasting/processed/pacing/19W010/OXYG_FFL_0204_190205_19W010.csv"
# 
# # :::::::: RAW DATA FILE
# 
# inputFileName <- "OXYG_FFL_0204_190205_19W010"
# :::::::: GROUP_BY VARIABLES

group_by_cols <- c("Source","Network", "Demo", "HH_NT_Daypart","Broadcast_Year","Cable_Qtr","Week","Date",
                   "Half_Hr","Start_Time","Show_Name","program_type","FirstAiring",
                   "Jan", "Feb", "Mar", "Apr", "May", "Jun",
                   "Jul", "Aug","Sep","Oct", "Nov", "Dec", 
                   "New_Years_Eve_ind","Memorial_Day_week_ind","Memorial_Day_ind","Halloween_ind",              
                   "Thanksgiving_ind","Christmas_week_ind","Christmas_ind","Christmas_week_bfr_ind",    
                   "Halloween_week_ind","Thanksgiving_week_ind","Independence_Day_week_ind","Easter_week_ind",             
                   "New_Years_Eve_week_ind","Easter_ind","Independence_Day_ind","Thanksgiving_week_aft_ind")

hourly_group_by_cols <- c("Source","Network", "Demo", "HH_NT_Daypart","Broadcast_Year",
                          "Cable_Qtr","Week","Date","FirstAiring","Show_Name","program_type",
                          "Jan", "Feb", "Mar", "Apr", "May", "Jun",
                          "Jul", "Aug","Sep","Oct", "Nov", "Dec", 
                          "New_Years_Eve_ind","Memorial_Day_week_ind","Memorial_Day_ind","Halloween_ind",              
                          "Thanksgiving_ind","Christmas_week_ind","Christmas_ind","Christmas_week_bfr_ind",    
                          "Halloween_week_ind","Thanksgiving_week_ind","Independence_Day_week_ind","Easter_week_ind",             
                          "New_Years_Eve_week_ind","Easter_ind","Independence_Day_ind","Thanksgiving_week_aft_ind")

daily_group_by_cols <- c("Source","Network","Demo","HH_NT_Daypart","Broadcast_Year",
                         "Cable_Qtr","Week","Date","Jan","Feb","Mar","Apr","May","Jun",
                         "Jul","Aug","Sep","Oct","Nov","Dec", 
                         "New_Years_Eve_ind","Memorial_Day_week_ind","Memorial_Day_ind","Halloween_ind",              
                         "Thanksgiving_ind","Christmas_week_ind","Christmas_ind","Christmas_week_bfr_ind",    
                         "Halloween_week_ind","Thanksgiving_week_ind","Independence_Day_week_ind","Easter_week_ind",             
                         "New_Years_Eve_week_ind","Easter_ind","Independence_Day_ind","Thanksgiving_week_aft_ind")

weekly_group_by_cols <- c("Source","Network","Demo","HH_NT_Daypart","Broadcast_Year",
                          "Cable_Qtr","Week","Jan","Feb","Mar","Apr","May","Jun",
                          "Jul","Aug","Sep","Oct","Nov","Dec",
                          "Memorial_Day_week_ind","Christmas_week_ind","Christmas_week_bfr_ind",    
                          "Halloween_week_ind","Thanksgiving_week_ind","Independence_Day_week_ind","Easter_week_ind",             
                          "New_Years_Eve_week_ind","Thanksgiving_week_aft_ind")

# :::::::::: VARIABLES TO KEEP IN THE INITIAL DATA FILTERING
keep_cols <- c("Source","Network", "Demo", "Broadcast_Year", "Cable_Qtr", "Date", "Week",
               "Show_Name", "NBC_Show_Name", "ShowName_Orig","program_type",
               "Start_Time", "End_Time", "Half_Hr", "HH_NT_Daypart","FirstAiring",
               "Tot_UE", "LS_Imps", "Nielsen_LS_Rating", "LS_Dur",
               "SC3_Impressions", "Nielsen_SC3_Rating", "SC3_C_Dur",
               "Jan", "Feb", "Mar", "Apr", "May", "Jun",
               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
               "Sun","Mon","Tue","Wed","Thu","Fri","Sat", 
               "New_Years_Eve_ind","Memorial_Day_week_ind","Memorial_Day_ind","Halloween_ind",              
               "Thanksgiving_ind","Christmas_week_ind","Christmas_ind","Christmas_week_bfr_ind",    
               "Halloween_week_ind","Thanksgiving_week_ind","Independence_Day_week_ind","Easter_week_ind",             
               "New_Years_Eve_week_ind","Easter_ind","Independence_Day_ind","Thanksgiving_week_aft_ind","DSN_SNAPPED_FA")

# ::::::::: VARIABLES TO KEEP IN THE OUTPUT DATA
output_cols <- c("NETWORK","DEMO","MODEL_TYPE","LAST_UPDATE","DATA_VERSION","MODEL_VERSION","STREAM","TIME_ID","SHOW_NAME",
                 "PROGRAM_TYPE","HH_NT_DAYPART","BROADCAST_YEAR","CABLE_QTR","QTRYEAR","BROADCAST_WEEK","BROADCAST_DATE",
                 "HOURS","HALF_HR","ACTUAL","PREDICT")
