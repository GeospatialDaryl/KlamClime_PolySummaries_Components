RWC_11482500_DailyQ_cln <- read.delim("X:/_05_KlamClime/07_inputTabular/USGS_Gage/RWC_11482500_DailyQ_cln.txt")

dfRWCq <- tbl_df(RWC_11482500_DailyQ_cln)
dfRWCq$Date <- as.Date(dfRWCq$datetime, "%Y-%m-%d")

