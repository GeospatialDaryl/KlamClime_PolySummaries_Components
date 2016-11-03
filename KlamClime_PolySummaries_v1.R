library(dplyr)
library(ggplot2)
library(tidyr)
library(reshape2)
library(maptools)
library(lazyeval)

genSeq <- function(inputList){
                i = 1
                mybiglist <- vector('integer',1116)
             for( year in inputList){
                     #print(year)
                     j = 1
                     rep(year,12) -> tmp
                     while(j < 13){
                             mybiglist[i] <- tmp[j]
                             i = i + 1
                             j = j + 1
                     }
                     
             }
        return(mybiglist)
        }

makeMeans_mm_to_m <- function(inputDF, par = "ppt"){
        
        #vDates <- as.Date(inputDF$TIMESTEP.ppt)
        
        select(inputDF, contains("mean")) -> meanTemp
        inputDF$hy -> vHY
        meanTemp_noHY <- meanTemp
        meanTemp_m <- meanTemp_noHY/1000
        tbl_df(meanTemp_m) -> meanTemp2     #now in m, 16
        
        #now calc Areas
        zeros <- matrix(rep(0.0,16*16),nrow = 16)
        diag(zeros) <- as.matrix(vAreas)
        
        if("hy" %in% names(meanTemp2)){
                meanTemp2 <- select(meanTemp2, -hy)
        }
        as.matrix(meanTemp2) %*% zeros -> tmp3
        
        tmpNames <- names(meanTemp)
        gsub(".m..",c(".m3.",par,"V"),tmpNames) -> newNames
        #return(newNames)
        dfTmp <- tbl_df(tmp3)
        
        dfTmp$Date <- as.Date(inputDF$TIMESTEP)
        dfTmp$hy <- vHY
        names(dfTmp)<- c(newNames[2:17],"Date", "HY")
        return(dfTmp)
}

makeMeans_mm_to_m_Hist <- function(inputDF, par = "ppt"){
        
        #vDates <- as.Date(inputDF$TIMESTEP.ppt)
        
        select(inputDF, contains("mean")) -> meanTemp
        inputDF$hy -> vHY
        meanTemp_noHY <- meanTemp
        meanTemp_m <- meanTemp_noHY/1000
        tbl_df(meanTemp_m) -> meanTemp2     #now in m, 16
        
        #now calc Areas
        zeros <- matrix(rep(0.0,16*16),nrow = 16)
        diag(zeros) <- as.matrix(vAreas)
        
        if("hy" %in% names(meanTemp2)){
                meanTemp2 <- select(meanTemp2, -hy)
        }
        as.matrix(meanTemp2) %*% zeros -> tmp3
        
        tmpNames <- names(meanTemp)
        gsub(".m..",c(".m3.",par,"V"),tmpNames) -> newNames
        #return(newNames)
        dfTmp <- tbl_df(tmp3)
        
        dfTmp$Date <- as.Date(inputDF$TIMESTEP)
        dfTmp$hy <- vHY
        names(dfTmp)<- c(newNames,"Date", "HY")
        return(dfTmp)
}

grabSID <- function(inputSID = 1003){
        C1 <- dfAccPPT_PCM$Date
        indxSID <- inputSID - 999
        C2  <- as.vector(dfAccPPT_GISS[,indxSID])
        C3  <- as.vector(dfAccPPT_CCSM[,indxSID])
        C4  <- as.vector(dfAccPPT_Miro[,indxSID])
        C5  <- as.vector(dfAccPPT_PCM[,indxSID])
        C6  <- as.vector(dfAccPPT_CNRM[,indxSID])
        names <- c("Date", "GISSpptV","CCSMpptV", "MiroPptV", "PCMpptV", "CNRMpptV")
        
        out<- cbind(C1, C2, C3, C4, C5, C6)
        names(out) <- names
        dfOut <- tbl_df(out)
        #names(dfOut) <- names
        return(dfOut)
}

grabSIDHist <- function(inputDF, inputSID = 1003){
        C1 <- inputDF$Date
        indxSID <- inputSID - 999
        C2  <- as.vector(inputDF[,indxSID])
        names <- c("Date", "HistpptV")
        
        out<- cbind(C1, C2)
        names(out) <- names
        dfOut <- tbl_df(out)
        #names(dfOut) <- names
        return(dfOut)    
}

getQuant <- function(inputVal, dfQuants=dfRWCQuants){
        for(i in 1:100){
                logicTest <- (dfQuants$Q_cfs[i] > inputVal)
                #print(logicTest)
                if(logicTest){
                        returnVal <- dfQuants$percentile[i]
                        return(returnVal)
                }
        }
        return(1.00)
}

buildQuants <- function(inputDF){
        
        
}     

nameFixer<- function(inputDF, suffix = ".ppt"){
        paste(names(inputDF), suffix, sep = "") -> names1
        inputDF$TIMESTAMP <- inputDF$TIMESTAMP.ppt
        names(inputDF) <- names1
}

# # # # # 
#  __
# [  ]
# [  ]
# \--/
#
# Load Data
# # Ppt

Phase1Huc10_CCSMrpc85_Ppt <- read.csv("X:/_05_KlamClime/07_inputTabular/Huc10/PPT/Phase1Huc10_CCSMrpc85_Ppt.csv")
Phase1Huc10_CNRMcm5rcp85_Ppt <- read.csv("X:/_05_KlamClime/07_inputTabular/Huc10/PPT/Phase1Huc10_CNRMcm5rcp85_Ppt.csv")
Phase1Huc10_GISS.AOM.A1B_Ppt <- read.csv("X:/_05_KlamClime/07_inputTabular/Huc10/PPT/Phase1Huc10_GISS-AOM-A1B_Ppt.csv")
Phase1Huc10_Miroc5Rcp26_Ppt <- read.csv("X:/_05_KlamClime/07_inputTabular/Huc10/PPT/Phase1Huc10_Miroc5Rcp26_Ppt.csv")
Phase1Huc10_PCM.A2_Ppt <- read.csv("X:/_05_KlamClime/07_inputTabular/Huc10/PPT/Phase1Huc10_PCM-A2_Ppt.csv")
#  Temp Data
#Phase1Huc10DEM4_CCSM_TMax_3p <- read.csv("X:/_05_KlamClime/07_inputTabular/Huc10DEM4/CCSM/Phase1Huc10DEM4_CCSM_TMax_3p.csv")

# #  Runoff
Phase1Huc10_CCSM_Run <- read.csv("X:/_05_KlamClime/07_inputTabular/Huc10/RUN/Phase1Huc10_CCSMrpc85_Run.csv")
Phase1Huc10_CNRM_Run <- read.csv("X:/_05_KlamClime/07_inputTabular/Huc10/RUN/Phase1Huc10_CNRMcm5rcp85_Run.csv")
Phase1Huc10_GISS_Run <- read.csv("X:/_05_KlamClime/07_inputTabular/Huc10/RUN/Phase1Huc10_GISS-AOM-A1B_Run.csv")
Phase1Huc10_Miro_Run <- read.csv("X:/_05_KlamClime/07_inputTabular/Huc10/RUN/Phase1Huc10_Miroc5Rcp26_Run.csv")
Phase1Huc10_PCM_Run  <- read.csv("X:/_05_KlamClime/07_inputTabular/Huc10/RUN/Phase1Huc10_PCM-A2_Run.csv")

# Copy to DF and Delete Temp Tables
# Precip
dfGISS <- tbl_df(Phase1Huc10_GISS.AOM.A1B_Ppt)
dfCCSM <- tbl_df(Phase1Huc10_CCSMrpc85_Ppt)
dfCNRM <- tbl_df(Phase1Huc10_CNRMcm5rcp85_Ppt)
dfPCM  <- tbl_df(Phase1Huc10_PCM.A2_Ppt)
dfMiro <- tbl_df(Phase1Huc10_Miroc5Rcp26_Ppt)

dfGISSrun <- tbl_df(Phase1Huc10_GISS_Run)
dfCCSMrun <- tbl_df(Phase1Huc10_CCSM_Run)
dfCNRMrun <- tbl_df(Phase1Huc10_CNRM_Run)
dfPCMrun  <- tbl_df(Phase1Huc10_PCM_Run)
dfMirorun <- tbl_df(Phase1Huc10_Miro_Run)

rm(Phase1Huc10_PCM.A2_Ppt)
rm(Phase1Huc10_CCSMrpc85_Ppt)
rm(Phase1Huc10_Miroc5Rcp26_Ppt)
rm(Phase1Huc10_CNRMcm5rcp85_Ppt)
rm(Phase1Huc10_GISS.AOM.A1B_Ppt)

rm(Phase1Huc10_CCSM_Run)
rm(Phase1Huc10_CNRM_Run)
rm(Phase1Huc10_GISS_Run)
rm(Phase1Huc10_Miro_Run)
rm(Phase1Huc10_PCM_Run)

dfGISS$Date <- as.Date(dfGISS$TIMESTEP)
dfPCM$Date  <- as.Date(dfPCM$TIMESTEP)
dfMiro$Date <- as.Date(dfMiro$TIMESTEP)
dfCNRM$Date <- as.Date(dfCNRM$TIMESTEP)
dfCCSM$Date <- as.Date(dfCCSM$TIMESTEP)

dfGISSrun$Date <- as.Date(dfGISSrun$TIMESTEP)
dfPCMrun$Date  <- as.Date(dfPCMrun$TIMESTEP)
dfMirorun$Date <- as.Date(dfMirorun$TIMESTEP)
dfCNRMrun$Date <- as.Date(dfCNRMrun$TIMESTEP)
dfCCSMrun$Date <- as.Date(dfCCSMrun$TIMESTEP)

dfGISS$hy[1:1116] <- genSeq(2006:2100)[1:1116] 
dfPCM$hy[1:1116]  <- genSeq(2006:2100)[1:1116] 
dfMiro$hy[1:1116] <- genSeq(2006:2100)[1:1116]
dfCNRM$hy[1:1116] <- genSeq(2006:2100)[1:1116] 
dfCCSM$hy[1:1116] <- genSeq(2006:2100)[1:1116]

dfGISSrun$hy[1:1116] <- genSeq(2006:2100)[1:1116] 
dfPCMrun$hy[1:1116]  <- genSeq(2006:2100)[1:1116] 
dfMirorun$hy[1:1116] <- genSeq(2006:2100)[1:1116]
dfCNRMrun$hy[1:1116] <- genSeq(2006:2100)[1:1116] 
dfCCSMrun$hy[1:1116] <- genSeq(2006:2100)[1:1116]

dfGISS$hyf <- paste("hy",dfGISS$hy, sep = "")
dfPCM$hyf  <- paste("hy",dfPCM$hy, sep = "")
dfMiro$hyf <- paste("hy",dfMiro$hy, sep = "")
dfCNRM$hyf <- paste("hy",dfCNRM$hy, sep = "")
dfCCSM$hyf <- paste("hy",dfCCSM$hy, sep = "")

dfGISSrun$hyf <- paste("hy",dfGISSrun$hy, sep = "")
dfPCMrun$hyf  <- paste("hy",dfPCMrun$hy, sep = "")
dfMirorun$hyf <- paste("hy",dfMirorun$hy, sep = "")
dfCNRMrun$hyf <- paste("hy",dfCNRMrun$hy, sep = "")
dfCCSMrun$hyf <- paste("hy",dfCCSMrun$hy, sep = "")

nameFixer<- function(inputDF, suffix = ".ppt"){
        paste(names(inputDF), suffix, sep = "") -> names1
        inputDF$TIMESTAMP <- inputDF$TIMESTAMP.ppt
        names(inputDF) <- names1
        }
nameFixer(dfGISS)
nameFixer(dfPCM)
nameFixer(dfMiro)
nameFixer(dfCNRM)
nameFixer(dfCCSM)

group_by(dfGISS, hy) -> dfGISS
group_by(dfPCM,  hy) -> dfPCM
group_by(dfMiro, hy) -> dfMiro
group_by(dfCCSM, hy) -> dfCCSM
group_by(dfCNRM, hy) -> dfCNRM

tblTmnSlopes <- read.csv("E:/tblTmnSlopes.txt")
areas <- cbind(tblTmnSlopes$SHCID, tblTmnSlopes$Area_m2)
areas3 <- areas
areas3[2:15, ] <- areas[1:14,]
areas3[1,] <- areas[15,]
areas3
dfAreas <- tbl_df(areas3)
names(dfAreas) <- c("SID", "Area_m2")
dfAreas[16,2] <- sum(dfAreas[,2]) 
dfAreas[16,1] <- 9999.
vAreas <- as.vector(dfAreas[,2])


dfAccPPT_GISS <- makeMeans_mm_to_m(dfGISS)
dfAccPPT_CCSM <- makeMeans_mm_to_m(dfCCSM)
dfAccPPT_Miro <- makeMeans_mm_to_m(dfMiro)
dfAccPPT_PCM  <- makeMeans_mm_to_m(dfPCM)
dfAccPPT_CNRM <- makeMeans_mm_to_m(dfCNRM)
x
dfRWC <- grabSID(1003)
dfRWCl <- melt(dfRWC, id="Date")

rwcTQ <- read.csv("X:/_05_KlamClime/07_inputTabular/USGS_Gage/RWC_tQ.csv")
dfRWCobs <- tbl_df(rwcTQ)

g <- ggplot(data = dfRWCl, aes(x = Date, y = value), color = variable)

ggplot(data = dfRWCl, aes(x = Date, y = value, color = variable)) +
        geom_line() + coord_cartesian(xlim = c(dfRWC$Date[900],dfRWC$Date[924])) +
        ggtitle("Redwood Creek Total Monthly Volume of Precip") +
        ylab("Precipitation/Month (m3/mo)")

#       #       #
#    Historic Data
#       #       #
Phase1Huc10_Historic_Ppt <- read.csv("X:/_05_KlamClime/07_inputTabular/Huc10/Hist/Phase1Huc10_Historic_Ppt.csv")
dfHistPpt <- tbl_df(Phase1Huc10_Historic_Ppt)
dfHistPpt$Date <- as.Date(dfHistPpt$TIMESTEP)

dfRWCtq <- tbl_df(rwcTQ)
dfRWCtq$Datef <- dfRWCtq$Date

dfRWCtq$Date <- as.Date(dfRWCtq$Datef, "%m/%d/%Y")

#g <- ggplot(dfm1, aes(y = dfm1$`dfRWC_Model_Obs$Q_cfs`, x = dfm1$`dfRWC_Model_Obs$HistpptV`))
#g + geom_point() + geom_smooth(method = "lm", se = FALSE)


dfHistPpt$Date <- as.Date(dfHistPpt$TIMESTEP)
makeMeans_mm_to_m_Hist(dfHistPpt) -> dfAccPPT_Hist
grabSIDHist(dfAccPPT_Hist) -> dfAccPPT_Hist_RWC

inner_join(dfAccPPT_Hist_RWC, dfRWCtq, by = "Date") -> dfRWC_Model_Obs
dfRWC_Model_Obs$PptV <- dfRWC_Model_Obs$HistpptV
m1 <- lm(Q_cfs~PptV  ,data = dfRWC_Model_Obs)

quants <- 0:100/100
RWC_11482500_DailyQ_cln <- read.delim("X:/_05_KlamClime/07_inputTabular/USGS_Gage/RWC_11482500_DailyQ_cln.txt")

dfRWCq <- tbl_df(RWC_11482500_DailyQ_cln)
dfRWCq$Date <- as.Date(dfRWCq$datetime, "%Y-%m-%d")

quantile(dfRWCq$Q_cfs, quants,na.rm = TRUE) -> dfRWCQuants
tbl_df(dfRWCQuants) -> dfRWCQuants

names(dfRWCQuants) <- "Q_cfs"
dfRWCQuants$percentile <- quants


dfAccPPT_PCM %>% group_by(HY) %>% summarize( sumHY = sum(SIDALLMEAN.m3.)) -> dfAccPPT_PCM_HYSums
summary(lm(dfAccPPT_PCM_HYSums$sumHY~ dfAccPPT_PCM_HYSums$HY))

dfAccPPT_CCSM %>% group_by(HY) %>% summarize( sumHY = sum(SIDALLMEAN.m3.)) -> dfAccPPT_CCSM_HYSums
summary(lm(dfAccPPT_CCSM_HYSums$sumHY~ dfAccPPT_CCSM_HYSums$HY))

dfAccPPT_GISS %>% group_by(HY) %>% summarize( sumHY = sum(SIDALLMEAN.m3.)) -> dfAccPPT_GISS_HYSums
summary(lm(dfAccPPT_GISS_HYSums$sumHY~ dfAccPPT_GISS_HYSums$HY))

dfAccPPT_Miro %>% group_by(HY) %>% summarize( sumHY = sum(SIDALLMEAN.m3.)) -> dfAccPPT_Miro_HYSums
summary(lm(dfAccPPT_Miro_HYSums$sumHY~ dfAccPPT_Miro_HYSums$HY))

dfAccPPT_CNRM %>% group_by(HY) %>% summarize( sumHY = sum(SIDALLMEAN.m3.)) -> dfAccPPT_CNRM_HYSums
summary(lm(dfAccPPT_CNRM_HYSums$sumHY~ dfAccPPT_CNRM_HYSums$HY))

AccPPT_by_SID <- function( inputDF, stats = c("SID1000MEAN.m3.","SID1001MEAN.m3.","SID1002MEAN.m3.","SID1003MEAN.m3.",
                                              "SID1004MEAN.m3.","SID1005MEAN.m3." ,"SID1006MEAN.m3." ,"SID1007MEAN.m3." ,"SID1008MEAN.m3." ,
                                              "SID1009MEAN.m3.","SID1010MEAN.m3." ,"SID1011MEAN.m3." ,"SID1012MEAN.m3." ,"SID1013MEAN.m3." ,
                                              "SID1014MEAN.m3.", "SIDALLMEAN.m3.")){
        
        outputDF <- tbl_df(stats)
        outputDF$slope <- rep(0,16)
        outputDF$intercept <- rep(0,16)
        outputDF$slope_pval <- rep(0,16)
        outputDF$intercept_pval <-rep(0,16)
        outputDF$slope_stdErr <- rep(0,16)
        outputDF$intercept_stdErr <- rep(0,16)
        
        i = 1
        for( stat in stats){
                inputDF %>% 
                        group_by(HY) %>%
                        summarize_( sumHY = interp(~sum(var, na.rm = TRUE),var = as.name(stat))) -> inputDF_HYSums
                        #summarize( sumHY = sum(stat)) -> inputDF_HYSums
                
                summary(lm(inputDF_HYSums$sumHY~ dfAccPPT_PCM_HYSums$HY)) -> lmSummary
                lmSummary$coef[1] -> outputDF$intercept[i]
                lmSummary$coef[2] -> outputDF$slope[i]
                lmSummary$coef[3] -> outputDF$intercept_stdErr[i]
                lmSummary$coef[4] -> outputDF$slope_stdErr[i]
                lmSummary$coef[7] -> outputDF$intercept_pval[i]
                lmSummary$coef[8] -> outputDF$slope_pval[i]
                
                i <- i + 1
                
        }
        return(outputDF)
}   

AccPPT_by_SID(dfAccPPT_PCM) -> dfLMsPCM
AccPPT_by_SID(dfAccPPT_CCSM) -> dfLMsCCSM
AccPPT_by_SID(dfAccPPT_CNRM) -> dfLMsCNRM
AccPPT_by_SID(dfAccPPT_GISS) -> dfLMsGISS
AccPPT_by_SID(dfAccPPT_Miro) -> dfLMsMiro

dfLMsAll <- dfLMsCCSM
dfLMsAll$param <- dfLMsAll$value
dfLMsCCSM$param <- dfLMsCCSM$value
dfLMsPCM$param <- dfLMsPCM$value
dfLMsGISS$param <- dfLMsGISS$value
dfLMsMiro$param <- dfLMsMiro$value
dfLMsCNRM$param <- dfLMsCNRM$value


names(dfLMsAll) -> names1
names1[2:7] -> goodNames
paste(goodNames, "_CCSM", sep = "") -> namesCCSM
paste(goodNames, "_PCM", sep = "") -> namesPCM
paste(goodNames, "_GISS", sep = "") -> namesGISS
paste(goodNames, "_Miro", sep = "") -> namesMiro
paste(goodNames, "_CNRM", sep = "") -> namesCNRM

c(names1[8],namesCCSM) -> names(dfLMsCCSM)
c(names1[8],namesPCM) -> names(dfLMsPCM)
c(names1[8],namesGISS) -> names(dfLMsGISS)
c(names1[8],namesMiro) -> names(dfLMsMiro)
c(names1[8],namesCNRM) -> names(dfLMsCNRM)

dfLMsAll <- dfLMsAll[,1]
names(dfLMsAll) <- "param"
dfLMsAll$SID <- as.integer(c(1000:1014, -9999))

left_join(dfLMsAll, dfLMsPCM[,1:2], by = "param") -> dfLMsAll1
left_join(dfLMsAll1, dfLMsCCSM[,1:2], by = "param") -> dfLMsAll2
left_join(dfLMsAll2, dfLMsGISS[,1:2], by = "param") -> dfLMsAll3
left_join(dfLMsAll3, dfLMsMiro[,1:2], by = "param") -> dfLMsAll4
left_join(dfLMsAll4, dfLMsCNRM[,1:2], by = "param") -> dfLMsAllslope

mutate(dfLMsAllslope, avg = (slope_PCM + slope_CCSM + slope_GISS + slope_Miro + slope_CNRM )/5.  ) -> dfLMsAll
select(dfLMsAllslope, -(SID) ) -> noSID
dfLMsAllslope[1:15,] -> dF
melt(dF ,id = "param") -> dF
ggplot(data = dF[16:90,], aes(x = param, y = value, color = variable)) -> g
g + geom_point(aes(group = variable)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))

write.csv(dfLMsCCSM, file = "dfLMsCCSM.csv")
write.csv(dfLMsPCM, file = "dfLMsPCM.csv")
write.csv(dfLMsGISS, file = "dfLMsGISS.csv")
write.csv(dfLMsMiro, file = "dfLMsMiro.csv")
write.csv(dfLMsCNRM, file = "dfLMsCNRM.csv")


