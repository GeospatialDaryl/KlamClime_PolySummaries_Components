processHUC10csv <- function(inputDF, killInput = TRUE){
        # FUNCTIONS #
        nameFixer<- function(inputDF, suffix = ".ppt"){
                paste(names(inputDF), suffix, sep = "") -> names1
                inputDF$TIMESTAMP <- inputDF$TIMESTAMP.ppt
                names(inputDF) <- names1
        }
        # # # # #
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
        # # # # #
        
        
        
        dfInput <- tbl_df(inputDF)
        
        if(killInput){rm(inputDF)}
        dfInput$Date <- as.Date(dfInput$TIMESTEP)
        dfInput$hy[1:1116] <- genSeq(2006:2100)[1:1116]      
        dfInput$hyf <- paste("hy",dfInput$hy, sep = "")
        return(dfInput)
        
}

processHUC10csv(dfGISSrun)