require(gridExtra)
require(ggplot2)

#import the satellite data, changing this will mess up the whole thing.
z_satData <- read.csv("E:/grad school/DIR Lab/Environmental Indices/CUTI_BEUTI_39N_34N.csv")
z_numVec <- seq(1,12085,by=1)
z_satData$dayNumber=z_numVec

#import the urchin collection dataset. Should be a ".csv" file collection dates in the first column in the format yyyy-mm-dd
#second column should be either the number 39 or 34, depending on collection site.
z_urchinData <- read.csv("E:/grad school/DIR Lab/Environmental Indices/colldate_site.csv")
z_urchinData <- na.omit(z_urchinData)

#add new columns to the dataset aka "z_urchinData".
z_urchinData$beginDate=NA
z_urchinData$minCUTI=NA
z_urchinData$maxCUTI=NA
z_urchinData$meanCUTI=NA
z_urchinData$sdCUTI=NA
z_urchinData$minBEUTI=NA
z_urchinData$maxBEUTI=NA
z_urchinData$meanBEUTI=NA
z_urchinData$sdBEUTI=NA

#Add functions into R memory. Run this once before running the for loop below.
getSumData39 <- function(){
  z_urchinData$minCUTI[currRow]=min(currRowRange$X39N.CUTI)
  z_urchinData$maxCUTI[currRow]=max(currRowRange$X39N.CUTI)
  z_urchinData$meanCUTI[currRow]=mean(currRowRange$X39N.CUTI)
  z_urchinData$sdCUTI[currRow]=sd(currRowRange$X39N.CUTI)
  z_urchinData$minBEUTI[currRow]=min(currRowRange$X39N.BEUTI)
  z_urchinData$maxBEUTI[currRow]=max(currRowRange$X39N.BEUTI)
  z_urchinData$meanBEUTI[currRow]=mean(currRowRange$X39N.BEUTI)
  z_urchinData$sdBEUTI[currRow]=sd(currRowRange$X39N.BEUTI)
  return(z_urchinData)
}

getSumData34 <- function(){
  z_urchinData$minCUTI[currRow]=min(currRowRange$X34N.CUTI)
  z_urchinData$maxCUTI[currRow]=max(currRowRange$X34N.CUTI)
  z_urchinData$meanCUTI[currRow]=mean(currRowRange$X34N.CUTI)
  z_urchinData$sdCUTI[currRow]=sd(currRowRange$X34N.CUTI)
  z_urchinData$minBEUTI[currRow]=min(currRowRange$X34N.BEUTI)
  z_urchinData$maxBEUTI[currRow]=max(currRowRange$X34N.BEUTI)
  z_urchinData$meanBEUTI[currRow]=mean(currRowRange$X34N.BEUTI)
  z_urchinData$sdBEUTI[currRow]=sd(currRowRange$X34N.BEUTI)
  return(z_urchinData)
}

getDataWindow <- function(z_collectionDate,z_dateWindowSize){
  z_collectionDayNum = z_satData[z_satData$date==z_collectionDate,9]
  z_collectionRange = z_satData[z_satData$dayNumber<=z_collectionDayNum & z_satData$dayNumber>(z_collectionDayNum-z_dateWindowSize),]
  z_beginDate = z_satData$date[z_collectionDayNum-z_dateWindowSize]
  return(z_collectionRange)
}

getRangeGraph39 <- function(currRowRange,beginDate,z_breakNames){
  urchin_plot_BEUTI <- ggplot(data = currRowRange,  mapping = aes(x = date, y = X39N.BEUTI)) +
    geom_point(alpha = 0.9, aes(color = X39N.BEUTI)) +
    labs(x = "Date",
         y = "BEUTI (mmol/s*m)",
         title = "BEUTI at 39N (Fort Bragg)",
         subtitle = paste("From", beginDate, "to", currRowDate, "(collection)",sep=" ")) + theme_bw(base_size = 9) +
    theme(axis.text.x = element_text(angle=45, hjust = 1))+
    scale_x_discrete(breaks=z_breakNames)
  urchin_plot_CUTI <- ggplot(data = currRowRange,  mapping = aes(x = date, y = X39N.CUTI)) +
    geom_point(alpha = 0.9, aes(color = X39N.CUTI)) +
    labs(x = "Date",
         y = "CUTI (m2/s)",
         title = "CUTI at 39N (Fort Bragg)",
         subtitle = paste("From", beginDate, "to", currRowDate, "(collection)",sep=" ")) + theme_bw(base_size = 9) +
    theme(axis.text.x = element_text(angle=45, hjust = 1))+
    scale_x_discrete(breaks=z_breakNames)
  return(arrangeGrob(urchin_plot_BEUTI,urchin_plot_CUTI,nrow=2))
}

getRangeGraph34 <- function(currRowRange,beginDate,z_breakNames){
  urchin_plot_BEUTI <- ggplot(data = currRowRange,  mapping = aes(x = date, y = X34N.BEUTI)) +
    geom_point(alpha = 0.9, aes(color = X34N.BEUTI)) +
    labs(x = "Date",
         y = "BEUTI (mmol/s*m)",
         title = "BEUTI at 34N (Gaviota)",
         subtitle = paste("From", beginDate, "to", currRowDate, "(collection)",sep=" ")) + theme_bw(base_size = 9) +
    theme(axis.text.x = element_text(angle=45, hjust = 1))+
    scale_x_discrete(breaks=z_breakNames)
  urchin_plot_CUTI <- ggplot(data = currRowRange,  mapping = aes(x = date, y = X34N.CUTI)) +
    geom_point(alpha = 0.9, aes(color = X34N.CUTI)) +
    labs(x = "Date",
         y = "CUTI (m2/s)",
         title = "CUTI at 34N (Gaviota)",
         subtitle = paste("From", beginDate, "to", currRowDate, "(collection)",sep=" ")) + theme_bw(base_size = 9) +
    theme(axis.text.x = element_text(angle=45, hjust = 1))+
    scale_x_discrete(breaks=z_breakNames)
  return(arrangeGrob(urchin_plot_BEUTI,urchin_plot_CUTI,nrow=2))
}

#gets the list of dates to display on the x axis. Change "z_axisDateSpread" to change the interval displayed (in days)"
getBreakNames <- function(currRowRange){
  z_axisDateSpread = 7
  z_breakNums <- seq(1,nrow(currRowRange),by=z_axisDateSpread)
  z_breakNames <- lapply(z_breakNums,function(x) currRowRange$date[x])
  return(z_breakNames)
}

#Function for retrieving a window of data
#Example: getDataWindow("<startDate>",<numberOfDaysToGoBackwards>)
#Example: getDataWindow("1999-01-01",100) will fetch the data 100 days before (and including) Jan 1, 1999.
#change z_dateWindowSize to change the number of days backwards from the collection date (collection date is included in the range).
z_dateWindowSize = 101

for(currRow in seq(1,nrow(z_urchinData),by=1)){
  currRowDate = z_urchinData[currRow,1]
  currRowRange = getDataWindow(currRowDate,z_dateWindowSize)
  z_beginDayNum = min(currRowRange$dayNumber)
  z_urchinData$beginDate[currRow] = z_satData$date[z_satData$dayNumber==z_beginDayNum]
  z_breakNames = getBreakNames(currRowRange)
  if (z_urchinData[currRow,2]==39){
    z_urchinData = getSumData39()
    g <- getRangeGraph39(currRowRange,z_urchinData$beginDate[currRow],z_breakNames)
  }else{
    z_urchinData = getSumData34()
    g <- getRangeGraph34(currRowRange,z_urchinData$beginDate[currRow],z_breakNames)
  }
  grid.arrange(g)
  z_filename = paste(currRowDate,".png",sep="")
  ggsave(filename=z_filename,g)
  write.csv(z_urchinData,file="Urchin_Summary_data.csv")
}
