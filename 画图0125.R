#==================
#       水数据修整
#       FanMin
#       Estabilish:20180101
#       Last:20180119
#==================
library(readxl)
library(ggplot2)
library(reshape2)
setwd("/Users/fanmin/Documents/GitHub/R/Water_Beijing")
foldernames <-
        paste("/Users/fanmin/Documents/GitHub/R/Water_Beijing",
              dir(pattern = "milan"),
              sep = "/")
for (yearsfolder in foldernames) {
        print(yearsfolder)
        year <- substr(yearsfolder, 53, 59)
        print(yearsfolder)
        setwd(yearsfolder)
        filenames <- list.files(pattern = "*.csv")
        for (fns in filenames) {
                temp <-
                        read.csv(
                                fns,
                                fileEncoding = "GB2312",
                                skip = 1,
                                stringsAsFactors = F
                        )
                if (fns == list.files()[1]) {
                        temp$X. <- substr(fns, 1, 2)
                        db <- temp
                } else{
                        temp$X. <- substr(fns, 1, 2)
                        db <- rbind(db, temp)
                }
        }
        dat <- db
        colnames(dat) <-c("ID","Date","Water.Content.20m","Water.Content.40m","Water.Content.60m","Water.Content.80m","Water.Content.100m")
        if (year == "2013") {
                dat$Date <-
                        as.POSIXct(strptime(dat$Date, "%m/%d/%y %H时%M分%S秒"))
        } else{
                dat$Date <- gsub("上午", "AM ", dat$Date)
                dat$Date <- gsub("下午", "PM ", dat$Date)
                dat$Date <-
                        as.POSIXct(strptime(dat$Date, "%m/%d/%y %p %I时%M分%S秒"))
        }
        dat$Water.Content.20m <- as.numeric(dat$Water.Content.20m)
        dat$Water.Content.40m <- as.numeric(dat$Water.Content.40m)
        dat$Water.Content.60m <- as.numeric(dat$Water.Content.60m)
        dat$Water.Content.80m <- as.numeric(dat$Water.Content.80m)
        dat$Water.Content.100m <- as.numeric(dat$Water.Content.100m)
        print(paste("以下是",year, "年数据的基本情况", sep =
                            ""))
        print(summary(dat))
        cat("\n")
        dat <- melt(dat, id.vars = c("Date", "ID"))
        dat1 <- dat[-which(dat$value<0),]
        dat2 <- dat
        dat2$value[which(dat2$value<0)] <- 0
        dat3 <- dat
        dat3$value[which(dat3$value<0)] <- dat3$value[which(dat3$value<0)]*-1
        jpeg(paste("/Users/fanmin/Documents/GitHub/R/Water_Beijing/test2/",year,".jpeg",sep=""),width = 1060, height = 500, units = "px", pointsize = 12,  quality = 300)
        graph <- ggplot(dat,aes(x = Date,y = value,group = variable,col = variable)) +
                geom_line() +
                facet_grid(ID ~ ., scales = "free_y") +
                ggtitle(paste("Water contenct in the earth in ",year,sep = "")) +
                theme(plot.title = element_text(hjust = 0.5),
                      legend.position = "bottom")
        plot(graph)
        dev.off()
        cat("\n")
        #no negative
        jpeg(paste("/Users/fanmin/Documents/GitHub/R/Water_Beijing/test2/",year,"_noNegative.jpeg",sep=""),width = 1060, height = 500, units = "px", pointsize = 12,  quality = 300)
        graph <- ggplot(dat1,aes(x = Date,y = value,group = variable,col = variable)) +
                geom_line() +
                facet_grid(ID ~ ., scales = "free_y") +
                ggtitle(paste("Water contenct in the earth in ",year,sep = "")) +
                theme(plot.title = element_text(hjust = 0.5),
                      legend.position = "bottom")
        plot(graph)
        dev.off()
        #negative = 0
        jpeg(paste("/Users/fanmin/Documents/GitHub/R/Water_Beijing/test2/",year,"_Negative0.jpeg",sep=""),width = 1060, height = 500, units = "px", pointsize = 12,  quality = 300)
        graph <- ggplot(dat2,aes(x = Date,y = value,group = variable,col = variable)) +
                geom_line() +
                facet_grid(ID ~ ., scales = "free_y") +
                ggtitle(paste("Water contenct in the earth in ",year,sep = "")) +
                theme(plot.title = element_text(hjust = 0.5),
                      legend.position = "bottom")
        plot(graph)
        dev.off()
        #negative *-1
        jpeg(paste("/Users/fanmin/Documents/GitHub/R/Water_Beijing/test2/",year,"_Negativebecomepositive.jpeg",sep=""),width = 1060, height = 500, units = "px", pointsize = 12,  quality = 300)
        graph <- ggplot(dat3,aes(x = Date,y = value,group = variable,col = variable)) +
                geom_line() +
                facet_grid(ID ~ ., scales = "free_y") +
                ggtitle(paste("Water contenct in the earth in ",year,sep = "")) +
                theme(plot.title = element_text(hjust = 0.5),
                      legend.position = "bottom")
        plot(graph)
        dev.off()
}




