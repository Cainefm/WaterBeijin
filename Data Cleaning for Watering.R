#==================
#       水数据修整
#       FanMin
#       Estabilish:20180101
#       Last:20180119
#==================
library(readxl)
library(ggplot2)

setwd("/Users/fanmin/Documents/GitHub/Water_Beijing")
filenames <- list.files(pattern = "*.xlsx")
for( fns in filenames){
        
        dat <- read_excel(fns)
        print(fns)
        colnames(dat) <- c("ID","Date","Water.Content.20m","Water.Content.40m","Water.Content.60m",
                           "Water.Content.80m","Water.Content.100m")
        dat$Date <- as.POSIXct(strptime(dat$Date,"%m/%d/%y %H:%M:%S"))
        dat <- melt(dat,id.vars=c("Date","ID"))
        ppi <- 300
        png(paste("./plot/",(gsub(".xlsx","",fns)),".png",sep=""), width=9*ppi, height=6*ppi, res=ppi)
        myplot <- ggplot(dat,aes(x=Date,y=value,group=variable,col=variable))+
                geom_line()+
                facet_grid(ID~.,scales="free_y")+
                ggtitle(paste("Water contenct in the dirt in ",(gsub(".xlsx","",fns)),sep=""))+
                theme(plot.title = element_text(hjust = 0.5))
        print(myplot)
        dev.off()
}


#先处理2017的数据，比较好
dat2017 <- read_excel("2017.xlsx")
dim(dat2017)
table(dat$variable)
