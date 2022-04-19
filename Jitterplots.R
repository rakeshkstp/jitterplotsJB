# Clean the environment
rm(list=ls(all=TRUE))

# load libraries
library(lubridate)
library(pals)
library(reshape2)
library(dplyr)

# Set ggplot variables
source("C:/Mytheme.R")

# Set working library
setwd("D:/ACCES/JamesBay/TimeSeries")

# Output filename
filename <- "JB_Jitterplot_Stations"

save_figure <- TRUE

# load data
load("Extracted_ag440.RData")
load("Extracted_kdpar.RData")
load("Extracted_chl.RData")
load("Extracted_par0m.RData")
load("Extracted_spmn.RData")
load("Extracted_spmr.RData")

# remove the dates
alldates <- kdpar$Date
ag440$Date <- NULL
chl$Date <- NULL
kdpar$Date <- NULL
par0m$Date <- NULL
spmn$Date <- NULL
spmr$Date <- NULL

# identify the stations with no valid values
checkvalid <- function(df) {
  zmax <- nrow(df)
  zzz <- zmax-sapply(chl, function(x) sum(is.na(x)))
  which(zzz<50)}
zag440 <- checkvalid(ag440)
zchl   <- checkvalid(chl)
zkdpar <- checkvalid(kdpar)
zpar0m <- checkvalid(par0m)
zspmn  <- checkvalid(spmn)
zspmr  <- checkvalid(spmr)

# getting the index of station when atleast one of the product is not available
z <- unique(c(zag440,zchl,zkdpar,zpar0m,zspmn,zspmr))
rm(zag440,zchl,zkdpar,zpar0m,zspmn,zspmr)

# removing the stations with no valid data
ag440 <- ag440[,-z]
chl   <- chl[,-z]
kdpar <- kdpar[,-z]
par0m <- par0m[,-z]
spmn  <- spmn[,-z]
spmr  <- spmr[,-z]

# removing the indices from trapline
trapline <- trapline[-z]
rm(z)

# Changing names of the stations with trapline for averaging
names(ag440) <- trapline
names(chl)   <- trapline
names(kdpar) <- trapline
names(par0m) <- trapline
names(spmn)  <- trapline
names(spmr)  <- trapline


# Average columns with same names
avgCols <- function(df,trapline,Date){
  
  names(df) <- trapline # the names change while removing the date column, therefore, renaming again
  df <- as.data.frame(sapply(unique(names(df)), function(col) rowMeans(df[names(df) == col]))) # sapply returns a list, therefore it is converted to dataframe
  df <- cbind(Date, df)
  return(df)
}

ag440 <- avgCols(ag440,trapline,alldates)
chl   <- avgCols(chl,trapline,alldates)
kdpar <- avgCols(kdpar,trapline,alldates)
par0m <- avgCols(par0m,trapline,alldates)
spmn <- avgCols(spmn,trapline,alldates)
spmr <- avgCols(spmr,trapline,alldates)

# Getting unique trapline names and calling it stations
station <- unique(trapline)
rm(trapline)

# Melt the dataframes
ag440 <- melt(ag440,id.vars = "Date",variable.name = "Station",value.name = "ag440")
chl   <- melt(chl,  id.vars = "Date",variable.name = "Station",value.name = "chl")
kdpar <- melt(kdpar,id.vars = "Date",variable.name = "Station",value.name = "kdpar")
par0m <- melt(par0m,id.vars = "Date",variable.name = "Station",value.name = "par0m")
spmn  <- melt(spmn, id.vars = "Date",variable.name = "Station",value.name = "spmn")
spmr  <- melt(spmr, id.vars = "Date",variable.name = "Station",value.name = "spmr")


# Merging dataframes into a single one for plotting
pdata <- ag440
pdata <- merge(pdata,chl,  by=c("Date","Station"), all.x=TRUE)
pdata <- merge(pdata,kdpar,by=c("Date","Station"), all.x=TRUE)
pdata <- merge(pdata,par0m,by=c("Date","Station"), all.x=TRUE)
pdata <- merge(pdata,spmn, by=c("Date","Station"), all.x=TRUE)
pdata <- merge(pdata,spmr, by=c("Date","Station"), all.x=TRUE)


# Adding a column month for plotting
pdata$Month <- month(pdata$Date)


gbreaks <- station
glabels <- station
gnames <- station
mlabels <- c("1"="Jan","2"="Feb","3"="Mar","4"="Apr",
             "5"="May","6"="Jun","7"="Jul","8"="Aug",
             "9"="Sep","10"="Oct","11"="Nov","12"="Dec")

cag440 <- pdata %>% group_by(Station) %>% summarise(valpoints = sum(!is.na(ag440)))
cchl <- pdata %>% group_by(Station) %>% summarise(valpoints = sum(!is.na(chl)))
cspm <- pdata %>% group_by(Station) %>% summarise(valpoints = sum(!is.na(spmr)))

cag440 <- cag440[match(gbreaks, cag440$Station),]
cchl <- cchl[match(gbreaks, cchl$Station),]
cspm <- cspm[match(gbreaks, cspm$Station),]

ratio.display=4.5
xlim <- c(1,length(station))

ylbl <- expression(paste(italic("a"),{}[g],"(440 nm) [",m^{-1},"]"))
ymax <- max(pdata$ag440,na.rm=TRUE)
ylim <- c(0,ymax+0.1*ymax)
ratio.values <- (xlim[2]-xlim[1])/(ylim[2]-ylim[1])
asp_rat <- ratio.values/ratio.display
g1 <- ggplot(data = pdata) +
  geom_jitter(aes(x=factor(Station, level = gbreaks),y = ag440, fill=Month), 
              shape=21,show.legend = TRUE,stroke=0.1,size=2) +
  geom_violin(aes(x=Station ,y = ag440),trim=FALSE,show.legend = FALSE,alpha =0,size=1,color="black") +
  annotate("text", x = c(1:length(station)), y=ymax, label = paste0("N = ",cag440$valpoints),size = 8/.pt) +
  labs(title = "", color = "", fill = "") +
  coord_fixed(ratio = asp_rat, ylim = ylim,
              expand = FALSE, clip = "on") +
  scale_fill_gradientn(colours = kovesi.cyclic_mygbm_30_95_c78_s25(12),breaks=seq(1,12,1),
                       limits=c(1,12), labels=mlabels) +
  scale_x_discrete(name = "", breaks = gbreaks, labels = gnames)  +  #seq(ymin, ymax, ystp)
  scale_y_continuous(name = ylbl, limits = ylim,
                     breaks = pretty_breaks())  +  #seq(ymin, ymax, ystp)
  mytheme  + theme(legend.position = c(0,1.25),plot.margin = unit(c(0,0,0,0), "cm"),
                   axis.text.x = element_blank())
g1

ylbl <- expression(paste(italic("C"),{}[SPM]," [g",m^{-3},"]"))
ymax <- max(pdata$spmr,na.rm=TRUE)
ylim <- c(1,100)
ratio.values <- (xlim[2]-xlim[1])/(log10(ylim[2])-log10(ylim[1]))
asp_rat <- ratio.values/ratio.display
g2 <- ggplot(data = pdata) +
  geom_jitter(aes(x=factor(Station, level = gbreaks) ,y = spmr, fill=Month),
              shape=21,show.legend = FALSE,stroke=0.1,size=2) +
  geom_violin(aes(x=Station ,y = spmr),trim=FALSE,show.legend = FALSE,alpha =0,size=1,na.rm=TRUE,color="black")+
  annotate("text", x = c(1:length(station)), y=ymax, label = paste0("N = ",cspm$valpoints),size = 8/.pt) +
  labs(title = "", color = "", fill = "") +
  coord_fixed(ratio = asp_rat, ylim = ylim,
              expand = FALSE, clip = "on") +
  scale_fill_gradientn(colours = kovesi.cyclic_mygbm_30_95_c78_s25(12),breaks=seq(1,12,1),
                       limits=c(1,12), labels=mlabels) +
  scale_x_discrete(name = "", breaks = gbreaks, labels = gnames)  +
  scale_y_continuous(name = ylbl, limits = ylim, trans = 'log10',
                    breaks = trans_breaks("log10", function(x) 10^round(x)),
                    labels = trans_format("log10", math_format(10^.x)),minor_breaks = log10_minor_break()) +
  mytheme  + theme(axis.text.x = element_blank())
g2


ylbl <- expression(paste(italic("C"),{}[Chl-a]," [mg ",m^{-3},"]"))
ymax <- 80#max(pdata$chl,na.rm=TRUE)
ylim <- c(0.1,100)
ratio.values <- (xlim[2]-xlim[1])/(log10(ylim[2])-log10(ylim[1]))
asp_rat <- ratio.values/ratio.display
g3 <- ggplot(data = pdata) +
  geom_jitter(aes(x=factor(Station, level = gbreaks) ,y = chl, fill=Month),
              shape=21,show.legend = FALSE,stroke=0.1,size=2) +
  geom_violin(aes(x=Station ,y = chl),trim=FALSE,show.legend = FALSE,alpha =0,size=1,na.rm=TRUE,color="black")+
  annotate("text", x = c(1:length(station)), y=ymax, label = paste0("N = ",cchl$valpoints),size = 8/.pt) +
  labs(title = "", color = "", fill = "") +
  coord_fixed(ratio = asp_rat, ylim = ylim,
              expand = FALSE, clip = "on") +
  scale_fill_gradientn(colours = kovesi.cyclic_mygbm_30_95_c78_s25(12),breaks=seq(1,12,1),
                       limits=c(1,12), labels=mlabels) +
  scale_x_discrete(name = "", breaks = gbreaks, labels = gnames)  +
  scale_y_continuous(name = ylbl, limits = ylim, trans = 'log10',
                     breaks = trans_breaks("log10", function(x) 10^round(x)),
                     labels = trans_format("log10", math_format(10^.x)),minor_breaks = log10_minor_break()) +
  mytheme  
g3



wdt <- 32
ht <- 28

if (save_figure)
{  if (!(grepl(".png", filename, fixed = TRUE)))
  filename <- paste0(filename,".png")
png(filename, width = wdt, height = ht, units = "cm", res = 300, bg = "white")
}

library(grid)
panel_width = unit(1,"npc") - sum(ggplotGrob(g1)[["widths"]][-3]) - unit(1,"line")
gg1 <- g1 + guides(fill= guide_colorbar(barwidth = panel_width*0.8, barheight = 0.5))
grid.newpage()
grid.draw(rbind(ggplotGrob(gg1), ggplotGrob(g2), ggplotGrob(g3)))


# Saving the tiff file, if we want to save figure
if (save_figure)
  dev.off()

