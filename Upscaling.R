# fuse PAS and poly -------------------------------------------------------

# load required libraries
library("raster")
library("maptools")
library("rgdal")
library("rosm")
library("prettymapr")
library("rgeos")

# set working directory accordingly
setwd("/home/bruno/Nextcloud/Promotion/024-Upscaling/")

# import PAS and poly
PAS <- raster("~/Nextcloud/Promotion/014-GIS/PAS/PAS.asc")
bepoly <- raster("~/Nextcloud/Promotion/014-GIS/proportion/poly.asc")

# crop poly to the extent of PAS
bepoly <- crop(bepoly,PAS)

# set NAs to 0
PAS[is.na(PAS)] <- 0
bepoly[is.na(bepoly)] <- 0

# extract values from both raster
dd <- getValues(PAS)
ee <- getValues(bepoly)

# decide spruce location according to proportion and random number
tt <- sapply(ee,FUN=function(x){ifelse(runif(1)<x,1,0)})

# multiply PAS and spruce location (0 = no spruce, 1 = spruce present)
dd <- dd*tt

# add values to PAS
PAS@data@values <- dd

# export result
writeRaster(PAS,"001-raw-data/PASpoly.tif",overwrite=T,proj=T)

# delete everything
rm(list=ls())


# create inf layers from 2020 geodatabase ---------------------------------

# load required libraries
library("terra")
library("rosm")
library("prettymapr")

# set path to data
fgdb <- "C:/Users/bruno/ownCloud/Promotion/012-Daten/NLP-Sachsen/GESAMT/daten_2020/daten/bkbefall_aus_dfe.gdb"

# import geodatabase
fc <- vect(fgdb)

# retrieve damage done by ips typographus
fc <- fc[fc$WS_URSACH3=="BDR",]

# get year of damage
d <- substr(fc$DATUM_BEOB,start = 0,stop=4)
d <- as.integer(d)

# insert data into shapefile
fc$Jahr = d

rm(d)

# retrieve damage done in 2019
fc <- fc[fc$Jahr==2019,]

# import baseraster for rasterization
baseraster <- rast("C:/Users/bruno/ownCloud/Promotion/012-Daten/NLP-Sachsen/base-raster.tif")

# rasterize data
r19 <- rasterize(fc,baseraster,field=fc$Jahr,fun="first",update=T)

# export raster
writeRaster(r19,"~/Nextcloud/Promotion/024-Upscaling/001-raw-data/infestations19","GTiff",overwrite=T)

# delete everything
dev.off()
rm(list=ls())

# create corrected tif files -------------------------------------------------------

# load required libraries
library("raster")
library("maptools")
library("rgdal")
library("rosm")
library("prettymapr")
library("rgeos")

# set working directory accordingly
setwd("/home/bruno/Nextcloud/Promotion/024-Upscaling/001-raw-data/")

# get names of all available files
temp <- list.files(full.names = F)

# load all raster files into one list
dl = lapply(temp,raster) 

# remove file extension
temp <- substr(temp,0,nchar(temp)-4)

# give the data its corresponding names
names(dl) <- temp

# delete names
rm(temp) 

# set projection correctly
for (i in 1:length(dl)) {
  proj4string(dl[[i]]) <- proj4string(dl[[1]])
}

# replace NAs with 0
for (i in 1:length(dl)) {
  dl[[i]][is.na(dl[[i]])] <- 0
}

# change working directory for export
setwd("/home/bruno/Nextcloud/Promotion/024-Upscaling/002-projected-tiffs/")

# export all raster as tiff files
for (i in 1:length(dl)) {writeRaster(dl[[i]],names(dl)[i],"GTiff",overwrite=T,proj=T)}

rm(list=ls())

# gather information on all sites ------------------------------------------------------------

# load required libraries
library("raster")
library("maptools")
library("rgdal")
library("rgeos")
library("prettymapr")

# set working directory accordingly
setwd("/home/bruno/Nextcloud/Promotion/024-Upscaling/")

# load needed data
PASpoly <- raster("002-projected-tiffs/PASpoly.tif")
dinf <- raster("002-projected-tiffs/infestations.tif")

# set cells with infestation to correct value for IPS-SPREADS
dinf[dinf==2015] <- 1
dinf[dinf==2016] <- 0
dinf[dinf==2017] <- 0

# get a look at the PAS raster
image(PASpoly)

# set factor for scaling of IPS-SPREADS worlds
af <- 50

# get raster with desired resolution from PAS raster
r0 <- aggregate(PASpoly,fact=af)

# remove all values from raster
values(r0) <- 0


library("rosm")

# project data to mercator for openstreetmap
r1 <- projectRaster(r0,crs=CRS("+init=epsg:3857"))

pdf("007-plots/UPS-overview-sites.pdf",width=15,height=9)

# plot without outer margins or plot border
prettymap({
  # plot nlp region as open street map layer
  osm.plot(r1,project=T,forcedownload = F,res=800,stoponlargerequest = F,zoomin=-2)
  
  # plot squares
  plot(rasterToPolygons(r1), add=TRUE, border='black', lwd=0.5) 
  
},

# plot scale and north arrow
drawbox = T, drawarrow = T
)
dev.off()

# plot 1 x 1 km cells
plot(rasterToPolygons(r0), add=TRUE, border='black', lwd=1) 

# set projection accordingly
proj4string(r0) <- proj4string(PASpoly)

# export empty output raster for inserting results later on
writeRaster(r0,"004-data-plots/empty-raster",format="GTiff",overwrite=T,proj=T)

# sum of infested trees for each cell
dsource <- aggregate(dinf,fact=af,fun=sum)

# spruce proportion for each cell
dprop <- PASpoly

dprop[dprop>0] <- 1

dprop <- aggregate(dprop,fact=af,fun=sum)

dprop <- dprop / (af * af) * 100

# primattract for each cell
dprim <- PASpoly

dprim[dprim==0] <- NA

dprim <- aggregate(dprim,fact=af,fun=mean)

# create table with results
df <- data.frame("ID"=c(1:ncell(r0)))

df$nsource = values(dsource)
df$prim = values(dprim)
df$prop = values(dprop)

df[is.na(df$prim),3] <- 0

# export result data frame
write.csv(df,"005-data-sites/data-all-sites.csv",row.names = F)

rm(list=ls())
dev.off()

# retrieve classification -------------------------------------------------

# set working directory accordingly
setwd("C:/Users/bruno/ownCloud/Promotion/024-Upscaling/")

# import data table with information on all sites
df <- read.csv("005-data-sites/data-all-sites.csv")

# load required libraries
library("ggplot2")

# create function for multiple ggplots
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# reduce data
df <- df[df$prim!=0 & df$prop!=0 & df$nsource!=0,]

# plot histograms for each parameter
p1 <- ggplot(df,aes(x=prim)) +
  geom_histogram() +
  theme_bw() +
  theme(axis.text=element_text(colour="black")) +
  labs(x="mean primary attractiveness [n]",y="amount [n]")

p2 <- ggplot(df,aes(x=prop)) +
  geom_histogram() +
  theme_bw() +
  theme(axis.text=element_text(colour="black")) +
  labs(x="spruce proportion [%]",y="amount [n]")

p3 <- ggplot(df,aes(x=nsource)) +
  geom_histogram() +
  theme_bw() +
  theme(axis.text=element_text(colour="black")) +
  labs(x="beetle source trees [n]",y="amount [n]")

multiplot(p1,p2,p3,cols=3)

# primattract
shapiro.test(df$prim)

# --> normal

a <- mean(df$prim)
b <- sqrt(var(df$prim))
bb <- max(df$prim)
aa <- min(df$prim)

table(cut(df$prim,c(aa-1,a-0.5*b,a+0.5*b,bb+1)))
table(cut(df$prim,c(0,3.87,4.59,5.9)))
tprim <- c(3.87,4.59,5.9)

# spruceprop
shapiro.test(df$prop)

a <- mean(df$prop)
b <- sqrt(var(df$prop))
bb <- max(df$prop)
aa <- min(df$prop)

table(cut(df$prop,c(aa-1,a-0.5*b,a+0.5*b,bb+1)))
table(cut(df$prop,c(0,46,66,100)))
tspruc <- c(46,66,100)

# ninfest
shapiro.test(df$nsource)

library("fitdistrplus")
descdist(df$nsource, discrete = TRUE)

a <- mean(df$nsource)
b <- sqrt(var(df$nsource))
bb <- max(df$nsource)
aa <- min(df$nsource)

table(cut(df$nsource,c(0,a-0.61*b,a+0.1*b,bb)))
table(cut(df$nsource,c(0,11,36,286)))

tninf <- c(0,11,36,286)

dev.off()

ddf <- data.frame("prim"=tprim,"prop"=tspruc,"nsource"=tninf[2:4])

colnames(ddf) <- c("primattract","spruceprop","nhost")

ddf

write.csv(ddf,"005-data-sites/level-selection.csv",row.names = F)

# plot histograms for each parameter with classification intervals
p1 <- ggplot(df,aes(x=prim)) +
  geom_histogram(binwidth=0.04) +
  geom_vline(xintercept=c(2.58,tprim),col="blue") +
  theme_bw() +
  theme(axis.text=element_text(colour="black")) +
  labs(x="mean primary attractiveness [n]",y="amount [n]")

p2 <- ggplot(df,aes(x=prop)) +
  geom_histogram(binwidth=1) +
  geom_vline(xintercept=c(0.48,tspruc),col="blue") +
  theme_bw() +
  theme(axis.text=element_text(colour="black")) +
  labs(x="spruce proportion [%]",y="amount [n]")

p3 <- ggplot(df,aes(x=nsource)) +
  geom_histogram(binwidth=1.5) +
  geom_vline(xintercept=tninf,col="blue") +
  theme_bw() +
  theme(axis.text=element_text(colour="black")) +
  labs(x="infested trees [n]",y="amount [n]")

multiplot(p1,p2,p3,cols=3)

# pdf("UPS-histogram-properties-all-sites.pdf",width=8,height=3)
jpeg("C:/Users/bruno/ownCloud/Promotion/010-Paper/003-IBM+ML/UPS-histogram-properties-all-sites.jpeg",
     width=8,height=3,units="in",res=800)
multiplot(p1,p2,p3,cols=3)
dev.off()

rm(list=ls())
dev.off()

# select sites by classification ------------------------------------------

# set working directory accordingly
setwd("/home/bruno/Nextcloud/Promotion/024-Upscaling/")

# import data table with information on all sites
df1 <- read.csv("005-data-sites/data-all-sites.csv")
df2 <- read.csv("005-data-sites/level-selection.csv")

# create categorization factor for spruce proportion
tosect <- function(x){
  ifelse(x <= df2[1,2],df2[1,2],
         ifelse(x <= df2[2,2],df2[2,2],df2[3,2]))
}

df1$proplev <- tosect(df1$prop)

# add level mid for calculating distance to level mid
tomid <- function(x){
  ifelse(x <= df2[1,2],df2[1,2] / 2,
         ifelse(x <= df2[2,2],df2[1,2] + ((df2[2,2]-df2[1,2])/2),df2[2,2] + ((df2[3,2]-df2[2,2])/2)))
}

df1$propmid <- tomid(df1$prop)

# create categorization factor for primattract
tosect <- function(x){
  ifelse(x <= df2[1,1],df2[1,1],
         ifelse(x <= df2[2,1],df2[2,1],df2[3,1]))
}

df1$primlev <- tosect(df1$prim)

# add level mid for calculating distance to level mid
tomid <- function(x){
  ifelse(x <= df2[1,1],df2[1,1] / 2,
         ifelse(x <= df2[2,1],df2[1,1] + ((df2[2,1]-df2[1,1])/2),df2[2,1] + ((df2[3,1]-df2[2,1])/2)))
}

df1$primmid <- tomid(df1$prim)

# create categorization factor for nsource
tosect <- function(x){
  ifelse(x == 0,0,
         ifelse(x <= df2[1,3],df2[1,3],
                ifelse(x <= df2[2,3],df2[2,3],df2[3,3])))
}

df1$sourcelev <- tosect(df1$nsource)

# add level mid for calculating distance to level mid
tomid <- function(x){
  ifelse(x == 0,0,
         ifelse(x <= df2[1,3],df2[1,3] / 2,
                ifelse(x <= df2[2,3],df2[1,3] + ((df2[2,3]-df2[1,3])/2),df2[2,3] + ((df2[3,3]-df2[2,3])/2))))
}

df1$sourcemid <- tomid(df1$nsource)

# calculate mean deviation from level mid
df1$primerr <- abs((df1$prim - df1$primmid)/df1$primmid*100)
df1$properr <- abs((df1$prop - df1$propmid)/df1$propmid*100)
df1$sourcerr <- ifelse(df1$sourcemid==0,0,abs((df1$nsource - df1$sourcemid)/df1$sourcemid*100))

df1$best <- (df1$primerr + df1$properr + 2 * df1$sourcerr) / 4

# order after best
df1 <- df1[order(df1$best),]

# split data frame according to combinations
dl <- split(df1,list(df1$primlev,df1$proplev,df1$sourcelev),drop=T)

# create result frame
df3 <- data.frame(cbind(0,0,0,0))

names(df3) <- c("prop","prim","nsource","ID")

# get area ID with lowest deviation for each combination
for (i in 1:length(dl)) {
  df3[i,1] <- dl[[i]][1,5]
  df3[i,2] <- dl[[i]][1,7]
  df3[i,3] <- dl[[i]][1,9]
  df3[i,4] <- dl[[i]]$ID[1]
}

# print result
df3 <- df3[order(df3$ID),]

# export result
write.csv(df3,"005-data-sites/selected-sites.csv",row.names = F)

rm(list=ls())

# crop simulation sites -------------------------------------------------

# load required libraries
library("rgdal")
library("raster")
library("maptools")

# set working directory accordingly
setwd("/home/bruno/Nextcloud/Promotion/024-Upscaling")

# get names of all available files
temp <- list.files(path="002-projected-tiffs",full.names = T)

# load all raster files into one list
dd = lapply(temp,raster) 

# remove file extension
temp <- substr(temp,21,nchar(temp)-4)

# give the data its corresponding names
names(dd) <- temp

# delete names
rm(temp) 

# set cells with infestation to correct value for IPS-SPREADS
dd$infestations[dd$infestations==2015] <- 1
dd$infestations[dd$infestations==2016] <- 0
dd$infestations[dd$infestations==2017] <- 0

# get a look at the PAS raster
image(dd$PAS)

# import empty raster
r0 <- raster("004-data-plots/empty-raster.tif")

de <- rasterToPolygons(r0)

rm(r0)

# import selected sites
df2 <- read.csv("005-data-sites/selected-sites.csv")

# delete sites without infestations / beetle sources
df2 <- df2[df2$nsource!=0,]

# extract data of selected sites
de <- de[df2$ID,]

image(dd$PAS)
plot(de,add=T,border="black")

# delete files not needed for IPS-SPREADS
dd <- within(dd,rm(baseraster,firstwave2015,firstwave2017,firstwave2018,infestations19))

# create list for results
dl <- list(1)

# crop all data for test areas
for (i in 1:length(de)) {
  dl[[i]] <- lapply(dd, function(x){crop(x,extent(de[i,]))})
}

# give the corresponding names
names(dl) <- as.character(df2$ID)

# delete not needed data
rm(dd,df2,i,de)

# create folders for input data for IPS-SPREADS
for (i in 1:length(dl)) {dir.create(file.path("003-data-IPS/input",names(dl[i])))}

# create vector with layer names for IPS-SPREADS
namelayer <- c("firstwave.asc","hostcapacity.asc","inf.asc","tree-height.asc","PAS.asc","sourcecapacity.asc")

for (j in 1:length(dl)) {
  for (i in 1:length(dl[[1]])) {
    writeRaster(dl[[j]][[i]],file.path("003-data-IPS/input",names(dl[j]),namelayer[i]),overwrite=T)
  }
}

rm(list=ls())
dev.off()

# get data of simulation sites --------------------------------------------

setwd("/home/bruno/Nextcloud/Promotion/024-Upscaling/")

df1 <- read.csv("005-data-sites/data-all-sites.csv")

df2 <- read.csv("005-data-sites/selected-sites.csv")

df1 <- subset(df1, ID %in% df2$ID)

write.csv(df1,"005-data-sites/raw-data-selected-sites.csv",row.names = F)

rm(list=ls())

# process results of IPS-SPREADS -----------------------------------------------------

# set working directory accordingly
setwd("C:/Users/Bruno/ownCloud/Promotion/024-Upscaling/")

# import data parts
df1 <- read.csv("003-data-IPS/output/results1.csv",skip=6)
df2 <- read.csv("003-data-IPS/output/results2.csv",skip=6)
df3 <- read.csv("003-data-IPS/output/results3.csv",skip=6)

# combine data parts
df <- rbind(df1,df2,df3)

# remove not needed data
rm(df1,df2,df3)

# remove not needed columns
df <- df[,-c(1,4)]

# rename columns
colnames(df) <- c("felling","ID","nhost1","nhost2","nhost3","prima1","prima2","prima3","nspruce","ncut1","ncut2","ncut3")

# calculate infested spruce for each generation
df$nhost2 <- df$nhost1 + df$nhost2
df$nhost3 <- df$nhost2 + df$nhost3

# calculate cut spruces for each generation
df$ncut2 <- df$ncut1 + df$ncut2
df$ncut3 <- df$ncut2 + df$ncut3

# calculate amount of spruces killed
df$ndead1 <- df$nhost1 + df$ncut1
df$ndead2 <- df$nhost2 + df$ncut2
df$ndead3 <- df$nhost3 + df$ncut3

# change site ID from char to num
df$ID <- as.numeric(substr(df$ID,start=7,stop=10))

# import data on source trees
dff <- read.csv("005-data-sites/selected-sites.csv")

# apply filter
dff <- dff[dff$ID %in% df$ID,]

# create factor from sites
df$nsource <- as.factor(df$ID)
df$primatt <- as.factor(df$ID)
df$spruceprop <- as.factor(df$ID)

# replace site name with levels
levels(df$nsource) <- dff$nsource
levels(df$primatt) <- dff$prim
levels(df$spruceprop) <- dff$prop

# delete not needed data
rm(dff)

# format as numeric
df$nsource <- as.numeric(as.character(df$nsource))
df$primatt <- as.numeric(as.character(df$primatt))
df$spruceprop <- as.numeric(as.character(df$spruceprop))

# classify nhost
toclassify <- function(x){
  ifelse(x==0,0,
         ifelse(x <=11,11,
                ifelse(x<=36,36,286)))
}

df$nhost1 <- toclassify(df$nhost1)
df$nhost2 <- toclassify(df$nhost2)
df$nhost3 <- toclassify(df$nhost3)

# classify primattract
toclassify <- function(x){
  ifelse(x<=3.87,3.87,
         ifelse(x<=4.59,4.59,5.9))
}

df$prima1 <- toclassify(df$prima1)
df$prima2 <- toclassify(df$prima2)
df$prima3 <- toclassify(df$prima3)

# calculate spruce proportion at end of simulation
df$prop1 <- (df$nspruce - df$ndead1) / 2500 * 100
df$prop2 <- (df$nspruce - df$ndead2) / 2500 * 100
df$prop3 <- (df$nspruce - df$ndead3) / 2500 * 100

# classify spruce proportion
toclassify <- function(x) {
  ifelse(x <= 46,46,
         ifelse(x <= 66,66,100))
}

df$prop1 <- toclassify(df$prop1)
df$prop2 <- toclassify(df$prop2)
df$prop3 <- toclassify(df$prop3)

rm(toclassify)

# create data frame without outputs
ddf <- df[,c(1,2,16,17,18)]

# triple this data frame (for the 3 generations)
ddf <- rbind(ddf,ddf,ddf)

# create ddf output columns based on the three corresponding columns of df
ddf$nsource2 <- c(df$nhost1,df$nhost2,df$nhost3)
ddf$prima2 <- c(df$prima1,df$prima2,df$prima3)
ddf$prop2 <- c(df$prop1,df$prop2,df$prop3)
ddf$ndead <- c(df$ndead1,df$ndead2,df$ndead3)

# remove not needed data
rm(df)

# add column with generation number
ddf$generation <- c(rep(1,nrow(ddf)/3),rep(2,nrow(ddf)/3),rep(3,nrow(ddf)/3))

# rearrange columns
ddf <- ddf[,c(10,1:9)]

# rename columns
colnames(ddf) <- c("generations","felling","ID","nsource1","prima1","prop1","nsource2","prima2","prop2","dead")

# split data frame
dl <- split(ddf,f=list(ddf$generations,ddf$felling,ddf$ID))

# create empty data frame
df <- ddf[1,]

# calculate variance of outputs for each site and scenario to find cases 
# with more than one destination
for (i in 1:length(dl)) {
  df[i,] <- dl[[i]][1,]
  df[i,7] <- var(dl[[i]][,7])
  df[i,8] <- var(dl[[i]][,8])
  df[i,9] <- var(dl[[i]][,9])
}

# delete obsolete data
rm(dl,i)

# calculate sum of variance to check all outputs simultaneously
df$sum <- df$nsource2 + df$prima2 + df$prop2

# extract combinations with variance above 0 (more than one destination)
df <- df[df$sum!=0,]

# extract combinations with more than one possible destination for site and scenario
for (i in 1:nrow(df)) {
  ifelse(i==1,
         ddf2 <- ddf[ddf[,1]==df[i,1] & ddf[,2]==df[i,2] & ddf[,3]==df[i,3],],
         ddf2 <- rbind(ddf2,ddf[ddf[,1]==df[i,1] & ddf[,2]==df[i,2] & ddf[,3]==df[i,3],]))
}

# remove obsolete data
rm(i)

# split data frame
dl <- split(ddf2[,c(1:9)],f=list(ddf2$generations,ddf2$felling,ddf2$ID),drop=T)

# check if there are more than two possible destinations for each site and scenario
lapply(dl, summary)

# extract frequencies of the two possible destinations per site and scenario
for (i in 1:length(dl)) {
  ifelse(i==1,df2 <- as.data.frame(table(dl[[i]])),df2 <- rbind(df2,as.data.frame(table(dl[[i]]))))
}

# split data frame according to alternative destinations
df2A <- df2[seq(1,34,2),]
df2B <- df2[seq(2,34,2),]

# insert B version of nsource2 and prima2 based on alternative destinations
df2A$nsource2B <- df2B$nsource2
df2A$prima2B <- df2B$prima2

# delete obsolete data
rm(df2,df2B,dl,i,df)

# convert frequency to probability
df2A$Freq <- df2A$Freq / 10

# rename column
colnames(df2A)[10] <- "probability"

# remove rows with alternative destinations from data
ddf <- ddf[-as.numeric(row.names(ddf2)),]

# calculate means
df <- aggregate(cbind(nsource1,prima1,prop1,nsource2,prima2,prop2,dead)~generations+felling+ID,ddf,mean)

df2 <- aggregate(dead~generations+felling+ID+nsource1+prima1+prop1+nsource2+prima2+prop2,ddf2,mean)

# remove obsolete data
rm(ddf,ddf2)

# create alternative output columns
df$nsource2B <- 0
df$prima2B <- 0
df$deadB <- 0
df2A$dead <- 0
df2A$deadB <- 0

# fill df2A with mean deads from df2
for (i in 1:nrow(df2A)) {
  for (ii in 1:nrow(df2)) {
    if(df2A[i,1]==df2[ii,1]&&df2A[i,2]==df2[ii,2]&&df2A[i,3]==df2[ii,3]&&df2A[i,7]==df2[ii,7]&&df2A[i,8]==df2[ii,8]) df2A[i,13] <- df2[ii,10]
    if(df2A[i,1]==df2[ii,1]&&df2A[i,2]==df2[ii,2]&&df2A[i,3]==df2[ii,3]&&df2A[i,11]==df2[ii,7]&&df2A[i,8]==df2[ii,8]) df2A[i,14] <- df2[ii,10]
    if(df2A[i,1]==df2[ii,1]&&df2A[i,2]==df2[ii,2]&&df2A[i,3]==df2[ii,3]&&df2A[i,7]==df2[ii,7]&&df2A[i,12]==df2[ii,8]) df2A[i,14] <- df2[ii,10]
  }
}

# delete obsolete
rm(df2,i,ii)

# insert probability for alternative destination in data frame
df$probability <- 1

# combine data frames
df <- rbind(df,df2A)

# remove obsolete
rm(df2A)

# shorten column names
colnames(df)[14] <- "proba"

# calculate probability for alternate destination to occur
df$proba <- 1 - df$proba

# add missing sites (sites without sources)
df0 <- read.csv("005-data-sites/selected-sites.csv")

# remove sites with sources
df0 <- df0[df0$nsource==0,]

# add missing columns to combine both data frames
df0$generations = 0
df0$felling = 0
df0$prop2 = df0$prop
df0$prima2 = df0$prim
df0$nsource2 = df0$nsource
df0$dead = 0
df0$proba = 0
df0$deadB = 0
df0$nsource2B = 0
df0$prima2B = 0

# rename columns
colnames(df0)[1:3] <- c("prop1","prima1","nsource1")

# multiply rows accordingly
df0 <- rbind(df0,df0,df0)

# set levels for generation accordingly
df0$generations <- c(rep(1,9),rep(2,9),rep(3,9))

# multiply rows accordingly
df0 <- rbind(df0,df0,df0,df0,df0)

# set levels for felling accordingly
df0$felling <- c(rep(0,27),rep(25,27),rep(50,27),rep(75,27),rep(100,27))

# combine both data frames
df <- rbind(df,df0)

# delete obsolete data
rm(df0)

# export data frame
write.csv(df,"005-data-sites/results-IPS-SPREADS.csv",row.names = F)

rm(list=ls())

# classify data of all sites ------------------------------------------------------

# set working directory accordingly
setwd("C:/Users/Bruno/ownCloud/Promotion/024-Upscaling/")

# import data of all sites
df <- read.csv("005-data-sites/data-all-sites.csv")

# rename columns
colnames(df) <- c("ID","nsource","prima","prop")

# classify columns 2 - 4
toclassify <- function(x) {
  ifelse(x==0,0,
         ifelse(x<=11,11,
                ifelse(x<=36,36,286)))
}

df$nsource <- toclassify(df$nsource)

toclassify <- function(x) {
  ifelse(x==0,0,
         ifelse(x<=3.87,3.87,
                ifelse(x<=4.59,4.59,5.9)))
}

df$prima <- toclassify(df$prima)

toclassify <- function(x) {
  ifelse(x==0,0,
         ifelse(x<=46,46,
                ifelse(x<=66,66,100)))
}

df$prop <- toclassify(df$prop)

# calculate living spruces based on spruce proportion
df$spruces <- df$prop * 2500 / 100

# export data frame as base frame for the meta model
write.csv(df,"005-data-sites/data-all-sites-classified.csv",row.names = F)

rm(list=ls())

# Meta-model validation ----------------------------------------

# set working directory accordingly
setwd("C:/Users/Bruno/ownCloud/Promotion/024-Upscaling/")

# import data of all sites
df <- read.csv("005-data-sites/data-all-sites-classified.csv")

# import results of IPS-SPREADS
df2 <- read.csv("005-data-sites/results-IPS-SPREADS.csv")

# create data frames for each scenario
dl <- split(df2,f=list(df2$generations,df2$felling),sep="-")

# delete obsolete data frame
rm(df2)

# create empty data frame to store results
df0 <- data.frame(df$ID)

# define number of years to be simulated
a <- 3

# set N to TRUE if neighbor rule is to be applied
N <- TRUE

# set C to TRUE if capacity rule is to be applied
C <- TRUE

for (z in 1:length(dl)) {
  for (J in 1:a) {
    
    # create data frame for calculation
    if(J==1){dl1 <- list(df)}
    
    if(J>1){dl1[[J]] <- dl1[[J-1]]}
    
    # +++++++++++++++++++++++++++++++++++++++++++++++++++ #
    # NEIGHBOR RULE 
    # +++++++++++++++++++++++++++++++++++++++++++++++++++ #
    if(N == TRUE) {
      
      for (i in 1:nrow(dl1[[J]])) {
        
        # create vector to store changes of infestation level
        if(i==1) {b <- dl1[[J]]$nsource}
        
        # check if cell contains spruces
        if(dl1[[J]]$prop[i]!=0){
          
          # store IDs of neighboring cells
          lp <- i - 1 # left patch
          rp <- i + 1 # right patch
          ap <- i - 132 # above patch
          bp <- i + 132 # below patch
          
          # check if there are neighbor cells
          ifelse(any(c(1:nrow(dl1[[J]])) == lp), lp <- lp, lp <- 1)
          ifelse(any(c(1:nrow(dl1[[J]])) == rp), rp <- rp, rp <- 1)
          ifelse(any(c(1:nrow(dl1[[J]])) == ap), ap <- ap, ap <- 1)
          ifelse(any(c(1:nrow(dl1[[J]])) == bp), bp <- bp, bp <- 1)
          
          # check if cell lies on left or right edge of world
          if((i-1)%%132 == 0){lp <- 1}
          if(i%%132 == 0){rp <- 1}
          
          # create  a variable to count neighbors with highest
          # infestation level
          counter <- 0
          
          # check how many neighbors have highest infestation level
          if(dl1[[J]]$nsource[lp] == 286){counter<-counter+1}
          if(dl1[[J]]$nsource[rp] == 286){counter<-counter+1}
          if(dl1[[J]]$nsource[ap] == 286){counter<-counter+1}
          if(dl1[[J]]$nsource[bp] == 286){counter<-counter+1}
          
          # increase infestation level if there is at least one neighbor
          if(counter >= 1 && (z+2)%%3 == 0) { # 1 level if 1 generation
            if(b[i]==36){b[i] <- 286}
            if(b[i]==11){b[i] <- 36}
            if(b[i]==0){b[i] <- 11}
          }

          if(counter >= 1 && (z+1)%%3 == 0) { # 2 levels if 2 generations
            if(b[i]==36){b[i] <- 286}
            if(b[i]==11){b[i] <- 286}
            if(b[i]==0){b[i] <- 36}
          }

          if(counter >= 1 && z%%3 == 0) { # 3 levels if 3 generations
            if(b[i]==36){b[i] <- 286}
            if(b[i]==11){b[i] <- 286}
            if(b[i]==0){b[i] <- 286}
          }
        }
        
        # update all values and remove obsolete variables
        if(i==nrow(dl1[[J]])){
          # update all influenced cells
          dl1[[J]]$nsource <- b
          
          # delete obsolete variables
          rm(b,ap,bp,lp,rp,counter)
        }
      }
    }
    
    # +++++++++++++++++++++++++++++++++++++++++++++++++++ #
    # BASE META-MODEL
    # +++++++++++++++++++++++++++++++++++++++++++++++++++ #
    for (i in 1:nrow(dl1[[J]])) {
      for (ii in 1:nrow(dl[[z]])) {
        if(dl[[z]]$prop1[ii] == dl1[[J]]$prop[i] && dl[[z]]$prima1[ii] == dl1[[J]]$prima[i] && dl[[z]]$nsource1[ii] == dl1[[J]]$nsource[i]) {
          
          ff <- runif(1,0,1)
          # random number to decide if alternative destination is reached
          if(ff >= dl[[z]]$proba[ii]) {
            dl1[[J]]$prop[i] <- dl[[z]]$prop2[ii]
            dl1[[J]]$prima[i] <- dl[[z]]$prima2[ii]
            dl1[[J]]$nsource[i] <- dl[[z]]$nsource2[ii]
          } else {
            dl1[[J]]$prop[i] <- dl[[z]]$prop2[ii]
            dl1[[J]]$prima[i] <- dl[[z]]$prima2B[ii]
            dl1[[J]]$nsource[i] <- dl[[z]]$nsource2B[ii]
          }
          
          # +++++++++++++++++++++++++++++++++++++++++++++++++++ #
          # CAPACITY / GROWTH / DEATH
          # +++++++++++++++++++++++++++++++++++++++++++++++++++ #
          if(C==TRUE) {
            # update living spruce amount on each cell
            if(ff >= dl[[z]]$proba[ii]) {
              dl1[[J]]$spruces[i] <- dl1[[J]]$spruces[i] - dl[[z]]$dead[ii]
            } else {
              dl1[[J]]$spruces[i] <- dl1[[J]]$spruces[i] - dl[[z]]$deadB[ii]
            }
              # check if all spruces are dead
              if(dl1[[J]]$spruces[i] <= 0){
                dl1[[J]][i,c(2:4)] <- 500
                dl1[[J]][i,5] <- 0
              }
            }
          }
        }
      }
    
    # name data frame accordingly
    names(dl1)[J] <- 2015 + J
    dl1[[J]]$year <- 2015 + J
    
    # remove all obsolete variables after last iteration
    if(J == a) {
      # rm(a,i,ii,J,C,N)
      df0 <- cbind(df0,dl1[[2]]$nsource,dl1[[3]]$nsource)
      names(df0)[2 * z] <- paste(names(dl[z]),"-2017",sep="")
      names(df0)[1 + 2 * z] <- paste(names(dl[z]),"-2018",sep="")
    }
  }
  if(z == length(dl)){
    if(N == TRUE && C == TRUE){
      write.csv(df0,"006-kappa/MM-N-C.csv",row.names = F)
    }
    if(N == TRUE && C == FALSE){
      write.csv(df0,"006-kappa/MM-N.csv",row.names = F)
    }
    if(N == FALSE && C == TRUE){
      write.csv(df0,"006-kappa/MM-C.csv",row.names = F)
    }
    if(N == FALSE && C == FALSE){
      write.csv(df0,"006-kappa/MM.csv",row.names = F)
    }
  }
}

# delete everything
rm(list=ls())

##
# create data for validating MM predictions
##
library("terra")

# import raster with infestations
r17 <- rast("002-projected-tiffs/infestations.tif")
r18 <- rast("002-projected-tiffs/infestations19.tif")

# set older data to 0
r17[r17<2017] <- 0
r18[r18<2018] <- 0

# set infestations to 1
r17[r17!=0] <- 1
r18[r18!=0] <- 1

# change resolution accordingly and calculate amount of infested trees
r17 <- aggregate(r17,fact=50,fun=sum)
r18 <- aggregate(r18,fact=50,fun=sum)

# import data on all sites
df <- read.csv("005-data-sites/data-all-sites.csv")

# add amount of infestationf for years 2017 and 2018 to the data frame
df$nsource17 = values(r17)
df$nsource18 = values(r18)

# create function for classification of source levels
toclassify <- function(x) {
  ifelse(x == 0,0,
         ifelse(x <= 11,11,
                ifelse(x >= 36,36,286)))
}

# classify source levels
df$nsource <- toclassify(df$nsource)
df$nsource17 <- toclassify(df$nsource17)
df$nsource18 <- toclassify(df$nsource18)

# save results
write.csv(df,"006-kappa/data-all-sites-all-years.csv",row.names = F)

# delete everything
rm(list=ls())

##
# calculate macro-averaged F1 scores
##

# import real world data 
df <- read.csv("006-kappa/data-all-sites-all-years.csv")

# import predictions of meta-model versions
MM <- read.csv("006-kappa/MM.csv")
MMC <- read.csv("006-kappa/MM-C.csv")
MMN <- read.csv("006-kappa/MM-N.csv")
MMNC <- read.csv("006-kappa/MM-N-C.csv")

# rename columns accordingly
colnames(MM)[1] <- "ID"
colnames(MMC)[1] <- "ID"
colnames(MMN)[1] <- "ID"
colnames(MMNC)[1] <- "ID"

# remove sites with no spruces
df <- df[df$prop>0,]
MM <- MM[MM$ID %in% df$ID,]
MMC <- MMC[MMC$ID %in% df$ID,]
MMN <- MMN[MMN$ID %in% df$ID,]
MMNC <- MMNC[MMNC$ID %in% df$ID,]

# create empty lists for results
dl17 <- list()
dl18 <- list()

# define infestation levels as factors
for (i in 2:ncol(MM)) {
  MM[,i] <- factor(MM[,i],levels=c(0,11,36,286))
  MMC[,i] <- factor(MMC[,i],levels=c(0,11,36,286))
  MMN[,i] <- factor(MMN[,i],levels=c(0,11,36,286))
  MMNC[,i] <- factor(MMNC[,i],levels=c(0,11,36,286))
}

# split data according to years
dl17[[1]] <- MM[,c(1,seq(2,30,2))]
dl18[[1]] <- MM[,c(1,seq(3,31,2))]

dl17[[2]] <- MMC[,c(1,seq(2,30,2))]
dl18[[2]] <- MMC[,c(1,seq(3,31,2))]

dl17[[3]] <- MMN[,c(1,seq(2,30,2))]
dl18[[3]] <- MMN[,c(1,seq(3,31,2))]

dl17[[4]] <- MMNC[,c(1,seq(2,30,2))]
dl18[[4]] <- MMNC[,c(1,seq(3,31,2))]

# remove obsolete data
rm(MM,MMN,MMC,MMNC)

# create result data frames
df17 <- data.frame("scenario"=colnames(dl17[[1]])[2:ncol(dl17[[1]])])
df18 <- data.frame("scenario"=colnames(dl18[[1]])[2:ncol(dl18[[1]])])

# create emtpy vector for computation
a <- c()

# change infestation levels to factor
df$nsource17 <- as.factor(df$nsource17)
df$nsource18 <- as.factor(df$nsource18)

# calculate macro-averaged F1 score for 2017 model predictions
for (i in 1:length(dl17)) {
  for (ii in 1:length(dl17[[i]])) {
    # create confusion matrix
    cm <- as.matrix(table(Actual = df$nsource17, Predicted = dl17[[i]][,ii]))
    # number of instances
    n <- sum(cm) 
    # number of classes
    nc <- nrow(cm) 
    # number of correctly classified instances per class 
    diag <- diag(cm) 
    rowsums <- apply(cm, 1, sum) 
    # number of instances per class
    colsums <- apply(cm, 2, sum) 
    # number of predictions per class
    accuracy <- sum(diag) / n 
    # overall accuracy
    precision <- diag / colsums 
    # remove NAs
    precision[is.na(precision)] <- 0
    # overall recall
    recall <- diag / rowsums 
    # remove NAs
    recall[is.na(recall)] <- 0
    # calculate F1 score
    f1 <- 2 * precision * recall / (precision + recall) 
    # remove NAs
    f1[is.na(f1)]  <- 0 
    # calculate macro-averaged F1 score
    macroF1 <- mean(f1)
    # store score into result vector a
    a[ii] <- macroF1
    # save result vector as column in result data frame
    if(ii == length(dl17[[i]])) {
      df17[,i+1] <- a[2:length(a)]
    }
  }
}

# rename columns
colnames(df17) <- c("scenario","MM","MMC","MMN","MMNC")


# calculate macro-averaged F1 score for 2018 model predictions
for (i in 1:length(dl18)) {
  for (ii in 1:length(dl18[[i]])) {
    # create confusion matrix
    cm <- as.matrix(table(Actual = df$nsource18, Predicted = dl18[[i]][,ii]))
    # number of instances
    n <- sum(cm) 
    # number of classes
    nc <- nrow(cm) 
    # number of correctly classified instances per class 
    diag <- diag(cm) 
    rowsums <- apply(cm, 1, sum) 
    # number of instances per class
    colsums <- apply(cm, 2, sum) 
    # number of predictions per class
    accuracy <- sum(diag) / n 
    # overall accuracy
    precision <- diag / colsums 
    # remove NAs
    precision[is.na(precision)] <- 0
    # overall recall
    recall <- diag / rowsums 
    # remove NAs
    recall[is.na(recall)] <- 0
    # calculate F1 score
    f1 <- 2 * precision * recall / (precision + recall) 
    # remove NAs
    f1[is.na(f1)]  <- 0 
    # calculate macro-averaged F1 score
    macroF1 <- mean(f1)
    # store score into result vector a
    a[ii] <- macroF1
    # save result vector as column in result data frame
    if(ii == length(dl18[[i]])) {
      df18[,i+1] <- a[2:length(a)]
    }
  }
}

# rename columns
colnames(df18) <- c("scenario","MM","MMC","MMN","MMNC")

# create data frame to compute the mean of both years
df00 <- df17

# compute the mean of both years
df00[,c(2:5)] <- (df17[,c(2:5)] + df18[,c(2:5)]) / 2

# combine all three data frames
df0 <- rbind(df17,df18,df00)

# delete everything else
rm(df17,df18,dl17,dl18,df)

# create column for years and mean
df0$year <- c(rep(2017,15),rep(2018,15),rep("mean",15))

# add column containing santiation felling intensities
df0$management <- c(rep(c(0,0,0,25,25,25,50,50,50,75,75,75,100,100,100),3))

# create column containing beetle generation numbers
df0$generation <- c(rep(c(1,2,3),15))

# remove obsolete column
df0 <- df0[,-1]

# change columns to factors
df0$management <- as.factor(df0$management)
df0$generation <- as.factor(df0$generation)
df0$year <- as.factor(df0$year)

# load required libraries
library("reshape2")
library("ggplot2")

# transform data frame for plotting with ggplot2
df0 <- melt(df0)

# rename levels for meta-model versions
levels(df0$variable) <- c("Basic Meta-model (MM)","MM + Spruce Dieback","MM + Beetle Spread","MM + Spread + Dieback")

# create labels with 2 digits for plotting
df0$value2 <- sprintf("%.2f",round(df0$value,2))

# plot results
ggplot(df0,aes(x=management,y=generation,fill=value)) +
  geom_tile(col="black") +
  geom_text(aes(label = value2),na.rm=T) +
  scale_fill_gradient(low = "#98C5E3", high = "#3A7EAB") +
  facet_grid(variable~year) +
  theme_bw() +
  labs(x="Sanitation felling intensity [%]",
       y="Beetle generations [n]",
       fill="Macro-averaged \nF1 score") +
  theme(axis.text.x = element_text(colour="black"),
        axis.text.y = element_text(colour="black"),
        legend.position = "top")

# ggsave("006-kappa/UPS-Kappa.pdf",width=7,height=8)
ggsave("C:/Users/bruno/ownCloud/Promotion/010-Paper/003-IBM+ML/UPS-Kappa.jpeg",width=7,height=8,dpi=800)

# delete everything
rm(list=ls())
dev.off()

# Meta-model application ---------------------------------------------------------------

# set working directory accordingly
setwd("C:/Users/Bruno/ownCloud/Promotion/024-Upscaling/")

# import data of all sites
df <- read.csv("005-data-sites/data-all-sites-classified.csv")

# store total amount of spruces on each patch
df$aspruces <- df$spruces

# import results of IPS-SPREADS
df2 <- read.csv("005-data-sites/results-IPS-SPREADS.csv")

# create data frames for each scenario
dl <- split(df2,f=list(df2$generations,df2$felling),sep="-")

# delete obsolete data frame
rm(df2)

# store data of all sites as baseline for simulation
dl1 <- list(df)

# define number of years to be simulated
a <- 21

# set N to TRUE if neighbor rule is to be applied
N <- TRUE

# set C to TRUE if capacity rule is to be applied
C <- TRUE

for (z in 1:length(dl)) {
  
  # create empty data frame to store results
  df0 <- data.frame(df$ID)
  
  for (J in 1:a) {
    
    # create data frame for calculation
    if(J==1){dl1 <- list(df)}
    
    if(J>1){dl1[[J]] <- dl1[[J-1]]}
    
    # +++++++++++++++++++++++++++++++++++++++++++++++++++ #
    # NEIGHBOR RULE 
    # +++++++++++++++++++++++++++++++++++++++++++++++++++ #
    if(N == TRUE) {
      
      for (i in 1:nrow(dl1[[J]])) {
        
        # create vector to store changes of infestation level
        if(i==1) {b <- dl1[[J]]$nsource}
        
        # check if cell contains spruces
        if(dl1[[J]]$prop[i]!=0){
          
          # store IDs of neighboring cells
          lp <- i - 1 # left patch
          rp <- i + 1 # right patch
          ap <- i - 132 # above patch
          bp <- i + 132 # below patch
          
          # check if there are neighbor cells
          ifelse(any(c(1:nrow(dl1[[J]])) == lp), lp <- lp, lp <- 1)
          ifelse(any(c(1:nrow(dl1[[J]])) == rp), rp <- rp, rp <- 1)
          ifelse(any(c(1:nrow(dl1[[J]])) == ap), ap <- ap, ap <- 1)
          ifelse(any(c(1:nrow(dl1[[J]])) == bp), bp <- bp, bp <- 1)
          
          # check if cell lies on left or right edge of world
          if((i-1)%%132 == 0){lp <- 1}
          if(i%%132 == 0){rp <- 1}
          
          # create  a variable to count neighbors with highest
          # infestation level
          counter <- 0
          
          # check how many neighbors have highest infestation level
          if(dl1[[J]]$nsource[lp] == 286){counter<-counter+1}
          if(dl1[[J]]$nsource[rp] == 286){counter<-counter+1}
          if(dl1[[J]]$nsource[ap] == 286){counter<-counter+1}
          if(dl1[[J]]$nsource[bp] == 286){counter<-counter+1}
          
          # increase infestation level if there is at least one neighbor
          if(counter >= 1 && (z+2)%%3 == 0) { # 1 level if 1 generation
            if(b[i]==36){b[i] <- 286}
            if(b[i]==11){b[i] <- 36}
            if(b[i]==0){b[i] <- 11}
          }
          
          if(counter >= 1 && (z+1)%%3 == 0) { # 2 levels if 2 generations
            if(b[i]==36){b[i] <- 286}
            if(b[i]==11){b[i] <- 286}
            if(b[i]==0){b[i] <- 36}
          }
          
          if(counter >= 1 && z%%3 == 0) { # 3 levels if 3 generations
            if(b[i]==36){b[i] <- 286}
            if(b[i]==11){b[i] <- 286}
            if(b[i]==0){b[i] <- 286}
          }
        }
        # update all values and remove obsolete variables
        if(i==nrow(dl1[[J]])){
          # update all influenced cells
          dl1[[J]]$nsource <- b
          
          # delete obsolete variables
          rm(b,ap,bp,lp,rp,counter)
        }
      }
    }
    
    # +++++++++++++++++++++++++++++++++++++++++++++++++++ #
    # BASE META-MODEL
    # +++++++++++++++++++++++++++++++++++++++++++++++++++ #
    for (i in 1:nrow(dl1[[J]])) {
      for (ii in 1:nrow(dl[[z]])) {
        if(dl[[z]]$prop1[ii] == dl1[[J]]$prop[i] && dl[[z]]$prima1[ii] == dl1[[J]]$prima[i] && dl[[z]]$nsource1[ii] == dl1[[J]]$nsource[i]) {
          
          dl1[[J]]$prop[i] <- dl[[z]]$prop2[ii]
          dl1[[J]]$prima[i] <- dl[[z]]$prima2[ii]
          dl1[[J]]$nsource[i] <- dl[[z]]$nsource2[ii]
          
          # +++++++++++++++++++++++++++++++++++++++++++++++++++ #
          # CAPACITY / GROWTH / DEATH
          # +++++++++++++++++++++++++++++++++++++++++++++++++++ #
          if(C==TRUE) {
            # update living spruce amount on each cell
            dl1[[J]]$spruces[i] <- dl1[[J]]$spruces[i] - dl[[z]]$dead[ii]
            
            # check if all spruces are dead
            if(dl1[[J]]$spruces[i] <= 0){
              dl1[[J]][i,c(2:4)] <- 500
              dl1[[J]][i,5] <- 0
            }
          }
        }
      } 
    }
    
    # name data frame accordingly
    names(dl1)[J] <- 2015 + J
    dl1[[J]]$year <- 2015 + J
    
    # store % of killed trees of all cells for each year of one scenario in one data frame
    if(J == a) {
      for (y in 2:length(dl1)) {
        # calculate % of killed trees for each cell
        dl1[[y]]$aspruces <- round((1 - dl1[[y]]$spruces / dl1[[y]]$aspruces) * 100,2)
        
        # create function to categorize % of killed trees
        tolevel <- function(x){
          ifelse(x==0,0,
                 ifelse(x<=25,25,
                        ifelse(x<=50,50,
                               ifelse(x<=75,75,100))))
        }
        
        # apply categorization for % of killed trees
        dl1[[y]]$aspruces <- tolevel(dl1[[y]]$aspruces)
        
        # store result of current scenario
        df0 <- cbind(df0,dl1[[y]]$aspruces)
      }
      colnames(df0) <- c("ID",seq(2017,(2015+a)))
      write.csv(df0,paste("004-data-plots/",names(dl)[z],".csv",sep=""),row.names = F)
    }
  }
}

# delete everything
rm(list=ls())

# analyze 20 year prediction --------------------------------------------------

# set working directory accordingly
# setwd("C:/Users/Bruno/ownCloud/Promotion/024-Upscaling/")
setwd("C:/Users/Bruno/ownCloud/Promotion/024-Upscaling/")

# load necessary libraries
library("terra")

# import empty raster
r0 <- rast("004-data-plots/empty-raster.tif")

# import shape with border of national park
nlp <- vect("004-data-plots/nlp_border.shp")

# import results of MM simulations
setwd("004-data-plots/")

temp <- list.files(pattern=".csv") # get file names

dl = lapply(temp, function(x){read.csv(x)}) # load all files into one list

temp <- substr(temp,1,nchar(temp)-4) # remove file extension

names(dl) <- temp # give the data its corresponding names

rm(temp) # delete names

# set wd accordingly
setwd("C:/Users/Bruno/ownCloud/Promotion/024-Upscaling/")

# mark areas outside the national park
tt <- mask(r0,nlp)

# create data frame with all area IDs and NAs values for ADJ areas
df <- as.data.frame(cbind("ID"=c(1:ncell(r0)),"values"=values(tt)))

# extract IDs of areas inside and outside national park
IDnlp <- df[complete.cases(df),1]
IDadj <- df[!complete.cases(df),1]

# extract IDs of areas within a 500 m section around the national park
nlpb <- buffer(nlp,width=500)

# set values of areas outside nlp and buffer to NA
tt <- mask(r0,nlpb)

# create data frame from this
df <- as.data.frame(cbind("ID"=c(1:ncell(r0)),"values"=values(tt)))

# extract IDs of areas inside buffer and nlp
IDbor <- df[complete.cases(df),1]

# extract IDs of areas only within the buffer
IDbor <- IDbor[!IDbor %in% IDnlp]

# export those IDs
write.csv(IDbor,"IDs-border.csv",row.names = F)

# delete all obsolete data
rm(df,tt,r0,nlp,nlpb,IDbor)

# store names of scenarios
a <- names(dl)

# retrieve results for 20 years prediction
dl <- lapply(dl, function(x){x[,20]})

# create data frames for inside and outside the nlp
dladj <- as.data.frame(lapply(dl,function(x){x[IDadj]}))
dlnlp <- as.data.frame(lapply(dl,function(x){x[IDnlp]}))

# save number of sites with spruces inside and outside the nlp
nADJ <- nrow(dladj[complete.cases(dladj),])
nNLP <- nrow(dlnlp[complete.cases(dlnlp),])

# name columns
colnames(dladj) <- a
colnames(dlnlp) <- a

# remove obsolete data
rm(IDadj,IDnlp,dl,a)

# melt data frames for plotting with ggplot
library("reshape2")

dladj <- melt(dladj)
dlnlp <- melt(dlnlp)

# create column for location of the areas
dladj$part <- "ADJ"
dlnlp$part <- "NLP"

# combine both data frames
dl <- rbind(dladj,dlnlp)

# create a column for the amount of beetle generations
dl$gen <- dl$variable

# set factor levels accordingly
levels(dl$gen) <- c(rep("1",5),rep("2",5),rep("3",5))

# rename levels for plotting
levels(dl$gen) <- c("1 generation","2 generations","3 generations")

# create a column for the sanitation felling intensity
dl$felling <- dl$variable

# set factor levels accordingly
levels(dl$felling) <- c(rep(c("0","100","25","50","75"),3))

#  reorder factor levels for plotting
dl$felling <- factor(dl$felling, levels=c("0","25","50","75","100"))

# rename levels for plotting
levels(dl$felling) <- c("0 % sanitation felling","25 % sanitation felling",
                        "50 % sanitation felling","75 % sanitation felling",
                        "100 % sanitation felling")

# transform results into factor for plotting
dl$value <- factor(dl$value,levels=c("0","25","50","75","100"))

# remove NAs
dl <- dl[!is.na(dl$value),]

# create column for counting
dl$cn <- 1

# calculate sum for each combination
dll <- aggregate(cn~value+part+gen+felling,dl,sum)

# calculate percentage for areas inside and outside the nlp
dll1 <- dll[dll$part=="NLP",]
dll2 <- dll[dll$part=="ADJ",]

dll1$cn <- round(dll1$cn / nNLP * 100,digits=2)
dll2$cn <- round(dll2$cn / nADJ * 100,digits=2)

dll <- rbind(dll1,dll2)

dll$part <- as.factor(dll$part)

levels(dll$part) <- c("adjacent forest area","national park")

colnames(dll)[2] <- "Section"

# choose color blind firendly colors
library("RColorBrewer")

pal <- brewer.pal(2,"Dark2")[-1]

# plot results
library("ggplot2")

ggplot(dll,aes(x=value,y=cn,fill=Section)) +
  geom_col(position = position_dodge2(preserve = "single")) +
  facet_grid(felling~gen) +
  scale_fill_manual(values=pal) +
  labs(y="Proportion [%] from all sites of each section") +
  scale_x_discrete(name ="Spruces killed [%] on each site",
                   breaks=c(0,25,50,75,100)) +
  theme_bw() +
  theme(axis.text.x = element_text(colour="black"),
        legend.position = "top",
        axis.text.y = element_text(colour="black"))

# ggsave("007-plots/UPS-barplot-scenarios.pdf",width=7,height=9)
ggsave("C:/Users/bruno/ownCloud/Promotion/010-Paper/003-IBM+ML/UPS-barplot-scenarios.jpeg",width=7,height=9,dpi=800)

# remove everything
rm(list=ls())
dev.off()

# create output raster with infestation risk ------------------------------------------------

# set working directory accordingly
setwd("C:/Users/Bruno/ownCloud/Promotion/024-Upscaling/")

# load necessary libraries
library("terra")
library("rosm")
library("sp")
library("prettymapr")
citation("terra")
# import empty raster
r0 <- rast("004-data-plots/empty-raster.tif")

# import shape with border of national park
nlp <- vect("004-data-plots/nlp_border.shp")

# import results of MM simulations
setwd("004-data-plots/")

temp <- list.files(pattern=".csv")

dl = lapply(temp, function(x){read.csv(x)}) # load all files into one list

temp <- substr(temp,1,nchar(temp)-4) # remove file extension

names(dl) <- temp # give the data its corresponding names

rm(temp) # delete names

# set wd accordingly
setwd("C:/Users/Bruno/ownCloud/Promotion/024-Upscaling/")

# store names of scenarios
b <- names(dl)

# extract results of 20 year prediction
dl <- lapply(dl, function(x){x <- x[,20]})

# copy base raster
dl2 <- lapply(dl,function(x){x <- r0})

# assign results to raster
for (i in 1:length(dl2)) {values(dl2[[i]]) <- dl[[i]]}

# assign names
names(dl2) <- names(dl)

# export output rasters
for (i in 1:length(dl2)) {writeRaster(dl2[[i]],paste("008-output-raster/",names(dl2)[i],".tif",sep=""))}

# delete everything
rm(list=ls())
dev.off()
