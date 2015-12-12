library(XML)
library(httr)
library(rvest)
library(magrittr)
library(gdata)
library(stringer)

#### get lat & longs ###
url <- read_html("http://www.pac.dfo-mpo.gc.ca/science/species-especes/pelagic-pelagique/herring-hareng/herspawn/locindex-eng.html")
a <- url %>%
  html_nodes("pre") %>%
  html_text()
data <- strsplit(a[[2]], split= "\r\n")
rm(url)
column_names <- strsplit(strsplit(a[[1]], split= "\r\n")[[1]][2]," ")[[1]]
column_names <- column_names[column_names!=""]
column_names <- column_names[!(column_names=="NAME")]
column_names[column_names=="LOCATION"] <- "LOCATION_NAME"

### split strings, trim, name columns appropriately, and convert characters to numbers where necessary
coords <- data.frame(apply(
  t(sapply(data[[1]],
    function(x) substring(x,
      c(1,2,9,19,43,48,53,56),
      c(1,8,18,42,47,52,55,76)))
    ),2, 
  trimws))[-1,-1]

row.names(coords) <- 1:nrow(coords)
names(coords) <- column_names
coords$LAT <- as.numeric(as.character(coords$LAT))
coords$LONG <- as.numeric(as.character(coords$LONG))
coords$SECTION <- factor(as.character(coords$SECTION))
coords$REGION <- factor(as.character(coords$REGION))
coords <- subset(coords,SECTION!="")
coords <- drop.levels(coords)
levels(coords$SECTION) <-formatC(as.numeric(as.character(levels(coords$SECTION))), width = 3, format = "d", flag = "0") 

coords$LONG <- with(coords,ifelse(LONG>0,-LONG,LONG))


read.table(paste0('http://www.pac.dfo-mpo.gc.ca/science/species-especes/pelagic-pelagique/herring-hareng/herspawn/',coords$SECTION[1],'tab-eng.html'), skip= 10, nrows= 1,fill= T)
header <- c("YEAR","N_SPAWN_RECORDS","SPAWN_HAB_INDEX","LENGTH","WIDTH","LAYERS","W_SST","MEAN_DOY","W_DOY","MIN_DOY","MAX_DOY","PERC_DIVER_SURVEY")

### function to scrape spawn data ###
scraper <- function(x) {
  url <- paste0('http://www.pac.dfo-mpo.gc.ca/science/species-especes/pelagic-pelagique/herring-hareng/herspawn/',x,'tab-eng.html')
  table <- read.table(url, skip = 14, fill = T, nrows= length(1940:2015))
  table$SECTION <- x
  table
}

### scrape away!! ###
spawn_data_list <- lapply(levels(coords$SECTION),scraper)
spawn_data <- ldply(spawn_data_list)
spawn_data <- spawn_data
names(spawn_data)[1:length(header)] <- header
spawn_data[,c("W_SST","MEAN_DOY","W_DOY","MIN_DOY","MAX_DOY")] <- apply(spawn_data[,c("W_SST","MEAN_DOY","W_DOY","MIN_DOY","MAX_DOY")],2,function(x) as.numeric(as.character(ifelse(x=="*",NA,x))))

### get header for catch data
url<- read_html("http://www.pac.dfo-mpo.gc.ca/science/species-especes/pelagic-pelagique/herring-hareng/herspawn/tab3head-eng.html")
a <- url %>%
  html_nodes("b") %>%
  html_text()
header_test <- strsplit(a[1], split= "\r\n")

### check header to make sure it fits with given header ###
header_test
header <- c("YEAR","N_CATCH_RECORDS","TOTAL_CATCH","C_JAN_APR","C_MAY_AUG","C_SEP_DEC","C_GILLNET","C_SEINE","C_TRAWL","C_SOK_EST")

### read in and clean catch data
catch <- read.table("http://www.pac.dfo-mpo.gc.ca/science/species-especes/pelagic-pelagique/herring-hareng/herspawn/hcatch1d-eng.html", fill = T)
catch_data <- subset(catch, V1%in%c(as.character(1950:2015)))
catch_data <- catch_data[,colSums(apply(catch_data,2,function(x)x==""))==0]
names(catch_data)  <- header
catch_data <- data.frame(apply(catch_data,2,function(x) as.numeric(as.character(x))))


### label sections ###
catch_section <- as.numeric(as.character(subset(catch, V9=="Section")$V10))
catch_section <- formatC(catch_section, width = 3, format = "d", flag = "0") 
catch_data$SECTION <- rep(catch_section, each= length(1950:2015))

catch_spawn_data <- join(spawn_data,catch_data, by= c("YEAR","SECTION"))

write.csv(catch_spawn_data, file= "1_Data/scraped_spawn_catch_data.csv",row.names= F)



