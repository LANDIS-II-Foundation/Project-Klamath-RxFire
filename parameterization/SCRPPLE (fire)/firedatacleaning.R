library(sf)
library(rgdal)
library(pbapply)

working_dir <- "/Volumes/LaCie/SCRPPLE/fire_perimeters_thru_090219"
dir <- "/Users/alisondeak/Desktop/SPA Lab/RScript&Data/Landis-Siskiyous"
setwd(working_dir)

# load in boundaries for study area
studyarea <- st_read(paste0(dir, "/FinalProducts/landscape_shape/landscape_shape.shp"))

#Create lists of files to load
perim_folders <- list.files(paste(working_dir, "/", sep=""), pattern = paste("\\d{4}\\_perimeters_dd83"))

open_files <- function(perim_folders){
  
  perim_file <- st_read(paste0(working_dir, "/", perim_folders,"/", perim_folders,".shp", sep=""))
  
}

#load in shapefiles for each year as a list; transform to mask's crs
perim_files <- pblapply(perim_folders, open_files)
perim_files <- pblapply(perim_files, st_transform, crs = st_crs(studyarea))

##each files naming scheme needs to be dealt with separately (unfortunately)
# 
# #2000
# year2000 <- perim_files[[1]]
# names(year2000)
# year2000$year <- "2000"
# year2000$uniqueID <- paste(year2000$agency, "-", year2000$unitid, "-", year2000$firenum, sep="")
# year2000 <- year2000[,c("firename", "date_", "method", "acres", "comments", "year", "uniqueID", "geometry")]
# names(year2000) <- c("firename", "date_recorded", "mapmethod", "acres", "comments", "year", "uniqueID", "geometry")
# year2000$load_date <- NULL
# head(year2000)
# 
# #2001
# year2001 <- perim_files[[2]]
# names(year2001)
# names(year2001) <- c("firename", "uniqueID", "acres", "date_recorded", "st_area_sh", "st_length_", "geometry")
# year2001 <- year2001[,c("firename", "uniqueID", "acres", "date_recorded", "geometry")]
# year2001$load_date <- NA
# year2001$comments <- NA
# year2001$mapmethod <- NA
# year2001$comments <- NA
# year2001$year <- "2001"
# head(year2001)
# 
# #2002
# year2002 <- perim_files[[3]]
# names(year2002)
# year2002 <- year2002[,c("event_num","year_", "event_name", "c_date", "c_method", "acres", "loaddate", "geometry")]
# names(year2002) <- c("uniqueID", "year", "firename", "date_recorded", "mapmethod", "acres", "load_date", "geometry")
# year2002$comments <- NA
# head(year2002)
# 
# #2003 
# year2003 <- perim_files[[4]]
# names(year2003)
# year2003 <- year2003[,c("firenum", "firename", "comments", "date_", "method", "source", "acres", "date_uploa", "geometry")]
# head(year2003)
# year2003$firename <- paste(year2003$firename, "-", year2003$comments, sep="")
# year2003$mapmethod <- paste(year2003$method, "-", year2003$source, sep="")
# year2003$year <- "2003"
# year2003<- year2003[,c("firenum", "firename", "comments", "date_", "mapmethod","acres", "date_uploa", "year", "geometry")]
# names(year2003) <- c("uniqueID", "firename", "comments", "date_recorded", "mapmethod", "acres", "load_date", "year", "geometry")
# head(year2003)
# 
# #2004
# year2004 <- perim_files[[5]]
# names(year2004)
# year2004$year <- "2004"
# year2004$firename <- paste(year2004$firename, "-", year2004$comments, sep="")
# year2004$mapmethod <- paste(year2004$method, "-", year2004$source, sep="")
# year2004 <- year2004[,c("acres", "firename", "firenum", "perim_date", "date_uploa", "comments", "year", "mapmethod", "geometry")]
# names(year2004) <- c("acres", "firename", "uniqueID", "date_recorded", "load_date", "comments", "year", "mapmethod", "geometry")
# head(year2004)
# 
# #2005
# year2005 <- perim_files[[6]]
# names(year2005)
# year2005$year <- "2005"
# year2005$firename <- paste(year2005$firename, "-", year2005$comments, sep="")
# year2005$mapmethod <- paste(year2005$method, "-", year2005$source, sep="")
# year2005 <- year2005[,c("acres", "firename", "firenum", "year", "perim_date", "mapmethod", "date_uploa", "comments")]
# names(year2005) <- c("acres", "firename", "uniqueID", "year", "date_recorded", "mapmethod", "load_date", "comments")
# head(year2005)
# 
# #2006
# year2006 <- perim_files[[7]]
# names(year2006)
# year2006$uniqueID <- paste(year2006$unit_id, "-", year2006$fire_num, sep="")
# year2006$firename <- paste(year2006$firename, "-", year2006$comments, sep="")
# year2006 <- year2006[,c("acres", "comments", "date_uploa", "year_", "date_", "uniqueID", "fire_name")]
# names(year2006) <- c("acres", "comments", "load_date", "year", "date_recorded", "uniqueID", "firename")
# head(year2006)
# year2006$mapmethod <- NA
# 
# #2007
# year2007 <- perim_files[[8]]
# names(year2007)
# year2007$uniqueID <- paste(year2007$unit_id, "-", year2007$fire_num, sep="")
# year2007$firename <- paste(year2007$fire_name, "-", year2007$fire, sep="")
# year2007$mapmethod <- NA
# year2007 <- year2007[,c("acres", "comments", "date_uploa", "year_", "date_", "uniqueID", "firename", "mapmethod")]
# names(year2007) <- c("acres", "comments", "load_date", "year", "date_recorded", "uniqueID", "firename", "mapmethod")
# head(year2007)
# 
# #2008
# year2008 <- perim_files[[9]]
# names(year2008)
# year2008$uniqueID <- paste(year2008$unit_id, "-", year2008$fire_num, sep="")
# year2008$firename <- paste(year2008$fire_name, "-", year2008$fire, sep="")
# year2008$mapmethod <- NA
# year2008 <- year2008[,c("acres", "comments", "load_date", "year_", "date_", "uniqueID", "firename", "mapmethod")]
# names(year2008) <- c("acres", "comments", "load_date", "year", "date_recorded", "uniqueID", "firename", "mapmethod")
# head(year2008)
# 
# #2009
# year2009 <- perim_files[[10]]
# names(year2009)
# year2009$uniqueID <- paste(year2009$unit_id, "-", year2009$fire_num, sep="")
# year2009$firename <- paste(year2009$fire_name, "-", year2009$fire, sep="")
# year2009$mapmethod <- NA
# year2009 <- year2009[,c("acres", "comments", "load_date", "year_", "date_", "uniqueID", "firename", "mapmethod")]
# names(year2009) <- c("acres", "comments", "load_date", "year", "date_recorded", "uniqueID", "firename", "mapmethod")
# head(year2009)
# 
# #2010
# year2010 <- perim_files[[11]]
# names(year2010)
# year2010$uniqueID <- paste(year2010$unit_id, "-", year2010$fire_num, sep="")
# year2010$firename <- paste(year2010$fire_name, "-", year2010$fire, sep="")
# year2010$mapmethod <- NA
# year2010 <- year2010[,c("acres", "comments", "load_date", "year_", "date_", "uniqueID", "firename", "mapmethod")]
# names(year2010) <- c("acres", "comments", "load_date", "year", "date_recorded", "uniqueID", "firename", "mapmethod")
# head(year2010)
# 
# #2011
# year2011 <- perim_files[[12]]
# names(year2011)
# year2011$uniqueID <- paste(year2011$unit_id, "-", year2011$fire_num, sep="")
# year2011$firename <- paste(year2011$fire_name, "-", year2011$fire, sep="")
# year2011$mapmethod <- NA
# year2011 <- year2011[,c("acres", "comments", "load_date", "year_", "date_", "uniqueID", "firename", "mapmethod")]
# names(year2011) <- c("acres", "comments", "load_date", "year", "date_recorded", "uniqueID", "firename", "mapmethod")
# head(year2011)
# 
# #2012
# year2012 <- perim_files[[13]]
# names(year2012)
# year2012$uniqueID <- paste(year2012$unit_id, "-", year2012$fire_num, sep="")
# year2012$firename <- paste(year2012$fire_name, "-", year2012$fire, sep="")
# year2012$mapmethod <- NA
# year2012 <- year2012[,c("acres", "comments", "load_date", "year_", "date_", "uniqueID", "firename", "mapmethod")]
# names(year2012) <- c("acres", "comments", "load_date", "year", "date_recorded", "uniqueID", "firename", "mapmethod")
# head(year2012)
# 
# #2013
# year2013 <- perim_files[[14]]
# names(year2013)
# year2013$uniqueID <- paste(year2013$unit_id, "-", year2013$fire_num, sep="")
# year2013$firename <- paste(year2013$fire_name, "-", year2013$fire, sep="")
# year2013$mapmethod <- NA
# year2013 <- year2013[,c("acres", "comments", "load_date", "year_", "date_", "uniqueID", "firename", "mapmethod")]
# names(year2013) <- c("acres", "comments", "load_date", "year", "date_recorded", "uniqueID", "firename", "mapmethod")
# head(year2013)

#2014
year2014 <- perim_files[[15]]
names(year2014)
year2014$uniqueID <- paste(year2014$unit_id, "-", year2014$fire_num, sep="")
year2014$firename <- paste(year2014$fire_name, "-", year2014$fire, sep="")
year2014$mapmethod <- NA
year2014 <- year2014[,c("acres", "comments", "load_date", "year_", "date_", "uniqueID", "firename", "mapmethod", "geometry")]
names(year2014) <- c("acres", "comments", "load_date", "year", "date_recorded", "uniqueID", "firename", "mapmethod", "geometry")
head(year2014)

#2015
year2015 <- perim_files[[16]]
names(year2015)
year2015$uniqueID <- paste(year2015$unit_id, "-", year2015$fire_num, sep="")
year2015$firename <- paste(year2015$fire_name, "-", year2015$fire, sep="")
year2015$mapmethod <- NA
year2015 <- year2015[,c("acres", "comments", "load_date", "year_", "date_", "uniqueID", "firename", "mapmethod", "geometry")]
names(year2015) <- c("acres", "comments", "load_date", "year", "date_recorded", "uniqueID", "firename", "mapmethod", "geometry")
head(year2015)

#2016
year2016 <- perim_files[[17]]
names(year2016)
year2016 <- year2016[,c("fireyear", "uniquefire", "incidentna", "perimeterd", "mapmethod", "gisacres", "datecurren", "comments", "geometry")]
names(year2016) <- c("year", "uniqueID", "firename", "date_recorded", "mapmethod", "acres", "load_date", "comments", "geometry")
head(year2016)

#2017
year2017 <- perim_files[[18]]
names(year2017)
year2017 <- year2017[,c("fireyear", "uniquefire", "incidentna", "perimeterd", "mapmethod", "gisacres", "datecurren", "comments", "geometry")]
names(year2017) <- c("year", "uniqueID", "firename", "date_recorded", "mapmethod", "acres", "load_date", "comments", "geometry")
head(year2017)

#2018
year2018 <- perim_files[[19]]
names(year2018)
year2018 <- year2018[,c("fireyear", "uniquefire", "incidentna", "perimeterd", "mapmethod", "gisacres", "datecurren", "comments", "geometry")]
names(year2018) <- c("year", "uniqueID", "firename", "date_recorded", "mapmethod", "acres", "load_date", "comments", "geometry")
head(year2018)

#2019
year2019 <- perim_files[[20]]
names(year2019)
year2019 <- year2019[,c("fireyear", "uniquefire", "incidentna", "perimeterd", "mapmethod", "gisacres", "datecurren", "geometry")]
names(year2019) <- c("year", "uniqueID", "firename", "date_recorded", "mapmethod", "acres", "load_date", "geometry")
year2019$comments <- NA
head(year2019)

allyears <- rbind(year2014, year2015, year2016, year2017, year2018, year2019)

rm(list = ls()[grep("year20", ls())])

#save as RDS
saveRDS(allyears, file = "allyears.rds")

# load in aggregated data
# allyears <- readRDS("allyears.rds")
# writeSpatialShape(allyears, "allyears")

#check crs'
crs(studyarea)
crs(allyears)

# #any bad polygons?
# sum(st_is_valid(allyears, byid=TRUE, reason=T) == FALSE)
# 
# #simplify polygons for allyears a tad
# allyears_simp <- st_make_valid(allyears)
# st_is_valid(allyears, byid=TRUE)
# 
# #any bad polygons?
# sum(st_is_valid(allyears, byid=TRUE) == FALSE)
# 
# #buffer to deal with 'bad polygons'
# 
# allyears_simp_buff <- st_buffer(allyears[!is.na(allyears)], 0.0)
# 
# #any bad polygons?
# sum(st_is_valid(allyears_proj, byid=TRUE) == FALSE)
# 
# #create a version for all fires intersecting study landscape
# allyears <- rm(allyears_simp_proj_buff)

#create a version that is cropped to study area 
landscape_allyears <-  allyears[studyarea,]

#save as RDS
saveRDS(landscape_allyears, file = "allyears.rds")

