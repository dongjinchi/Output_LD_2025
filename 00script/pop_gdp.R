setwd("D:\\615DJC\\05PhDresearch\\03Climate_Economy\\02new\\02weights")
library(dplyr)
library(terra)
library(raster)
library(sp)
library(sf)
library(readr)
####空间地图####
global_map<-st_read("F:\\01数据库\\社会经济数据库\\01GDP数据\\gridgdp_Kummu\\world_area\\world_area.shp", 
                    fid_column_name = 'FID')
global_map<-sf::st_transform(global_map,crs="+proj=longlat +datum=WGS84")
global_map<- global_map[, -2]
####urbanization####
#urban population
library(ncdf4)
upop <-nc_open("F:\\01数据库\\社会经济数据库\\02人口数据\\03hyde\\urban_population.nc")
upop_data <- ncvar_get(upop, "urban_population")

lon <- ncvar_get(upop, "lon")
lat <- ncvar_get(upop, "lat")
raster_template <- rast(nrows = length(lat), ncols = length(lon), 
                        xmin = min(lon), xmax = max(lon), 
                        ymin = min(lat), ymax = max(lat),
                        crs = "+proj=longlat +datum=WGS84")
region_upop_join<-data.frame()
global_map_vect <-vect(global_map)
for (i in c(93:123)) {
  r <- raster_template
  values(r) <- t(upop_data[,,i])  # 转置数据以匹配栅格
  pop <- r
  region_pop <-terra::extract(pop, fun=sum,global_map_vect,bind=T,na.rm=T)
  region_pop_df<-terra::as.data.frame(region_pop)
  colnames(region_pop_df)[2]<-"upop"
  year = 1897+i
  region_pop_df$year<-year
  region_upop_join<-bind_rows(region_upop_join,region_pop_df)
  print(year)
}

write_excel_csv(region_upop_join,file = "GDL_urbpopulation2020.csv")

#rural population
rpop <-nc_open("F:\\01数据库\\社会经济数据库\\02人口数据\\03hyde\\rural_population.nc")
rpop_data <- ncvar_get(rpop, "rural_population")

lon <- ncvar_get(rpop, "lon")
lat <- ncvar_get(rpop, "lat")
raster_template <- rast(nrows = length(lat), ncols = length(lon), 
                        xmin = min(lon), xmax = max(lon), 
                        ymin = min(lat), ymax = max(lat),
                        crs = "+proj=longlat +datum=WGS84")
region_rpop_join<-data.frame()
global_map_vect <-vect(global_map)
for (i in c(93:123)) {
  r <- raster_template
  values(r) <- t(rpop_data[,,i])  # 转置数据以匹配栅格
  pop <- r
  region_pop <-terra::extract(pop, fun=sum,global_map_vect,bind=T,na.rm=T)
  region_pop_df<-terra::as.data.frame(region_pop)
  colnames(region_pop_df)[2]<-"rpop"
  year = 1897+i
  region_pop_df$year<-year
  region_rpop_join<-bind_rows(region_rpop_join,region_pop_df)
  print(year)
}

write_excel_csv(region_rpop_join,file = "GDL_rurpopulation.csv")


upop<-read_csv("GDL_urbpopulation.csv")
rpop<-read_csv("GDL_rurpopulation.csv")