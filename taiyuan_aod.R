
suppressMessages(library(gdalUtils))
suppressMessages(library(raster))
suppressMessages(library(rgdal)) 
suppressMessages(library(rgeos))
## Warning: package 'rgeos' was built under R version 3.4.4
suppressMessages(library(plyr))
suppressMessages(library(magrittr))
suppressMessages(library(sp))
suppressMessages(library(mapview))
suppressMessages(library(MODISTools))
suppressMessages(library(ggplot2))



read_hdf = function(file, n) {
  sds = get_subdatasets(file)
  f = tempfile(fileext = ".tif") # the tiff file is saved as a temporary file
  gdal_translate(sds[n], f)#no crs is defined here, which is easy for the transformation later to longitude and latitude
  brick(f) # the ouptut is a multi-layer raster file
}



aod_dir = "C:/Users/zy125/Box Sync/Postdoc/shanxi/data/aod"
p<-r[, c(1,2)] #need to run a single file and get the first two columns as all the tiles have the same 1200*1200 x*y
n=1



for(year in 2019) {
  # Read HDF files list from AOD directory
  setwd(file.path(aod_dir, year))
  
  files = list.files(pattern = "MCD19A2.*\\.hdf$", recursive = TRUE) # note that MAIACTAOT is for TERRA data and MAIACAAOT is for AQUA data
  
  
  for(f in files) {
    
    # Read data
    sds = get_subdatasets(f)
    
    # Choose which subdatasets you want to retrieve from the hdf file
    Optical_Depth_055 = read_hdf(f, grep("grid1km:Optical_Depth_055", sds))
    AOT_Uncertainty = read_hdf(f, grep("grid1km:AOD_Uncertainty", sds))
    AOT_QA = read_hdf(f, grep("grid1km:AOD_QA", sds))
    
    # Create a different name for each layer 
    names(Optical_Depth_055) = paste0("Optical_Depth_055.", letters[1:nlayers(Optical_Depth_055)])
    names(AOT_Uncertainty) = paste0("AOT_Uncertainty.", letters[1:nlayers(AOT_Uncertainty)])
    names(AOT_QA) = paste0("AOT_QA.", letters[1:nlayers(AOT_QA)])
    
    
    # Stack all the raster together
    r = stack(Optical_Depth_055, AOT_Uncertainty, AOT_QA)
    r = as.data.frame(r, xy=TRUE)
    
    # Add filename
    r$date =
      f %>%
      strsplit("\\.") %>%
      sapply("[", 2) %>%
      substr(2, 8) %>%
      as.Date(format = "%Y%j")%>% 
      as.character(format = "%m.%d.%y")
    
    r$Optical_Depth_055_mean= rowMeans(r[,3:(3+nlayers(Optical_Depth_055))], na.rm=TRUE)
    
    p<-cbind(p, r$Optical_Depth_055_mean)
    
    names(p)[n+2]<-paste0(r$date[1])
    
    n=n+1
    
    # Combine results
    
  }
  
}


p<-r[, c(1,2)]

for (n in 1:7)
{
  p[, n+2]<-result[[n]]$Optical_Depth_055_mean
  
  names(p)[n+2]<-paste0(result[[n]]$date[1])
  
}



#the modis is using the Sinusoidal_projection 
# https://github.com/ropensci/MODISTools/blob/master/R/coordinate_conversion.R
#transform sinusoidal_projection to CRS ESPG:4326
#package MODISTools is used here 



lat_lon<-sin_to_ll(p$x, p$y)

p<-cbind(p,lat_lon)

p$all.mean<-rowMeans(p[, 3:367], na.rm=TRUE)

p$na_count <- apply(p, 1, function(x) sum(is.na(x)))


p_crs<-st_as_sf(p, coords=c("longitude_ll", "latitude_ll" ))

p_geo<-st_set_crs(p_crs, 4326)

st_write(p_geo,"aod2019_geo.shp", driver = "ESRI Shapefile")

taiyuan_boun<-st_read("C:/Users/zy125/Box Sync/Postdoc/shanxi/data/tysq.shp")

p_clipped<-st_intersection(p_geo, taiyuan_boun)

ggplot() + geom_sf(data = p_clipped, colour = "light gray", fill = NA)

mapview(p_clipped, zcol='all.mean')

st_write(p_clipped,"ty_aod2019_4326.shp", driver = "ESRI Shapefile")
