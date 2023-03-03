### CaMP DRought code calculations for annual GCBM runs
### Dan Thompson, CFS, daniel.thompson@nrcan-rncan.gc.ca
### Updated 2023-02-17

library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting
library(sf)
library(terra)

  
  # Data source: https://doi.org/10.24381/cds.0e89c522 
  # direct ECMWF fire weather historical grid product, for use from 2012 onwards.  Superceding older CWFIS gridded product.
  # the two products are comparable in terms of capturing regional drought extent.
  # prior data source was NARR FWI from https://www.publish.csiro.au/wf/WF17008 

  #local storage location of downloaded daily .nc files from ECMWF
  setwd("E:/GIS_scratch/ECMWF-DC") 

  #### new approach:
  ## 1. Read the netcdfs from ECMWF of daily DC using the following string scheme:
  ## ECMWF_FWI_DC_19930401_1200_hr_v4.0_con
  
  
  ###load the centroids of all the Parks Locations:
  
  ParksCentroids <- read.csv("E:/GIS_scratch/ECMWF-DC/ParksExtents/ParksCentroids.csv") #the centroid long/lat of the parks of interest (not doing zonal stats for now)
  
  ParksCentroids <- as.data.frame(ParksCentroids)
  
  ParksCentroids.SpatVector <- vect(ParksCentroids,geom=c("X","Y"),crs="epsg:4326",keepgeom=TRUE)
  
  
  ParksMaxDC <- array(data=NA,dim=c(1,3))
  colnames(ParksMaxDC) <- c("ID","MaxDC","Year")
  
  DC.example <- rast("~/CaMP-Calcs/DC_annualMax_template.tif") # example file containing NACID projection and gridding system for GCBM
  crs(DC.example)
  
  DC.ext <- ext(-360, 0, -90, 90) #note that ECMWF .nc files start at the Prime Meridian as their xmin, so for Canada-only analysis, shifting over and cropping to maintain negative longitude over Canada
  DC.crop <- ext(-150,-45,40,80)
  
  ##also need 1993
  for (year in 2021:2022) {
    setwd("E:/GIS_scratch/ECMWF-DC")
    ## GOAL: read the daily .nc file, then stack all of them and read the max value from the entire stack of nc for a given year.
   
    pattern1 <- c(year)
    #(1)make a list of all files with *year* in the directory, via https://stackoverflow.com/a/59307366
    
    #create a list of all applicable files with the year in the file name. !! will capture ALL files, not just .nc!!!
    DC.files <- list.files("E:/GIS_scratch/ECMWF-DC", pattern=paste0(pattern1, collapse="|"), full.names=TRUE)
    
    DC1 <- rast(DC.files[1]) #use terra to rasterize
    ext(DC1) <- DC.ext#reproject, but keep global extent
    #DC1new <- crop(DC1,DC.ext)
    
    DCBrick <- c(DC1,warn=FALSE) #make this into a Brick
    
    
    for (i in 2:length(DC.files)){
    #(2)make into a stack
    ###open each raster:
    rasterNC <- rast(DC.files[i]) #take each next .nc and rasterize it
    ext(rasterNC) <- DC.ext #reproject
  

    
    #then write it to the ongoing brick
    DCBrick <- c(DCBrick,rasterNC)
    }
    
    #(3) grab the per-pixel max value from the stack of nc files
    maxAnnual <- max(DCBrick)
    
    maxAnnual <- crop(maxAnnual,DC.crop) #then crop to Area of Interest
    
    crs(maxAnnual) <- "epsg:4326" #define crs as geographic
    
    ##now that it is cropped and reprojected, extract point locations of maxAnnual for each park and add it alongside the year to the dataframe:
    
    maxAnnual <- terra::project(maxAnnual, DC.example) #then reproject just the final maxAnnual grid to the NACID projection and smaller pixel size.
    
    
    outName <- paste0("DCmax",year,".tif")
    setwd("E:/GIS_scratch/ECMWF-DC/out/")
    writeRaster(maxAnnual,filename = outName,overwrite = TRUE) #make unique file and output 1 TIFF per year
    }
  #plot(maxAnnual)
  
  
  
  
  #### update 2023-02-17: make a 1990-2019 
  
  #make stack of all the maxAnnual rasters:
  setwd("E:/GIS_scratch/ECMWF-DC/out/")
  DCMax.files <- list.files("E:/GIS_scratch/ECMWF-DC/out/",pattern=paste0("DCmax",collapse="|"), full.names=TRUE)
  
  MaxDCStack <- rast(DCMax.files) # that was handy to throw just a giant list of tiff files as terra::rast :)
  
  # then compute the per-pixel 80th percentile:
  
  DroughtCode80thPercentile_For_Forward <- quantile(MaxDCStack, probs=seq(0.80))
  
  writeRaster(DroughtCode80thPercentile_For_Forward,filename = "DroughtCode80thPercentile_For_Forward.tif",overwrite = TRUE) 
  
  
  
  ###2023-02-28 Parks Canada subset
  
  ### 1) create a SpatVector object with a list of long/lat centroids for each park
  
  ### 2) loop through the file list and grab the max annual DC for each park for each year (do this in the loop above)
  ParksCentroids.SpatVector.NACID <- terra::project(ParksCentroids.SpatVector, MaxDCStack)
  ParksDCMaxPerYear <- extract(MaxDCStack,ParksCentroids.SpatVector.NACID,method="simple")
  write.csv(ParksDCMaxPerYear,"ParksDCMaxPerYear.csv")
  
  ### 3) do the same for the spinup DC as well
  
  