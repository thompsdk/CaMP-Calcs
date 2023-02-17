### CaMP DRought code calculations

### here's the 2019 era TPS of point stations:
library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting
library(sf)
library(terra)

  
  
  setwd("E:/GIS_scratch/ECMWF-DC")

  #### new approach:
  ## 1. Read the netcdfs from ECMWF of daily DC using the following string scheme:
  ## ECMWF_FWI_DC_19930401_1200_hr_v4.0_con
  
  DC.example <- rast("~/CaMP-Calcs/DC_annualMax_template.tif")
  crs(DC.example)
  DC.ext <- ext(-360, 0, -90, 90)
  DC.crop <- ext(-150,-45,40,80)
  
  ##also need 1993
  for (year in 1993:1993) {
    setwd("E:/GIS_scratch/ECMWF-DC")
    ## GOAL: read the daily .nc file, then stack all of them and read the max value from the entire stack of nc for a given year.
   
    pattern1 <- c(year)
    #(1)make a list of all files with *year* in the directory, via https://stackoverflow.com/a/59307366
    
    ##open a single file
    DC.files <- list.files("E:/GIS_scratch/ECMWF-DC", pattern=paste0(pattern1, collapse="|"), full.names=TRUE)
    
    DC1 <- rast(DC.files[1])
    ext(DC1) <- DC.ext
    #DC1new <- crop(DC1,DC.ext)
    
    DCBrick <- c(DC1,warn=FALSE)
    
    
    for (i in 2:length(DC.files)){
    #(2)make into a stack
    ###open each raster:
    rasterNC <- rast(DC.files[i])
    ext(rasterNC) <- DC.ext
  
    
    #then write it to a brick
    DCBrick <- c(DCBrick,rasterNC)
    }
    
    #(3) grab the per-pixel max value from the stack of nc files
    
    #maxAnnual <- calc(DCBrick, function(x) max(x, na.rm = TRUE)) #since there's NAs
    
    maxAnnual <- max(DCBrick)
    maxAnnual <- crop(maxAnnual,DC.crop)
    crs(maxAnnual) <- "epsg:4326"
    maxAnnual <- terra::project(maxAnnual, DC.example)
    
    ## and then reproject to the grid of the example tif:
    
    outName <- paste0("DCmax",year,".tif")
    setwd("E:/GIS_scratch/ECMWF-DC/out/")
    writeRaster(maxAnnual,filename = outName,overwrite = TRUE)
    }
  plot(maxAnnual)
  