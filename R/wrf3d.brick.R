#'  @title Rasterize and Reproject 3D fields
#'  @description
#'  \code{wrf3d.brick} reprojects and rasterizes WRF 3D fields to lat lon
#'  @param subset3D a 3D array with the input field 
#'  @param lon_WRF a matrix with the WRF longitudes 
#'  @param lat_WRF a matrix with the WRF latitudes
#'  @param proj.latlon proj4 string witht the desired projection
#'  @param WRFproj proj4 string witht the current projection
#'  @param reproject \code{logical} whether or not to reproject 
#'  @return a reprojected raster stack object

#'  @author I. Lopez-Coto, 2014 (israel.lopez@@dfa.uhu.es / inl@@nist.gov)
#'  @export


wrf3d.brick <-
  function(subset3d,
           lon_WRF,
           lat_WRF,
           proj.latlon,
           WRFproj,
           zout,
           reproject = TRUE){
    
    require(parallel)
    
    r.out <-
      mclapply(1:dim(subset3d)[[3]],
               function(ilayer) {
                 wrf2d.raster(subset = subset3d[, , ilayer],
                              lon_WRF,
                              lat_WRF,
                              proj.latlon,
                              WRFproj,
                              reproject = FALSE) %>%
                   return()
               },
               mc.cores = detectCores() - 1,
               mc.preschedule = FALSE) %>%
      brick() 
    
    if (reproject) {
      r.out <- projectRaster(r.out, crs = proj.latlon)
    }
    
      if(max(zout) == 1.0){
        names(r.out) <- paste0("LEV.",round(zout[1:nlayers(r.out)],3))
      } else {
        names(r.out) <- paste0("HGT.",zout[1:nlayers(r.out)])
      }
     
      return(r.out)
  }


# 
# system.time(
#   r.out <-  foreach(vl = 1:dim(subset3d)[[3]]) %dopar%
#     wrf2d.raster(subset = subset3d[, , vl],
#                  lon_WRF,
#                  lat_WRF,
#                  proj.latlon,
#                  WRFproj,
#                  reproject = FALSE) %>% 
#     brick()    )
# 
# system.time(
#   for (vl in 1:dim(subset3d)[[3]]){
#     rr.p <- wrf2d.raster(
#       subset = subset3d[, , vl],
#       lon_WRF,
#       lat_WRF,
#       proj.latlon,
#       WRFproj,
#       reproject = FALSE
#     )
#     if (vl == 1) {
#       r.out <- rr.p
#     } else {
#       r.out <- addLayer(r.out, rr.p)
#     }
#     
#     
#   }  # end vertical levels
# )

