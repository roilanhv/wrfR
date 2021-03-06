#'  @title Convert WRF data to regular 3D cube (lon, lat, z) 
#'  @description
#'  \code{getWRFcube} extracts WRF default (meteorological variables) + additional variables to regular 3D cube (lon, lat, z)  from WRF files. 
#'  \strong{WARNING:} this function may be slow and require a lot of memory depending on the input file size.
#'  @param nc pointer to the WRF netcdf file as produced by \link{open.nc}
#'  
#'  @param time_ind \code{integer} with the time index within the file being processed
#'  @param vlev number of vertical levels (default = 10)
#'  @param HGT_WRF 3D terrain elevation array as produced by \link{getGRID}
#'  @param lon_WRF 2D WRF longitudes matrix as produced by \link{getGRID}
#'  @param lat_WRF 2D WRF latitudes matrix as produced by \link{getGRID}
#'  @param WRFproj proj4 string witht the current projection as produced by \link{getGRID}
#'  @param var4d vector of strings containing the name(s) of 4D WRF variable(s) to extract 
#'  @param var3d vector of strings containing the name(s) of 3D WRF variable(s) to extract 
#'  @param reproject \code{logical} whether or not to reproject 
#'  @return list containing raster objects for the variables \cr
#'  c('U3D','V3D','W3D','WS3D','WD3D','P3D','T3D','TH3D','TD3D',var4d,var3d)   

#'  @author I. Lopez-Coto, 2014 (israel.lopez@@dfa.uhu.es / inl@@nist.gov)
#'  @export

getWRFcube <-
  function(nc_file,
           time_ind = 1,
           vlev = 37,
           zout = seq(10,300,25),
           var4d = c('TKE_PBL'),
           var3d = c('PBLH','HFX','LH','UST'),
           reproject = TRUE) {
    
    message('Reading file: ', nc_file)
    
    require(RNetCDF)
    
    ## Constants
    P0 = 1000  #hPa
    kappa = 0.2854
    A = 2.53e8 #kPa
    B = 5.42e3 #K (Kelvin)
    e = 0.622 #(approximated from R'/Rv)
    R = 287.05 #J/kg dry air
    
    nc <- open.nc(nc_file)
    nc_grid <- getGRID(nc_file)
    
    proj.latlon <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
    
    WRFproj <- nc_grid$WRFproj
    lon_WRF <- nc_grid$lon_WRF
    lat_WRF <- nc_grid$lat_WRF
    levs <- var.get.nc(nc, variable = "ZNW")
    time <- var.get.nc(nc,"Times")
    
    if(is.null(vlev)) vlev <- length(levs)-1
    
    if (is.null(zout)) {
      levs <- levs[1:vlev]
    } else {
      levs <- zout
    } 
  
    ### Read the WRF file
    
    message('- Projection and grid readed')
    message('-- Reading variables: ')
    
    TWRF <-
      var.get.nc(nc,
                 "T",
                 start = c(1, 1, 1, time_ind),
                 count = c(NA, NA, vlev, 1))
    message('  + T')
    
    QVAPOR <-
      var.get.nc(nc,
                 "QVAPOR",
                 start = c(1, 1, 1, time_ind),
                 count = c(NA, NA, vlev, 1))
    message('  + QVAPOR')
    
    U.stag <-
      var.get.nc(nc,
                 "U",
                 start = c(1, 1, 1, time_ind),
                 count = c(NA, NA, vlev, 1))
    U <- apply(U.stag, c(2, 3), diff) / 2 + U.stag[1:(dim(U.stag)[1]) - 1, , ]
    message('  + T')
    
    V.stag <-
      var.get.nc(nc,
                 "V",
                 start = c(1, 1, 1, time_ind),
                 count = c(NA, NA, vlev, 1))
    V <-
      aperm(apply(V.stag, c(1, 3), diff) / 2, c(2, 1, 3)) + V.stag[, 1:(dim(V.stag)[2]) - 1, ]
    message('  + V-WIND')
    
    W.stag <-
      var.get.nc(nc,
                 "W",
                 start = c(1, 1, 1, time_ind),
                 count = c(NA, NA, vlev + 1, 1))
    W <-
      aperm(apply(W.stag, c(1, 2), diff) / 2, c(2, 3, 1)) + W.stag[, , 1:(dim(W.stag)[3] - 1)]
    message('  + W-WIND')
    
    Uearth <- 0 * U
    Vearth <- 0 * V
  
      for (nlev in 1:vlev){
        Uearth[, , nlev] <-
          U[, , nlev] * nc_grid$cosalpha + V[, , nlev] * nc_grid$sinalpha
        Vearth[, , nlev] <-
          V[, , nlev] * nc_grid$cosalpha - U[, , nlev] * nc_grid$sinalpha
      }
  
  
  PH.stag <-
    var.get.nc(nc,
               "PH",
               start = c(1, 1, 1, time_ind),
               count = c(NA, NA, vlev + 1, 1))
  PH <-
    aperm(apply(PH.stag, c(1, 2), diff) / 2, c(2, 3, 1)) + PH.stag[, , 1:(dim(PH.stag)[3]) - 1]
      message('  + PH')
  
  PHB.stag <-
    var.get.nc(nc,
               "PHB",
               start = c(1, 1, 1, time_ind),
               count = c(NA, NA, vlev + 1, 1))
  PHB <-
    aperm(apply(PHB.stag, c(1, 2), diff) / 2, c(2, 3, 1)) + PHB.stag[, , 1:(dim(PHB.stag)[3]) - 1]
  message('  + PHB')
  
  P <-
    var.get.nc(nc,
               "P",
               start = c(1, 1, 1, time_ind),
               count = c(NA, NA, vlev, 1))
  
  PB <-
    var.get.nc(nc,
               "PB",
               start = c(1, 1, 1, time_ind),
               count = c(NA, NA, vlev, 1))
  
  p <- (P + PB) / 100
  message('  + P')
  
  z <- array(0, dim = c(dim(P)[1], dim(P)[2], dim(P)[3] - 1))
  zabg <- z
  Temp <- array(0, dim = c(dim(P)[1], dim(P)[2], dim(P)[3] - 1))
  Td <- Temp
  nlvl <- dim(P)[3]
  
  for (k in 1:(nlvl - 1)) {
    
    z[, , k] <- ((((PH[, , k] + PH[, , k + 1]) / 2) + ((PHB[, , k] + PHB[, , k + 1]) / 2)) / 9.81)
    
    zabg[, , k] <- z[, , k] - nc_grid$HGT_WRF
    
    Temp[, , k] = (TWRF[, , k] + 300) * ((P[, , k] + PB[, , k]) / (P0 * 100)) ^ kappa - 273.15
    
    Td[, , k] = B / log(A * e / (QVAPOR[, , k] * (P[, , k] + PB[, , k]) / 1000)) - 273.15
    
  }
  message('  + HGT, TEMP, TD')
  
  ## Process the data
    
  message('-- Begining reprojection and/or vertical interpolation')
  
  U3D <- wrf3d.brick(subset3d = vert.interp(Uearth,zabg,zout),
                     lon_WRF,
                     lat_WRF,
                     proj.latlon,
                     WRFproj,
                     zout = levs,
                     reproject); message('  + U-WIND')
  V3D <- wrf3d.brick(subset3d = vert.interp(Vearth, zabg, zout),
                     lon_WRF,
                     lat_WRF,
                     proj.latlon,
                     WRFproj,
                     zout = levs,
                     reproject) ; message('  + V-WIND')
  W3D <- wrf3d.brick(subset3d = vert.interp(W, zabg, zout),
                     lon_WRF,
                     lat_WRF,
                     proj.latlon,
                     WRFproj,
                     zout = levs,
                     reproject); message('  + W-WIND')
  P3D <- wrf3d.brick(subset3d = vert.interp(p, zabg, zout),
                     lon_WRF,
                     lat_WRF,
                     proj.latlon,
                     WRFproj,
                     zout = levs,
                     reproject); message('  + P')
  T3D <- wrf3d.brick(subset3d = vert.interp(Temp, zabg, zout),
                     lon_WRF,
                     lat_WRF,
                     proj.latlon,
                     WRFproj,
                     zout = levs,
                     reproject); message('  + T')
  TD3D <- wrf3d.brick(subset3d = vert.interp(Td, zabg, zout),
                      lon_WRF,
                      lat_WRF,
                      proj.latlon,
                      WRFproj,
                      zout = levs,
                      reproject); message('  + TD')
  TH3D <- wrf3d.brick(subset3d = (vert.interp(TWRF, zabg, zout) + 300 - 273.15),
                      lon_WRF,
                      lat_WRF,
                      proj.latlon,
                      WRFproj,
                      zout = levs,
                      reproject); message('  + TH')
  
  WS3D <- sqrt(U3D^2 + V3D^2)
  message('  + WS')
  WD3D <- WS3D
  
  for (vl in 1:nlayers(U3D)) {
    WD3D[[vl]] <- atan2(U3D[[vl]], V3D[[vl]]) * 180 / (pi) + 180
  }
  names(WD3D) <- names(WS3D)
  message('  + WD')
  
  ZABG <- wrf3d.brick(subset3d = vert.interp(zabg,zabg,zout),
                      lon_WRF,
                      lat_WRF,
                      proj.latlon,
                      WRFproj,
                      zout = levs,
                      reproject)
  
  st.WRF.temp <-list(time,U3D,V3D,W3D,WS3D,WD3D,P3D,T3D,TH3D,TD3D,ZABG)
  varnames <- c('TIME','U3D','V3D','W3D','WS3D','WD3D','P3D','T3D','TH3D','TD3D','ZABG')
  
  ## Extract 4D variables and interpolate (var4d)
  message('- Other variables requested:')
  
  if (length(var4d)>0)  {
    message('  * 4D-VAR')
    for (v in 1:length(var4d)){
      
      assign(paste(var4d[v], '_temp', sep = ''),
             var.get.nc(
               nc,
               var4d[v],
               start = c(1, 1, 1, time_ind),
               count = c(NA, NA, vlev - 1, 1)
             ))
      
      assign(
        paste(var4d[v], '3D', sep = ''),
        wrf3d.brick(
          subset3d = vert.interp(get(paste(
            var4d[v], '_temp', sep = ''
          )), zabg, zout),
          lon_WRF,
          lat_WRF,
          proj.latlon,
          WRFproj,
          zout = levs,
          reproject
        )
      )
      message('   - ',paste(var4d[v], '3D', sep = ''))
      st.WRF.temp[[length(st.WRF.temp) + 1]] <-
        get(paste(var4d[v], '3D', sep = ''))
      varnames <- c(varnames, var4d[v])
    }
  }       
  
  ## Extract 3D variables (var3d)
  
  if (length(var3d) > 0) {
    d3temp <- NULL
    message('  * 2D-VAR')
    for (v in 1:length(var3d)) {
      
      assign(paste(var3d[v], '_temp', sep = ''),
             var.get.nc(
               nc,
               var3d[v],
               start = c(1, 1, time_ind),
               count = c(NA, NA, 1)
             ))
      
      assign(
        paste(var3d[v], '2D', sep = ''),
        wrf2d.raster(
          subset = get(paste(var3d[v], '_temp', sep = '')),
          lon_WRF,
          lat_WRF,
          proj.latlon,
          WRFproj,
          reproject
        )
      )
      message('   - ',paste(var3d[v], '2D', sep = ''))
      st.WRF.temp[[length(st.WRF.temp) + 1]] <-
        get(paste(var3d[v], '2D', sep = ''))
      varnames <- c(varnames, var3d[v])
    }
  }
  
  names(st.WRF.temp) <- varnames
  message('End..')
  return(st.WRF.temp)
  
}  ## end function



