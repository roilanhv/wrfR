#'  @title Localize stations indexes
#'  @description
#'  \code{locWRFst} localizes the lat and lon indexes for each station within the WRF grid
#'  @param fileWRF a string containing the WRF filename
#'  @param st.data a data.frame  [  st.data <- data.frame(st.data$Lat,st.data$Lon,st.data$AGL,st.data$Name)  ]
#'  @return A dataframe containing stations information \cr
#'  dimnames(st.loc)[[1]] = stations names # \cr 
#'  dimnames(st.loc)[[2]] = c('lat','lon','elev','lat_indx','lon_indx')  
#'  @author I. Lopez-Coto, 2013 (israel.lopez@@dfa.uhu.es / inl@@nist.gov)
#'  @export


locWRFst <- function(fileWRF, st.data){
  
  require(raster)
  
  ## Get the WRF grid
  
  WRFgrid <- getGRID(fileWRF)
  for (i in 1:length(names(WRFgrid))) {
    assign(names(WRFgrid)[[i]], WRFgrid[[i]])
  }
  
  ## Localize stations indexes
  
  for (st in 1:dim(st.data)[1])
  {
    # Get the station location
    dist <-
      pointDistance(
        c(st.data$lon[st], st.data$lat[st]),
        cbind(as.vector(lon_WRF), as.vector(lat_WRF)),
        longlat = TRUE,
        r = 6370 * 1e3
      ) # Uses WRF earth radius
    dist <- matrix(dist, ncol = ncol(lon_WRF), nrow = nrow(lon_WRF))
    stloc <- which(dist == min(dist), arr.ind = TRUE)
    
    st.data$lat_indx <- stloc[2]
    st.data$lon_indx <- stloc[1]
  }
  
  return(st.data)
}
