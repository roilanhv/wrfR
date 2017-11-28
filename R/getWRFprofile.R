#'  @title Convert and extract WRF vertical profiles at stations locations
#'  @description
#'  \code{getWRFprofile} extracts WRF vertical profiles of default (meteorological variables) + additional variables at stations locations from WRF files. 
#'  @param filenames a vector of WRF filenames (It can be length(filenames) == 1)
#'  @param st.loc dataframe as produced by locWRFst() 
#'  @param vlev number of vertical levels (default = 10)
#'  @param var4d vector of strings containing the name(s) of 4D WRF variable(s) to extract 
#'  @param masl \code{logical} whether or not the elevation is above sea level
#'  @return list containing the dataframes for each station \cr
#'  c('name','lat','lon','date','z','u','v','w','ws','wd','p','T','theta','Td',var4d)

#'  @author I. Lopez-Coto, 2014 (israel.lopez@@dfa.uhu.es / inl@@nist.gov)
#'  @export


getWRFprofile <-
  function(filenames,
           st.loc,
           vlev = 50,
           masl = FALSE,
           var4d = NULL){
    
  require(RNetCDF)
  require(tidyverse)
  require(raster)
    
  lapply(1:length(filenames), function(f){
    
    fileWRF <- readRDS(filenames[f])
    date.in.WRF <- strptime(fileWRF$TIME, tz = "BRT", format = "%Y-%m-%d_%H:%M:%S")
    
      # Extract the stations data and interpolate to the correct height (m)
      
      lapply(1:nrow(st.loc), function(i){
        
        if (masl){
          z.st <- z[st.loc$lon_indx[i],st.loc$lat_indx[i],1:dim(z)[3]]
        } else {
          z.st <- as.vector(fileWRF$ZABG[st.loc$lon_indx[i],st.loc$lat_indx[i],1:vlev])
        }
        
        U.st <- as.vector(fileWRF$U3D[st.loc$lon_indx[i],st.loc$lat_indx[i],   ])
        V.st <- as.vector(fileWRF$V3D[st.loc$lon_indx[i],st.loc$lat_indx[i],   ])
        w.st <- as.vector(fileWRF$W3D[st.loc$lon_indx[i],st.loc$lat_indx[i],   ])
        p.st <- as.vector(fileWRF$P3D[st.loc$lon_indx[i],st.loc$lat_indx[i],   ])
        t.st <- as.vector(fileWRF$T3D[st.loc$lon_indx[i],st.loc$lat_indx[i],   ])
        td.st <- as.vector(fileWRF$TD3D[st.loc$lon_indx[i],st.loc$lat_indx[i], ])
        th.st <- as.vector(fileWRF$TH3D[st.loc$lon_indx[i],st.loc$lat_indx[i], ] +300-273.15)
        
        ws<-sqrt(U.st^2 + V.st^2)
        wd<-atan2(U.st,V.st)*180/(pi)+180
        
        ## Extract 4D variables and interpolate (var4d)
        # if (length(var4d) > 0)
        # {
        #   d4temp <- NULL
        #   for (v in 1:length(var4d))
        #   {
        #     assign(paste(var4d[v], '_temp', sep = ''),
        #            var.get.nc(
        #              nc,
        #              var4d[v],
        #              start = c(st.loc$lon_indx[i], st.loc$lat_indx[i], 1, time_ind),
        #              count = c(1, 1, vlev - 1, 1)
        #            ))
        #     temp2 <-  get(paste(var4d[v], '_temp', sep = ''))
        #     d4temp <- cbind(d4temp, temp2)
        #   }
        # }
        
        ## Store it as a data.frame
        st.WRF.temp <-
          data.frame(
            name = rep(st.loc$name[i],length(vlev)),
            lat = rep(st.loc$lat[i],length(vlev)),
            lon = rep(st.loc$lon[i],length(vlev)),
            date = rep(date.in.WRF[time_ind],length(vlev)),
            z = z.st[1:vlev],
            u = U.st[1:vlev],
            v = V.st[1:vlev],
            w = w.st[1:vlev],
            ws = ws[1:vlev],
            wd = wd[1:vlev],
            p = p.st[1:vlev],
            t = t.st[1:vlev],
            th = th.st[1:vlev],
            td = td.st[1:vlev],
            stringsAsFactors = F )
        
        # if (length(var4d) > 0){
        #   st.WRF.temp <- cbind(st.WRF.temp, d4temp)
        # }
        
      }) %>%
        bind_rows() 
        
    }) %>% bind_rows() %>% 
    return()
  
} ## End function

