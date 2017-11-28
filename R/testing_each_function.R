
# install.packages("RNetCDF",dependencies = TRUE)
### TESTE DAS FUNÇOES DO wrfR package
require(RNetCDF)
require(tidyverse)

require(sp)
require(raster)
require(rgdal)


nc_file <-  "~/MEGAtools/linhares/d02/wrfout_d02_2016-09-20_13:00:00"

gridWRF <- getGRID(filename = nc_file)
saveRDS(gridWRF,"~/MEGAtools/linhares/process/gridWRF_d02.rds")
fileWRF <- nc_file
  
WRF_OUT <-
  getWRFcube(
    nc_file = nc_file,
    vlev = NULL,
    zout = NULL,
    reproject = TRUE )

saveRDS(WRF_OUT,paste0("~/MEGAtools/linhares/process/",basename(nc_file),".rds"))

filenames <- paste0("~/MEGAtools/linhares/process/",basename(nc_file),".rds")

st.loc <- 
locWRFst(fileWRF = nc_file, 
         st.data = data.frame( name = "Test", lat = -19.3,lon = -39.456, elev = 23))


WRF_OUT$U3D %>% extent() -> extent_d02
WRF_OUT$TIME

WRF_OUT$T3D %>% 
  .[[2:5]] %>%
  as.data.frame(xy = TRUE) %>% 
  as.tbl() %>% 
  ggplot(aes(x = x, y = y, fill = LEV.0.993, z = LEV.0.993)) +
  geom_raster(interpolate = TRUE) +
  geom_path(data = readRDS("~/MEGA/DOUTORADO/BAGS/maps/Linhares_d02.rds"),
            aes(x = long, y = lat, group = group),
            color = "black",size = .5, inherit.aes = FALSE) +
  geom_contour(color = "red",binwidth = 2) +
  coord_fixed()


getWRFprofile(filenames = filenames,st.loc = st.loc)

