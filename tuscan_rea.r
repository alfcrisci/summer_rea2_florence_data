###########################################################################################################
# R code to retrieve weather data extracted from 
# NCEP reanalisys2 Data (Kanamitsu et al. (2002), NCEP-DOE AMIP-II Reanalysis (R-2). Bull. Amer. Meteor. Soc., 83, 1631-1643)
# by using RNCEP R packages (Kemp, M.U., van Loon, E.E., Shamoun-Baranes, J.,and Bouten, W. (2011). RNCEP: global weather and climate data at 
# your fingertips. Methods in Ecology and Evolution, DOI:10.1111/j.2041-210X.2011.00138.x.
#
# Florence location considered= 11.24,43.77
# Variables 'air.sig995','rhum.sig995','uwnd.sig995','vwnd.sig995',"dswrf.sfc"
# 
# Authors : Alfonso Crisci (a.crisci@ibimet.cnr.it) & Marco Morabito (m.morabito@ibimet.cnr.it).
# Institution: IBIMET CNR Institute of Biometeorology - Florence- www.ibimet.cnr.it
# Data license: CC-BY 4.0 
###########################################################################################################


#install.packages("RNCEP")
#install.packages("lubridate")
#install.packages("raster")
#install.packages("XLConnect")

library(RNCEP)
library(lubridate)
library(raster)
library(XLConnect)


#########################################################################################################

setwd("")

#########################################################################################################

rncep2stack=function(data,projdata='+proj=longlat + datum=WGS84') {
  df=NCEP.array2df(data)
  time_labels=unique(df$datetime)  
  df=df[,c(1,3,2,4)]
  res=stack(lapply(as.list(time_labels),
                   function(x) {r=rasterFromXYZ(subset(df,datetime==x)[,2:4]);
                   proj4string(r)<- CRS(projdata);
                   return(r)}));
  names(res)=time_labels;
  return(res)
}

#########################################################################################################

vars=c('air.sig995','rhum.sig995','uwnd.sig995','vwnd.sig995',"dswrf.sfc")

res=list()

for (i in 1:length(vars)) {
  
lev=ifelse(i==5,"gaussian","surface")

temp=NCEP.gather(vars[i],
                     level=lev, 
                     months.minmax=c(5,9), 
                     years.minmax=c(2016,2017),
                     lat.southnorth=c(40,45), 
                     lon.westeast=c(8,12), 
                     reanalysis2 =T,
                     status.bar=FALSE);


res[[i]]=rncep2stack(temp)

}

land_tuscan=crop(raster("land.nc"),extent(res[[1]]))

# sea mask ( land==1 )

# [1,]    1    1    1
# [2,]    0    0    1
# [3,]    0    0    0


#########################################################################################################

saveRDS(res,"tuscan_reanalisys_array.rds")
saveRDS(land_tuscan,"land_tuscan_rea.rds")

#########################################################################################################
res=readRDS("tuscan_reanalisys_array.rds")

timesdata=strptime(names(res[[1]]),format="X%Y_%m_%d_%H",tz="UTC")
monthly=month(timesdata)
dates=as.Date(timesdata)
#########################################################################################################
firenze_geo=SpatialPoints(cbind(11.24,43.77))
proj4string(firenze_geo)=CRS('+proj=longlat + datum=WGS84')


florence_df=data.frame(time=timesdata,
           month=monthly,
           dates=dates,
           tair_sfc=as.numeric(extract(res[[1]],firenze_geo,method="bilinear"))-273.16,
           rh_sfc=as.numeric(extract(res[[2]],firenze_geo,method="bilinear")),
           uwd_sfc=as.numeric(extract(res[[3]],firenze_geo,method="bilinear")),
           vwd_sfc=as.numeric(extract(res[[4]],firenze_geo,method="bilinear")),
           dswf_sfc= as.numeric(extract(res[[5]],firenze_geo,method="bilinear"))
           )
saveRDS(florence_df,"florence_df_2016_2017.rds")

if(exists("florence_df_2016_2017.xls")) rm("florence_df_2016_2017.xls")                               
XLConnect::writeWorksheetToFile("florence_df_2016_2017.xls",florence_df,"Summer data 2016-2017")


