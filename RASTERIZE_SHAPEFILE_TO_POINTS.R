require(raster)
require(rgdal)
require(sp)


shp_test<-readOGR("D:/ADMIN/test","test")
r <- raster(ncol=1080, nrow=1080)
extent(r) <- extent(shp_test)

sha<-rasterize(shp_test,r )

sha[which(sa[])]



sha_cell<-sha
sha_cell[]<-1:ncell(sha_cell)
sha_cell[which(is.na(sha[]))]<-NA

points<-as.data.frame(xyFromCell(sha,na.omit(sha_cell[])))
coordinates(points)<- ~x+y


##1000 points
samp<-sample(points,1000)

plot(points)

plot(samp,col="red",add=T)

write.csv(samp,"D:/ADMIN/test/sample.csv",quote=F,row.names = F)

