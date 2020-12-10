library(raster)



rpaths = c(r1700 = "anthromes_2_GeoTIFF/1700/anthro2_a1700.tif",
           r1800 = "anthromes_2_GeoTIFF/1800/anthro2_a1800.tif",
           r1900 = "anthromes_2_GeoTIFF/1900/anthro2_a1900.tif",
           r2000 = "anthromes_2_GeoTIFF/2000/anthro2_a2000.tif")

raster_stack = stack(rpaths)
# from Ellis et al. 2010, coding of the raster values
# value	level
# 11	Dense Settlements 
# 12	Dense Settlements
# 21	Villages 
# 22	Villages
# 23	Villages
# 24	Villages 
# 31	Croplands 
# 32	Croplands
# 33	Croplands
# 34	Croplands 
# 41	Rangelands
# 42	Rangelands
# 43	Rangelands 
# 51	Seminatural
# 52	Seminatural
# 53	Seminatural
# 54	Seminatural 
# 61	Wildlands
# 62	Wildlands 
# we change the values so there is resolution lost and only 6 classes are used
rc_stack =  as.integer(raster_stack/10) 
NAvalue(rc_stack) = 0

# we reclassify to make semi-natural and wild as "natural"
m <- c(0, 4, 0, 
       4, 6, 100)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc_stack_rcl = reclassify(rc_stack, rclmat)
plot(rc_stack_rcl)



## create transition rasters  
nber_raster_trans = 100

for (j in 1:nlayers(rc_stack_rcl)){
  ss = stack()
  if (j+1>nlayers(rc_stack_rcl)){
    last = raster(rc_stack_rcl, nlayers(rc_stack_rcl))
    names(last) = paste(names(last), formatC(0, digits = 2, flag = "0"), sep = "_")
    ss = stack(ss, last)
    writeRaster(ss, filename = "trans.tif", suffix = 'names', bylayer=TRUE , format="GTiff", overwrite=TRUE)
    break}
  else{
    start = raster(rc_stack_rcl,j)
    end = raster(rc_stack_rcl,j+1)
    
    ss = stack(ss, start)
    names(ss)[1] = paste(names(start), formatC(0, digits = 2, flag = "0"), sep = "_")
    for (i in 1:(nber_raster_trans-1)){
      
      trans = start + i/nber_raster_trans * (end-start)
      
      names(trans) = paste(names(start),formatC(i, digits = 2, flag = "0"), sep = "_")
      ss = stack(ss, trans)
    
      }
    writeRaster(ss, filename = "trans.tif", suffix = 'names', bylayer=TRUE , format="GTiff", overwrite=TRUE)
  }
}
ss = stack(ss, raster(rc_stack_rcl, nlayers(rc_stack_rcl)))

names(ss)
#plot(ss)

writeRaster(ss, filename = "trans.tif", suffix = 'names', bylayer=TRUE , format="GTiff", overwrite=TRUE)


# create transition raster of rangeland 2000

r2000 = raster(rc_stack,4)
r2000_rangeland = (r2000 == 4) * 100
plot(r2000_rangeland)

r_2000_wild = raster(rc_stack_rcl,4)
r_2000_wild_30 = (r_2000_wild ==100 )*25
plot(r_2000_wild_30)


r_2000_wild_rangeland = r2000_rangeland + r_2000_wild_30
#NAvalue(r_2000_wild)=0
#NAvalue(r_2000_wild_rangeland)=0
plot(r_2000_wild_50)
plot(r_2000_wild_rangeland)
#NAvalue(r_2000_wild_rangeland)=0

#writeRaster(r_2000_wild_rangeland, "r_2000_wild_rangeland_35.tiff", format="GTiff", overwrite=TRUE)

start_rangeland = (r2000_rangeland == 100) * 100
plot(start_rangeland)
start = start_rangeland # start is wild 30 and rangeland 0
values(start)[1]=100

plot(start)
end = r2000_rangeland # end is wild 30 and rangeland 100
ss = stack(start+ r_2000_wild_30)

nber_layers = 111
for (i in 1:(nber_layers-1)){
  trans = start + i/nber_layers * (end-start)
  
  trans = trans + start
  names(trans) = paste(names(start),formatC(i, digits = 2, flag = "0"), sep = "_")
  ss = stack(ss, trans)
}
plot(ss)
ss
writeRaster(ss, filename = "rangeland_wild25_pixel.tif", suffix = 'names', bylayer=TRUE , format="GTiff", overwrite=TRUE)


# create 2 files svg from 1700 and 2000, the ones used in the progress bar
# we change the values so there is resolution lost and only 6 classes are used
rc_stack =  as.integer(raster_stack/10) 
NAvalue(rc_stack) = 0

# we reclassify to make semi-natural and wild as "natural"
m <- c(1, 4, 3, 
       4, 6, 100)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc_stack_rcl = reclassify(rc_stack, rclmat)
plot(rc_stack_rcl)
NAvalue(rc_stack_rcl)=0
plot(rc_stack_rcl)
par(bg=NA)
png(filename= "1700.png",width = 15, height = 15, res = 300, units = "cm", bg = NA)
plot(rc_stack_rcl,1, breaks=c(0,48,100), col = c("#22272D","#009A00"))
#dev.copy(png,'2000.png')
dev.off()

rr = (r2000_rangeland==100)*20 + raster(rc_stack_rcl,4)
#NAvalue(rr)=0
png(filename= "2000_wild_rangeland.png",width = 15, height = 15, res = 300, units = "cm", bg = NA)
plot(rr, breaks=c(15, 25,100), col = c("#AFEC37", "#00C61A"))
dev.off()
