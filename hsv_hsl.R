hsv_to_hsl<-function(HSV){
 
 
 
 HSV.x<-HSV[1]
 HSV.y<-HSV[2]
 HSV.z<-HSV[3]
 
 
 HSL.x = HSV.x;
 HSL.z = (2 - HSV.y) * HSV.z
 HSL.y = HSV.y * HSV.z
 if (HSL.z <= 1) {HSV.y=HSV.y/HSL.z
 }else {
   HSL.y=HSL.y/(2 - HSL.z)
 }
 
 HSL.z = HSL.z/2
 HSL<-c(HSL.x,HSL.y,HSL.z)
  return (HSL)
 }





hsl_to_rgb <- function(h, s, l) {
  h <- h / 360
  r <- g <- b <- 0.0
  if (s == 0) {
    r <- g <- b <- l
  } else {
    hue_to_rgb <- function(p, q, t) {
      if (t < 0) { t <- t + 1.0 }
      if (t > 1) { t <- t - 1.0 }
      if (t < 1/6) { return(p + (q - p) * 6.0 * t) }
      if (t < 1/2) { return(q) }
      if (t < 2/3) { return(p + ((q - p) * ((2/3) - t) * 6)) }
      return(p)
    }
    q <- ifelse(l < 0.5, l * (1.0 + s), l + s - (l*s))
    p <- 2.0 * l - q
    r <- hue_to_rgb(p, q, h + 1/3)
    g <- hue_to_rgb(p, q, h)
    b <- hue_to_rgb(p, q, h - 1/3)
  }
  return(rgb(r,g,b))
}

# r, g, b = 0.0 - 1 (0 - 100%)
# returns h/s/l in a vector, h = 0-360 deg, s = 0.0 - 1 (0-100%), l = 0.0 - 1 (0-100%)
# rgb_to_hsl <- function(r, g, b) {
#   val_max <- max(c(r, g, b))
#   val_min <- min(c(r, g, b))
#   h <- s <- l <- (val_max + val_min) / 2
#   if (val_max == val_min){
#     h <- s <- 0
#   } else {
#     d <- val_max - val_min
#     s <- ifelse(l > 0.5, d / (2 - val_max - val_min), d / (val_max + val_min))
#     if (val_max == r) { h <- (g - b) / d + (ifelse(g < b, 6, 0)) }
#     if (val_max == g) { h <- (b - r) / d/ + 2 }
#     if (val_max == b) { h <- (r - g) / d + 4 }
#     h <- (h / 6) * 360
#   }
#   return(c(h=h, s=s, l=l))
# }

# HSV<-c(H,S,V)
# x<-hsv_to_hsl(HSV)
# y<-hsl_to_rgb(HSV[1], HSV[2], HSV[3])


rgbmin<-c(122,29,237)
#rgbmax<-c(242,0,6)

rgbmin<-rgb2hsv(rgbmin)
rgbnew<-rgbmin
rgbnew<-c(rgbmin[1],rgbmin[2],rgbmin[3])


class<-5
nclass<-1:class
grades<-360

npart<-1
list_2<-list()
for(i in 1:length(nclass)){
  
  delta<- grades/nclass[[i]]
  hue_new<-rgbnew[1]+(delta*npart)
  
  
  list_2[[i]]<-rgbnew
  
  #
  list_2[[i]][1]<-hue_new
  
};rm(i)

list_3<-list()

for(j in 1:length(list_2)){
  
list_3[[j]] <-hsv_to_hsl(list_2[[j]])
  
};rm(j)


list_4<-list()
for(k in 1:length(list_2)){
  
  list_4[[k]] <-hsl_to_rgb(list_3[[k]][1],list_3[[k]][2],list_3[[k]][3])
  
};rm(k)

# HSV<-list_2[[1]]
# x<-hsv_to_hsl(HSV)
#y<-hsl_to_rgb(HSV[1], HSV[2], HSV[3])
# y<-hsl_to_rgb(HSV[1], HSV[2], HSV[3])

