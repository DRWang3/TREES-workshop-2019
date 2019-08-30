## some functions 

# aggregate simulated data to hourly
agg.sim = function(time.vec, sim.vec){
  
  if(length(sim.vec%%2 == 0)){ ## if even length
    end.pt = length(sim.vec)
  } else{
    end.pt = length(sim.vec)-1
  }
  
  res=vector(); k=1
  for(i in seq(1, end.pt, by= 2)){
    res[k] = mean(sim.vec[i:(i+1)])
    k=k+1
  }
  
  res.time = time.vec[ seq(1, end.pt, by= 2) ] + 0.25/24 # add 15 minutes (half of 1/2 hr)
  return( cbind(res.time, res))
}

## format swc data given the genotype and treatment
make.sd.mean.df = function(genotype, trt, swc){
  row.idx = intersect(which(swc$Treatment == trt), which(swc$Geno == genotype) ) 
  dat = swc[row.idx,]
  
  sd.mean = cbind( matrix(by(dat$SWC_adjust, dat$DOY, FUN = mean, na.rm=T)) , matrix(by(dat$SWC_adjust, dat$DOY, FUN = sd, na.rm=T)) )
  num=vector() ## number of observations
  for(i in 1:dim(sd.mean)[1]){
    num= c(num, length(which(dat$DOY == sort(unique(dat$DOY))[i] ) ) )
  }
  sd.mean = cbind(sd.mean, num, sort(unique(dat$DOY)))
  return( list(sd.mean, dat ) )
}

# add standard error to plot
add.se = function(x, avg, sdev, colr = "black"){
  arrows(x, avg-sdev, x, avg+sdev, length=0.05, angle=90, code=3, col= colr)
}

# add leaf area points, given a treatment
add.lfarea.pts = function(trtment, pch.in = 1){
  if(trtment == "D1"){
    dat = cc.d1.ela
    #clr= "orange"
  } 
  if (trtment == "D2"){
    dat = cc.d2.ela
    #clr = "red"
  }
  if (trtment == "WW"){
    dat = cc.ww.ela
    #clr="blue"
  }
  ## get total leaf area per individual 
  tmp= by( dat[, 6:14], dat$ID  , FUN=colSums, na=T)
  num.indiv = length(tmp)
  
  lf.all = matrix(nrow= num.indiv, ncol = 9)
  for(i in 1:num.indiv){
    lf.all[i,] = tmp[[i]]
    lf.all[i, (which(lf.all[i,] == 0)) ] = NA
  }
  
  mean.lf = colMeans(lf.all, na.rm=T)
  se.lf = apply(lf.all, 2, FUN= function(x){sd(x, na.rm=T)/sqrt(sum(!is.na(x))) })
  res.mat= cbind(cbind(days[1:9], mean.lf, se.lf))
  res.mat = res.mat[which(days<=136),]
  points(res.mat[,1], res.mat[,2], lwd=2, cex=2 , pch = pch.in)
  add.se(res.mat[,1], res.mat[,2], res.mat[,3])
}