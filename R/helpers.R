# helper functions
###############

# Layout matrix for multipanel functions that can be used by layout()

lma<-function(rows,cols,commonx1=TRUE,commony1=TRUE,commonx2=FALSE,commony2=FALSE,byrow=TRUE) {

  ma1<-matrix(c(1:(cols*rows)),rows,cols,byrow=byrow)

  if (commonx1) {
    r1<-rep(max(ma1)+1,ncol(ma1))
  } else {
    r1<-seq(max(ma1)+1,by=1,l=ncol(ma1))
  }

  if (commonx2) {
    r2<-rep(max(r1)+1,ncol(ma1))
  } else {
    r2<-seq(max(r1)+1,by=1,l=ncol(ma1))
  }

  ma2<-rbind(r2,ma1,r1)

  if (commony1) {
    c1<-c(max(ma2)+1,rep(max(ma2)+2,nrow(ma2)-2),max(ma2)+3)
  } else {
    c1<-seq(max(ma2)+1,l=nrow(ma2))
  }

  if (commony2) {
    c2<-c(max(c1)+1,rep(max(c1)+2,nrow(ma2)-2),max(c1)+3)
  } else {
    c2<-seq(max(c1)+1,l=nrow(ma2))
  }

  ma3<-cbind(c1,ma2,c2)

  ma3
}

# Format numeric vector

ff<-function(x,dig=2) {
  formatC(x,format="f",digits=dig)
}

# Format point estimat with confidence interval

ff_ci<-function(est,dig=NULL,fs="pe (lci - uci)") {

  if (!is.na(sum(est))) {

    if (is.null(dig)) {
      dig1<-ifelse(est[1]<10,2,ifelse(est[1]<100,1,0))
      dig2<-ifelse(est[2]<10,2,ifelse(est[2]<100,1,0))
      dig3<-ifelse(est[3]<10,2,ifelse(est[3]<100,1,0))
    } else {
      dig1<-dig2<-dig3<-dig
    }

    fs<-sub("pe",ff(est[1],dig=dig1),fs)
    fs<-sub("lci",ff(est[2],dig=dig2),fs)
    fs<-sub("uci",ff(est[3],dig=dig3),fs)
    fs
  } else {
    fs<-NA
    fs
  }
}

