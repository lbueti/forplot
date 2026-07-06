###################
# helper functions
###################


#-------------------------------------------------------------------
# Layout matrix for multipanel functions that can be used by layout()
#-------------------------------------------------------------------

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


#-----------------------
# Format numeric vector
#-----------------------


# Format numeric vector

ff<-function(x,dig=2) {
  formatC(x,format="f",digits=dig)
}

# Format point estimat with confidence interval

ff_ci<-function(est,dig=NULL,fs="pe (lci to uci)") {

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


#-----------------------
# Convert forest objects
#-----------------------

check_convert<-function(fobj, item, lay) {

	if (!("fobj" %in% class(fobj) )) {
		stop("fobj has to be of class fobj - use genfobj to define it.")
	}

	if (lay=="s") {
		if (!any((grepl("s\\d+",fobj$setup$layout)))) {
			stop("fobj must contain a ",lay," item.")
		}
	} else {
		if (!any(fobj$setup$layout==lay)) {
			stop("fobj must contain a ",lay," item.")
		}
	}

	if (is.null(item)) {

		if (lay=="s") {
			itemnr<-which((grepl("s\\d+",fobj$setup$layout)))
		} else {
			itemnr<-which(fobj$setup$layout==lay)
		}

	} else {
		if (!is.numeric(item)) {

			vnames<-unlist(lapply(fobj$items,function(x) x$vname))

			if (!all(item %in% vnames)) {
				stop("Item have to be numeric or match a columns name from 'dat'")
			}

			itemnr<-match(item,unlist(lapply(fobj$items,function(x) x$vname)))

		} else {
			itemnr<-item
		}
	}

	if (!all(substr(fobj$setup$layout[itemnr],1,1)==lay)) {
		stop("'item' must be a ",lay," item.")
	}

	return(itemnr)
}


#----------------------------
#header: helper to get header names
#----------------------------

headernames<-function(dat, layout) {

  colnr<-1:length(layout)
  repel<-rep(1,length(layout))
  
  sf<-which(layout %in% "f")
  repel[sf]<-3
  
  ss<-which(grepl("s\\d+",layout))
  nstrip<-as.numeric(substr(layout[ss],2,nchar(layout[ss])))
  repel[ss]<-nstrip
  
  sb<-which(layout=="b")
  repel[sb]<-1
  
  sd<-which(layout=="d")
  repel[sd]<-1
  
  lr<-rep(layout,repel)
  hl<-rep(colnr,repel)
  
  li<-length(lr[!lr %in% c("b","d")])
  
  if (li!=ncol(dat)) {
	stop(paste0("The 'layout' does not match 'dat' - layout implies ",
		li," columns, dat has " ,ncol(dat),"."))
  }
  
  if ((sum(sb) + sum(sd))>0) {
    cnb <- rep(NA, ncol(dat) + length(sb) + length(sd))
    cnb[- which(lr %in% c("b","d"))] <- colnames(dat)
    cnb[which(lr %in% "b")]<-"boxplot"
	cnb[which(lr %in% "d")]<-"density"
  } else {
    cnb<-colnames(dat)
  }
  
  h<-cnb[!duplicated(hl)] 
  
  return(h)
  
		
  #sf<-which(layoutm %in% "f")
  #for (sfi in sf) {
  #	if (sfi<length(colnr)){
  #	  colnr[(sfi+1):length(colnr)]<-colnr[(sfi+1):length(colnr)]+2
  #  }	
  #}

  #sf<-which(grepl("s\\d+",layoutm))
  #for (sfi in sf) {
  #  nstrip<-as.numeric(substr(layoutm[sfi],2,nchar(layoutm[sfi])))
  #  colnr[(sfi+1):length(colnr)]<-colnr[(sfi+1):length(colnr)] + (nstrip-1)
  #}

  #h<-colnames(dat[,colnr])

  #if (sum(layout=="b")>0) {
  #  bps<-which(layout=="b")
  #  for (i in 1:length(bps)) {
  #    bpi<-bps[i] + i -1
  #    h<-c(h[1:(bpi-1)],"boxplot",h[bpi:length(h)])
  #  }
  #}
  
  #return(h)
}


