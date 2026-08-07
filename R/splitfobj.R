
#' splitfobj
#'
#' Splits an fobj and allows to insert subtitles over the whole width of the forest plot
#'	Changes to fobj to an sfobj.
#'
#' @param fobj a forest plot object of class 'fobj'
#' @param atrows number of the row(s) at which the fibj should be split. The split is always before the indicated rows.
#' @param subtitle optional character vector with  subtitles, has to be of the same length as atrows
#' @param lheights Optional numeric vector with the relative heights of header, subtitles, main panels and footer.
#'	The length has to correspond to 3 + 2*number of inserted subtitles.
#' @param ... options to be passed to \code{\link[graphics]{text}}
#'
#' @returns a split forest plot object of class 'sfobj'
#'
#' @export
#'
#' @importFrom utils modifyList
#'
#' @examples
#'
#' fobj<-genfobj(layout = c("t","t","t","t","t","t","f","t"),
#' 	dat = forplotdata,
#'	lwidths = c(0.8,0.4,0.6,0.4,0.6,1,1,0.5))
#'	sfobj<-splitfobj(fobj,
#'		atrows=c(3, 5),
#'		subtitle=c("A first long title is added here",
#'			"A second even longer title is added here"))
#' plotfobj(fobj)
#'
splitfobj<-function(fobj, atrows, subtitle = NA, lheights = NA, ...)	{

	input<-list(...)
	
	if (any(atrows>nrow(fobj$dat))) {
		stop("atrows must not be larger than the number of rows of the data frame.")
	}
	
	#generate new layout matrix:
	#-----------------
	
	ma<-matrix(NA,nrow = 3 + 2*length(atrows) , ncol=length(fobj$setup$layout)+2)
	
	#header:
	ma[1,]<-1

	#inserts
	if (1 %in% atrows) {
		ma[2,]<-2
		atrowsm<-atrows[atrows!=1]
		ma<-ma[1:(nrow(ma)-1),]
	} else {
		atrowsm<-atrows
	}
	
	if (length(atrowsm)>0) {
		startp<-min(which(is.na(ma[,1])))
		inspos<-seq(startp+1,l=length(atrowsm),by=2)
		ma[inspos,]<-rep((1:length(atrowsm))+max(ma,na.rm=TRUE),ncol(ma))
	}
	
	fr<-1:((length(atrowsm)+1)*length(fobj$setup$layout)) + max(ma,na.rm=TRUE)
	#footer:
	ma[nrow(ma),]<-max(fr)+1
	#margins
	ma[,1]<-max(fr)+2
	ma[,ncol(ma)]<-max(fr)+3
	tma<-t(ma)
	tma[is.na(tma)]<-fr
	ma<-t(tma)

	#split data and y positions
	#-----------------
	
	splits <- cumsum(1:nrow(fobj$dat) %in% atrows)
	dlist <- split(fobj$dat, splits)
	y.atlist<-split(fobj$setup$y.at, splits)
	yspacelist<-lapply(y.atlist,function(x) ifelse(length(x)>1,abs(mean(diff(x)))/2,0.25))
	ylimlist<-lapply(1:length(y.atlist),function(x)
		c(min(y.atlist[[x]])-yspacelist[[x]], max(y.atlist[[x]])+yspacelist[[x]]))
	
	if  (sum(!is.na(lheights))==0) {
	
		lheights<-rep(NA,nrow(ma))
		lheights[1]<-fobj$setup$lheights[1]
		lheights[length(lheights)]<-fobj$setup$lheights[length(fobj$setup$lheights)]
		
		if (1 %in% atrows) {
			lheights[2]<-fobj$setup$lheights[1]
		}
		
		rheights<-length(dlist)/nrow(fobj$dat)*unlist(lapply(dlist,function(x) nrow(x)))
		nfact<-fobj$setup$lheights[2]/sum(rheights)	
		lheights[is.na(lheights)][seq(1,sum(is.na(lheights)), by = 2)]<-rheights*nfact
				
		lheights[is.na(lheights)]<-fobj$setup$lheights[1]

	} else {
		if (length(lheights)!=(3 + 2*length(atrowsm))) {
			stop("'lheights' must be a numeric vector of length 3 + 2*number of inserted subtitles
				(-1 if atrows includes 1).")
		}
	}
	
	#prepare output
	#-----------------
	if (all(is.na(subtitle))) {
		subtitle<-rep("",length(atrows))
	}
	
	insertrows<-vector(length=length(atrows), mode="list")
	for (ir in 1:length(atrows)) {

		insertrows[[ir]]<-
		list(plot = list(x = 0, type="n", xlim=c(0,1), ylim=c(0,1), yaxt="n", ylab="", xlab="", 
			axes=FALSE, xaxs = "i", yaxs = "i"),
		text = list(x = 0, y = 0.5, labels = subtitle[ir], adj = c(0,0.5)))

		insertrows[[ir]]$text<-modifyList(insertrows[[ir]]$text, input)
	}				
					
	sfobj<-list(
		setup = list(lmatrix=ma,lwidths=fobj$setup$lwidths,lheights=lheights, atrows=atrows),
		subtitle = insertrows,
		header = fobj$header,
		vector(mode="list",length=length(dlist)))

	names(sfobj)<-c("setup","subtitle","header","split_fobj")

	#generate split fobjs
	#-----------------
	ir<-1
	for (ir in 1:length(dlist)) {
		
		#adapt setup 
		fobji<-fobj
		fobji$dat<-dlist[[ir]]
		fobji$setup$y.at<-y.atlist[[ir]]
		fobji$setup$ylim<-ylimlist[[ir]]
		
		#for all text items: adapt y 
		i<-7
		j<-3
		z<-4
		za<-1
		for (i in 1:length(fobj$setup$layout)) {
			
			for (j in 1:length(fobji$items[[i]])) {
				
				itemsub<-fobji$items[[i]][[j]]
				
				if (!is.list(itemsub)) {
					if (length(itemsub)==ncol(fobj$dat)) {
						fobji$items[[i]][[j]]<-
							split(fobj$items[[i]][[j]], splits)[[ir]]
					} 
					if (names(fobji$items[[i]])[j]=="ylim") {
						fobji$items[[i]][[j]]<-ylimlist[[ir]]	
					}
				} else {
					for (z in 1:length(fobji$items[[i]][[j]])) {
						itemsub<-fobji$items[[i]][[j]][[z]]
						
						if (!is.list(itemsub)) {
						
							if (length(itemsub)==ncol(fobj$dat)) {
								fobji$items[[i]][[j]][[z]]<-
									split(fobj$items[[i]][[j]][[z]], splits)[[ir]]
							}
							if (names(fobji$items[[i]][[j]])[z]=="ylim") {
								fobji$items[[i]][[j]][[z]]<-ylimlist[[ir]]				
							}
						} else {
							for (za in 1:length(fobji$items[[i]][[j]][[z]])) {
								
								itemsub<-fobji$items[[i]][[j]][[z]][[za]]
								
								if (length(itemsub)==ncol(fobj$dat)) {
									fobji$items[[i]][[j]][[z]][[za]]<-
									split(fobj$items[[i]][[j]][[z]][[za]], splits)[[ir]]
								}
								if (names(fobji$items[[i]][[j]][[z]])[za]=="ylim") {
									fobji$items[[i]][[j]][[z]][[za]]<-ylimlist[[ir]]				
								}
							}	
						}
					}
					
				}
			}
		}	
		sfobj[["split_fobj"]][[ir]]<-fobji
		
	}

	class(sfobj) <- c("sfobj", class(sfobj))

	return(sfobj)

}


#Options for subtitles:
#---------------------

#' subtitle_text
#'
#' Modify subtitle text of a split forest plot object (sfobj).
#'	Passed to \code{\link[graphics]{text}}.
#'
#' @param sfobj a split forest plot object of class sfobj
#' @param snr subtitle to be modified
#' 	If NULL (the default), all subtitles are affected
#' @param ... options to be passed to \code{\link[graphics]{text}}
#'
#' @returns a split forest plot object of class 'sfobj'
#'
#' @export
#'
#' @importFrom utils modifyList
#'
#' @examples
#'
#' fobj<-genfobj(layout = c("t","t","t","t","t","t","f","t"),
#' 	dat = forplotdata,
#'	lwidths = c(0.8,0.4,0.6,0.4,0.6,1,1,0.5))
#'	sfobj<-splitfobj(fobj,
#'		atrows=c(3, 5),
#'		subtitle=c("A first long title is added here",
#'			"A second even longer title is added here"))
#'	sfobj<-subtitle_text(sfobj, col="red")
#' 	plotfobj(fobj)
#'
subtitle_text<-function(sfobj, snr = NULL, ...)	{
	
	if (!("sfobj" %in% class(sfobj) )) {
		stop("sfobj has to be of class sfobj - use genfobj and splitfobj to define it")
	}
	
	if (is.null(snr)) {
		snr<-1:length(sfobj$subtitle)
	}
	
	input<-list(...)

	for (itn in snr) {

		sfobj$subtitle[[itn]]$text<-modifyList(sfobj$subtitle[[itn]]$text, input)
	}

	return(sfobj)
}

#subtitle_stripes
#----------

#' subtitle_stripes
#'
#' Add or modify stripes for subtitle in a split forest plot object (sfobj).
#'	Passed to \code{\link[graphics]{rect}}.
#'
#' @param sfobj a split forest plot object of class 'sfobj'
#' @param snr subtitle to be modified
#' 	If NULL (the default), all subtitles are affected
#' @param ... options to be passed to \code{\link[graphics]{rect}}
#'
#' @returns a split forest plot object of class 'sfobj'
#'
#' @export
#'
#' @importFrom utils modifyList
#'
#' @examples
#'
#' fobj<-genfobj(layout = c("t","t","t","t","t","t","f","t"),
#' 	dat = forplotdata,
#'	lwidths = c(0.8,0.4,0.6,0.4,0.6,1,1,0.5))
#'	sfobj<-splitfobj(fobj,
#'		atrows=c(3, 5),
#'		subtitle=c("A first long title is added here",
#'			"A second even longer title is added here"))
#'	sfobj<-subtitle_stripes(sfobj)
#' 	plotfobj(fobj)
#'
subtitle_stripes<-function(sfobj, snr = NULL, ...)	{

	if (!("sfobj" %in% class(sfobj) )) {
		stop("sfobj has to be of class sfobj - use genfobj and splitfobj to define it")
	}
	
	if (is.null(snr)) {
		snr<-1:length(sfobj$subtitle)
	}
	
	input<-list(...)
	
	itn<-1
	for (itn in snr) {
		
		if (is.null(sfobj$subtitle[[itn]]$stripes)) {
			bds<-sfobj$subtitle[[itn]]$plot$ylim
			sfobj$subtitle[[itn]]$stripes<-
				list(xleft = NA, ybottom = bds[1], xright = NA, ytop = bds[2], 
					col = rgb(.1,.1,.1,.1), border = NA)	
		}
		
		sfobj$subtitle[[itn]]$stripes<-modifyList(sfobj$subtitle[[itn]]$stripes, input)
	}
	
	return(sfobj)
}

#subtitle_gridlines
#----------

#' subtitle_gridlines
#'
#' Add or modify gridlines for subtitles in a split forest plot object (sfobj).
#'	Passed to \code{\link[graphics]{abline}}.
#'
#' @param sfobj a split forest plot object of class 'sfobj'
#' @param snr subtitle to be modified
#' 	If NULL (the default), all subtitles are affected
#' @param ... options to be passed to \code{\link[graphics]{abline}}
#'
#' @returns a split forest plot object of class 'sfobj'
#'
#' @export
#'
#' @importFrom utils modifyList
#'
#' @examples
#'
#' fobj<-genfobj(layout = c("t","t","t","t","t","t","f","t"),
#' 	dat = forplotdata,
#'	lwidths = c(0.8,0.4,0.6,0.4,0.6,1,1,0.5))
#'	sfobj<-splitfobj(fobj,
#'		atrows=c(3, 5),
#'		subtitle=c("A first long title is added here",
#'			"A second even longer title is added here"))
#'	sfobj<-subtitle_gridlines(sfobj)
#' 	plotfobj(fobj)
#'
subtitle_gridlines<-function(sfobj, snr = NULL, ...)	{

	if (!("sfobj" %in% class(sfobj) )) {
		stop("sfobj has to be of class sfobj - use genfobj and splitfobj to define it")
	}
	
	if (is.null(snr)) {
		snr<-1:length(sfobj$subtitle)
	}
	
	input<-list(...)
	
	itn<-1
	for (itn in snr) {
		
		if (is.null(sfobj$subtitle[[itn]]$gridlines)) {
		
			bds<-sfobj$subtitle[[itn]]$plot$ylim
			
			sfobj$subtitle[[itn]]$gridlines<-
				list(h = rev(bds), xpd = TRUE)	
		
		}
		
		sfobj$subtitle[[itn]]$gridlines<-
			modifyList(sfobj$subtitle[[itn]]$gridlines, input)
	}
	
	return(sfobj)
}
