#' plotfobj
#'
#' @param fobj a forest plot object
#'
#' @returns a plot
#'
#' @export
#'
#' @importFrom graphics abline rect
#' @importFrom stats aggregate
#'
#' @examples
#'
#' fobj<-genfobj(layout = c("t","t","t","t","t","t","f","t"), 
#' 	dat = forplotdata,
#'	lwidths = c(0.8,0.4,0.6,0.4,0.6,1,1,0.5))
#' plotfobj(fobj)
#'
plotfobj<- function(fobj) {

	ma<-lma(rows=1,cols=length(fobj$setup$layout),commonx1=TRUE,commonx2=TRUE)

	layout(ma,heights=fobj$setup$lheights,widths=c(0.01,fobj$setup$lwidths,0.01))

	par(mar=c(0,0,0,0))

	#items
	for (i in 1:length(fobj$setup$layout)) {

		if (fobj$setup$layout[i]=="t") {
			do.call(plot, fobj$items[[i]]$plot)
			do.call(text, fobj$items[[i]]$text)


			lapply(fobj$gridlines, function(x) do.call(abline, x))

			if (!is.null(fobj$stripes)) {
				fobj$stripes$xleft<-par("usr")[1]
				fobj$stripes$xright<-par("usr")[2]
				do.call(rect, fobj$stripes)
			}
		}

		if (fobj$setup$layout[i]=="f") {
			do.call(plot, fobj$items[[i]]$plot)
			do.call(axis, fobj$items[[i]]$axis)
			do.call(points, fobj$items[[i]]$points)
			do.call(mapply, c(FUN = arrows, fobj$items[[i]]$arrows))

			if (!is.null(fobj$items[[i]]$refline)) {
				do.call(lines, fobj$items[[i]]$refline)
			}

			if (!is.null(fobj$items[[i]]$direction)) {
				do.call(mtext, fobj$items[[i]]$direction)
			}

			lapply(fobj$gridlines, function(x) do.call(abline, x))

			if (!is.null(fobj$stripes)) {
				fobj$stripes$xleft<-par("usr")[1]
				fobj$stripes$xright<-par("usr")[2]
				do.call(rect, fobj$stripes)
			}
		}

		if (grepl("s\\d+",fobj$setup$layout[i])) {
			do.call(plot, fobj$items[[i]]$plot)
			do.call(axis, fobj$items[[i]]$axis)
			do.call(abline, fobj$items[[i]]$hline)

			nstrip<-as.numeric(substr(fobj$setup$layout[i],2,nchar(fobj$setup$layout[i])))

			for (nsi in 1:nstrip) {
				do.call(points, fobj$items[[i]][[paste0("points",nsi)]])
			}

			if (!is.null(fobj$items[[i]]$borders)) {
				lapply(fobj$items[[i]]$borders, function(x) do.call(abline, x))
			}

			lapply(fobj$gridlines, function(x) do.call(abline, x))

			if (!is.null(fobj$stripes)) {
				fobj$stripes$xleft<-par("usr")[1]
				fobj$stripes$xright<-par("usr")[2]
				do.call(rect, fobj$stripes)
			}
		}

		if (fobj$setup$layout[i]=="b") {
			do.call(plot, fobj$items[[i]]$plot)
			do.call(boxplot, fobj$items[[i]]$boxplot)
			do.call(axis, fobj$items[[i]]$axis)

			lapply(fobj$gridlines, function(x) do.call(abline, x))

			if (!is.null(fobj$stripes)) {
				fobj$stripes$xleft<-par("usr")[1]
				fobj$stripes$xright<-par("usr")[2]
				do.call(rect, fobj$stripes)
			}

		}
		
		if (fobj$setup$layout[i]=="d") {
			
			do.call(plot, fobj$items[[i]]$plot)
			do.call(axis, fobj$items[[i]]$axis)
			lapply(fobj$items[[i]]$lines, function(x) lapply(x, function(u) do.call(lines,u)))	
			
			lapply(fobj$gridlines, function(x) do.call(abline, x))
			
			if (!is.null(fobj$stripes)) {
				fobj$stripes$xleft<-par("usr")[1]
				fobj$stripes$xright<-par("usr")[2]
				do.call(rect, fobj$stripes)
			}		
		}
	}


	#footer
	plot(0,type="n",axes=FALSE,ylim=c(0,1),xlim=c(0,1),xlab="",ylab="")


	#header
	headernr<-length(fobj$header)

	if (headernr>0) {

		plot(x = 0, type="n", xlim=c(0,1), ylim=c(0,1), yaxt="n", ylab="", xlab="", axes=FALSE)

		xsize<-par("usr")[2]-par("usr")[1]
		rwidth<-fobj$setup$lwidths
		cwidth<-(xsize/sum(rwidth)*rwidth)

		hi<-1
		for (hi in 1:headernr) {

			if (is.null(fobj$header[[hi]]$text$x)) {

				wdf<-cbind(ind=fobj$header[[hi]]$hlayout,cwidth)
				wdf<-aggregate(cwidth ~ ind, data = wdf, FUN = sum)
				cwidthi<-wdf$cwidth

				if (length(cwidthi)!=length(fobj$header[[hi]]$text$labels)) {
					stop("Length of 'labels' must match number of distinct values in 'hlayout'")
				}

				lb<-c(par("usr")[1],par("usr")[1] + cumsum(cwidthi)[-length(cwidthi)])
				ub<-par("usr")[1] + cumsum(cwidthi)
				fobj$header[[hi]]$text$x<-(lb+ub)/2

			}

			do.call(text, fobj$header[[hi]]$text)
		}
	}

}

