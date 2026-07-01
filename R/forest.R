#************************************#
#* Function for forest plots
#* Author: Lukas Buetikofer
#* Date created: 2018-05-03
#* Last update: 2026-06-12
#* **********************************#

#' forest
#'
#' produces forest plots
#'
#' required input is a data frame with a column called vlabel, beta, beta_lci and beta_uci.
#'
#' @param dat data frame with variables called
#' vlabel (labels), nx with x=1,2,... (num or chr columns with strings, e.g. number of observations or descriptives),
#' beta, beta_lci, beta_uci (num columns with point estimates and confidence interval)
#' optionally beta_format (num or chr column with formatted text to be printed along forest, generated from beta if not given)
#' px with x=1,2,.. (num or chr columns with p-value(s))
#' propx with x=1,2,.. (num columns with proportion in the groups, generates simple scatter plot)
#' @param nrows number of rows, setting for layout, default is 1
#' @param ncols number of columns, setting for layout, typically derived from the data
#' @param nns number of n columns, derived from data if not given
#' @param lheights lheights: relative height of rows, length is typically 3 (header, data ,footer)
#' @param lwidths lwidths: relative width of columns, length correpsonds to columns in forest plus 2 (left and right margin)
#' @param y.at position of lines, 1:nrow(dat) by default
#' @param font font for variable vlabel, length has to correspond to rows of dat, default is rep(1,nrow(dat))
#' @param arrow logical, whether to use arrows if xlim does not include confidence interval limits
#' @param arrow_length length of the edges of the arrow head (in inches)
#' @param arrow_angle angle from the shaft of the arrow to the edge of the arrow head
#' @param cap_length length of the confidence interval cap, 0 if none
#' @param shift_label_col inset of labels in the first column, default is 0
#' @param center_label_col centering of label in first column
#' @param shift_textbeta_col inset of formatted effects (beta_format)
#' @param shift_ymax down-shift of maximal y-value, smaller space to top
#' @param shift_ymin  up-shift of minimal y-value, smaller space to bottom
#' @param xlim limits for x-axis in forest plots, derived from beta by default
#' @param xlab  position of labels for x-axis, derived from beta by default
#' @param xlab_text text at the labels, derived from beta by default
#' @param xlab_cex size of x-axis labels (not title)
#' @param xlab_line  position of x-axis label (not title)
#' @param plim limits for x-axis of the proportion scatterplot (if any), c(0,1) by default
#' @param plab position of labels for x-axis of the proportion scatterplot (if any), pretty(plim) by default
#' @param plab_text text at the labels, 100*plab by default (percentages from proportion)
#' @param tck x-axis tick length
#' @param shift_xaxis shift position of x-axis
#' @param xtitle x axis title and format,
#' list with x (xpos), y (ypos), textr and textl (text at the right/left side), cex (text size)
#' @param lwd line widths
#' @param pcol color if symbols are used, not active
#' @param lscale logicial, beta given on log scale, use exp to format beta, default is FALSE
#' @param ps  points for plot, list with pch, cex, and col
#' @param prps  points for proportion scatterplot, list with pch, cex, and col
#' @param header for table, either a character vector or a list with any of
#'  x (vector with xpos), y (single y-position), text (character vector with labels), cex (text size), col (text colour),
#'  the list can be >1 to define more than one header line
#' @param ref reference line, list with x (xposition), extend (extension on top), lty (line type), col (line color), lwd (line width)
#' @param bottomline line at the bottom if not NA
#' @param headline line for header if not NA, 1 for one line at the bottom, 2 for a line at the bottom and top
#' @param headline_pos vector with position of lower and upper headline (if applicable), default c(0,1)
#' @param sideline logical, whether to show lines at the side of the proportion scatterplot
#' @param bpdat Optional data frame in a long format used to generate boxplots, must have columns
#'	value (the outcome value), variable (the outcome variable, safest as a factor to preserve the order in the plot), and
#'	arm (the treatment arm, safest as a factor to preserve the order in the plot)
#' @param bpopt List of options to pass to boxplot, with elements "col" and "boxwex"
#' @param bplim limits for x-axis of the boxplot (if any), c(min,max) by default
#' @param bplab position of labels for x-axis of the boxplot (if any), pretty(bplim) by default
#' @param bplab_text text at the labels, bplab by default
#' @param beta2 if not NULL a second forest is generated, needs variables beta2, beta_lci2 and beta_uci2 in dat
#' @param xlab2 see xlab for 2nd forest
#' @param xlab_text2 see xlab_text for 2nd forest
#' @param xlim2 see xlim for 2nd forest
#' @param xtitle2 see xtitle for 2nd forest
#' @param ... options passed to ff_ci for formatting the effects (if beta_format not given)
#'
#' @return forest plot
#'
#' @export
#'
#' @importFrom grDevices rgb
#' @importFrom graphics arrows layout axis lines mtext par points text boxplot
#'
#' @examples
#' data(forplotdata)
#' # Minimal example
#'  fplot(dat=forplotdata[,c("vlabel","beta","beta_lci","beta_uci")])
#'
#' # Standard example
#' fplot(dat=forplotdata)
#'
#' # Set widths and heights
#' lwidths<-c(0.05,0.5,0.2,0.8,0.2,0.8,1.2,1.2,0.5,0.05)
#' lheights<-c(0.14,1,0.08)
#' fplot(dat=forplotdata,lwidths=lwidths,lheights=lheights)
#'
#'
#' # Include header:
#' header<-c("","Group1\nN","Group1\nmean (sd)","Group2\nN","Group2\nmean (sd)",
#'	"Mean difference\n95% CI","","P-value")
#' fplot(dat=forplotdata,lwidths=lwidths,lheights=lheights,header=header)
#'
#' header<-list(list(y=0.7,
#' 	text=c("Group1","Group2","Mean difference (95% CI)","P-value"),
#' 	x=c(0.10,0.32,0.7,0.98)),
#' 	list(y=0.3,text=c("N","mean (sd)","N","mean (sd)"),
#' 	x=c(0.07,0.18,0.28,0.38)))
#' fplot(dat=forplotdata,header=header,lwidths=lwidths,lheights=lheights)
#'
#' # Reference line and xlim
#' xtitle<-list(x=0.86,y=0.2,textl="Group 1 better  ",textr="  Group 2 better")
#' fplot(dat=forplotdata,header=header,lwidths=lwidths,lheights=lheights,
#'	xtitle=xtitle,ref=list(x=0),xlim=c(-1,0.5))
#'
#' # Shift x-axis, labels and title
#' xtitle<-list(x=0.86,y=0.6,textl="Group 1 better  ",textr="  Group 2 better")
#' fplot(dat=forplotdata,header=header,lwidths=lwidths,lheights=lheights,
#'	ref=list(x=0,col=2,extend=2),
#'	xtitle=xtitle,xlim=c(-1,0.5),shift_xaxis=0.3,xlab_line=-0.8)
#'
#' # Lines at header and bottom
#'	fplot(dat=forplotdata,header=header,lwidths=lwidths,lheights=lheights,
#'  	ref=list(x=0,col=2,extend=2),
#'		xtitle=xtitle,xlim=c(-1,0.5),shift_xaxis=0.3,xlab_line=-0.8,
#'		headline=2,bottomline=1)
#'
#' #Add boxplot for continuous outcomes
#' lwidths<-c(0.05,0.5,0.3,0.6,0.3,0.6,0.8,1.2,1.2,0.5,0.05)
#' lheights<-c(0.08,1,0.04)
#'
#' header<-list(list(y=0.7,
#' 	text=c("Group1","Group2","Mean difference (95% CI)","P-value"),
#' 	x=c(0.10,0.3,0.7,0.98),col=c("red","blue","black","black")),
#' 	list(y=0.3,text=c("N","mean (sd)","N","mean (sd)"),
#' 	x=c(0.07,0.15,0.25,0.32)))
#'
#' xtitle<-list(x=0.88,y=0.3,textl="Group 1 better  ",textr="  Group 2 better")
#'
#' fplot(dat=forplotdata,header=header,lwidths=lwidths,lheights=lheights,
#'   	ref=list(x=0,col=2,extend=2),
#' 	xtitle=xtitle,xlim=c(-1,0.5),shift_ymin=0.5, shift_xaxis=0.5,xlab_line=-1,
#' 	headline=2,bottomline=1,
#' 	bpdat=forplotdata_bp)
#'
#' #Add a scatterplot for proportions
#' data(forplotdata)
#' lwidths<-c(0.05,0.5,0.2,0.6,0.2,0.6,1.0,1.2,1.0,0.5,0.05)
#' lheights<-c(0.08,1,0.04)
#' header<-list(list(y=0.7,
#' 	text=c("Group1","Group2","Proportions (%)","Risk difference in %","P-value"),
#' 	x=c(0.10,0.25,0.45,0.7,0.98)),
#' 	list(y=0.3,text=c("N","n (%)","N","n (%)","(1: red, 2: blue)", "(95% CI)"),
#' 	x=c(0.07,0.15,0.22,0.30,0.45,0.7)))
#'  xtitle<-list(x=0.83,y=0.4,textl="Group 1 better  ",textr="  Group 2 better")
#'
#' prps<-list(list(pch=16,cex=1.5,col=rgb(1,0,0,0.5)),
#'             list(pch=16,cex=1.5,col=rgb(0,0,1,0.5)))
#'
#' fplot(dat=forplotdata_prop,lwidths=lwidths,lheights=lheights,
#' 	header=header,
#' 	prps=prps,
#' 	xtitle=xtitle,ref=list(x=0),
#' 	xlab_line=-1.0,xlab=c(-0.2,0,0.2,0.4), xlab_text=c(-20,0,20,40),
#' 	headline=2,bottomline=1,sideline=TRUE,
#' 	shift_ymin=0.5, shift_xaxis=0.5)
#'
fplot<-function(dat,
                nrows=1,ncols=NA,
                nns=NA,
                lheights=c(0.1,1,0.05),lwidths=NA,
                y.at=NA,font=NA,
				arrow=TRUE,arrow_length=0.05,arrow_angle=30,
				cap_length=0,
                shift_label_col=0,center_label_col=NA,
                shift_textbeta_col=0,
                shift_ymax=0,shift_ymin=0,
                xlim=NA,xlab=NA,xlab_text=NA,
				plim=NA,plab=NA,plab_text=NA,
				xlab_cex=0.6,xlab_line=0,tck=-0.04,shift_xaxis=0,
                xtitle=NA,
                lwd=1,
                pcol=rgb(.1,.1,.1,.2),
                lscale=FALSE,
                ps=NA,
				prps=NA,
                header=NA,
                ref=list(x=NA,extend=0,lty=2,col="grey50",lwd=lwd),
                bottomline=NA,headline=NA,headline_pos=c(0,1),sideline=FALSE,
				bpdat=NULL,bpopt=NA,bplim=NA,bplab=NA,bplab_text=NA,
				beta2=NULL, xlab2=NA,xlab_text2=NA,xlim2=NA,xtitle2=NA,
				...) {

  #%%%%%%%%%%%
  #setup
  #%%%%%%%%%%%

  if (is.na(ncols)) {
    if (sum(grepl("beta_format",colnames(dat)))==0) {
      ncols<-ncol(dat)-1
    } else {
      ncols<-ncol(dat)-1-1
    }
  }
  nprops<-sum(regexpr("^prop",colnames(dat))==1)
  if (nprops>0) {
	ncols<-ncols-(nprops-1)
  }
  if (!is.null(beta2)) {
    if (sum(grepl("beta_format2",colnames(dat)))==0) {
      ncols<-ncols - 1
    } else {
      ncols<-ncols - 1-1
    }
  }

  if (!is.null(bpdat)) {
    ncols<-ncols+1
  }

  if (is.na(nns)) {
    nns<-sum(regexpr("^n",colnames(dat))==1)
  }

  if (sum(!is.na(lwidths))==0) {
    lwidths<-c(0.1,rep(1,ncols),0.1)
  }

  #font of labels
  if  (sum(!is.na(font))==0) {
    font<-rep(1,nrow(dat))
  } else {
    stopifnot(length(font)==nrow(dat))
  }

  #scales
  if  (sum(!is.na(y.at))==0) {
    y.at<-1:nrow(dat)
  } else {
    stopifnot(length(y.at)==nrow(dat))
  }

  ylim<-c(0+shift_ymin,max(y.at)-shift_ymax)

  if (sum(!is.na(xlim))==0) {
    xlim<-c(min(dat$beta_lci,na.rm=TRUE),max(dat$beta_uci,na.rm=TRUE))
  }
  if (sum(!is.na(xlab))==0) {
    xlab<-pretty(xlim)
  }
  xlim<-c(min(xlab,xlim),max(xlab,xlim))

  if (!is.null(beta2)) {
    if (sum(!is.na(xlim2))==0) {
  	  xlim2<-c(min(dat$beta_lci2,na.rm=TRUE),max(dat$beta_uci2,na.rm=TRUE))
    }
    if (sum(!is.na(xlab2))==0) {
  	  xlab2<-pretty(xlim2)
    }
    xlim2<-c(min(xlab2,xlim2),max(xlab2,xlim2))
  }

  #points
  if (sum(!is.na(ps))==0) {
    ps<-list(pch=15,cex=1,col=1)
  }

 #points props
  nprops<-sum(regexpr("^prop",colnames(dat))==1)
  if (nprops>0) {
	  if (sum(!is.na(prps))==0) {
		prps<-vector(length=nprops,mode="list")
		for (i in 1:nprops) {
			prps[[i]]<-list(pch=1,cex=1,col=i)
		}
	  }
  }

  #pval
  np<-sum(regexpr("^p",colnames(dat))==1 &  regexpr("^prop",colnames(dat))==(-1))

  #reference
  refopt<-c("x","extend","lty","col","lwd")
  refdef<-list(NA,0,2,"grey50",lwd)
  for (i in 1:length(refopt)) {
    if (is.null(ref[[refopt[i]]])) {
      ref[[refopt[i]]]<-refdef[[i]]
    }
  }

  #standard column ordering
  seq<-colnames(dat)
  npl<-which(seq=="vlabel")
  if (nns!=0) {
	for (ni in 1:nns) {
		npl<-c(npl,which(seq==paste0("n",ni)))
	}
  }
  #boxplot, after n by default
  if (!is.null(bpdat)) {
	npl<-c(npl,max(npl+0.5))
  }

  if (nprops!=0) {
	npl<-c(npl,min(which(grepl("^prop",seq))))
  }
  if (sum(grepl("beta_format",colnames(dat)))==0) {
    npl<-c(npl,which(seq=="beta"))
    npl<-c(npl,npl[length(npl)]+1)
  } else {
	npl<-c(npl,which(seq=="beta"))
	npl<-c(npl,which(seq=="beta_format"))
  }
  if (!is.null(beta2)) {
	if (sum(grepl("beta_format2",colnames(dat)))==0) {
		npl<-c(npl,which(seq=="beta2"))
		npl<-c(npl,npl[length(npl)]+1)
	} else {
		npl<-c(npl,which(seq=="beta2"))
		npl<-c(npl,which(seq=="beta_format2"))
	}
  }
  if (np!=0) {
	for (pi in 1:np) {
		npl<-c(npl,which(seq==paste0("p",pi)))
	}
  }
  npl2<-(1:ncols)[order(npl)]

  #%%%%%%%%%%%
  #plot
  #######

  #layout
  #%%%%%%%%%%%

  ma<-lma(rows=nrows,cols=ncols,commonx1=TRUE,commonx2=TRUE)
  if (nrows==1) {
	ma[2,2:(ncols+1)]<-npl2
  }

  layout(ma,heights=lheights,widths=lwidths)

  par(mar=c(0,0,0,0))
  par(lwd=lwd)

  #reverse
  dat<-dat[nrow(dat):1,]

  #row names
  plot(0,type="n",xlim=c(0,1),ylim=ylim,yaxt="n",ylab="",xlab="",axes=FALSE)
  if (is.na(center_label_col)) {
    text(dat$vlabel,x=par("usr")[1]+shift_label_col,y=y.at,adj=c(0,0.5),font=rev(font))
  } else {
    text(dat$vlabel,x=0.5+shift_label_col,y=y.at,adj=c(0.5,0.5),font=rev(font))
  }

  if (!is.na(bottomline)) {
    lines(x=c(par("usr")[1],par("usr")[2]),y=c(shift_xaxis,shift_xaxis),xpd=TRUE)
  }


  #n columns
  #%%%%%%%%%%%

  if (nns!=0) {
    for (ni in 1:nns) {
      plot(0,type="n",xlim=c(0,1),ylim=ylim,yaxt="n",ylab="",xlab="",axes=FALSE)
      if (nns==1) {
        labs<-dat[,grepl("^n",colnames(dat))]
      } else {
        labs<-dat[,paste0("n",ni)]
      }
      text(labs,x=0.5,y=y.at,adj=c(0.5,0.5))
      if (!is.na(bottomline)) {
        lines(x=c(par("usr")[1],par("usr")[2]),y=c(shift_xaxis,shift_xaxis),xpd=TRUE)
      }
    }
  }

  #prop column
  #############

  if (nprops!=0) {
    if (sum(!is.na(plim))==0) {
		pmi<-min(0,min(dat[,grepl("^prop",colnames(dat))]))
		pma<-max(1,max(dat[,grepl("^prop",colnames(dat))]))
		plim<-c(pmi,pma)
	}
	if (sum(!is.na(plab))==0) {
	  plab<-pretty(plim)
	}

	plot(0,type="n",xlim=plim,ylim=ylim,yaxt="n",ylab="",xlab="",axes=FALSE)
	clabs<-numeric(0)
	for (pi in 1:nprops) {
		if (nprops==1) {
			labs<-dat[,grepl("^prop",colnames(dat))]
		  } else {
			labs<-dat[,paste0("prop",pi)]
		  }
		clabs<-cbind(clabs,labs)
		points(labs,y.at,pch=prps[[pi]][["pch"]],cex=prps[[pi]][["cex"]],col=prps[[pi]][["col"]])
	}
	for (i in 1:length(y.at)) {
		if (!all(is.na(data.frame(clabs)[i,]))) {
			lines(x=plim,y=c(y.at[i],y.at[i]),lty=3)
		}
	}

    #axis
    if (sum(!is.na(plab_text))==0) {
      plab_text<-100*plab
    }
    axis(side=1,pos=shift_xaxis,at=plab,labels=rep("",length(plab)),las=1,tick=TRUE,tck=tck,lwd=lwd)
    mtext(side=1,line=xlab_line,at=plab,text=plab_text,cex=xlab_cex)

    if (!is.na(bottomline)) {
      lines(x=c(par("usr")[1],par("usr")[2]),y=c(shift_xaxis,shift_xaxis),xpd=TRUE)
    }

    #lines
    if (sideline) {
	  lines(x=c(plim[1],plim[1]),y=c(shift_xaxis,par("usr")[4]+shift_ymax),xpd=FALSE,col=1)
	  lines(x=c(plim[2],plim[2]),y=c(shift_xaxis,par("usr")[4]+shift_ymax),xpd=FALSE,col=1)
    }
  }

   #boxplot
  #############

  if (!is.null(bpdat)) {
	if (sum(!is.na(bplim))==0) {
	  bplim<-c(min(bpdat$value),max(bpdat$value))
	}
	if (sum(!is.na(bplab))==0) {
	  bplab<-pretty(bplim)
	  bplab<-bplab[bplab>bplim[1] & bplab<bplim[2]]
	}
	pm<-0.2
	gap<-0.1
	bwidth <- 2*pm - gap

	bp.at<-sort(c(y.at-pm,y.at+pm))

	bpoptused<-list(NA,length=2)
	names(bpoptused)<-c("col","boxwex")
	bpoptused[["col"]]<-c(rgb(1,0,0,0.3),rgb(0,0,1,0.3))
	bpoptused[["boxwex"]]<-bwidth

	if (sum(!is.na(bpopt))>0) {
		if (!is.null(bpopt[["col"]])) {
			bpoptused[["col"]]<-bpopt[["col"]]
		}
		if (!is.null(bpopt[["boxwex"]])) {
			bpoptused[["boxwex"]]<-bpopt[["boxwex"]]
		}
	}

	boxplot(value ~ rev(arm)*variable, data=bpdat,at=bp.at,boxwex = bpoptused[["boxwex"]], horizontal=TRUE,
		ylim=bplim,xlim=ylim,yaxt="n",ylab="",xlab="",axes=FALSE, col=rev(bpoptused[["col"]]))

	 #axis
	if (sum(!is.na(bplab_text))==0) {
	  bplab_text<-bplab
	}
	axis(side=1,pos=shift_xaxis,at=bplab,labels=rep("",length(bplab)),las=1,tick=TRUE,tck=tck,lwd=lwd)
	mtext(side=1,line=xlab_line,at=bplab,text=bplab_text,cex=xlab_cex)

	if (!is.na(bottomline)) {
	  lines(x=c(par("usr")[1],par("usr")[2]),y=c(shift_xaxis,shift_xaxis),xpd=TRUE)
	}

	#lines
	if (sideline) {
	  lines(x=c(bplim[1],bplim[1]),y=c(shift_xaxis,par("usr")[4]+shift_ymax),xpd=FALSE,col=1)
	  lines(x=c(bplim[2],bplim[2]),y=c(shift_xaxis,par("usr")[4]+shift_ymax),xpd=FALSE,col=1)
	}
  }



  #beta text
  #%%%%%%%%%%%

  plot(0,type="n",xlim=c(0,1),ylim=ylim,yaxt="n",ylab="",xlab="",axes=FALSE)
  if (sum(grepl("beta_format",colnames(dat)))==0) {
    if (lscale) {
      risk<-apply(dat[,c("beta","beta_lci","beta_uci")],1,function(x) ff_ci(exp(x),...))
    } else {
      risk<-apply(dat[,c("beta","beta_lci","beta_uci")],1,function(x) ff_ci(x,...))
    }
    risk[is.na(dat$beta)]<-""
  } else {
    risk<-dat$beta_format
  }
  text(risk,x=0.5+shift_textbeta_col,y=y.at,adj=c(0.5,0.5))

  if (!is.na(bottomline)) {
    lines(x=c(par("usr")[1],par("usr")[2]),y=c(shift_xaxis,shift_xaxis),xpd=TRUE)
  }


  #beta
  #%%%%%%%%%%%

  plot(0,type="n",xlim=xlim,ylim=ylim,yaxt="n",ylab="",xlab="",axes=FALSE)

  #lines and axis
  if (!is.na(ref[["x"]])) {
    lines(x=rep(ref[["x"]],2),y=c(ylim[1]-shift_ymin+shift_xaxis,ylim[2]+shift_ymax+ref[["extend"]]),
          lty=ref[["lty"]],col=ref[["col"]],lwd=ref[["lwd"]])
  }
  if (sum(!is.na(xlab_text))==0) {
    xlab_text<-xlab
  }
  axis(side=1,pos=shift_xaxis,at=xlab,labels=rep("",length(xlab)),las=1,tick=TRUE,tck=tck,lwd=lwd)
  mtext(side=1,line=xlab_line,at=xlab,text=xlab_text,cex=xlab_cex)

  if (!is.na(bottomline)) {
    lines(x=c(par("usr")[1],par("usr")[2]),y=c(shift_xaxis,shift_xaxis),xpd=TRUE)
  }

  #points and arrows
  #symbols(dat$beta,y.at,squares=1/dat$beta_se,add=TRUE,inches=0.15,bg=pcol,fg=NA)
  points(dat$beta,y.at,pch=ps[["pch"]],cex=ps[["cex"]],col=ps[["col"]])

  #arrows and caps
  sel<-dat$beta_lci<min(xlim) | dat$beta_uci>max(xlim)
  sel<-sel & !is.na(sel)
  if (any(sel) & arrow==TRUE) {
	dats<-dat[sel,]
	for (li in 1:nrow(dats)) {
		if (dats$beta_lci[li]<min(xlim)) {
			code<-1
			dats$beta_lci[li]<-min(xlim)
		}
		if (dats$beta_uci[li]>max(xlim)) {
			code<-2
			dats$beta_uci[li]<-max(xlim)
		}
		if (dats$beta_lci[li]<min(xlim) & dats$beta_uci[li]>max(xlim)) {
			code<-3
		}
		arrows(y0=y.at[sel][li],y1=y.at[sel][li],x0=dats$beta_lci[li],x1=dats$beta_uci[li],
			code=code,angle=arrow_angle,length=arrow_length)
		if (cap_length>0 & code!=3) {
			code<-ifelse(code==1,2,1)
			arrows(y0=y.at[sel][li],y1=y.at[sel][li],x0=dats$beta_lci[li],x1=dats$beta_uci[li],
				code=code,angle=90,length=cap_length)
		}
	}
	arrows(y0=y.at[!sel],y1=y.at[!sel],x0=dat$beta_lci[!sel],x1=dat$beta_uci[!sel],code=3,angle=90,length=cap_length)
  } else {
	arrows(y0=y.at,y1=y.at,x0=dat$beta_lci,x1=dat$beta_uci,code=3,angle=90,length=cap_length)
  }



  #2nd beta:
  ##########
  if (!is.null(beta2)) {

	#2nd beta text:
	##########

     plot(0,type="n",xlim=c(0,1),ylim=ylim,yaxt="n",ylab="",xlab="",axes=FALSE)
	  if (sum(grepl("beta_format2",colnames(dat)))==0) {
		if (lscale) {
		  risk<-apply(dat[,c("beta2","beta_lci2","beta_uci2")],1,function(x) ff_ci(exp(x),...))
		} else {
		  risk<-apply(dat[,c("beta2","beta_lci2","beta_uci2")],1,function(x) ff_ci(x,...))
		}
		risk[is.na(dat$beta2)]<-""
	  } else {
		risk<-dat$beta_format2
	  }
	  text(risk,x=0.5+shift_textbeta_col,y=y.at,adj=c(0.5,0.5))

	  if (!is.na(bottomline)) {
		lines(x=c(par("usr")[1],par("usr")[2]),y=c(shift_xaxis,shift_xaxis),xpd=TRUE)
	  }

    #2nd beta
	#######
    plot(0,type="n",xlim=xlim2,ylim=ylim,yaxt="n",ylab="",xlab="",axes=FALSE)

	#lines and axis
    if (!is.na(ref[["x"]])) {
      lines(x=rep(ref[["x"]],2),y=c(ylim[1]-shift_ymin+shift_xaxis,ylim[2]+shift_ymax+ref[["extend"]]),
            lty=ref[["lty"]],col=ref[["col"]],lwd=ref[["lwd"]])
    }
    if (sum(!is.na(xlab_text2))==0) {
      xlab_text2<-xlab2
    }
    axis(side=1,pos=shift_xaxis,at=xlab2,labels=rep("",length(xlab2)),las=1,tick=TRUE,tck=tck,lwd=lwd)
    mtext(side=1,line=xlab_line,at=xlab2,text=xlab_text2,cex=xlab_cex)

    if (!is.na(bottomline)) {
      lines(x=c(par("usr")[1],par("usr")[2]),y=c(shift_xaxis,shift_xaxis),xpd=TRUE)
    }

	#points and arrows
    points(dat$beta2,y.at,pch=ps[["pch"]],cex=ps[["cex"]],col=ps[["col"]])

    #arrows and caps
    sel<-dat$beta_lci2<min(xlim2) | dat$beta_uci2>max(xlim2)
    sel<-sel & !is.na(sel)
    if (any(sel) & arrow==TRUE) {
	  dats<-dat[sel,]
	  for (li in 1:nrow(dats)) {
	  	if (dats$beta_lci2[li]<min(xlim2)) {
	  		code<-1
	  		dats$beta_lci2[li]<-min(xlim2)
	  	}
	  	if (dats$beta_uci2[li]>max(xlim2)) {
	  		code<-2
	  		dats$beta_uci2[li]<-max(xlim2)
	  	}
	  	if (dats$beta_lci2[li]<min(xlim2) & dats$beta_uci2[li]>max(xlim2)) {
	  		code<-3
	  	}
	  	arrows(y0=y.at[sel][li],y1=y.at[sel][li],x0=dats$beta_lci2[li],x1=dats$beta_uci2[li],
	  		code=code,angle=arrow_angle,length=arrow_length)
	  	if (cap_length>0 & code!=3) {
	  		code<-ifelse(code==1,2,1)
	  		arrows(y0=y.at[sel][li],y1=y.at[sel][li],x0=dats$beta_lci2[li],x1=dats$beta_uci2[li],
	  			code=code,angle=90,length=cap_length)
	  	}
	  }
	  arrows(y0=y.at[!sel],y1=y.at[!sel],x0=dat$beta_lci2[!sel],x1=dat$beta_uci2[!sel],code=3,angle=90,length=cap_length)
    } else {
	  arrows(y0=y.at,y1=y.at,x0=dat$beta_lci2,x1=dat$beta_uci2,code=3,angle=90,length=cap_length)
    }


  }

  #p columns
  #%%%%%%%%%%%

  if (np!=0) {
    for (pi in 1:np) {
      plot(0,type="n",xlim=c(0,1),ylim=ylim,yaxt="n",ylab="",xlab="",axes=FALSE)
      if (np==1) {
        labs<-dat[,grepl("^p",colnames(dat)) & !grepl("^prop",colnames(dat))]
      } else {
        labs<-dat[,paste0("p",pi)]
      }
      text(labs,x=0.5,y=y.at,adj=c(0.5,0.5))
      if (!is.na(bottomline)) {
        lines(x=c(par("usr")[1],par("usr")[2]),y=c(shift_xaxis,shift_xaxis),xpd=TRUE)
      }
    }
  }

  #axis text
  #%%%%%%%%%%%

  plot(0,type="n",axes=FALSE,ylim=c(0,1),xlim=c(0,1))
  if (sum(!is.na(xtitle))>0) {
    text(x=xtitle[["x"]],y=xtitle[["y"]],xtitle[["textr"]],adj=c(0,0.5),xpd=TRUE,cex=xtitle[["cex"]])
    text(x=xtitle[["x"]],y=xtitle[["y"]],xtitle[["textl"]],adj=c(1,0.5),xpd=TRUE,cex=xtitle[["cex"]])
  }
  if (sum(!is.na(xtitle2))>0) {
    text(x=xtitle2[["x"]],y=xtitle2[["y"]],xtitle2[["textr"]],adj=c(0,0.5),xpd=TRUE,cex=xtitle2[["cex"]])
    text(x=xtitle2[["x"]],y=xtitle2[["y"]],xtitle2[["textl"]],adj=c(1,0.5),xpd=TRUE,cex=xtitle2[["cex"]])
  }

  #header
  #%%%%%%%%%%%

  yline<-0.5
  yadj<-0.5

  if (!is.null(header)) {

    #defaults
    xsize<-par("usr")[2]-par("usr")[1]
    lws<-lwidths[2:(length(lwidths)-1)]
    hpc<-numeric(0)
    for (li in 0:(length(lws)-1)) {
      if (li==0) {
        hpi<- (lws[1]/2)/sum(lws)
        hpi<-hpi*xsize+par("usr")[1]
        if (is.na(center_label_col)) {
          hpi<-par("usr")[1]
        } else {
        }
      } else {
        hpi<-(sum(lws[1:li]) + lws[(li+1)]/2)/sum(lws)
        hpi<-hpi*xsize + par("usr")[1]
      }
      hpc<-c(hpc,hpi)
    }

    ht<-colnames(dat)[!(colnames(dat) %in% c("beta_uci","beta_format"))]
	ht<-sub("prop1","proprtions",ht)
	ht<-ht[!grepl("^prop[0-9]",ht)]
    ht[ht %in% c("beta","beta_lci")]<-c("beta (ci)","")
    if (!is.null(beta2)) {
      ht<-ht[!(ht %in% c("beta_uci2","beta_format2"))]
      ht[ht %in% c("beta2","beta_lci2")]<-c("beta2 (ci)","")
    }

    headerd<-list(x=hpc,y=yline,text=ht,cex=1,col=NULL)

    #add defaults if missing
    if (sum(!is.na(header))>0) {
      if (!is.list(header)) {
        headerd[["text"]]<-header
        header<-list(headerd)
		nhead<-1
      } else {
        if (!is.list(header[[1]])) {
          header<-list(header)
        }
        nhead<-length(header)
        for (nhd in 1:nhead) {
          for (hel in names(headerd)) {
            if (is.null(header[[nhd]][[hel]])) {
              header[[nhd]][[hel]]<-headerd[[hel]]
            }
          }
        }
      }
    } else {
      header<-list(headerd)
      nhead<-1
    }

    #lines
    plot(0,type="n",axes=FALSE,ylim=c(0,1),xlim=c(0,1))
    if (!is.na(headline)) {
      lines(x=c(par("usr")[1],par("usr")[2]),y=c(headline_pos[1],headline_pos[1]),xpd=TRUE)
      if (headline==2) {
        lines(x=c(par("usr")[1],par("usr")[2]),y=c(headline_pos[2],headline_pos[2]),xpd=TRUE)
      }
    }

    #text
    #points(x=hpc,y=rep(0.5,length(hpc)))
    #lines(x=c(par("usr")[2],par("usr")[1]),y=c(0.5,0.5))

    for (nhd in 1:nhead) {
      for (i in 1:length(header[[nhd]][["text"]])) {
        if (is.na(center_label_col) & i==1) {
          xadj<-0
        } else {
          xadj<-0.5
        }
        text(x=header[[nhd]][["x"]][i],y=header[[nhd]][["y"]],header[[nhd]][["text"]][i],
             adj=c(xadj,yadj),xpd=TRUE,cex=header[[nhd]][["cex"]],col=header[[nhd]][["col"]][i])
      }
    }
  }
}
