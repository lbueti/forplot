#************************************#
#* Function for forest plots
#* Author: Lukas Buetikofer
#* Date created: 03.05.2018
#* Last update: November 2020
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
#' @param xlab  position of labels for x-axis, derived from beta by default
#' @param xlab_text text at the labels, derived from beta by default
#' @param xlim limits for x-axis in forest plots, derived from beta by default
#' @param xlab_cex size of x-axis labels (not title)
#' @param xlab_line  position of x-axis label (not title)
#' @param tck x-axis tick length
#' @param shift_xaxis shift position of x-axis
#' @param xtitle x axis title and format,
#' list with x (xpos), y (ypos), textr and textl (text at the right/left side), cex (text size)
#' @param lwd line widths
#' @param pcol color if symbols are used, not active
#' @param lscale beta given on log scale, use exp to format beta, default is no
#' @param ps  points for plot, list with pch, cex, and col
#' @param header for table, either a character vector or a list with any of
#'  x (vector with xpos), y (single y-position), text (character vector with labels), cex (text size),
#'  the list can be >1 to define more than one header line
#' @param ref reference line, list with x (xposition), extend (extension on top), lty (line type), col (line color), lwd (line width)
#' @param bottomline lines at the bottom if not NA
#' @param headline  1 line for header if not NA, 2 lines if 2
#' @param headline_pos  vector with position of lower and upper headline (if applicable), default c(0,1)
#' @param beta2 if not NULL a second forest is generated, needs variables beta2, beta2_lci and beta2_uci in dat
#' @param xlab2 see xlab for 2nd forest
#' @param xlab_text2 see xlab_text for 2nd forest
#' @param xlim2 see xlim for 2nd forest
#' @param xtitle2 see xtitle for 2nd forest
#' @param ... options passed to ff_ci for formatting the effects (if beta_format not goven)
#'
#' @return forest plot
#'
#' @export
#'
#' @importFrom grDevices rgb
#' @importFrom graphics arrows layout axis lines mtext par points text
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
#' header<-c("","Group1\nN","Group0\nmean (sd)","Group2\nN","Group2\nmean (sd)",
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
#' fplot(dat=forplotdata,header=header3,lwidths=lwidths,lheights=lheights,
#'	ref=list(x=0,col=2,extend=2),
#'	xtitle=xtitle,xlim=c(-1,0.5),shift_xaxis=0.3,xlab_line=-0.8)
#'
#' # Lines at header and bottom
#'	fplot(dat=forplotdata,header=header,lwidths=lwidths,lheights=lheights,
#'  	ref=list(x=0,col=2,extend=2),
#'		xtitle=xtitle,xlim=c(-1,0.5),shift_xaxis=0.3,xlab_line=-0.8,
#'		headline=2,bottomline=1)
#'
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
                xlab=NA,xlab_text=NA,xlim=NA,xlab_cex=0.6,xlab_line=0,tck=-0.04,shift_xaxis=0,
                xtitle=NA,
                lwd=1,
                pcol=rgb(.1,.1,.1,.2),
                lscale=c("no","yes"),
                ps=NA,
                header=NA,
                ref=list(x=NA,extend=0,lty=2,col="grey50",lwd=lwd),
                bottomline=NA,headline=NA,headline_pos=c(0,1),
				beta2=NULL, xlab2=NA,xlab_text2=NA,xlim2=NA,xtitle2=NA,
				...) {

  #%%%%%%%%%%%
  #setup
  #%%%%%%%%%%%

  lscale<-match.arg(lscale)

  if (is.na(ncols)) {
    if (sum(grepl("beta_format",colnames(dat)))==0) {
      ncols<-ncol(dat)-1
    } else {
      ncols<-ncol(dat)-1-1
    }
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

  #pval
  np<-sum(regexpr("^p",colnames(dat))==1)

  #reference
  refopt<-c("x","extend","lty","col","lwd")
  refdef<-list(NA,0,2,"grey50",lwd)
  for (i in 1:length(refopt)) {
    if (is.null(ref[[refopt[i]]])) {
      ref[[refopt[i]]]<-refdef[[i]]
    }
  }

  #%%%%%%%%%%%
  #plot
  #######

  #layout
  #%%%%%%%%%%%

  ma<-lma(rows=nrows,cols=ncols,commonx1=TRUE,commonx2=TRUE)
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


  #beta text
  #%%%%%%%%%%%

  plot(0,type="n",xlim=c(0,1),ylim=ylim,yaxt="n",ylab="",xlab="",axes=FALSE)
  if (sum(grepl("beta_format",colnames(dat)))==0) {
    if (lscale=="yes") {
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
  #symbols(dat$beta,y.at,squares=1/dat$beta_se,add=TRUE,inches=0.15,bg=pcol,fg=NA)
  points(dat$beta,y.at,pch=ps[["pch"]],cex=ps[["cex"]],col=ps[["col"]])
  arrows(y0=y.at,y1=y.at,x0=dat$beta_lci,x1=dat$beta_uci,code=3,angle=90,length=0)

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

  #2nd beta:
  ##########

  #text:
  if (!is.null(beta2)) {
     plot(0,type="n",xlim=c(0,1),ylim=ylim,yaxt="n",ylab="",xlab="",axes=FALSE)
	  if (sum(grepl("beta_format2",colnames(dat)))==0) {
		if (lscale=="yes") {
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

    #beta2

    plot(0,type="n",xlim=xlim2,ylim=ylim,yaxt="n",ylab="",xlab="",axes=FALSE)
    points(dat$beta2,y.at,pch=ps[["pch"]],cex=ps[["cex"]],col=ps[["col"]])
    arrows(y0=y.at,y1=y.at,x0=dat$beta_lci2,x1=dat$beta_uci2,code=3,angle=90,length=0)

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
  }

  #p columns
  #%%%%%%%%%%%

  if (np!=0) {
    for (pi in 1:np) {
      plot(0,type="n",xlim=c(0,1),ylim=ylim,yaxt="n",ylab="",xlab="",axes=FALSE)
      if (np==1) {
        labs<-dat[,grepl("^p",colnames(dat))]
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
    ht[ht %in% c("beta","beta_lci")]<-c("beta (ci)","")
    headerd<-list(x=hpc,y=yline,text=ht,cex=1)

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
             adj=c(xadj,yadj),xpd=TRUE,cex=header[[nhd]][["cex"]])
      }
    }
  }
}


