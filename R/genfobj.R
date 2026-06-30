
#' genfobj
#'
#' First step to generate a forest plot. Generates a fobj that can be plotted.
#'
#' @param layout layout of the plot, character vector with 't' (text), 'f' (forest), 's' (strip), or 'b' (boxplot).
#' @param dat data frame that should be plotted
#' @param obs Optional data frame with the observations for the boxplot.
#'	Required of layout includes 'b'.
#' @param lwidths Optional numeric vector with the relative widths of the columns. Must have the same length as layout.
#' @param lheights Optional numeric vectotr of length 3 with the relative heights of header, main panel and footer.
#' @param y.at Optional numeric vector with the position of the rows. Usually not required.
#' @param ylim Optional limits of the rows. Usually not required.
#'
#' @returns a forest plot object of class 'fobj'
#'
#' @export
#'
#' @examples
#'
#' fobj<-genfobj(layout = c("t","t","t","t","t","t","f","t"),
#' 	dat = forplotdata,
#'	lwidths = c(0.8,0.4,0.6,0.4,0.6,1,1,0.5))
#' plotfobj(fobj)
#'
genfobj<-function(layout, dat, obs = NULL,
	lwidths = NA, lheights = NA,
	y.at = NA, ylim = NA) {

	if (sum(!is.na(y.at))==0) {
		y.at<-nrow(dat):1
	} else {
		if (length(y.at)!=nrow(dat)) {
			warning("Length of y.at does not match number of rows of dat, default used")
			y.at<-1:nrow(dat)
		}
	}

	if (sum(!is.na(ylim))==0) {
		yspace<-diff(range(y.at))/20

		ylim<-c(min(y.at)-yspace, max(y.at)+yspace)
	}

	if  (sum(!is.na(lheights))==0) {
		hf<-(3 * nrow(dat)) / (10 * nrow(dat) + 200)
		lheights<-c(hf,1,hf)
	} else {
		if (length(lheights)!=length(layout)) {
			stop("'lheights' must be a numeric vector of length 3
				with the relative heights of header, main panel and footer.")
		}
	}


	if (sum(!is.na(lwidths))==0) {
		lwidths<-rep(1,length(layout))
	} else {
		if (length(lwidths)!=length(layout)) {
			stop("'lwidths' must be a numeric vector of length of 'layout'
				with the relative widths of each column in the final plot.")
		}
	}

	hnames<-headernames(dat = dat, layout = layout)

	items<-vector(length=length(layout), mode="list")
	j<-1

	for (i in 1:length(layout)) {

		if (layout[i]=="t") {

			addi<-list(
				type = layout[i],
				vname = hnames[i],
				plot = list(x = 0, type="n", xlim=c(0,1), ylim=ylim, yaxt="n", ylab="", xlab="", axes=FALSE, xaxs = "i", yaxs = "i"),
				text = list(x = 0.5, y = y.at, labels = dat[,j], adj = c(0.5,0.5))
			)

			j<-j+1
		}

		if (layout[i]=="f") {

			xlim<-c(min(dat[,j+1],na.rm=TRUE),max(dat[,j+2],na.rm=TRUE))
			xlab<-pretty(xlim)
			xlim<-c(min(xlab,xlim),max(xlab,xlim))
			xlab_text<-xlab

			slow<-dat[,j+1]<min(xlim)
			shigh<-dat[,j+2]>max(xlim)
			sboth<-dat[,j+1]<min(xlim) & dat[,j+2]>max(xlim)
			code<-rep(3,nrow(dat))
			code[slow]<-1
			code[shigh]<-2
			code[sboth]<-3
			angle<-rep(90,nrow(dat))
			angle[slow | shigh | sboth]<-30

			addi<-list(
				type = layout[i],
				vname = hnames[i],
				plot = list(x=0, type="n", xlim = xlim, ylim = ylim,
					yaxt="n" ,ylab="", xlab="", axes=FALSE, xaxs = "i", yaxs = "i"),
				axis = list(side = 1, at = xlab, labels = xlab_text, line = 0,
					pos = ylim[1], las = 1),
				points = list(x = dat[,j], y = y.at, pch = 15),
				arrows = list(y0 = y.at, y1 = y.at, x0 = dat[,j+1], x1 = dat[,j+2],
					code = code, angle = angle, length = 0.05))

			j<-j+3
		}

		if (grepl("s\\d+",layout[i])) {

			nstrip<-as.numeric(substr(layout[i],2,nchar(layout[i])))

			xlim<-c(min(dat[,j:(j+nstrip-1)],na.rm=TRUE),max(dat[,j:(j+nstrip-1)],na.rm=TRUE))
			xlab<-pretty(xlim)
			xlim<-c(min(xlab,xlim),max(xlab,xlim))
			xlab_text<-xlab

			cols<-1:nstrip

			addi<-list(
				type = layout[i],
				vname = hnames[i],
				plot = list(x=0, type="n", xlim = xlim, ylim = ylim,
					yaxt="n" ,ylab="", xlab="", axes=FALSE, xaxs = "i", yaxs = "i"),
				axis = list(side = 1, at = xlab, labels = xlab_text, line = 0,
					pos = ylim[1], las = 1),
				hline = list(h = y.at, lty = 3))

			for (nsi in 1:nstrip) {
				addi<-append(addi,
					list(points = list(x = dat[,j + nsi - 1], y = y.at, pch = 1, col = cols[nsi])))
			}
			names(addi)[names(addi)=="points"]<-paste0("points",1:nstrip)

			j<-j+2

		}

		if (layout[i]=="b") {

			if (is.null(obs)) {
				stop("The boxplot ('b' in layout) requires an extra dataset given in 'obs'.")
			} else {
				if (!all(c("out","var","arm") %in% names(obs))) {
					stop("'obs' must be a data frame with columns 'out', 'var' and 'arm'.")
				}
			}

			xlim<-c(min(obs$out),max(obs$out))
			xlab<-pretty(xlim)
			xlim<-c(min(xlab,xlim),max(xlab,xlim))
			xlab_text<-xlab
			pm<-0.2
			gap<-0.1
			bwidth <- 2*pm - gap
			bp.at<-sort(c(y.at-pm,y.at+pm))
			cols<-c(rgb(1,0,0,0.3),rgb(0,0,1,0.3))

			addi<-list(
				type = layout[i],
				vname = hnames[i],
				plot = list(x=0, type="n", xlim = xlim, ylim = ylim,
					yaxt="n" ,ylab="", xlab="", axes=FALSE, xaxs = "i", yaxs = "i"),
				boxplot = list(formula = out ~ rev(arm)*rev(var), data=obs,
					at=bp.at, boxwex = bwidth, horizontal=TRUE, axes=FALSE, col=cols, add=TRUE),
				axis = list(side = 1, at = xlab, labels = xlab_text, line = 0,
					pos = ylim[1], las = 1))

		}


		items[[i]]<-addi

	}

	#collect
	fobj<-list(dat = dat, obs = obs,
		setup=list(layout = layout, lwidths = lwidths, lheights = lheights, y.at = y.at, ylim = ylim),
		items = items)

	class(fobj) <- c("fobj", class(fobj))


	#header
	fobj<-header(fobj)

	#stripes
	#fobj<-stripes(fobj)

	#gridlines
	#fobj<-gridlines(fobj)

	#add forest sub-items
	#if (any(fobj$setup$layout=="f")) {
	#	fobj<-f_direction(fobj)
	#	fobj<-f_refline(fobj)
	#}

	#add strip subitems
	#if (any((grepl("s\\d+",fobj$setup$layout)))) {
	#	fobj<-s_borders(fobj)
	#}

	return(fobj)

}



#---------------------
#Functions for each element
#---------------------

# text (f)
#---------------------

#' t_options
#'
#' Modify text (t) items of a forest plot object (fobj).
#'	Passed to \code{\link[graphics]{text}}.
#'
#' @param fobj a forest plot object of class fobj
#' @param item item to be modified, either a number or the name of the column in fobj$dat.
#' 	If NULL (the default), all items of type 't' are affected
#' @param ... options to be passed to \code{\link[graphics]{text}}
#'
#' @returns a forest plot object of class 'fobj'
#'
#' @export
#'
#' @importFrom utils modifyList
#'
#' @examples
#'
#' fobj<-genfobj(layout = c("t","t","t","t","t","t","f","t"),
#' 	dat = forplotdata, lwidths = c(0.8,0.4,0.6,0.4,0.6,1,1,0.5))
#' fobj<-t_options(fobj = fobj, item = 1, col = 2)
#' plotfobj(fobj)
t_options<-function(fobj, item = NULL, ...)	{

	itemnr<-check_convert(fobj = fobj, item = item, lay = "t")

	input<-list(...)

	for (itn in itemnr) {

		if (fobj$setup$layout[itn]!="t") {
			stop(paste0("Item ",itn," is not a text item"))
		}

		fobj$items[[itn]]$text<-modifyList(fobj$items[[itn]]$text, input)
	}

	return(fobj)
}



#forest (f)
#---------------------

#' f_axis
#'
#' Modify axis in forest (f) items of a forest plot object (fobj).
#'	Passed to \code{\link[graphics]{axis}}.
#'
#' @param fobj a forest plot object of class 'fobj'
#' @param item item to be modified, either a number or the name of the column in fobj$dat.
#' 	If NULL (the default), all items of type 'f' are affected
#' @param ... options to be passed to  \code{\link[graphics]{axis}}
#'
#' @returns a forest plot object of class 'fobj'
#'
#' @export
#'
#' @importFrom utils modifyList
#'
#' @examples
#'
#' fobj<-genfobj(layout = c("t","t","t","t","t","t","f","t"),
#' 	dat = forplotdata, lwidths = c(0.8,0.4,0.6,0.4,0.6,1,1,0.5))
#' fobj<-f_axis(fobj = fobj, at = seq(-1,0.2, by=0.4),
#'	labels=seq(-1,0.2, by=0.4), tck=-0.03, mgp = c(2,0.5,0))
#' plotfobj(fobj)
f_axis<-function(fobj, item = NULL, ...) {

	itemnr<-check_convert(fobj = fobj, item = item, lay = "f")

	input<-list(...)

	for (itn in itemnr) {

		if ("xlim" %in% names(input)) {
			fobj$items[[itn]]$plot$xlim<-input$xlim
			input<-input[names(input)!="xlim"]
		}
		if ("ylim" %in% names(input)) {
			fobj$items[[itn]]$plot$ylim<-input$ylim
		}

		fobj$items[[itn]]$axis<-modifyList(fobj$items[[itn]]$axis, input)
	}

	return(fobj)
}


#' f_points
#'
#' Modify points in forest (f) items of a forest plot object (fobj).
#'	Passed to \code{\link[graphics]{points}}.
#'
#' @param fobj a forest plot object of class 'fobj'
#' @param item item to be modified, either a number or the name of the column in fobj$dat.
#' 	If NULL (the default), all items of type 'f' are affected
#' @param ... options to be passed to \code{\link[graphics]{points}}
#'
#' @returns a forest plot object of class 'fobj'
#'
#' @export
#'
#' @importFrom utils modifyList
#'
#' @examples
#'
#' fobj<-genfobj(layout = c("t","t","t","t","t","t","f","t"),
#' 	dat = forplotdata, lwidths = c(0.8,0.4,0.6,0.4,0.6,1,1,0.5))
#' fobj<-f_points(fobj = fobj, pch = 16, cex = 1.5)
#' plotfobj(fobj)
f_points<-function(fobj, item = NULL, ...) {

	itemnr<-check_convert(fobj = fobj, item = item, lay = "f")

	input<-list(...)

	for (itn in itemnr) {

		fobj$items[[itn]]$points<-modifyList(fobj$items[[itn]]$points, input)
	}

	return(fobj)
}

#' f_arrows
#'
#' Modify arrows (confidence intervals) in forest (f) items of a forest plot object (fobj).
#'	Passed to \code{\link[graphics]{arrows}}.
#'
#' @param fobj a forest plot object of class 'fobj'
#' @param item item to be modified, either a number or the name of the column in fobj$dat.
#' 	If NULL (the default), all items of type 'f' are affected
#' @param ... options to be passed to \code{\link[graphics]{arrows}}
#'
#' @returns a forest plot object of class 'fobj'
#'
#' @export
#'
#' @importFrom utils modifyList
#'
#' @examples
#'
#' fobj<-genfobj(layout = c("t","t","t","t","t","t","f","t"),
#' 	dat = forplotdata, lwidths = c(0.8,0.4,0.6,0.4,0.6,1,1,0.5))
#' fobj<-f_arrows(fobj = fobj, col = 2)
#' plotfobj(fobj)
f_arrows<-function(fobj, item = NULL, ...) {

	itemnr<-check_convert(fobj = fobj, item = item, lay = "f")

	input<-list(...)

	for (itn in itemnr) {

		fobj$items[[itn]]$arrows<-modifyList(fobj$items[[itn]]$arrows, input)
	}

	return(fobj)
}


#' f_refline
#'
#' Add and modify the reference line in forest (f) items of a forest plot object (fobj).
#'	Passed to \code{\link[graphics]{lines}}.
#'
#' @param fobj a forest plot object of class 'fobj'
#' @param item item to be modified, either a number or the name of the column in fobj$dat.
#' 	If NULL (the default), all items of type 'f' are affected
#' @param ... options to be passed to \code{\link[graphics]{lines}}
#'
#' @returns a forest plot object of class 'fobj'
#'
#' @export
#'
#' @importFrom utils modifyList
#'
#' @examples
#'
#' fobj<-genfobj(layout = c("t","t","t","t","t","t","f","t"),
#' 	dat = forplotdata, lwidths = c(0.8,0.4,0.6,0.4,0.6,1,1,0.5))
#' fobj<-f_refline(fobj = fobj, x = c(0,0))
#' plotfobj(fobj)
f_refline<-function(fobj, item = NULL, ...) {

	itemnr<-check_convert(fobj = fobj, item = item, lay = "f")

	input<-list(...)

	for (itn in itemnr) {

		if (is.null(fobj$items[[itn]]$refline)) {
			fobj$items[[itn]]$refline<-list(x = rep(0,2), y = c(fobj$setup$ylim[1], fobj$setup$ylim[2]), lty = 2)
		}

		fobj$items[[itn]]$refline<-modifyList(fobj$items[[itn]]$refline, input)
	}

	return(fobj)
}


#' f_direction
#'
#' Add and modify direction indicator in forest (f) items of a forest plot object (fobj).
#'	Passed to \code{\link[graphics]{mtext}}.
#'
#' @param fobj a forest plot object of class 'fobj'
#' @param item item to be modified, either a number or the name of the column in fobj$dat.
#' 	If NULL (the default), all items of type 'f' are affected
#' @param ... options to be passed to  \code{\link[graphics]{mtext}}
#'
#' @returns a forest plot object of class 'fobj'
#'
#' @export
#'
#' @importFrom utils modifyList
#'
#' @examples
#'
#' fobj<-genfobj(layout = c("t","t","t","t","t","t","f","t"),
#' 	dat = forplotdata, lwidths = c(0.8,0.4,0.6,0.4,0.6,1,1,0.5))
#' fobj<-f_direction(fobj = fobj, text = "A better    B better",
#' 	line = 1.6)
#' plotfobj(fobj)
f_direction<-function(fobj, item= NULL, ...) {


	itemnr<-check_convert(fobj = fobj, item = item, lay = "f")

	input<-list(...)

	for (itn in itemnr) {

		if (is.null(fobj$items[[itn]]$direction)) {
			fobj$items[[itn]]$direction<-list(side = 1, line = 2.5, at = 0, text = "A better    B better", cex=0.6)
		}

		fobj$items[[itn]]$direction<-modifyList(fobj$items[[itn]]$direction, input)
	}

	return(fobj)
}




# strip plot (s)
#---------------------

#' s_axis
#'
#' Modify axis of stripe (s) items of a forest plot object (fobj).
#' Passed to \code{\link[graphics]{axis}}.
#'
#' @param fobj a forest plot object of class 'fobj'
#' @param item item to be modified, either a number or the name of the column in fobj$dat.
#' 	If NULL (the default), all items of type 's' are affected
#' @param ... options to be passed to \code{\link[graphics]{axis}}
#'
#' @returns a forest plot object of class 'fobj'
#'
#' @export
#'
#' @importFrom utils modifyList
#'
#' @examples
#'
#'fobj<-genfobj(layout = c("t","t","t","t","t","s2","t","f","t"),
#'	dat = forplotdata_prop,
#'	lwidths = c(0.6,0.4,0.6,0.4,0.6,1.0,1.2,1,0.5))
#'fobj<-s_axis(fobj=fobj, xlim = c(0,1),
#'  at = seq(0,1,by=0.25), labels = seq(0,100,by=25))
#'plotfobj(fobj)
s_axis<-function(fobj, item = NULL, ...) {

	itemnr<-check_convert(fobj = fobj, item = item, lay = "s")

	input<-list(...)

	for (itn in itemnr) {

		if ("xlim" %in% names(input)) {
			fobj$items[[itn]]$plot$xlim<-input$xlim
			input<-input[names(input)!="xlim"]
		}
		if ("ylim" %in% names(input)) {
			fobj$items[[itn]]$plot$ylim<-input$ylim
		}

		fobj$items[[itn]]$axis<-modifyList(fobj$items[[itn]]$axis, input)
	}

	return(fobj)
}

#' s_hline
#'
#' Modify the hiorzontal line in stripe (s) items of forest plot object (fobj).
#' Passed to \code{\link[graphics]{abline}}.
#'
#' @param fobj a forest plot object of class 'fobj'
#' @param item item to be modified, either a number or the name of the column in fobj$dat.
#' 	If NULL (the default), all items of type 's' are affected
#' @param ... options to be passed to \code{\link[graphics]{abline}}
#'
#' @returns a forest plot object of class 'fobj'
#'
#' @export
#'
#' @importFrom utils modifyList
#'
#' @examples
#'
#'fobj<-genfobj(layout = c("t","t","t","t","t","s2","t","f","t"),
#'	dat = forplotdata_prop,
#'	lwidths = c(0.6,0.4,0.6,0.4,0.6,1.0,1.2,1,0.5))
#'fobj<-s_hline(fobj=fobj, lty = 4)
#'plotfobj(fobj)
#'
s_hline<-function(fobj, item = NULL, ...) {

	itemnr<-check_convert(fobj = fobj, item = item, lay = "s")

	input<-list(...)

	for (itn in itemnr) {

		fobj$items[[itn]]$hline<-modifyList(fobj$items[[itn]]$hline, input)
	}

	return(fobj)
}



#' s_points
#'
#' Modify points in stripe (s) items of a forest plot object (fobj).
#' Passed to \code{\link[graphics]{points}}.
#'
#' @param fobj a forest plot object of class 'fobj'
#' @param item item to be modified, either a number or the name of the column in fobj$dat.
#' 	If NULL (the default), all items of type 's' are affected
#' @param pointnr points to be modified. If NULL (the default), all points are affected.
#' @param ... options to be passed to \code{\link[graphics]{points}}
#'
#' @returns a forest plot object of class 'fobj'
#'
#' @export
#'
#' @importFrom utils modifyList
#'
#' @examples
#'
#'fobj<-genfobj(layout = c("t","t","t","t","t","s2","t","f","t"),
#'	dat = forplotdata_prop,
#'	lwidths = c(0.6,0.4,0.6,0.4,0.6,1.0,1.2,1,0.5))
#'fobj<-s_points(fobj=fobj, pch = 16, cex=1.5)
#'fobj<-s_points(fobj=fobj, pointnr = 1, col = "red")
#'fobj<-s_points(fobj=fobj, pointnr = 2, col = "blue")
#'plotfobj(fobj)
#'
#'
s_points<-function(fobj, item = NULL, pointnr = NULL, ...) {

	itemnr<-check_convert(fobj = fobj, item = item, lay = "s")

	input<-list(...)

	for (itn in itemnr) {

		if (is.null(pointnr)) {
			pointnr<-as.numeric(substr(fobj$setup$layout[itn],2,nchar(fobj$setup$layout[itn])))

			for (pn in 1:pointnr) {

				fobj$items[[itn]][[paste0("points",pn)]]<-
					modifyList(fobj$items[[itn]][[paste0("points",pn)]], input)
			}
		} else {

			fobj$items[[itn]][[paste0("points",pointnr)]]<-
					modifyList(fobj$items[[itn]][[paste0("points",pointnr)]], input)

		}
	}

	return(fobj)
}


#' s_borders
#'
#' Add and modify borders of a sripe (s) item of a forest plot object (fobj).
#' Passed to \code{\link[graphics]{abline}}.
#'
#' @param fobj a forest plot object of class 'fobj'
#' @param item item to be modified, either a number or the name of the column in fobj$dat.
#' 	If NULL (the default), all items of type 's' are affected
#' @param bordernr border to be modified, 1 (left) or 2 (right).
#'	If NULL (the default), all borders are affected.
#' @param ... options to be passed to \code{\link[graphics]{abline}}
#'
#' @returns a forest plot object of class 'fobj'
#'
#' @export
#'
#' @importFrom utils modifyList
#'
#' @examples
#'
#'fobj<-genfobj(layout = c("t","t","t","t","t","s2","t","f","t"),
#'	dat = forplotdata_prop,
#'	lwidths = c(0.6,0.4,0.6,0.4,0.6,1.0,1.2,1,0.5))
#'fobj<-s_borders(fobj=fobj)
#'plotfobj(fobj)
#'
s_borders<-function(fobj, item = NULL, bordernr = NULL, ...) {

	itemnr<-check_convert(fobj = fobj, item = item, lay = "s")

	input<-list(...)

	for (itn in itemnr) {

		if (is.null(fobj$items[[itn]]$borders)) {

			fobj$items[[itn]]$borders<-list(list(v = fobj$items[[itn]]$plot$xlim[1]),list(v = fobj$items[[itn]]$plot$xlim[2]))

		}


		if (is.null(bordernr)) {

			bordernr<-1:2

			for (pn in bordernr) {

				fobj$items[[itn]]$borders[[pn]]<-
					modifyList(fobj$items[[itn]]$borders[[pn]], input)
			}

		} else {

				fobj$items[[itn]]$borders[[pn]]<-
					modifyList(fobj$items[[itn]]$borders[[pn]], input)

		}
	}

	return(fobj)
}


# boxplot (b)
#---------------------

#' b_boxplot
#'
#' Modify boxplot (b) items of a forest plot object (fobj).
#' Passed to \code{\link[graphics]{boxplot}}.
#'
#' @param fobj a forest plot object of class 'fobj'
#' @param item item to be modified, either a number or the name of the column in fobj$dat.
#' 	If NULL (the default), all items of type 'b' are affected
#' @param ... options to be passed to \code{\link[graphics]{boxplot}}
#'
#' @returns a forest plot object of class 'fobj'
#'
#' @export
#'
#' @importFrom utils modifyList
#'
#' @examples
#'
#'fobj<-genfobj(layout = c("t","t","t","t","t","b","t","f","t"),
#'	dat = forplotdata, obs = forplotdata_bp,
#'  lwidths = c(0.6,0.4,0.6,0.4,0.6,1,1,1,0.5))
#'fobj<-b_boxplot(fobj, boxwex = 0.2)
#'plotfobj(fobj)
#'
b_boxplot<-function(fobj, item = NULL, ...) {

	itemnr<-check_convert(fobj = fobj, item = item, lay = "b")

	input<-list(...)

	for (itn in itemnr) {

		if ("xlim" %in% names(input)) {
			fobj$items[[itn]]$plot$xlim<-input$xlim
			input<-input[names(input)!="xlim"]
		}
		if ("ylim" %in% names(input)) {
			fobj$items[[itn]]$plot$ylim<-input$ylim
		}

		fobj$items[[itn]]$boxplot<-modifyList(fobj$items[[itn]]$boxplot, input)
	}

	return(fobj)
}

#' b_axis
#'
#' Modify boxplot (b) items of a forest plot object (fobj).
#' Passed to \code{\link[graphics]{axis}}.
#'
#' @param fobj a forest plot object of class 'fobj'
#' @param item item to be modified, either a number or the name of the column in fobj$dat.
#' 	If NULL (the default), all items of type 'b' are affected.
#' @param ... options to be passed to  \code{\link[graphics]{axis}}.
#'
#' @returns a forest plot object of class 'fobj'
#'
#' @export
#'
#' @importFrom utils modifyList
#'
#' @examples
#'
#'fobj<-genfobj(layout = c("t","t","t","t","t","b","t","f","t"),
#'	dat = forplotdata, obs = forplotdata_bp,
#'  lwidths = c(0.6,0.4,0.6,0.4,0.6,1,1,1,0.5))
#'fobj<-b_axis(fobj, at = seq(2,10,by=2), labels = seq(2,10,by=2))
#'plotfobj(fobj)
#'
b_axis<-function(fobj, item = NULL, ...) {

	itemnr<-check_convert(fobj = fobj, item = item, lay = "b")

	input<-list(...)

	for (itn in itemnr) {

		if ("xlim" %in% names(input)) {
			fobj$items[[itn]]$plot$xlim<-input$xlim
			input<-input[names(input)!="xlim"]
		}
		if ("ylim" %in% names(input)) {
			fobj$items[[itn]]$plot$ylim<-input$ylim
		}

		fobj$items[[itn]]$axis<-modifyList(fobj$items[[itn]]$axis, input)
	}

	return(fobj)
}

#---------------------
#header
#---------------------

#' header
#'
#' Modify the header of a forest plot object (fobj).
#'	Passed to \code{\link[graphics]{text}}.
#'
#' @param fobj a forest plot object of class 'fobj'
#' @param hlayout layout of header columns. Numeric vector with the same length as the labels.
#' @param headernr header row to be modified.
#'	If NULL (the default), all header rows are affected
#' @param ... options to be passed to \code{\link[graphics]{text}}
#'
#' @returns a forest plot object of class 'fobj'
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
#' #Custom names
#'fobj<-header(fobj = fobj,
#'  labels = c("","Group1\nN","Group1\nmean (sd)","Group2\nN","Group2\nmean (sd)",
#'	"Mean difference\n95% CI","","P-value"),
#'  y = 0.6)
#'plotfobj(fobj)
#'
#' #Several lines
#'fobj<-header(fobj = fobj, hlayout = c(1,2,2,3,3,4,4,5),  headernr = 1,
#'	labels=c("","Arm A","Arm B","Mean diff (95% CI)","P-value"),
#'	y = 0.9)
#'fobj<-header(fobj = fobj, hlayout = c(1,2,3,4,5,6,7,8), headernr = 2,
#'	labels=c("","N","Mean (sd)","N","Mean (sd)","","",""),y=0.3)
#'plotfobj(fobj)
#'
header<-function(fobj, hlayout = NULL, headernr = NULL, ...)	{

	if (!("fobj" %in% class(fobj) )) {
		stop("fobj has to be of class fobj - use genfobj to define it")
	}

	input<-list(...)

	if (length(fobj$header)==0) {

		h<-headernames(dat = fobj$dat, layout = fobj$setup$layout)

		fobj$header<-list(list(hlayout = 1:length(fobj$setup$layout),
			text = list(x = NULL, y = 0.5, labels = h, adj = c(0.5,0.5))))
	}


	if (is.null(headernr)) {
		headernr<-length(fobj$header)
	}


	for (htn in headernr) {

		if (length(fobj$header)<headernr) {
			for (i in (length(fobj$header)+1):headernr) {
				fobj$header<-append(fobj$header,list(fobj$header[[i-1]]))
			}
		}

		if (!is.null(hlayout)) {
			fobj$header[[htn]]$hlayout<-hlayout
		}

		fobj$header[[htn]]$text<-modifyList(fobj$header[[htn]]$text, input)
	}

	return(fobj)
}


#---------------------
#Layout add ons
#---------------------

# grid options
#----------------

#' gridlines
#'
#' Add or modify horizontal lines to a forest plot object (fobj).
#'	Passed to \code{\link[graphics]{abline}}.
#'
#' @param fobj a forest plot object of class 'fobj'
#' @param gridnr grid line to be modified.
#'	If NULL (the default), all grid lines are affected
#' @param ... options to be passed to Passed to \code{\link[graphics]{abline}}.
#'
#' @returns a forest plot object of class 'fobj'
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
#' fobj<-gridlines(fobj = fobj)
#' plotfobj(fobj)
gridlines<-function(fobj, gridnr = NULL, ...)	{

	if (!("fobj" %in% class(fobj) )) {
		stop("fobj has to be of class fobj - use genfobj to define it")
	}

	input<-list(...)

	if (length(fobj$gridlines)==0) {
		fobj$gridlines<-list(list(h = fobj$setup$ylim[1], xpd = TRUE),list(h = fobj$setup$ylim[2], xpd = TRUE))
	}

	if (is.null(gridnr)) {
		gridnr<-length(fobj$gridlines)
	}

	for (htn in gridnr) {

		if (length(fobj$gridlines)<gridnr) {
			for (i in (length(fobj$gridlines)+1):gridnr) {
				fobj$gridlines<-append(fobj$gridlines,list(fobj$gridlines[[i-1]]))
			}
		}


		fobj$gridlines[[htn]]<-modifyList(fobj$gridlines[[htn]], input)
	}

	return(fobj)
}


#stripes
#----------

#' stripes
#'
#' Add or modify stripes in a forest plot object (fobj).
#'	Passed to \code{\link[graphics]{rect}}.
#'
#' @param fobj a forest plot object of class 'fobj'
#' @param ... options to be passed to \code{\link[graphics]{rect}}
#'
#' @returns a forest plot object of class 'fobj'
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
#' fobj<-stripes(fobj = fobj)
#' plotfobj(fobj)
stripes<-function(fobj, ...)	{

	if (!("fobj" %in% class(fobj) )) {
		stop("fobj has to be of class fobj - use genfobj to define it")
	}

	input<-list(...)

	if (is.null(fobj$stripes)) {
		lb<-c(fobj$setup$ylim[1],fobj$setup$ylim[1] + fobj$setup$y.at[-length(fobj$setup$y.at)])
		ub<-c(fobj$setup$ylim[1] + fobj$setup$y.at)
		ys<-cbind(lb,ub)[seq(1,length(lb),by=2),]

		fobj$stripes<-list(xleft = NA, ybottom = ys[,1], xright = NA, ytop = ys[,2], col = rgb(.1,.1,.1,.1), border = NA)
	}

	fobj$stripes<-modifyList(fobj$stripes, input)

	return(fobj)
}


