#example dataset with a continuous variable

set.seed(1)
forplotdata<-numeric(0)
forplotdata_bp<-numeric(0)
for (i in 1:10) {
	n0<-100
	n1<-100
	nt<-n0+n1
	dat0<-rnorm(n0,5,1)
	dat1<-rnorm(n1,5.5,1)

	forplotdata_bp<-rbind(forplotdata_bp,data.frame(out=c(dat0,dat1),var=paste0("var",i),arm=c(rep(1,n0),rep(2,n1))))

	tt<-t.test(dat0,dat1)
	out<-data.frame(vlabel=paste0("out",i),
		n1=n0,
		n2=paste0(sprintf("%1.1f",mean(dat0))," (",sprintf("%1.1f",sd(dat0)),")"),
		n3=n1,
		n4=paste0(sprintf("%1.1f",mean(dat1))," (",sprintf("%1.1f",sd(dat1)),")"),
		beta=mean(dat0)-mean(dat1),beta_lci=tt$conf.int[1],beta_uci=tt$conf.int[2],
		p1=sprintf("%0.3f",tt$p.value))

	out$beta_format<-paste0(sprintf("%2.2f",out$beta)," (",sprintf("%2.2f",out$beta_lci)," to ",sprintf("%2.2f",out$beta_uci),")")

	out<-out[c("vlabel","n1","n2","n3","n4","beta_format","beta","beta_lci","beta_uci","p1")]

	forplotdata<-rbind(forplotdata,out)
}

forplotdata_bp$var<-factor(forplotdata_bp$var,levels=paste0("var",1:10))
forplotdata_bp$arm<-factor(forplotdata_bp$arm,levels=1:2)

usethis::use_data(forplotdata, overwrite = TRUE)
usethis::use_data(forplotdata_bp, overwrite = TRUE)

#example dataset with a binary variable inlcuding the proporions

set.seed(1)
forplotdata_prop<-numeric(0)
for (i in 1:10) {
	n0<-100
	n1<-100
	nt<-n0+n1
	dat0<-rbinom(n0,1,0.5)
	dat1<-rbinom(n1,1,0.4)
	x<-c(sum(dat0),sum(dat1))
	n<-c(length(dat0),length(dat1))
	tt<-prop.test(x,n)
	out<-data.frame(vlabel=paste0("out",i),
		n1=n0,
		n2=paste0(sprintf("%1.0f",sum(dat0))," (",sprintf("%1.0f",mean(dat0)*100),"%)"),
		n3=n1,
		n4=paste0(sprintf("%1.0f",sum(dat1))," (",sprintf("%1.0f",mean(dat1)*100),"%)"),
		prop1=mean(dat0),
		prop2=mean(dat1),
		beta=(mean(dat0)-mean(dat1)),beta_lci=tt$conf.int[1],beta_uci=tt$conf.int[2],
		p1=sprintf("%0.3f",tt$p.value))

	out$beta_format<-paste0(sprintf("%1.1f",(mean(dat0)-mean(dat1))*100),"% (",
    sprintf("%1.1f",tt$conf.int[1]*100)," to ",
	  sprintf("%1.1f",tt$conf.int[2]*100),"%)")

	out<-out[c("vlabel","n1","n2","n3","n4","prop1","prop2","beta_format","beta","beta_lci","beta_uci","p1")]

	forplotdata_prop<-rbind(forplotdata_prop,out)
}

usethis::use_data(forplotdata_prop, overwrite = TRUE)

