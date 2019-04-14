#Esercitazione 4 Argomento: Previsione kriging
# creare un nuovo file per lo script seguente.
# pacchetti di R necessari per l'esercitazione
# maps, geoR, stats,utils ,scatterplot3d
# per controllare i pacchetti disponibili digitare library()
# per avere informazioni sulla singola libreria library(help=nomelibreria). Esempio: library(help=geoR)
#
# menu file - new file - R script
# salvare il file: 
#	menu file- save as
#	oppure usare icona
#
# Parte I:    preparazione dati e librerie -----------------------------
  require(geoR)			# carica il pacchetto geoR
  dat.om<-soja98; str(dat.om)
  obj <- dat.om[,c("X","Y","MO")]
  summary(obj)
#
# Parte II:   stima del variogramma -----------------------------
  attach(obj)  # aggiunge il dataset alla sessione di lavoro
#
# stima variogramma via wls
  soja.geo <- as.geodata(obj,coords.col=1:2,data.col=3)	# converte oggetto in classe geodata
  soja.var <- variog(soja.geo, estimator.type="classical")	# variogramma empirico
	plot(soja.var, main="variogramma empirico")			# variogramma per punti
  soja.var.fit <-variofit(soja.var,ini.cov.pars=c(25,35),weights="cressie",
		cov.model="exponential", fix.nugget=FALSE,nugget=20)
  lines(soja.var.fit);	soja.var.fit
	 a<-soja.var.fit$cov.pars
#
# Parte III:  preparazione griglia di previsione -----------------------------
# griglia di spaziatura size x size su X e Y
  size=30
	YY<-round(seq(ceiling(min(Y)),floor(max(Y)), length=size),2)
	XX<-round(seq(ceiling(min(X)),floor(max(X)), length=size),2)
	griglia<-expand.grid(X=XX,Y=YY)
	dim(griglia)
# Parte IV:   previsione - kriging ordinario -----------------------------
  krg.or <- krige.conv(geodata=soja.geo, 
	      		  loc=griglia,
			        krige=krige.control(cov.pars=a,
				              cov.model="exponential",
				              nugget=soja.var.fit$nugget))		
# rappresentazione grafica della previsione
  win.graph()
  par(cex=0.5,mfrow=c(2,2))
    	image(krg.or,xlim=c(-15,floor(max(X))),ylim=c(0,120)) 
	    legend.krige(x.leg=c(-13,-8), y.leg=c(30,100), val=krg.or$predict,
		    vert=TRUE, off=0.7, cex=0.9, scale.vals=pretty(krg.or$predict)
		    )
	contour(krg.or,add=T,coords.data=soja.geo$coords,cex=0.3)
	persp(krg.or,xlab="longitudine",ylab="latitudine",zlab="log-catch",ticktype="detailed")
	image(krg.or, val=sqrt(krg.or$krige.var), main="kriging std. errors",
		xlim=c(min(X),floor(max(X))))
	legend.krige(x.leg=c(-13,-8), y.leg=c(30,100), val=krg.or$krige.var,
		vert=TRUE, off=0.7, cex=0.9, scale.vals=pretty(krg.or$krige.var)
	)
	points.geodata(soja.geo,pt.divide="data.proportional")
  par(cex=1)
#
# Parte V:    previsione - kriging universale -----------------------------
  obj.var.u <- variog(soja.geo, estimator.type="classical",
		trend="2nd")		# variogramma empirico su dati detrendizzati da geor
  obj.var.fit.u <- variofit(obj.var.u,
	  ini.cov.pars=c(10,40),	cov.model="exponential", fix.nugget=FALSE,nugget=15)
  plot(obj.var.u)
  lines(obj.var.fit.u)
  krg.uk <- krige.conv(geodata=soja.geo, 
		loc=griglia,
		krige=krige.control(type.krige = "ok",
		trend.d="2nd",
		trend.l="2nd",
		cov.pars=obj.var.fit.u$cov.pars,
		nugget=obj.var.fit.u$nugget,
		cov.model="exponential"
		))
# rappresentazione grafica della previsione
  win.graph()
  par(mfrow=c(2,2),cex=0.5)
    	image(krg.uk, main="kriging universale",xlim=c(-15,floor(max(X))),ylim=c(0,120))
    	legend.krige(x.leg=c(-13,-8), y.leg=c(30,100), val=krg.uk$predict,
		    vert=TRUE, off=0.7, cex=0.9, scale.vals=pretty(krg.uk$predict)
		    )
    	contour(krg.uk,add=T)
	    points.geodata(soja.geo,pt.divide="quintiles", col=1:5,add.to.plot=T)
    	persp(krg.uk,xlab="X",ylab="Y",zlab="log-catch",ticktype="detailed")
    	image(krg.uk, val=sqrt(krg.uk$krige.var), main="kriging std. errors",
    		xlim=c(-15,floor(max(X))),ylim=c(0,120));	
	    contour(krg.uk,add=T)
	    legend.krige(x.leg=c(-13,-8), y.leg=c(30,100), val=krg.uk$krige.var,
		    vert=TRUE, off=0.7, cex=0.9, scale.vals=pretty(krg.uk$krige.var)
	  )
	points.geodata(soja.geo,pt.divide="data.proportional")
	par(cex=1)
# stima della media
  krg.uk$beta		
  krg.or$beta
#
# Esercizio. Costruire la mappa detrendizzando i dati tramite una regressione polinomiale e
#            krigando i residui. Ricostruire poi la mappa sommando alla previsione kriging il trend 
#            sulla griglia di previsione
#  
# Parte VI:   cross validazione via kriging ordinario -----------------------------
#
  date()	#--- ora inizio elaborazione
  xvalid.notrend<-xvalid(soja.geo, coords = soja.geo$coords, data = soja.geo$data,
	      model=soja.var.fit)
  date()	#--- ora fine elaborazione
#
  hist(xvalid.notrend$error,main="istogramma residui",xlab="residui")
  plot(xvalid.notrend$data,xvalid.notrend$predicted,ylab="previsione",xlab="osservazioni")
  plot(xvalid.notrend$predicted,xvalid.notrend$error,ylab="residui",xlab="previsione")
  plot(xvalid.notrend$data,xvalid.notrend$error,ylab="residui",xlab="osservazioni")
	# effetto di smoothing
  win.graph()
    n<-length(xvalid.notrend$error)
    points.geodata(coords=soja.geo$coords,data=xvalid.notrend$error,main="mappa dei residui",
	    pt.divide="data.proportional",col=gray(1:n/n))
    text(x=soja.geo$coords, labels=round(xvalid.notrend$error,2),cex=0.7,col=2,adj=c(0,1))
	  # aggiunge i valori al plot
# misure di fit 
    cor(xvalid.notrend$data,xvalid.notrend$predicted)
    mean(xvalid.notrend$error^2)
# Parte VII:  griglie e kriging dentro shapefile -----------------------------
    setwd("C:/Users/borgoni/Google Drive/Lavoro/Didattica/StatisticaSpaziale/SpaceStat2016-17/Laboratorio/Lab4")
# lettura dati e stima variogramma
    a=read.table("dati_krg.csv",header=T)
    obj  <- as.geodata(a,coords.col=1:2,data.col=3)	
    vario <- variog(obj, estimator.type="classical")	# variogramma empirico
    plot(vario, main="variogramma empirico")			# variogramma per punti
    vario.fit <-variofit(vario,ini.cov.pars=c(30,10000),weights="cressie",
                         cov.model="exponential", fix.nugget=FALSE,nugget=0)
    lines(vario.fit);	
    cov.pars=vario.fit$cov.pars
# lettura shape
    library(maptools)
    library(geoR)
    poly<-readShapePoly("acquif.shp",verbose=TRUE) 
        plot(poly,col="lightblue")
    # ricodifica del bordo come matrice    
    library(ggplot2)
    poligono <- fortify(poly)
    p=as.matrix(poligono[,c("long","lat")])
# griglia e kriging
    X=bbox(poly)[1,]
    Y=bbox(poly)[2,]
    size=100
    YY<-round(seq(ceiling(min(Y)),floor(max(Y)), length=size),2)
    XX<-round(seq(ceiling(min(X)),floor(max(X)), length=size),2)
    griglia<-expand.grid(X=XX,Y=YY)
    dim(griglia)
    plot(poly)
    points(griglia,cex=0.1)
    # plot 
    krg.or <- krige.conv(geodata=obj, loc=griglia,
                         krige=krige.control(cov.pars=cov.pars,
                                             cov.model="exponential",
                                             nugget=0),
                         borders=p)
    win.graph()
    image(krg.or) 
    legend.krige(x.leg=c(1575000, 1580000), y.leg=c(4986785, 5010000), val=krg.or$predict,
                 vert=TRUE, off=0.7, cex=0.9, scale.vals=pretty(krg.or$predict)
    )
    contour(krg.or,add=T,coords.data=a$coords,cex=0.3)
    points.geodata(obj,add=T,pt.divide="quintiles", col=1:5)