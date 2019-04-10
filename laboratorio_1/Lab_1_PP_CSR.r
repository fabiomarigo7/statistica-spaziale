### Esercitazione 1: uso shape file e dati PP

# Indicazioni generali -------------------------------------------------------
# creare un nuovo file per lo script seguente
# menu file- new file - R script
#
# salvare il file:	menu file - save  oppure usare icona
#
# copiare tutti i file necessari all'esercitazione in una cartella e
# definire questa cartella come cartella di lavoro 
# tramite menù (per R-Studio): session/set working directory/choose directory...
##
# Elenco dei file 
# dataset:  dataset Incendi_2003.csv - locazioni degli incendi in Lombardia nel 2003 
#           WLR300.txt               - difetti rilevati su wafer di silicio
# mappa lombardia GaussBoaga: lombardia.shp, lombardia.dbf, lombardia.shx
# mappa NUTS3 Austria coordinate geografiche: austrianuts3.shp, austrianuts3.dbf, austrianuts3.shx
#
# librerie richieste: maptools e spatstat (e dipendenze) 
# per controllare i pacchetti disponibili digitare library()
# per informazioni sulla singola libreria library(help=nomelibreria). 
# Esempio: library(help=spatstat)
#
# PARTE 1: lettura shape file e visualizzazione layer e punti ------------
# dati point pattern
### testo di utile consultazione per le librerie sp e spdep 
### Bivand RS, Pebesma EJ, Gómez-Rubio V, 2008 Applied Spatial Data Analysis with R. libro on line, Springer
#
# definire cartella di lavoro
#setwd("C:/...")
#
require(maptools)   ### aggiunge il pacchetto maptools a sessione lavoro
require(rgdal)
# 
# lettura shape file Lombardia in R (in coordinate GaussBoaga)
  	lomb.poly<-readOGR("lombardia.shp",verbose=TRUE) 
    plot(lomb.poly)
# lettura dati 
    inc=incendi <- read.table("Incendi_2003.csv", header=T,sep=";");   
      str(incendi) 	      # descrizione sintetica
# plot dati su mappa
    points(incendi$EstN,incendi$Nord)
#
# PARTE 2: Esempio shape in coordinate geografiche -------------------------
# dati areali
  aus.poly<-readShapePoly("austrianuts3.shp", 
                        IDvar="ID",verbose=TRUE) 			### lettura shape file in R
  plot(aus.poly,col = "green",border="red")	### mappa regione     
  slotNames(aus.poly)
  slot(aus.poly,"data")
    aus.poly@data   # equivalente
  a=slot(aus.poly,"polygons"); 
  length(a); 
  a[[1]]
    centr=coordinates(aus.poly)
    points(centr,col="red")
    text(centr[,1],centr[,2],aus.poly$ID,col="blue")
# estrazione sotto shape
  xx<-aus.poly[aus.poly$ID=="AT13",]    		# poligono di Vienna
  plot(xx,add=T,col="blue")
#
# PARTE 3: PP: uso dello shape per definire la finestra di un PPS  -  pacchetto: spatstat ------------
require(spatstat)    			# carica la libreria 
#
# incendi_lombardia
  ppp<-incendi[,c("EstN","Nord")]
  ppp0=as.ppp(ppp,W=lomb.poly);ppp0
# plot PP
  plot(ppp0,cex=0.5,main=,"Incendi in Lombardia nel 2003")
#
# PPS marcati con dimensione incendio
  ppp<-incendi[,c("EstN","Nord","Ettari")]
  ppp0=as.ppp(ppp,W=lomb.poly,mark=ppp$Ettari);ppp0
  plot(ppp0,
       main=,"Incendi in Lombardia nel 2003 differenziati per estensione"
       )
#
  d<-read.table("WLR300.txt",header=T) 	# lettura dataset esterno in formato testo
  str(d)						                  # breve descrizione variabili
#
  W <- disc(96950, c(0,0))			# definisce una finestra circolare per il processo
  pp<-ppp(d$X,d$Y,window=W); summary(pp)
  plot(pp,cex=0.6,main="locazioni eventi",cex.main =1.2,lwd=3)

# PARTE 4: PP: Analisi dell'ipotesi CSR per point pattern data -  pacchetto: spatstat --------
# test basato sul quadrat counts
  qx<-quadratcount(pp,4,4)	### tabella 4x4
  plot(qx)

  te0<-quadrat.test(pp,4); te0					 ###test CSR chi-quadro
  plot(te0, col="red", cex=1, lty=1.5, lwd=3,bg = gray(0.7)) ### conteggi, attesi,residui
  plot(pp, pch="+", cols="green", cex=1.5, lwd=1.2,add=T)

  qx10<-quadratcount(pp,10,10)	### tabella 10x10
  plot(qx10)
  te10 <- quadrat.test(pp,10)	###test dispersione per 10x10
  te10
# prima valutazione clustering 
# istogramma distanze dal primo vicino
  hist(nndist(pp),main="distanza dal primo vicino",xlab="distanza")
# test basato sulla distribuzione del NN
  GGb<-Gest(pp, r=NULL, breaks=NULL,correction="none");  
  plot(GGb, raw ~ r,main="FR della distanza dal primo vicino",xlab="r")
  plot(GGb, theo~ r,add=T,col=2,lty=3)
  legend(25000, 0.2,  legend=c("F.R. empirica","F.R. teorica"),lty=c(1,3), col=c(1,2),bty="n")
  GGb	#   descrizione dell'output
  #   test tipo G FR corretta effetto bordo
  GG<-Gest(pp, r=NULL, breaks=NULL);  
  GG	#   descrizione dell'output
  v<-plot(GG, cbind(rs,theo) ~ theo, main="pp-plot")
#  inviluppo Monte Carlo per test su CSR
	envpp<-envelope(pp,fun=Gest,nsim=100,nrank=,verbose=TRUE,saveall=F)
	win.graph()
  	aa<-plot(envpp,main="inviluppo MC",xlab="y")
 	  aa
# PARTE 5: PP: stima kernel dell'intensità: mappe di intensità -  pacchetto: spatstat --------
# stima intensità 
  Z <- density.ppp(pp, 15000)
  win.graph()
    plot(Z,main="mappa dell'intensità kernel"); 
    plot(pp,add=T,cex=0.6,col="red")
    persp(Z)
#
# lettura shape file Lombardia
	lomb.poly<-readShapePoly("lombardia.shp",verbose=TRUE) ### lettura shape file in R
# incendi_lombardia
	ppp=incendi[,c("EstN","Nord")]
	ppp0=as.ppp(ppp,W=lomb.poly);ppp0
	plot(ppp0,cex=0.5,main=,"Incendi in Lombardia nel 2003")
# stima intensità
  Z <- density.ppp(ppp0, varcov=diag( c(var(ppp$EstN),var(ppp$Nord))/16)) 
  win.graph()
    plot(Z,main="mappa dell'intensità kernel"); 
    plot(ppp0,add=T,cex=0.4)