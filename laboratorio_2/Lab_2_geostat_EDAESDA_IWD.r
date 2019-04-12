### Esercitazione 2: analisi esplorativa per dati geostatistici
#
# pacchetti di R necessari per l'esercitazione
# maps, geoR, stats,utils ,scatterplot3d, spatstat
# per controllare i pacchetti disponibili digitare library()
# per avere informazioni sulla singola libreria library(help=nomelibreria). 
# Esempio: library(help=geoR)
#
# Se lo script è digitato da tastiera creare un nuovo file per lo script seguente
# menu file- new file - R script
# salvare il file: 
#	menu file- save as
#	oppure usare icona
# Prima di leggere il file reindirizzare la directory di riferimento 
# tramite menù session/Set Working Directory...
#
# PARTE 1: lettura, preparazione e esplorazione "non spaziale" dei dati EDA ----------------------------------------------
  setwd("C:/Users/borgoni/Google Drive/Lavoro/Didattica/StatisticaSpaziale/LaboratorioInf/Lab2")

  myscallops <- read.table("scallops.txt", header=T)	
# check importazione dati
	dim(myscallops) 	  # numero di osservazioni e variabili 
	myscallops[1:5,]	  # prime 5 righe del dataset
	str(myscallops)		  # descrizione sintetica dataset
# descrizione dei dati
	summary(myscallops)	
  hist(myscallops$tcatch,main="Istogramma scallops data",xlab="numero",ylab="frequenza") 	# istogramma
# trasformazione di variabili 
	myscallops[,"lgcatch"] <- log(myscallops$tcatch+1)	# aggiunge trasformata log di tcatch
	summary(myscallops$lgcatch)						
	hist(myscallops$lgcatch)	
# PARTE II: esplorazione "spaziale" dei dati ESDA: omoschedasticità ed effetto proporzionale-----------------------
  # visualizzazione dislocazione dei dati
	plot(myscallops$long, myscallops$lat,xlab="longitudine", ylab="latitudine")	# mappa rilevazioni
  library(maps)					# carica libreria per mappe
  map("usa")						# mappa degli USA- data set in R
  map("usa",fill=T,col=3, xlim=c(-78,-65), ylim=c(38.2,41.5))	# seleziona area di interesse	vedere il summary di scallops
  points(myscallops$long, myscallops$lat,cex=0.8)	# aggiunge punti
  # Esercizio: modificare lo zoom dell'area di interesse
  ## esplorazione stazionarietà: omoschedasticità ed effetto proporzionale tramite medie e varianze locali
  celle=4		        # numero di classi per direzione
  # visualizzazione delle celle utilizzate 
  library(spatstat)
  w<-convexhull.xy(x=myscallops$long,y=myscallops$lat)	# involucro convesso 
  X<-ppp(myscallops$long,myscallops$lat,window=w)		    # oggetto point pattern 
                                                        # facciamo finta che i siti siano un PP per poter usare la funzione quadratcount
  plot(X,w,main="")
  map("usa",fill=T,col=3, xlim=c(-78,-65), ylim=c(38.2,41.5),add=T)
  qx<-quadratcount(X,celle,celle)	### tabella size x size
  plot(qx,add=T)
# 
  ## calcolo statistiche locali
  temp=data.frame(tcatch=myscallops$tcatch,
	  lgcatch=myscallops$lgcatch,
	  xclass=cut(myscallops$long,celle),
	  yclass=cut(myscallops$lat,celle),
	  conta=rep(1,length(myscallops$lgcatch))
	  )
  m<-data.frame(
	  media=as.vector(tapply(temp$lgcatch, INDEX=list(temp$xclass,temp$yclass),FUN="mean")),
	  varianza=as.vector(tapply(temp$lgcatch, INDEX=list(temp$xclass,temp$yclass),FUN="var")),
	  frequenza=as.vector(tapply(temp$conta, INDEX=list(temp$xclass,temp$yclass),FUN="sum"))
	  )
  m
#
  par(mfrow=c(2,1))
  	plot(varianza~media,m); 	
	  abline(lm(varianza~media,m),col=2)
	  plot(m$varianza,ylab="varianza"); abline(h=var(temp$lgcatch),col=2)
#
# PARTE III: esplorazione per la stazionarietà di larga scala (in media) -------------------
# relazione fra log-conteggi e locazioni
  win.graph()
  par(mfrow=c(1,2))	
    plot(lgcatch~lat,data=myscallops,
	        ylab="trasformata conteggi",xlab="latitudine")	# log conteggi vs latitudine
    plot(lgcatch~long,data=myscallops,
	        ylab="trasformata conteggi",xlab="longitudine")	# log conteggi vs latitudine
  # aggiunge linea di tendenza via kernel
  win.graph()
  par(mfrow=c(1,2))
  with(myscallops, {
    plot(lat, lgcatch)
    lines(ksmooth(lat,  lgcatch, "normal", bandwidth=sd(myscallops$lat)),  col=2)
    plot(long, lgcatch)
    lines(ksmooth(long, lgcatch, "normal", bandwidth=sd(myscallops$long)), col=2)
    })
# analisi 3D
  win.graph()
  require(scatterplot3d)
  s3d<-scatterplot3d(myscallops$long, myscallops$lat, myscallops$lgcatch,
	                   xlab="longitudine",ylab="latitudine",zlab="log-conteggi",
                      col.grid="lightblue", pch=20,type="h")
  require(rgl)
    plot3d(myscallops$long, myscallops$lat, myscallops$lgcatch,
         xlab="longitudine",ylab="latitudine",zlab="log-conteggi",
         type="h",col.grid="lightblue", pch=20)
# usando geoR
  library(geoR)		# carica il pacchetto geoR
	obj <- cbind(myscallops$long,myscallops$lat,myscallops$lgcatch)
	scallops.geo <- as.geodata(obj,coords.col=1:2,data.col=3)	# converte oggetto in classe geodata
	  class(scallops.geo); is.list(scallops.geo); length(scallops.geo); names(scallops.geo); 
	  head(scallops.geo$data);  head(scallops.geo$coords)
	win.graph()
	points.geodata(scallops.geo,pt.divide="quintiles", col=1:5,xlim=c(-75,-71.1), ylim=c(38.2,41.5))
	  legend(-72.4, 39.5, pch=19, col=1:5, pt.cex=(1:5)/3,
    c("1° quintile","2° quintile","3° quintile","4° quintile","5° quintile"))
	plot(w,add=T)
	map("usa",fill=T,col=3, xlim=c(-78,-65), ylim=c(38.2,41.5),add=T)
  # multiplot in geoR
  plot.geodata(scallops.geo,scatter3d = FALSE)
  plot.geodata(scallops.geo,scatter3d = TRUE, lowess=T)
#
# PARTE IV: ESDA di larga scala tramite superfici --------------------------------
# griglia rettangolare su cui interpolare la superficie
	lat.lim <- range(myscallops$lat)	# calcola il range
	lon.lim <- range(myscallops$lon)
	y <- seq(floor(lat.lim[1]),floor(lat.lim[2])+1,by=0.1)
	x <- seq(floor(lon.lim[1]),floor(lon.lim[2])+1,by=0.1)
	gr<-griglia<-expand.grid(x=x,y=y); dim(griglia)
	# check
	win.graph(); 		plot(w,xlab="longitudine",ylab="latitudine"); points(griglia)
  # si identificano i punti griglia dentro involucro convesso 
	ok <- inside.owin(griglia$x, griglia$y, w) # nodi dentro involucro	
	# check
	win.graph(); 	
	  plot(w); 
	  points(griglia$x[ok], griglia$y[ok],cex=0.5)
		points(griglia$x[!ok], griglia$y[!ok], pch="x",cex=0.5,col="green")
#
	library(gstat);	
	library(maptools)
		locazioni=data.frame(lon=myscallops$lon,lat=myscallops$lat,lgcatch=myscallops$lgcatch)
		coordinates(locazioni)=c("lon","lat")
		coordinates(griglia) = ~x+y
		idw.p=gstat::idw(formula=lgcatch ~ 1, locations=locazioni, newdata=griglia, 
		          nmax = 15, idp = 2)
		idw.o=as.data.frame(idw.p)
		names(idw.o)[1:3]<-c("long","lat","lgcatch")
		idw.o[!ok,"lgcatch"] <- NA
		surface<-matrix(idw.o$lgcatch,byrow=F,nrow=length(y))
		int.scp <-list(x=x,y=y,z=surface) #--- lista per grafici

# diagramma a sfumature di colore
	win.graph()
		image(int.scp, xlab="Longitude", ylab="Latitude",xlim=c(-75.5,-71.50), ylim=c(38,42))	
		contour(int.scp, add=T)
		points.geodata(scallops.geo,pt.divide="quintiles", col=1:5,add=T)
		map("usa", add=T, xlim=c(-74,-71), ylim=c(38.2,41.5),fill=T,col=3,	xlab="longitudine",ylab="latitudine")	
		plot(w,border="gray",add=T,lwd=3)		
		map("usa", add=T, xlim=c(-74,-71), ylim=c(38.2,41.5),fill=T,col=3,	xlab="longitudine",ylab="latitudine")	
 
	win.graph()
		persp(int.scp,xlab="longitudine",ylab="latitudine",zlab="lgcatch",
		      expand=1,theta=30,phi=20,ticktype="detailed" )		
# Esercizio: modificare angolazioni tramite i parametri theta e phi 
#            per esempio porre theta = 60 e phi= 20 e provare diversi incroci 