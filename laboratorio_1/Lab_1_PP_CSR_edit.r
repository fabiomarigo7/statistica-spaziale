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
setwd("E:/Statistica_Spaziale/Lab1")
#LIBRERIE NECESSARIE ALL'ANALISI
require(maptools)   ### aggiunge il pacchetto maptools a sessione lavoro
require(rgdal)


# nei 5 file lombardia.sbn, sbx,dbf ecc servono
#per fare i contorni della lomabrdia e produrre la mappa

# dbf shp shx sono essenziali!!!!
# questi tre file assieme producono lo shape file della regione
##(in questo caso contorni della regione da realizzare)
##--> ci consentono di creare la mappa su r


###readOGR()
##.shp è lo shape file vero e proprio
#ci serve per importare questi file

# lettura shape file Lombardia in R (in coordinate GaussBoaga)
  	lomb.poly<-readOGR("lombardia.shp",verbose=TRUE)
  	##serve per importare lo shape file
    plot(lomb.poly)
    #ne fa un plot specifico pèr Formal class 'SpatialPolygonsDataFrame'

    
# lettura dati 
    #creo un doppio file inc e incendi, leggendo il csv 
    inc=incendi <- read.table("Incendi_2003.csv", header=T,sep=";");   
    str(incendi) 	      # descrizione sintetica
    #ettari estensione incendio (marker)
    ##nord estn --> coordinate dell'origine dell'incendio in Gauss_boaga
    #380 oss= 380 incendi
    
# plot dati su mappa
    #con point sovrapponiamo i punti di innesco alla mappa in questione 
    points(incendi$EstN,incendi$Nord)
#
    
    
# PARTE 2: Esempio shape in coordinate geografiche -------------------------

##adesso vediamo come R si comporta nel leggere gli shape file, usiamo 
    #un altro file piu articolato di quello della lomabrida.
    ##shape file che contiene confini amministrativi dell'Austria
    #in termini di nuts3 (codifica usata (5--> dettaglio max, 1 intera europa))
    #leggiamo mappa che codifica confini proivincie austriace (3)
    ##NB readshapepoly===(equivalente) readOGR di prima, sono equivalenti
    #readshapepoly--> libreria maptools
    #entrambi importano lo shape file
    
# dati areali
  #idvar--> specificachiave tabella dbf associata allo shape file
  aus.poly<-readShapePoly("austrianuts3.shp", 
                        IDvar="ID",verbose=TRUE) 			### lettura shape file in R
  
  ##ci dice in lettura: Shapefile type: Polygon, (5), # of Shapes: 35
    ###35 provincie austria --> codificate con 35 poligoni
    ##shape di tipo poligonali
  plot(aus.poly,col = "green",border="red")	### mappa regione     
  ##vediamo cosa ce dentro questo oggetto, quali slot compongono 
  #l'oggetto. Slot--> sottooggetti in r
  slotNames(aus.poly)
  #[1] "data"        "polygons"    "plotOrder"   "bbox"       
  #[5] "proj4string"
  #a noi interesseranno due di questi oggetti. 
  # questi 5 slot contengono tutta l'informazione contenuta in 
  #tutto lo shape file. .dbf ,.shx , .shp
  ## in data troviamo il contenuto della tabella dati .dbf
  
  # per accederci si puo utilizzare 
  
  # 1)fz slot
  #slot(oggetto, cheslotvoglio?)
  slot(aus.poly,"data")
  #prima colonna codice associato alla area, seconda colonna nome esteso
  ###in data abbiamo tutta l'informazione alfanumerica associata alla mappa (era contenuta in .dbf)
  
  # 2) con @
  aus.poly@data   # equivalente
  # chiocciola serve per accedere, come $ ma per oggetti non codificati
  #su R
  
  ##secondo oggetto rilevante, dopo data Poligons
  # è uno slot articolato, salviamolo in un oggetto. 
  a=slot(aus.poly,"polygons"); 
  length(a); #è una lista lunga 35
  a[[1]] #lista--> indicizzo con doppia quadra
  #cosa contiene ognuno dei 35 elementi?
  ###contiene tanta informazione: la prima componente della lista
  #è un oggetto di classe polygons
  ##primo elemento coordinate del centroide labpt
  ###secondo elemento ci da area, superficie del nostro poligono
  
  ##terzo eleemnto hole T/F--> boh non l'ha detto
  
  ##elemnto piu importante coords 
  ##coordiante latitudine e longitudine di 335 punti. 
  #sono i punti che sono identificati sul bordo del confine di quel poligono
  #per riuscire poi a rappresentare il poligono
  ###georef dei bordi ci consente di rappresentare una mappa veritiera 
  ### della mappa. 
   ## geocodifica avvenuta tramite coordinate angolari. 
  
  ##l'informazione contenuta nello slot coords la trova in .shp
  ### questa informazione la apprende da file .shp
  
  #[334,] 16.18338 48.17002
  #vediamo che l'informazione spaziale avviene a livello di punti, di nodi
  #di confine sui poligoni di interesse. 
  #organizzati in formato matriciale e vettoraile, un vettore in una matrice
  
  ##diverso dal formato raster dei gis
  
    #come estrarre informazione dal nostro oggetto?
    
    #voglio estrarre il centroide, ovverro informazione imamgazzianta
    #in testa all'oggetto stesso 
    centr=coordinates(aus.poly)
    #centr è di fatto una matrice con 35 righe e 2 colonne
    #le colonne sono lat e long
    #ogni riga è un area, una provincia diversa. 
    #serve il centroide perche se vogliamo aggiungere del testo es
    #alla mappa allora lo posizioniamo al centro del poligono
    
    #ad esempio stampiamo sulla mappa nel centroide il codice amministratio della 
    #provincia.
    points(centr,col="red")
    text(centr[,1],centr[,2],aus.poly$ID,col="blue")
    text(centr[,1],centr[,2],"CR7",col="blue")
    #text si aspetta --> ascissa, oridnata del punto, testo da riportare nel punto
    

class(aus.poly)  
# [1] "SpatialPolygonsDataFrame"
# attr(,"package")
# [1] "sp"

###la classe che riceve lo shape è un oggetto di classe particolare
  #è un dataframe spazializzato. 



##come faccio a estrarre dei pezzi dallo shape?
###estraggo e metto in xx in poligono con codice ID=AT13

##NB si estrae in modo analogo a come si fa nei dataframe. 
# estrazione sotto shape
  xx<-aus.poly[aus.poly$ID=="AT13",]    		# poligono di Vienna
  plot(xx,add=T,col="blue")
#
  ##costruisco un nuovo sottoshape da shape diversi
  
### adesso viste queste cose teoriche possiamo vedere le cose pratiche che abbiamo fatto 
  #in questo corso. 
  
  
  
# PARTE 3: PP: uso dello shape per definire la finestra di un PPS  -  pacchetto: spatstat ------------

##vediamo funzioni che implementano tecniche della prima settimana di lezione
  
  ##lombardia come regione di interesse. 
  
  ##vediamo se il processo è CSR 
  #e poi vedere la mappa di questo processo
  
  
  #1) costruire oggetto che contiente informazione rilevante per processo di punto
  
  ###--> point pattern
  ###--> regione dove si evolve il processo (supporto)
  
  

  
##librerie che fa tutto per analisi di processo di punto.
  
require(spatstat)    			# carica la libreria 
# vogliamo costruire oggetto che contiene processo di punto 
# incendi_lombardia --> oggetto intemedio che mi serve per costruire oggetto proc punt
  ##contiene lat e long di ogni punto di innesco. 
  ##NB importante ordine, prima ascissa poi ordinata, prima longitudine e poi latitudine. 
  ppp<-incendi[,c("EstN","Nord")]
  #as.ppp tratta quello che gli viene passatto come se fosse un point pattern
  #  prende in ingresso locazione dei punti e supporto e li coombina in un unico elemento
  ppp0=as.ppp(ppp,W=lomb.poly);ppp0 ##informazione combianta
# plot PP
  plot(ppp0,cex=0.5,main=,"Incendi in Lombardia nel 2003")
  ##abbiamo riprodotto la mappettina di cui parlavamo prima
  
  class(ppp0) #[1] "ppp"
  #classe specifica della libreria statspat apposta per fare analisi
  #di punto. 
  ##classe la faccio per poi applicarci dei metodi apposta.
  #as.ppp produce un oggetto di classe ppp
  
  #adesso vediamo come costruire un processo con caratteristiche di marcature
  #mark=estensione incendio.
  #devo riorindare 1) longitudine, 2)latitudine 3)marker
  ##SEMRPE COSI
# PPS marcati con dimensione incendio
  ppp<-incendi[,c("EstN","Nord","Ettari")]
  ###RICOSTRUISCO OGGETTO PPPO con unica differenza che 
  #adesso gli dico qual è il marker da utilizzare
  ###default: mark=Null
  ppp0=as.ppp(ppp,W=lomb.poly,mark=ppp$Ettari);ppp0
  
  #adesso riplotto il file, ha i cerchi grandi come grandezza marcati
  #ho una tematizzazione dei punti, in base alla estensione
  plot(ppp0,
       main=,"Incendi in Lombardia nel 2003 differenziati per estensione"
       )

  # adesso facciamo un test per la CSR!!!

#######Test CSR  
    
##NUOVO DATASET, DATI su difetti nella superficie del wafer  
#
  d<-read.table("WLR300.txt",header=T) 	# lettura dataset esterno in formato testo
  str(d)						                  # breve descrizione variabili
#X,y sono le locazione dei difetti. 
  
  
#plotto i dati, voglio vedere se è CSR
  #W=finestra del progetto, un wafer con 20cm di diametro
  #supporto=cerchio di raggio 960550 micron, circa 10 cm di raggio
  #disc funzione di questa libreria che permette di costruire palle
  ## d-dimensionali. 
  ##NB non solo palle, disc ha un parametro che cambiandolo cambia il 
  #poligono che voglio costruire. (il poligono si fa con owin che specifica poligono
  #non credo con disc)
  
  #c(0,0)--> centrato sullo 0. il mio sistema di riferiemento è il centro del wafer
  
  W <- disc(96950, c(0,0))			# definisce una finestra circolare per il processo
  #funzione ppp: ascisse, ordiante, finestra_proc
  pp<-ppp(d$X,d$Y,window=W); summary(pp) #ci da caratteristiche 
  ##66 punti. av intensioty=  #eventi/AreaSupporto 
  #è gia la stima del parametro lambda se il processo fosse omogeneo!!! 
  ##single connected closed polygon with 128 vertices
  ##approx il cerchio con una superficie di 128 vertici. 
  ##mi riporta area della finestra. Window area = 29516900000 square units
  
  plot(pp,cex=0.6,main="locazioni eventi",cex.main =1.2,lwd=3)


# PARTE 4: PP: Analisi dell'ipotesi CSR per point pattern data -  pacchetto: spatstat --------
  
# test basato sul quadrat counts
  qx<-quadratcount(pp,4,4)	### tabella 4x4
  plot(qx)
  ##con quadratcounts partiziono l'aree del cerchio in cellettine e conto
  #quanti eventi trovo in quell'area. 
  # quadratcounts(oggettoppp, #celleascisse, #celle ordinata)
  
  #quadrat.test--> produce il test basato sul chiquadro. 
  #quadrat test, costuisdce gia dietro oggetto quadrat count. 
  #la chiama implicitamente- 
  te0<-quadrat.test(pp,4); te0					 ###test CSR chi-quadro
  #df=16celle-1=15. test BILATERALE DI RIFIUTO. 
  ##P-VALUE MOLTO PICCOLO --> HP DI CSR NON MI SEMBRA VALIDO. 
  
  # ricostruiamo graficamente come è fatto questo test. 
  plot(te0, col="red", cex=1, lty=1.5, lwd=3,bg = gray(0.7)) ### conteggi, attesi, residui
  #informazione oss in alto a sx
  ###informazione attesa è quella in alto a dx in ogni cella, qui aggiusta 
  #informazione attesa in base all'area della celletta. 
  ##valore in basso è il residuo di pearson (att-oss)/att
  
  #++++---> sono le locazioni e i valori osservati. 
  plot(pp, pch="+", cols="green", cex=1.5, lwd=1.2,add=T)
  
  
  #potremmo rifare il test con #celllette diversi, es 10*10
  qx10<-quadratcount(pp,10,10)	### tabella 10x10
  plot(qx10)
  te10 <- quadrat.test(pp,10)	###test dispersione per 10x10
  te10 #respingo ancora hp nulla. 
  ###df=87 gdl perche? 12 celle sono fuori dalla regione di interesse
  #88-1=87
  
  #r ci da un waning. Numero di eventi troppo basso rispetto al numero 
  #di cellette che abbiamo implementato. 
  ##test quadrat counts TROPPO SOGGETTIVO. !
  
##passiamo quindi a test diversi NON QUADRAT COUNTS , diasgnostiche grafiche
  ##VOGLIAMO VALUTARE LA PRESENZA DI CLUSTERING, prima diagnostica
  #molto grezzo è la distanza dal primo vicino. 
  
# prima valutazione clustering 
# istogramma distanze dal primo vicino
  ##nndist produce distanza dal primo vicino
  hist(nndist(pp),main="distanza dal primo vicino",xlab="distanza")
##coda sx molto alta, quindi ci aspettiamo che il pp sia caratterizzato dal cluster

  
# test basato sulla distribuzione del NN
  ##Gest fa tutto per  test: distribuzione empirica, teorica e grafico di confronto
  ##creaimo un oggetto GGb, di tipo Gest, che poi plotteremo.
  # Per produrre il test GRAFICO. 
  GGb<-Gest(pp, r=NULL, breaks=NULL,correction="none");
  ##correction=None, nessun CORREZIONE PER EFFETTO BORDO. se non lo specifichiamo 
  #fa una correzione automatica
  ##noi faremo sempre none SEMPRE!!!! 
  
  plot(GGb, raw ~ r,main="FR della distanza dal primo vicino",xlab="r")
  plot(GGb, theo~ r,add=T,col=2,lty=3)
  legend(25000, 0.2,  legend=c("F.R. empirica","F.R. teorica"),lty=c(1,3), col=c(1,2),bty="n")
  ##legend (posizione, cosa deve comparire nella legenda. lty è che tipo di curva rappreseta,
  #col il colore)
  
  ###grazie a Gest ho fatto tutto cio che mi interessa per diagnostica grafica
  
  ##vediamo cosa contiene oggetto GGb: contiene tre elementi
  # Math.label     Description                 
  # r    r              distance argument r  
  ##--> r contiene glie elementi delle distanze dove valutiamo 
  ##fz rip empirica e teorica. 
  ###
  # theo G[pois](r)     theoretical Poisson G(r)    
  ##--> in theo fz rip teorica negli r punti
  # raw  hat(G)[raw](r) uncorrected estimate of G(r)
  ## in raw fz rip empirica negli r punti
  
  ###quindi per plottare i grafici plottiamo raw inf z di r e teo in fz di r
  GGb	#   descrizione dell'output
  #   test tipo G FR corretta effetto bordo
  GG<-Gest(pp, r=NULL, breaks=NULL);  
  GG	#   descrizione dell'output
  v<-plot(GG, cbind(rs,theo) ~ theo, main="pp-plot")
  
  ##ADESSO VOGLIAMO FARE INVILUPPO: funzione envelope
#  inviluppo Monte Carlo per test su CSR
	envpp<-envelope(pp,fun=Gest,nsim=100,nrank=,verbose=TRUE,saveall=F)
	##nsim=100, simulo 100 volte montecarlo, faccio 100 simulazione montecarlo
	#e poi calcola minimo e max in ogni punto r per creare la griglia
	#fun=Gest, cosi gli diciamo che vogliamo fz rip per distanza dal primo vicino
  #altro parametro che potrebbe essere di interesse è il parametro saveAll
	#se = T vengono salvate tutte le simulazioni. 
	win.graph()
  	aa<-plot(envpp,main="inviluppo MC",xlab="y")
 	  aa #sintesi del plot, serve per interpretare output righe del grafico 
 	##non serve fare numero nsim troppo alto, converge molto velocemente
 	  
 	  
# PARTE 5: PP: stima kernel dell'intensità: mappe di intensità -  pacchetto: spatstat --------
###vogliamo stimare l'intensita kernel, (stima non parametrica. )
 	  
 	  
 	  # stima intensità 
 	  
 	  #fz di stima la fa tutta density.ppp(oggettopp,lisciamento(il nostro h)
 	  ##di default utilizza la gaussiana standard)
 	  ##--> 15k diventa la deviazione standard nelle due componenti x1,x2
  Z <- density.ppp(pp, 15000)
 	  ##produce mappa
  win.graph()
    plot(Z,main="mappa dell'intensità kernel"); 
    plot(pp,add=T,cex=0.6,col="black")
    ##questa mappa si fa con un grigliato molto fine e su ogni pixel si calcola 
    #intensita, come elemento minimo dell'immagine ce un pixel
    #struttura raster, perche unita minima è un pixel.
    #puntini in rosso (io ho fatto in nero) aggiunto rappresenta locazioni eventi originali
    #####a-----
    ##produce stesso grafico ma 3d
    persp(Z) 
    
    
# adesso facciamo la stessa cosa ma sui pp della LOMBARDIA
    ##--> QUINDI SU UNA FINESTRA MOLTO IRREGOLARE. 
# lettura shape file Lombardia
	lomb.poly<-readShapePoly("lombardia.shp",verbose=TRUE) ### lettura shape file in R
# incendi_lombardia (ricostruisco pp)
	ppp=incendi[,c("EstN","Nord")]
	ppp0=as.ppp(ppp,W=lomb.poly);ppp0
	plot(ppp0,cex=0.5,main=,"Incendi in Lombardia nel 2003")
# stima intensità (faccio analisi di prima)
  Z <- density.ppp(ppp0, varcov=diag( c(var(ppp$EstN),var(ppp$Nord))/16)) 
  win.graph()
    plot(Z,main="mappa dell'intensità kernel"); 
    plot(ppp0,add=T,cex=0.4)
    
    
##Cosa cambia? RISPETTO A PRIMA HO MODIFICATO PARAMETRO DI LISCIAMENTO.    
varcov=diag( c(var(ppp$EstN),var(ppp$Nord))/16); varcov
## sto passandfo non solo una varianza comune, ma una matrice di var cov per le due dimensioni
          # [,1]      [,2]
# [1,] 134128703         0
# [2,]         0 103567809
#sto costruendo una matrice diagonale con parametri di lisciamento
## proporionale alla variabilita della componente. 
## compenso la variabilita nelle direzioni varie con un parametro di 
#lisciamento piu elevato. 

#il /16 serve per aggiustare empiricamente la mappa
##Idea: non sistema di peso isotroPico ma anisotropico che assegna 
#piu variabilita dove ce piu varianza. 

##per scegliere h si potrebbero avere delle funzioni che cross validando 
#trova questo h, solo che tendono a essere troppo locali e produrre quindi 
#mappe troppo locali. 
    
####################################### FINE ###############################################################