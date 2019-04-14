#Esercitazione 4 Argomento: Previsione kriging

# Parte I:    preparazione dati e librerie -----------------------------
  require(geoR)			# carica il pacchetto geoR
  dat.om<-soja98; str(dat.om)
  obj <- dat.om[,c("X","Y","MO")]
  summary(obj)

  
# Parte II:   stima del variogramma -----------------------------
  attach(obj)  # aggiunge il dataset alla sessione di lavoro

# stima variogramma via wls
  soja.geo <- as.geodata(obj,coords.col=1:2,data.col=3)	# converte oggetto in classe geodata
  soja.var <- variog(soja.geo, estimator.type="classical")	# variogramma empirico
	plot(soja.var, main="variogramma empirico")			# variogramma per punti
  soja.var.fit <-variofit(soja.var,ini.cov.pars=c(25,35),weights="cressie",
		cov.model="exponential", fix.nugget=FALSE,nugget=20)
  lines(soja.var.fit);	soja.var.fit
	 a<-soja.var.fit$cov.pars 
	 #stima del variogramma in funzione della previsione kriging che andremo a fare

	 
# Parte III:  preparazione griglia di previsione -----------------------------
# griglia di spaziatura size x size su X e Y
	 #alla fine otterremo una mappa e dobbiamo stimarla discretizzando in pixel
  size=30
	YY<-round(seq(ceiling(min(Y)),floor(max(Y)), length=size),2) #seq tra min e max delle orinate
	#creo un grigliato che copre esattamente il campo di variazione
	XX<-round(seq(ceiling(min(X)),floor(max(X)), length=size),2) #come per ordinate
	griglia<-expand.grid(X=XX,Y=YY)
	dim(griglia) #ordinate e ascisse dei 900 quadrati della griglia 30x30
	plot(griglia)
	#non abbiamo uno shape, dobbiamo creare noi lo spazio in cui ci sono info
	

	# Parte IV:   previsione - kriging ordinario -----------------------------
	#applichiamo il kriging ad ogni punto di questa griglia, per ogni punto calcoliamo
	#la previsione come comb lineare di tutti i punti campionari
	#per 900 volte calcoliamo inversa matrice Gamma e vettore gamma
  krg.or <- krige.conv(geodata=soja.geo, 
	      		  loc=griglia,
			        krige=krige.control(cov.pars=a,
				              cov.model="exponential",
				              nugget=soja.var.fit$nugget))		#in geoR
	#dati, locazioni delle previsioni (griglia), krige:struttura di correlazione costruita preliminarmente
	#krige.control: stime soglia parziale e range salvate in a
	#modello di variogramma (esponenziale), 
	#valore stimato del nugget (soja.var.fit lista in cui c'? anche nugget)
	#dobbiamo mettere tutti gli elementi che servono a risolvere il sistema.
	#quindi come ? fatto il variogramma come forma e quali sono i suoi parametri
	
	#krige.conv: model with constant mean
	#krige.conv: Kriging performed using global neighbourhood 
	#processo stazionario in media e ce lo dice
	#kriging che usa vicinato di tipo globale, usa tutti i punti campionari
	#con questa funzione non si pu? fare krig locale
	#se il campione ? molto grande per? ? un problema
	names(krg.or)
	#predict contiene le previsioni
	#krige.var stima varianza kriging, per ogni punto abbiamo l'errore commesso
	#beta.est stima del parametro mu delle medie
	#gli altri non ci interessano
	
	#prev krig derivata con assunzione di normalit? 
	
# rappresentazione grafica della previsione (OBIETTIVO:mappa)
  win.graph()
  par(cex=0.5,mfrow=c(2,2))
  #primo grafico: image e contour
    	image(krg.or,xlim=c(-15,floor(max(X))),ylim=c(0,120)) 
	    legend.krige(x.leg=c(-13,-8), y.leg=c(30,100), val=krg.or$predict,
		    vert=TRUE, off=0.7, cex=0.9, scale.vals=pretty(krg.or$predict)
		    )
	    #image: oggetti: risultato kriging, estrae previsioni e le riporta sul grafico
	    #xlim e ylim dicono dove rappresentare
	    #legend.krige costruisce legenda da solo argomenti: dove posizionarla (x.leg e y.leg)
	    #in val valori della variabile plottata, scale.vals specifichiamo 
	    #come sono ripartite le fasce di valore
    	contour(krg.or,add=T,coords.data=soja.geo$coords,cex=0.3)
    	#aggiunge curve di livello, coords.data permette di specificare coordinate punti di rilevazione
    	#riporta quindi siti di campionamento, se ? troppo fitto ? meglio di no
	persp(krg.or,xlab="longitudine",ylab="latitudine",zlab="log-catch",ticktype="detailed") #3d
	#terzo grafico
	image(krg.or, val=sqrt(krg.or$krige.var), main="kriging std. errors",
		xlim=c(min(X),floor(max(X))))
	legend.krige(x.leg=c(-13,-8), y.leg=c(30,100), val=krg.or$krige.var,
		vert=TRUE, off=0.7, cex=0.9, scale.vals=pretty(krg.or$krige.var)
	)
	#plotto non i predict ma la componente krige.var
	#rosso basso, molto vicino a 0
	#mappa liscia, errore di prevision molto basso
	#i siti di previsione coprono in modo abbastanza omogeneo la superficie da prevedere
	#dato che ? un previsore esatto non c'? errore di previsione se prevedo sul sito di camp
	#grigliato di previsione molto simile ai punti campionari
	#non ci sono zone distanti dai punti campionati in cui avremmo errore pi? alto
	points.geodata(soja.geo,pt.divide="data.proportional")
  par(cex=1)
  #4 grafici:
  #1,1 grafico della previsione con sfumature di colore, rosso bassi, bianco elevati
  #1,2 stessa cosa in 3d
  #2,1 superficie stimata delle varianze del kriging
  #2,2 rappresentazione dei punti campionari usati per la stima, 
  #sono pi? grandi quanto pi? ? alto il valore rilevato l?
  #possiamo  vedere se ? coerente con le previsioni fatte sulla base di quelle osservazioni

  
# Parte V:    previsione - kriging universale -----------------------------
  #prima soluzione dell'ultima slide
  #si detrendizza e in un secondo momento si stima il variogramma
  obj.var.u <- variog(soja.geo, estimator.type="classical",
		trend="2nd")		# variogramma empirico su dati detrendizzati da geor
  #stima regressione, calcola y cappello, calcola residui e calcola 
  #variogramma su di essi
  obj.var.fit.u <- variofit(obj.var.u,
	  ini.cov.pars=c(10,40),	cov.model="exponential", fix.nugget=FALSE,nugget=15)
  #stimo con minimi quadrati
  par(cex=0.5,mfrow=c(1,1))
  plot(obj.var.u)
  lines(obj.var.fit.u)
  
  krg.uk <- krige.conv(geodata=soja.geo, 
        		loc=griglia,
        		krige=krige.control(type.krige = "ok",#universale ma si deve mettere ok
                            		trend.d="2nd",
                            		trend.l="2nd",
        		                    #trend.d e trend.l specificano il trend che vogliamo usare nel ku
        		                    #polinomio di secondo grado completo di latitudine e longitudine
        		                    #sia cos? che elevate al quadrato
        		                    #serve anche per kriging esterno, potrenbbe esserci covariata ulteriore
        		                    #sarebbe da specificare in trend.l
        		                    #qua ? inutile ma ? da specificare uguale all'altra
                            		cov.pars=obj.var.fit.u$cov.pars,
                            		nugget=obj.var.fit.u$nugget,
                            		cov.model="exponential"
        		))
  #previsione
  #modello con fz delle medie polinomio di secondo ordine
  #il resto ? uguale che nel krig ordinario
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
	#stessi grafici di prima
	#mappa se meno omogenea di prima
# stima della media
  krg.uk$beta		
  krg.or$beta
  #due componenti beta. stime dei paraemtri della media
  #per quello ord e per quello univ
  #stima con trend polinomiale ha 6 coeff perch? ? di secondo grado

  
# Esercizio. Costruire la mappa detrendizzando i dati tramite una regressione polinomiale e
#            krigando i residui. Ricostruire poi la mappa sommando alla previsione kriging il trend 
#            sulla griglia di previsione

  
# Parte VI:   cross validazione via kriging ordinario -----------------------------
  date()	#--- ora inizio elaborazione
  #funzione che implementa da sola la cross validazione
  #non prevediamo con variogramma ma su di esso si basa la previsione krig 
  #quindi se variogramma fa schifo previsione fa schifo
  xvalid.notrend<-xvalid(soja.geo, coords = soja.geo$coords, data = soja.geo$data,
	      model=soja.var.fit)
  #lista di tante componenti
  date()	#--- ora fine elaborazione
#
  par(mfrow=c(1,1))
  hist(xvalid.notrend$error,main="istogramma residui",xlab="residui")
  #error residui di cross validazione
  #centrati su 0
  plot(xvalid.notrend$data,xvalid.notrend$predicted,ylab="previsione",xlab="osservazioni")
  #i dati disponibili verso le osservazioni previste sui siti campionari+
  #correlazione tra osservati e previsti e ci fa piacere
  plot(xvalid.notrend$predicted,xvalid.notrend$error,ylab="residui",xlab="previsione")
  #valori previsti vs errori
  #sparpagliato, ci piace perch? non c'? andamento diverso a seconda della risposta, no eteroschedasticit?
  plot(xvalid.notrend$data,xvalid.notrend$error,ylab="residui",xlab="osservazioni")
	#residui vs osservazioni, ci aspetteremmo che i residui siano belli sparpagliati
  #indipendentemente da ci? che osservo l'errore ? casuale
  #kriging invece sottostima valori all'estremo inferiore e sovrastima valori piccoli
  #non ci sorprende perch? la var del kriging comprime la varianza, dove ho alta var il krig tende a ridurla e viceversa
  #effetto di smoothing che mi aspetterei
  #k sottostima valori bassi e sovrastima quelli alti
  #vale la pena fare loocv? togliere una sola osservazione?
  #si perch? il kriging ? un previsore esatto, la cv permette di fare diagnostica 
  #perch? altrimenti riproduco le oss e non avrei residui da trattare
  
  # effetto di smoothing
  win.graph()
    n<-length(xvalid.notrend$error)
    points.geodata(coords=soja.geo$coords,data=xvalid.notrend$error,main="mappa dei residui",
	    pt.divide="data.proportional",col=gray(1:n/n))
    text(x=soja.geo$coords, labels=round(xvalid.notrend$error,2),cex=0.7,col=2,adj=c(0,1))
	  # aggiunge i valori al plot
    
  #per ogni sito campionario valore del residuo
    #pallino pi? grande quanto pi? ? grqnde il residuo
    #utile per capire dove sulla mappa si fanno gli errori
    #dove osservo pallini grandi so che la previsione in quella regione ? particolarmente distorta
    #i residui si possono rip?ortare sulla mappa, utile!
    #dove l'errore ? piccolo posso anche buttare via l'osservazione
    #dove ? alto no, l'osservazione ? utile perch? se la tolgo sbaglio molto
    
    #cv per valutare previsioni
    #si pu? usare per confrontare modelli alternativi anche
# misure di fit 
    cor(xvalid.notrend$data,xvalid.notrend$predicted)
    mean(xvalid.notrend$error^2)
    #eqm che si pu? imputare alla scelta di covariogramma o variogramma
    #ripetendo per altre scelte si pu? fare un confronto
    
    
# Parte VII:  griglie e kriging dentro shapefile -----------------------------
    #qualit? dell'acqua in termini di concenntrazione di arsenico
    #dobbiamo restringere la previsione nella regione di interesse
    
# lettura dati e stima variogramma
    a=read.table("dati_krg.csv",header=T)
    obj  <- as.geodata(a,coords.col=1:2,data.col=3)	
    vario <- variog(obj, estimator.type="classical")	# variogramma empirico
    plot(vario, main="variogramma empirico")			# variogramma per punti
    vario.fit <-variofit(vario,ini.cov.pars=c(30,10000),weights="cressie",
                         cov.model="exponential", fix.nugget=FALSE,nugget=0)
    lines(vario.fit);	
    cov.pars=vario.fit$cov.pars#salvo output
# lettura shape
    library(maptools)
    library(geoR)
    poly<-readShapePoly("acquif.shp",verbose=TRUE) 
        plot(poly,col="lightblue")
    # ricodifica del bordo come matrice  serve in krige.conv  
    library(ggplot2)
    poligono <- fortify(poly)#produce un df in cui forza l'info contenuta nello shape
    #i nodi e altro, ci interessano solo lat e long
    p=as.matrix(poligono[,c("long","lat")])#costruisco matrice coordinate
# griglia e kriging stessa gi? usata
    X=bbox(poly)[1,]
    Y=bbox(poly)[2,]
    size=100#con 500 pixel pi? precisi
    YY<-round(seq(ceiling(min(Y)),floor(max(Y)), length=size),2)
    XX<-round(seq(ceiling(min(X)),floor(max(X)), length=size),2)
    griglia<-expand.grid(X=XX,Y=YY)
    dim(griglia)
    plot(poly)
    points(griglia,cex=0.1)
    #la griglia si sovrappone alla regione di interesse
    # plot 
    krg.or <- krige.conv(geodata=obj, loc=griglia,
                         krige=krige.control(cov.pars=cov.pars,
                                             cov.model="exponential",
                                             nugget=0),
                         borders=p)#dobbiamo mettere i contorni del poligono in cui vogliamo la previsione
    #2 colonne e k righe, k punti dislocati sul poligono
    
    #graficamente
    win.graph()
    image(krg.or) 
    legend.krige(x.leg=c(1575000, 1580000), y.leg=c(4986785, 5010000), val=krg.or$predict,
                 vert=TRUE, off=0.7, cex=0.9, scale.vals=pretty(krg.or$predict)
    )
    contour(krg.or,add=T,coords.data=a$coords,cex=0.3)
    points.geodata(obj,add=T,pt.divide="quintiles", col=1:5)
    
    