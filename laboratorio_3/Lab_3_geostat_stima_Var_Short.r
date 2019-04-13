#  STIMA DEL VARIOGRAMMA TRAMITE METODI NON PARAMETRICI E PARAMETRICI
#  uso delle stime per analisi esplorativa e variografica
#
# pacchetti di R necessari per l'esercitazione
# maps, geoR, sp, gstat, stats, utils
# per controllare i pacchetti disponibili digitare library()
# per avere informazioni sulla singola libreria library(help=nomelibreria). Esempio: library(help=geoR)
#
# Se lo script è digitato da tastiera creare un nuovo file per lo script seguente
# menu file- new file - R script
# salvare il file: 
#	menu file- save as
#	oppure usare icona
#
# PARTE I:   Caricamento librerie e preparazione dati --------------------------------
require(geoR)			# carica il pacchetto geoR
  dat.om<-soja98
	  str(dat.om) 		# numero di osservazioni e variabili 
	  summary(dat.om)	# descrittive per ogni variabile nel data set
  d1<-obj <- dat.om[,c("X","Y","MO")];	summary(obj)
#
#  prepara dati per GeoR 
  dat.om.geo <- as.geodata(obj,coords.col=1:2,data.col=3)		# converte oggetto in classe geodata
	plot(dat.om.geo)
#
# PARTE II:  Analisi variabilità di piccola scala - nuvola del variogramma e variogramma empirico --------------------------------
# nuvola del variogramma 
	plot(variog(dat.om.geo, estimator.type="classical",option="cloud"),cex=0.4)	#variogramma cloud
# variogramma empirico
  dat.om.var <- variog(dat.om.geo,	estimator.type="classical", uvec=14)		
	  dat.om.var	      # interpretare le componenti della lista con l'aiuto dell'help
  plot(dat.om.var)	# variogramma per punti
#
# PARTE III: Stima del variogramma robusta, non parametrica, variogramma empirici direzionali e detrendizzati --------------------------------
# variogramma robusto
  dat.om.var.robust <- variog(dat.om.geo, estimator.type="modulus",uvec=14)	
  par(mfrow=c(1,2))	
	  plot(dat.om.var,main="variogramma empirico",ylim=c(0,110));	
	  plot(dat.om.var.robust,main="variogramma robusto",ylim=c(0,110))
# lisciamento kernel del variogramma
  vario.s <- variog(dat.om.geo, op="sm", band=sd(dat.om.var$u)) 
  
  par(mfrow=c(1,1))	
	  plot(vario.s,type="l",ylim=c(0,110))
	  # ripetere il grafico usando come parametro di lisciamento la sd delle distanza (vedere istruzione per nuvola del variogramma) 
	  points(dat.om.var$u, dat.om.var$v, col=2)	# aggiunge variogramma empirico al plot
# variogrammi direzionali
	vario.0  <- variog(dat.om.geo,  dir=0,    tol=pi/8)
	vario.45 <- variog(dat.om.geo,  dir=pi/4, tol=pi/8)
	vario.90 <- variog(dat.om.geo,  dir=pi/2, tol=pi/8)
	vario.135 <- variog(dat.om.geo, dir=pi*3/4, tol=pi/8)
  par(mfrow=c(1,1))						# ripristina una finestra unica per il grafico	
	  plot(c(0,180),c(0,100),type="n",xlab="distanza",ylab="semivarianza")#,type="p"
	  lines(dat.om.var)
	  lines(vario.0 , col=2,lty=2)
	  lines(vario.45, col=3,lty=3)
	  lines(vario.90, col=4,lty=4)
	  lines(vario.135,col=5,lty=5)
  	legend("topleft", cex=0.8, 
        legend=c("omnidirectional", expression(0 * degree), 
				expression(45 * degree), expression(90 * degree), 
				expression(135 * degree)), lty=c(1:5),col=1:5)
				# notare come il variogramma in direzione ortogonale al trend 
				# sembri meno influenzato da esso confrontare con detrendizazione dei dati
#
# stima del variogramma in presenza di trend
  dat.om.var.notr <- variog(dat.om.geo,trend ="1st", estimator.type="classical", uvec=14)		# variogramma empirico
	plot(dat.om.var.notr)					# variogramma per punti
#
# detrendiziamo solo per componente X
  plot(variog(
		as.geodata(obj=data.frame(obj[,1:2],res=residuals(lm(obj$MO~obj$Y))),
		coords.col=1:2,data.col=3),uvec=14
		),ylim=c(0,60))
# Esercizio:
# detrendizzare manualmente i dati con un trend di primo grado e costuire il variogramma dei residui
# aggiungere per esercizio il variogramma detrendizzato ai variogrammi direzionali
#
# PARTE IV:  Stima parametrica del variogramma tramite LS--------------------------------
  max.dist<-150	# massima distanza per la stima del variogramma
  dat.om.var <- variog(dat.om.geo, estimator.type="classical",max.dist=max.dist)	# variogramma empirico
	plot(dat.om.var, main="valutazione visiva variogrammi")		
	  lines.variomodel(cov.model = "exp", cov.pars = c(25,35), nug =20,
          max.dist=max.dist,col=2,lty=2) 				# aggiunge plot 'teorico' con valori fissati
	  abline(h=45)
# Esercizio: Aggiungere al grafico precedente il variogramma esponenziale con soglia 35 e 45.
#            Aggiungere una legenda al grafico. 
#	           Replicare l'esercizio con funzioni di variogramma diverse: sferico e wave
#
# stima di un variogramma parametrico di tipo esponenziale - metodo WLS
	dat.om.var.fit <- variofit(dat.om.var,ini.cov.pars=c(25,35),
		    cov.model="exponential", fix.nugget=FALSE,nugget=20)
  	summary(dat.om.var.fit)
	  dat.om.var.fit$cov.pars  # stima di  soglia parziale e range 
    dat.om.var.fit$nugget    # stima del nugget
    dat.om.var.fit$value     # somma dei quadrati
#
# confronto grafico tra i variogrammi parametrici stimati e il variogramma empirico
# effetto ponderazione e massima distanza
  dat.om.var <- variog(dat.om.geo, estimator.type="classical")	     # variogramma empirico
	    plot(dat.om.var)
  dat.om.var.fit.ols <- variofit(dat.om.var,
	  	ini.cov.pars=c(25,35),
		  cov.model="exponential", fix.nugget=FALSE,nugget=20,weights = "equal" )
	lines(dat.om.var.fit.ols,col=2, lty=2, lwd=2)	# aggiunge plot stime ols
  dat.om.var.fit.wls <- variofit(dat.om.var,
	  	ini.cov.pars=c(25,35),
		  cov.model="exponential", fix.nugget=FALSE,nugget=20)
	lines(dat.om.var.fit.wls,col=3, lty=3, lwd=3)		# aggiunge plot stime WLS
  dat.om.var.fit.ols.MD<- variofit(dat.om.var, ini.cov.pars=c(25,35), 
	  		cov.model="exponential", fix.nugget=FALSE,nugget=20, 
		  	weights="equal", max.dist=max.dist)
	lines(dat.om.var.fit.ols.MD,col=4, lty=4, lwd=2)		# aggiunge plot stime OLS
	dat.om.var.fit.wls.MD<- variofit(dat.om.var, ini.cov.pars=c(25,35), 
			cov.model="exponential", fix.nugget=FALSE,nugget=20, 
			max.dist=max.dist)
	lines(dat.om.var.fit.wls.MD,col=5, lty=5,lwd=3)		# aggiunge plot stime OLS
	legend(15,20,c("variogramma empirico",
				"variogramma ols - modello esponenziale",
				"variogramma wls - modello esponenziale",
				paste("variogramma ols - modello esponenziale max dist=",
				max.dist,sep=""),
				paste("variogramma wls - modello esponenziale max dist=",
				max.dist,sep="")),
				lty=1:5, col=1:5,cex=0.9,bty="n")
# confronto fra modelli
	round(rbind(
  	wls.maxdist150=summary(dat.om.var.fit.wls.MD)$estimated.pars,
	  ols.maxdist150=summary(dat.om.var.fit.ols.MD)$estimated.pars,
	  wls.maxdist=summary(dat.om.var.fit.wls)$estimated.pars,
	  ols.maxdist=summary(dat.om.var.fit.ols)$estimated.pars
	),1)
#
# stima modello sferico
  dat.om.var.fit.sphe.ols <- variofit(dat.om.var,ini.cov.pars=c(35,70),
	  	cov.model="spherical", fix.nugget=FALSE, nugget=10, weights = "equal",
		  max.dist=max.dist)
  plot(dat.om.var)
  lines(dat.om.var.fit.sphe.ols,lwd=2)
# confronto fra modelli
  round(
	  rbind(
		  modello.sferico=c(summary(dat.om.var.fit.sphe.ols)$estimated.pars,
		     range.effettivo=summary(dat.om.var.fit.sphe.ols)$practicalRange,
	  	   SumSquares=summary(dat.om.var.fit.sphe.ols)$sum.of.squares)
		  ,
		  modello.esponenziale=c(summary(dat.om.var.fit.ols.MD)$estimated.pars,
		     range.effettivo=summary(dat.om.var.fit.ols.MD)$practicalRange,
		     SumSquares=summary(dat.om.var.fit.ols.MD)$sum.of.squares)
	   ),
	  digits=1)
#
# Esercizio: riportare su uno stesso grafico i due variogrammi, sferico ed esponenziale stimati
# tramite ols insieme al variogramma empirico
#
# prima di procedere provate questa
  par(mfrow=c(1,1))
	eyefit(dat.om.var)	# potrebbe non funzionare in alcune versioni di R
#
# PARTE V:   Stima parametrica del variogramma tramite ML--------------------------------
	dat.om.lik.fit <- likfit(dat.om.geo,ini.cov.pars=c(35,46),
		cov.model="exponential",trend="cte", fix.nugget=FALSE,nugget=20,
		nospatial=FALSE,messages=T)
	print(dat.om.lik.fit)
	summary(dat.om.lik.fit)
#
  par(mfrow=c(1,1))
    plot(dat.om.var)
	  lines(dat.om.var.fit,col=2, lty=2)		# aggiunge plot stime WLS
	  lines(dat.om.lik.fit,col=5, lty=3)    # aggiunge plot stime ML
	  legend("bottomright",c("variogramma empirico","variogramma WLS","variogramma ML"),
	  col=c(1:3),lty=c(1:3))
#
# PARTE VI:  Valutazione della variabilità del variogramma via MC e test grafici via permutazione--------------------------------
# idea: si simulano campioni da traiettorie di struttura note 
#       fissando i parametri del variogramma ai valori stimati (wls)
#       e si calcolano i variogramma empirici su dati simulati 
#
# parametri usati per simulare le traiettorie ottenuti via WLS
  alpha=dat.om.var.fit.wls$nugget		      # nugget
  phi<-dat.om.var.fit.wls$cov.par[2]	    # range
  C0=dat.om.var.fit.wls$cov.par[1]+alpha	# soglia
  # esempio con 4 simulazioni
  set.seed(123)
  n<-200		#### numero siti di "rilevazione"
  m<-grf(n=n, cov.pars=dat.om.var.fit.wls$cov.par,nsim=4,nugget=alpha,xlim=c(0,150),ylim=c(0,115))
  win.graph();	par(mfrow=c(2,2))
  for(i in 1:4){
	  dat3<-data.frame(m$coords,z=m$data[,i])		       # ###- nuovo "campione" con autocorrelazione data
	  dat3.geo <- as.geodata(dat3,coords.col=1:2,data.col=3) # converte oggetto in classe geodata
	  dat3.z.vario <- variog(dat3.geo, estimator.type="classical")
	  plot(dat3.z.vario)
	  lines.variomodel(cov.model = "exp", cov.pars = c(C0-alpha,phi), nug =alpha,
		  max.dist=max.dist ,col=2,lty=2)  # parametrizzazioe del covariogramma come richiesto da geoR
    }
# valutazione della variabilità del variogramma win.graph();	par(mfrow=c(1,1))
  par(mfrow=c(1,1))
  dat.om.env.mod <- variog.model.env(dat.om.geo, 
					obj.v = dat.om.var, model.pars =dat.om.var.fit)	#simulazione da modello
 	plot(dat.om.var, env =dat.om.env.mod )
	lines(dat.om.var.fit,col=2, lty=2)
#
# valutazione della correlazione spaziale - test grafico via permutazione
  dat.om.env <- variog.mc.env(dat.om.geo, obj.var = dat.om.var,save.sim=T)	
  plot(dat.om.var, envelope = dat.om.env)