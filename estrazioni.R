###############################################################################
#  23-nov-2017   MR
#  25-lug-2019   MR dockerizzato
#
#  segnalazione delle discrepanza tra le info contenute nella tabella METEO.A_Sensori2Destinazioni
#  e le info contenute nel file estratto dal REM AnagraficaEstrazioni.csv
#
#====== librerie
library(DBI)
library(RMySQL)
#library(RODBC)

#=========  funzioni
#+ gestione dell'errore
neverstop<-function(){
  print("EE..ERRORE durante l'esecuzione dello script!! Messaggio d'Errore prodotto:\n")
  quit()
}
options(show.error.messages=TRUE,error=neverstop)
#=========== file out
fileout<-"diff_estrazioni.out"


cat("ESTRAZIONI DAL REM2   ", date(), "\n\n",file=fileout)

#cat( "leggo file estrazioni dal DBunico\n", file=fileout,append=TRUE)

REM2 <- read.csv ( "AnagraficaEstrazioni.csv" , header = TRUE ,  as.is = TRUE, sep=";")
print("lettura file anagraficaestrazioni")
#
#
#==============================================================================
#cat( "leggo sensori del DBmeteo su cui verificare le destinazioni\n", file=fileout,append=TRUE)

#------------------------------------------------------------------------------
#MySQL(max.con=16,fetch.default.rec=500,force.reload=FALSE)
#definisco driver
drv<-dbDriver("MySQL")
#apro connessione con il db descritto nei parametri del gruppo "Gestione"
#nel file "/home/meteo/.my.cnf
#conn<-try(dbConnect(drv,group="Visualizzazione"))
conn<-try(dbConnect(drv, user=as.character(Sys.getenv("MYSQL_USR")), password=as.character(Sys.getenv("MYSQL_PWD")), dbname=as.character(Sys.getenv("MYSQL_DBNAME")), port=as.numeric(Sys.getenv("MYSQL_PORT")),host=as.character(Sys.getenv("MYSQL_HOST"))))
if (inherits(conn,"try-error")) {
  print( "ERRORE nell'apertura della connessione al DBmeteo \n")
  print( " Eventuale chiusura connessione malriuscita ed uscita dal programma \n")
  dbDisconnect(conn)
  rm(conn)
  dbUnloadDriver(drv)
  quit(status=1)
}
#
# estraggo tutta l'anagrafica DBmeteo
anagraficaDBmeteo <- try(dbGetQuery(conn, "select IDsensore , NOMEtipologia , CONCAT(Comune,' ',IFNULL(Attributo,'')) as Nome from A_Sensori, A_Stazioni where A_Stazioni.IDstazione=A_Sensori.IDstazione"),silent=TRUE)
# sostituisco gli attributi NA con stringa vuota (perchè non compaia NA nei nomi)
#anagraficaDBmeteo$Attributo[is.na(anagraficaDBmeteo$Attributo)==T]<-""
#
# individuo nel file di estrazione del REM l'indice dei record relativi a sensori presenti nel DBmeteo
sensori_DBmeteo<-try(dbGetQuery(conn, "select IDsensore from A_Sensori"),silent=TRUE)
ii <- which(REM2$X...Idsensore %in% sensori_DBmeteo$IDsensore)
#

#################################
# ciclo sulle destinazioni nel file

# ricavo numero massimo di destinazioni REM
nmax <- max(array(as.numeric(unlist(strsplit(colnames(REM2[3:length(REM2)]),"X")))),na.rm=TRUE)
print(nmax)
n <- 1
while(n < nmax + 1){  #
#
# info da REM2
REM2_idSensore <- REM2$X...Idsensore[ii]
REM2_colonna <- eval(parse(text=paste("REM2$X",n,"[ii]",sep="")))
REM2_NOMEcolonna <- eval(parse(text=paste("REM2$X",n,"[1]",sep="")))

 # leggo destinazioni nel DBmeteo
 destinazione<-try(dbGetQuery(conn, paste("select IDdestinazione,Destinazione,IDdestinazioneREM,DestinazioneREM from A_Destinazioni where IDdestinazioneREM =",n,sep="")),silent=TRUE)

if (is.null(REM2_colonna) == T){#se non c'è nel file del REM
 if(length(destinazione$IDdestinazione) == 0) {#se non c'è nemmeno nel DBmeteo 
  #nulla
 }else{
  cat ( "\n\n ------  destinazione numero", n, " non esiste nel file AnagraficaEstrazioni.csv     -----\n",file=fileout,append=T)
 }
}else{
 # se destinazione esiste filtro sulle S
 ss <- which(gsub(" ","",REM2_colonna)=="S")
 REM2_idSensore_destinazione<-REM2_idSensore[ss]
 # cerco destinazione corrispondente nel DBmeteo
 if (n!=3 && (length(destinazione$IDdestinazione) == 0)){ # se non si tratta della importazione dati nel DBmeteo ma non è in tabella A_Destinazioni
  # non esiste nel DBmeteo.
  if(length(REM2_idSensore_destinazione)>0)cat( " \n\n ATTENZIONE  # REM: ID=",n," " , REM2_NOMEcolonna, " - NON ESISTE NEL DBMETEO MA COMPRENDE ", length(REM2_idSensore_destinazione), " SENSORI PRESENTI NEL DBMETEO  \n\n",file=fileout,append=T)
 }else{
  if(n==3){  # se si tratta della importazione dati nel DBmeteo leggo la flag Importato
  cat ( "   # ID_REM=",n," (", destinazione$DestinazioneREM, ") - importazione dati nel DBmeteo!!!",file=fileout,append=T)
  query <- "select IDsensore from A_Sensori where Importato = 'Yes'"
  }else{ # se non si tratta della importazione dati nel DBmeteo leggo destinazioni A_Destinazioni
  cat ( "   # ID_REM=",n," (", destinazione$DestinazioneREM, ") - ID_DBmeteo=",destinazione$IDdestinazione," (",destinazione$Destinazione, ") - ",file=fileout,append=T)
  query <- paste("select * from A_Sensori2Destinazione where Destinazione =", destinazione$IDdestinazione," and DataFine is null",sep="")
   }
  #
  DBmeteo<-try(dbGetQuery(conn, query),silent=TRUE)
  #
  if (length(DBmeteo$IDsensore)==length(REM2_idSensore_destinazione)){
   cat ( "n. sensori ", length(DBmeteo$IDsensore),"\n" ,file=fileout,append=T)
  }else{
   cat ( "n. da DBmeteo=", length(DBmeteo$IDsensore), "- n. da REM2=", length(REM2_idSensore_destinazione),"-",file=fileout,append=T)
  #
  indice_REM2<-which(!REM2_idSensore_destinazione %in% DBmeteo$IDsensore)
  indice_DBmeteo<-which(!DBmeteo$IDsensore %in% REM2_idSensore_destinazione)
  #
  cat("\n       n. solo nel REM2:=",length(indice_REM2) ,file=fileout,append=T)
  i<-1
  while(i<length(indice_REM2)+1){
   cat("\n       ",REM2_idSensore_destinazione[indice_REM2[i]],"-",anagraficaDBmeteo$NOMEtipologia[anagraficaDBmeteo$IDsensore==REM2_idSensore_destinazione[indice_REM2[i]]], " ", anagraficaDBmeteo$Nome[anagraficaDBmeteo$IDsensore==REM2_idSensore_destinazione[indice_REM2[i]]],   file=fileout,append=T)
   i<-i+1
  }
  #
  cat("\n       n. solo nel DBmeteo:=",length(indice_DBmeteo) ,"\n",file=fileout,append=T)
  i<-1
  while(i<length(indice_DBmeteo)+1){
   cat("      ",DBmeteo$IDsensore[indice_DBmeteo[i]],"-",anagraficaDBmeteo$NOMEtipologia[anagraficaDBmeteo$IDsensore==DBmeteo$IDsensore[indice_DBmeteo[i]]], " ", anagraficaDBmeteo$Nome[anagraficaDBmeteo$IDsensore==DBmeteo$IDsensore[indice_DBmeteo[i]]], "\n", file=fileout,append=T)
   i<-i+1
  }
 }
 }#fine if(is.null(destinazione$IDdestinazione) == T){
}#fine if(is.null(REM2_colonna) == T)
n <- n + 1
}  #fine while

cat ( "\n\nChiusura connessione DBmeteo ed uscita dal programma con successo!!! ",date(),file=fileout,append=T)
dbDisconnect(conn)
rm(conn)
#dbUnloadDriver(drv)
warnings()
quit(status=0)
