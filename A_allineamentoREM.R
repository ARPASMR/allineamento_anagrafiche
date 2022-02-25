###############################################################################
#  22-luglio-2019   MR  
#
# verifica e segnala eventuali disallineamenti tra le tabelle di anagrafica del REM e del DBmeteo
#
#==============================================================================
library(DBI)
library(RMySQL)
#==============================================================================
#+ gestione dell'errore
neverstop<-function(){
  print("EE..ERRORE durante l'esecuzione dello script!! Messaggio d'Errore prodotto:\n")
  quit()
}
options(show.error.messages=TRUE,error=neverstop)

dir_output<-"./"
dir_anagrafica<-"./"
fileout<-paste(dir_output,"generico.txt",sep="")

fileout_ID<-paste(dir_output,"checkIDSTAZIONE.txt",sep="")
fileout_NOME<-paste(dir_output,"checkNOME.txt",sep="")
fileout_LOC<-paste(dir_output,"checkLOC.txt",sep="")
fileout_ATT<-paste(dir_output,"checkATT.txt",sep="")
fileout_COMUNE<-paste(dir_output,"checkCOMUNE.txt",sep="")
fileout_PROVINCIA<-paste(dir_output,"checkPROVINCIA.txt",sep="")
fileout_BACINO<-paste(dir_output,"checkBACINO.txt",sep="")
fileout_FIUME<-paste(dir_output,"checkFIUME.txt",sep="")
fileout_RETE<-paste(dir_output,"checkRETE.txt",sep="")
fileout_QUOTA<-paste(dir_output,"checkQUOTA.txt",sep="")
fileout_COORD<-paste(dir_output,"checkCOORD.txt",sep="")
fileout_TIP<-paste(dir_output,"checkTIPOLOGIA.txt",sep="")
fileout_DATAI<-paste(dir_output,"checkDATAI.txt",sep="")
fileout_DATAF<-paste(dir_output,"checkDATAF.txt",sep="")
fileout_STORICO<-paste(dir_output,"checkSTORICO.txt",sep="")
fileout_FREQUENZA<-paste(dir_output,"checkFREQUENZA.txt",sep="")

fileout_destinazioni<-paste(dir_output,"PC_e_FormWeb.out",sep="")

cat(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><  ESITO DEI CHECK GENERICI\n\n",file=fileout)
cat(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><  ESITO DEI CHECK sui ID \n\n",file=fileout_ID)
cat(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><  ESITO DEI CHECK sui NOMI \n\n",file=fileout_NOME)
cat(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><  ESITO DEI CHECK sulle LOCALITA \n\n",file=fileout_LOC)
cat(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><  ESITO DEI CHECK sull ATTRIBUTO \n\n",file=fileout_ATT)
cat(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><  ESITO DEI CHECK sui COMUNI \n\n",file=fileout_COMUNE)
cat(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><  ESITO DEI CHECK sulle PROVINCE \n\n",file=fileout_PROVINCIA)
#cat(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><  ESITO DEI CHECK sulle BACINO \n\n",file=fileout_BACINO)
cat(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><  ESITO DEI CHECK sulle FIUME \n\n",file=fileout_FIUME)
cat(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><  ESITO DEI CHECK sulle RETI \n\n",file=fileout_RETE)
cat(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><  ESITO DEI CHECK sulla QUOTA \n\n",file=fileout_QUOTA)
cat(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><  ESITO DEI CHECK sulle COORDINATE \n\n",file=fileout_COORD)
cat(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><  ESITO DEI CHECK sulle TIPOLOGIE \n\n",file=fileout_TIP)
cat(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><  ESITO DEI CHECK sulla DATA MINIMA \n\n",file=fileout_DATAI)
cat(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><  ESITO DEI CHECK sulla DATA MASSIMA \n\n",file=fileout_DATAF)
cat(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><  ESITO DEI CHECK sulla flag di STORICO \n\n",file=fileout_STORICO)
cat(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><  ESITO DEI CHECK sulla FREQUENZA \n\n",file=fileout_FREQUENZA)

cat ( " ------   invii a PROTEZIONE CIVILE e disponibilità in FORM WEB   -----\n",file=fileout_destinazioni)

#==============================================================================
#   LEGGO INFO DEL REM 
#==============================================================================
####   per ripulire eventuali caratteri invisibili.....
file_anagrafica <- paste(dir_anagrafica,"AnagraficaSensori.csv",sep="")
#cat file_anagrafica | tr -d '\277' | tr -d '\273' | tr -d '\357' > file_anagrafica
REM2 <- read.csv ( file_anagrafica , header = TRUE , dec=",", quote="\"", as.is = TRUE, sep=";", na.strings = c("-9999","")) 

#==============================================================================
#   LEGGO INFO DEL DBmeteo 
#==============================================================================

#MySQL(max.con=16,fetch.default.rec=500,force.reload=FALSE)
drv<-dbDriver("MySQL")
conn<-try(dbConnect(drv, user=as.character(Sys.getenv("MYSQL_USR")), password=as.character(Sys.getenv("MYSQL_PWD")), dbname=as.character(Sys.getenv("MYSQL_DBNAME")), port=as.numeric(Sys.getenv("MYSQL_PORT")),host=as.character(Sys.getenv("MYSQL_HOST"))))
if (inherits(conn,"try-error")) {
  print( "ERRORE nell'apertura della connessione al DBmeteo \n")
  print( " Eventuale chiusura connessione malriuscita ed uscita dal programma \n")
  dbDisconnect(conn)
  rm(conn)
  dbUnloadDriver(drv)
  quit(status=1)
}

stazioni_eccezioni <- try(dbGetQuery(conn,"select IDstazione from A_Stazioni_eccezioni"), silent=TRUE)
IDstaz_eccezioni <- stazioni_eccezioni$IDstazione
sensori_eccezioni <- try(dbGetQuery(conn,"select IDsensore from A_Sensori_eccezioni"), silent=TRUE)
IDsens_eccezioni <- sensori_eccezioni$IDsensore

tipologie_idrometeo_DBmeteo <-try(dbGetQuery(conn,"select Nome , IdTipologia , Info  from A_Tipologia"), silent=TRUE)
tipologie_eccezioni_DBmeteo <-try(dbGetQuery(conn,"select IdTipologia , Info  from A_Tipologia_eccezioni"), silent=TRUE)
tipologieDBmeteo <- c(tipologie_idrometeo_DBmeteo$IdTipologia,tipologie_eccezioni_DBmeteo$IdTipologia)
# identifico eventuali discrepanze tra le tipologie nel REM e quelle nel DBmeteo
aux<-which(!(REM2$IdTipologia %in% tipologieDBmeteo))
 if (length(REM2$IdTipologia[aux])>0) {
  cat("\n tipologie presenti nel REM ma non nel DBmeteo\n", file=fileout,append=T)
  iii<-1
  while(iii<length(REM2$IdTipologia[aux])+1){
   cat(REM2$IdTipologiai[aux])
  iii <- iii+1
  }
 }else{
  cat("tutte le tipologie del REM sono presenti nel DBmeteo\n", file=fileout,append=T)
 }

aux<-which(!(tipologieDBmeteo %in% REM2$IdTipologia))
 if (length(tipologieDBmeteo[aux])>0) {
  cat("\n tipologie presenti nel DBmeteo ma non nel REM\n", file=fileout,append=T)
  iii<-1
  while(iii<length(tipologieDBmeteo[aux])+1){
   cat(tipologieDBmeteo[aux])
  iii <- iii+1
  }
 }else{
  cat("tutte le tipologie del DBmeteo sono presenti nel REM\n", file=fileout,append=T)
 }



# da qui in poi lavoro sulle tipologie di interesse idrometeo
oo<-which(REM2$IdTipologia %in% tipologie_idrometeo_DBmeteo$IdTipologia)
REM2_IdReteVis <- REM2$IdReteVis[oo]
REM2_NomeReteVis <- REM2$NomeReteVis[oo]
REM2_idStazione <- REM2$idStazione[oo]
REM2_Nome <- REM2$Nome[oo]
REM2_IdTipologia<-REM2$IdTipologia[oo]
REM2_idSensore <- REM2$IdSensore[oo]
REM2_Storico <- REM2$Storico[oo]
REM2_Comune <- REM2$Comune[oo]
REM2_Localita <- REM2$Localita[oo]
REM2_Attributo <- REM2$Attributo[oo]
REM2_Provincia <- REM2$Provincia[oo]
REM2_Bacino <- REM2$Bacino[oo]
REM2_Fiume <- REM2$Fiume[oo]
REM2_Quota <- REM2$Quota[oo]
REM2_UTM_Est <- REM2$UTM.Est[oo]
REM2_UTM_Nord <- REM2$UTM.Nord[oo]
REM2_UTM_Est_Staz <- REM2$UTM.Est.Staz[oo]
REM2_UTM_Nord_Staz <- REM2$UTM.Nord.Staz[oo]
REM2_DataMinimaRT <- REM2$DataMinimaRT[oo]
REM2_DataMassimaRT <- REM2$DataMassimaRT[oo]
REM2_Frequenza <- REM2$Frequenza[oo]
#
REM2_FormWeb <- REM2$visibilitaweb[oo]
REM2_invioPC <- REM2$invioPC[oo]


DBmeteo<-try(dbGetQuery(conn,"SET NAMES utf8"), silent=TRUE)
DBmeteo<-try(dbGetQuery(conn, "
SELECT 
A_Stazioni.IDstazione, Localita, Attributo,IDsensore, IDrete, Comune, Provincia, Bacino,Fiume,NOMEstazione, QuotaSensore, truncate(Y(A_Sensori.CoordUTM),0) as UTM_Nord, truncate(X(A_Sensori.CoordUTM),0) as UTM_Est, truncate(Y(A_Stazioni.CoordUTM),0) as UTM_Nord_Staz, truncate(X(A_Stazioni.CoordUTM),0) as UTM_Est_Staz ,IDrete, IDtipologia, NOMEtipologia,DataInizio,DataFine,Storico, AggregazioneTemporale    
FROM 
A_Stazioni,A_Sensori ,A_Tipologia 
WHERE A_Stazioni.IDstazione=A_Sensori.IDstazione and A_Tipologia.Nome=A_Sensori.NOMEtipologia"), silent=TRUE)
# rimuovi spazi bianchi che precedono e che seguono la stringa
DBmeteo$NOMEstazione<-sub(' +$', '', DBmeteo$NOMEstazione)

#------------------------------------------------------------------------------

cat("\nmmmmmmm    Ricerca sensori appartenenti al DBunico MA NON appartenenti al DBmeteo\n",file=fileout,append=T)

aux<-REM2_idSensore %in% DBmeteo$IDsensore

 if (length(REM2_idSensore[!aux])>0) {
  cat("\n idSensore, idStazione, Nome, IdTipologia, IdRete\n", file=fileout,append=T)
  iii<-1
  while(iii<length(REM2_idSensore[!aux])+1){
   if ((is.element(REM2_idStazione[!aux][iii] , IDstaz_eccezioni)==FALSE ) && (is.element(REM2_idSensore[!aux][iii] , IDsens_eccezioni)==FALSE )){
   # se non è tipologia di diagnostica segnalo
   if(REM2_IdTipologia[!aux][iii] != 138 && REM2_IdTipologia[!aux][iii] != 168){
   cat(as.vector(REM2_idSensore[!aux][iii]),
       ",",
       as.vector(REM2_idStazione[!aux][iii]),
       ",",
       as.vector(REM2_Nome[!aux][iii]),
       ",",
       as.vector(REM2_IdTipologia[!aux][iii]),
       ",",
       as.vector(REM2_IdReteVis[!aux][iii])  ,"\n",file=fileout,append=T)
    }else{
   # se è tipologia di diagnostica segnalo solo se è rete INM
   if( REM2_IdReteVis[!aux][iii] == 3 ){
   cat(as.vector(REM2_idSensore[!aux][iii]),
       ",",
       as.vector(REM2_idStazione[!aux][iii]),
       ",",
       as.vector(REM2_Nome[!aux][iii]),
       ",",
       as.vector(REM2_IdTipologia[!aux][iii]),
       ",",
       as.vector(REM2_IdReteVis[!aux][iii])  ,"\n",file=fileout,append=T)
    }
    }
   }
  iii<-iii+1
  }
 } else {
   cat("\n sensori trovati 0","\n",file=fileout,append=T)
 }

cat("\n mmmmmm  Ricerca sensori appartenenti al DBmeteo ma non appartenenti al DBunico\n",file=fileout,append=T)
aux<-DBmeteo$IDsensore %in% REM2_idSensore 
if (length(DBmeteo$NOMEstazione[!aux])>0) {
cat("\n sensori trovati: ", length(DBmeteo$IDsensore[!aux]),"\n", file=fileout,append=T)
  cat("\n idSensore, idStazione, Nome, IdTipologia, IdRete\n", file=fileout,append=T)
  iii<-1
  while(iii<length(DBmeteo$IDsensore[!aux])+1){
   cat(as.vector(DBmeteo$IDsensore[!aux][iii]),
       ",",
       as.vector(DBmeteo$IDstazione[!aux][iii]),
       ",",
       as.vector(DBmeteo$NOMEstazione[!aux][iii]),
       ",",
       as.vector(DBmeteo$NOMEtipologia[!aux][iii]),
       ",",
       as.vector(DBmeteo$IDrete[!aux][iii])  ,"\n",file=fileout,append=T)
  iii<-iii+1
  }
} else {
  cat("\nsensori trovati 0\n",file=fileout,append=T)
}


#------------------------------------------------------------------------------
i<-1
while(i<=length(DBmeteo$IDsensore)) {
#cat("sensore:", DBmeteo$IDsensore[i]," \n")
  j<-which(REM2_idSensore==DBmeteo$IDsensore[i])

  if (length(j)!=1) {
   #cat("-----------------------------------------------------------------\n",file=fileout,append=T)
   #cat(paste(i,". ID sensore =",DBmeteo$IDsensore[i],DBmeteo$NOMEstazione[i]),"\n",file=fileout,append=T)
   #  cat("l'ID stazione esiste nel DB METEO ma non nel DBunico\n",file=fileout,append=T)
   #  cat(paste("ID stazione =",DBmeteo$IDstazione[i]),"\n",file=fileout,append=T)
  } else {
######################    controllo sugli ID delle stazioni     ##################################
    if ( is.na(DBmeteo$IDstazione[i]) | is.na(REM2_idStazione[j]) ) {
      if ( is.na(DBmeteo$IDstazione[i]) & is.na(REM2_idStazione[j]) ) {
  #      cat("NOMEstazione...OK\n",file=fileout,append=T)
      } else {
  cat("-----------------------------------------------------------------\n",file=fileout_ID,append=T)
  cat(paste(i,". ID sensore =",DBmeteo$IDsensore[i]),"\n",file=fileout_ID,append=T)
        cat(paste("DBmeteo: ",DBmeteo$IDstazione[i],DBmeteo$NOMEstazione[i]),"\n",file=fileout_ID,append=T)
        cat(paste("DBunico: ",REM2_idStazione[j],REM2_Nome[j]),"\n",file=fileout_ID,append=T)
      }
    } else {
      if (DBmeteo$IDstazione[i]==REM2_idStazione[j]){
  #      cat("NOMEstazione...OK\n",file=fileout,append=T)
      } else {
  cat("-----------------------------------------------------------------\n",file=fileout_ID,append=T)
  cat(paste(i,". ID sensore =",DBmeteo$IDsensore[i]),"\n",file=fileout_ID,append=T)
        cat(paste("DBmeteo: ",DBmeteo$IDstazione[i],DBmeteo$NOMEstazione[i]),"\n",file=fileout_ID,append=T)
        cat(paste("DBunico: ",REM2_idStazione[j],REM2_Nome[j]),"\n",file=fileout_ID,append=T)
      }
    }
######################    controllo sui NOMI delle stazioni     ##################################
#Encoding(DBmeteo$NOMEstazione[i])<-"latin-1"
#Encoding(REM2_Nome[j])<-"latin-1"
Encoding(DBmeteo$NOMEstazione[i])<-"UTF-8"
Encoding(REM2_Nome[j])<-"UTF-8"
DBmeteo$NOMEstazione[i] <- iconv(DBmeteo$NOMEstazione[i], to="ASCII//TRANSLIT")
REM2_Nome[j] <- iconv(REM2_Nome[j], to="ASCII//TRANSLIT")
    DBmeteo$NOMEstazione[i]=gsub("'","",DBmeteo$NOMEstazione[i])
    DBmeteo$NOMEstazione[i]=gsub(" ","",DBmeteo$NOMEstazione[i])
    DBmeteo$NOMEstazione[i]=gsub("-","",DBmeteo$NOMEstazione[i])
    DBmeteo$NOMEstazione[i]=gsub("\\$","",DBmeteo$NOMEstazione[i])
    DBmeteo$NOMEstazione[i]=gsub("\\*","",DBmeteo$NOMEstazione[i])
    DBmeteo$NOMEstazione[i]=gsub("\u60","",DBmeteo$NOMEstazione[i])
    DBmeteo$NOMEstazione[i]=gsub("\uc8","",DBmeteo$NOMEstazione[i])
    DBmeteo$NOMEstazione[i]=gsub("\uE0","",DBmeteo$NOMEstazione[i])
    REM2_Nome[j]=gsub("'","",REM2_Nome[j])
    REM2_Nome[j]=gsub(" ","",REM2_Nome[j])
    REM2_Nome[j]=gsub("-","",REM2_Nome[j])
    REM2_Nome[j]=gsub("\\$","",REM2_Nome[j])
    REM2_Nome[j]=gsub("\\*","",REM2_Nome[j])
    REM2_Nome[j]=gsub("\u60","",REM2_Nome[j])
    REM2_Nome[j]=gsub("\uc8","",REM2_Nome[j])
    REM2_Nome[j]=gsub("\uE0","",REM2_Nome[j])

    DBmeteo$NOMEstazione[i]=toupper(DBmeteo$NOMEstazione[i])
    REM2_Nome[j]=toupper(REM2_Nome[j])

    if ( is.na(DBmeteo$NOMEstazione[i]) | is.na(REM2_Nome[j]) ) {
      if ( is.na(DBmeteo$NOMEstazione[i]) & is.na(REM2_Nome[j]) ) {
  #      cat("NOMEstazione...OK\n",file=fileout,append=T)
      } else {
  cat("-----------------------------------------------------------------\n",file=fileout_NOME,append=T)
  cat(paste(i,". ID stazione =",DBmeteo$IDstazione[i],DBmeteo$NOMEstazione[i]),"\n",file=fileout_NOME,append=T)
        cat(paste("DBmeteo: ",DBmeteo$NOMEstazione[i]),"\n",file=fileout_NOME,append=T)
        cat(paste("DBunico: ",REM2_Nome[j]),"\n",file=fileout_NOME,append=T)
      }
    } else {
      if (DBmeteo$NOMEstazione[i]==REM2_Nome[j]){
  #      cat("NOMEstazione...OK\n",file=fileout,append=T)
      } else {
  cat("-----------------------------------------------------------------\n",file=fileout_NOME,append=T)
  cat(paste(i,". ID stazione =",DBmeteo$IDstazione[i],DBmeteo$NOMEstazione[i]),"\n",file=fileout_NOME,append=T)
        cat(paste("DBmeteo: ",DBmeteo$NOMEstazione[i]),"\n",file=fileout_NOME,append=T)
        cat(paste("DBunico: ",REM2_Nome[j]),"\n",file=fileout_NOME,append=T)
      }
    }
######################    controllo sulle LOCALITA  delle stazioni     ##################################
Encoding(DBmeteo$Localita[i])<-"UTF-8"
Encoding(REM2_Localita[j])<-"UTF-8"
    DBmeteo$Localita[i]=gsub("'","",DBmeteo$Localita[i])
    DBmeteo$Localita[i]=gsub(" ","",DBmeteo$Localita[i])
    REM2_Localita[j]=gsub("'","",REM2_Localita[j])
    REM2_Localita[j]=gsub(" ","",REM2_Localita[j])
#DBmeteo$Localita[i] <- enc2utf8(DBmeteo$Localita[i])
#REM2_Localita[j] <- enc2utf8(REM2_Localita[j])
   #DBmeteo$Localita[i] <- iconv(DBmeteo$Localita[i], to="ASCII//TRANSLIT")
   # REM2_Localita[j] <- iconv(REM2_Localita[j], to="ASCII//TRANSLIT")
    DBmeteo$Localita[i]=gsub(" ","",DBmeteo$Localita[i])
    REM2_Localita[j]=gsub(" ","",REM2_Localita[j])
    DBmeteo$Localita[i]=gsub("\u2013","",DBmeteo$Localita[i])
    DBmeteo$Localita[i]=gsub("-","",DBmeteo$Localita[i])
    REM2_Localita[j]=gsub("\u2013","",REM2_Localita[j])
    REM2_Localita[j]=gsub("-","",REM2_Localita[j])

    DBmeteo$Localita[i]=toupper(DBmeteo$Localita[i])
    REM2_Localita[j]=toupper(REM2_Localita[j])

    if(is.na(REM2_Localita[j])==FALSE &  REM2_Localita[j]=="ND")REM2_Localita[j]=""
    #if(is.na(REM2_Localita[j])==FALSE &  REM2_Localita[j]=="ND")REM2_Localita[j]="--"
    if ( is.na(DBmeteo$Localita[i]) | is.na(REM2_Localita[j]) ) {
      if ( is.na(DBmeteo$Localita[i]) & is.na(REM2_Localita[j]) ) {
  #      cat("NOMEstazione...OK\n",file=fileout,append=T)
      } else {
  cat("-----------------------------------------------------------------\n",file=fileout_LOC,append=T)
  cat(paste(i,". ID stazione =",DBmeteo$IDstazione[i],DBmeteo$NOMEstazione[i]),"\n",file=fileout_LOC,append=T)
        cat(paste("DBmeteo: ",DBmeteo$Localita[i]),"\n",file=fileout_LOC,append=T)
        cat(paste("DBunico: ",REM2_Localita[j]),"\n",file=fileout_LOC,append=T)
      }
    } else {
      if (DBmeteo$Localita[i]==REM2_Localita[j]){
  #      cat("NOMEstazione...OK\n",file=fileout,append=T)
      } else {
  cat("-----------------------------------------------------------------\n",file=fileout_LOC,append=T)
  cat(paste(i,". ID stazione =",DBmeteo$IDstazione[i],DBmeteo$NOMEstazione[i]),"\n",file=fileout_LOC,append=T)
        cat(paste("DBmeteo: ",DBmeteo$Localita[i]),"\n",file=fileout_LOC,append=T)
        cat(paste("DBunico: ",REM2_Localita[j]),"\n",file=fileout_LOC,append=T)
      }
    }
######################    controllo sull ATTRIBUTO  delle stazioni     ##################################
# converto i NULL  e i "NA" in stringhe vuote
if(is.null(DBmeteo$Attributo[i])==T)DBmeteo$Attributo[i]=""
if(is.null(REM2_Attributo[j])==T)REM2_Attributo[j]=""
if(is.na(DBmeteo$Attributo[i])==T)DBmeteo$Attributo[i]=""
if(is.na(REM2_Attributo[j])==T)REM2_Attributo[j]=""
#Encoding(REM2_Attributo[j])<-"latin-1"
#DBmeteo$Attributo[i]<-enc2utf8(DBmeteo$Attributo[i])
#Encoding(DBmeteo$Attributo[i])<-"utf-8"
DBmeteo$Attributo[i] <- iconv(DBmeteo$Attributo[i], "", "ASCII//TRANSLIT")
DBmeteo$Attributo[i] <- iconv(DBmeteo$Attributo[i], "", "ASCII//TRANSLIT")

REM2_Attributo[j] <- iconv(REM2_Attributo[j], to="ASCII//TRANSLIT")
    DBmeteo$Attributo[i]=gsub("'","",DBmeteo$Attributo[i])
    DBmeteo$Attributo[i]=gsub(" ","",DBmeteo$Attributo[i])
    REM2_Attributo[j]=gsub("'","",REM2_Attributo[j])
    REM2_Attributo[j]=gsub(" ","",REM2_Attributo[j])
#    cat(DBmeteo$IDstazione[i],"  ", REM2_Attributo[j], "   ", DBmeteo$Attributo[i], "\n")
    if(is.na(REM2_Attributo[j])==FALSE &  REM2_Attributo[j]=="ND")REM2_Attributo[j]="--"
    if ( is.na(DBmeteo$Attributo[i]) | is.na(REM2_Attributo[j]) ) {
      if ( is.na(DBmeteo$Attributo[i]) & is.na(REM2_Attributo[j]) ) {
  #      cat("NOMEstazione...OK\n",file=fileout,append=T)
      } else {
  cat("-----------------------------------------------------------------\n",file=fileout_ATT,append=T)
  cat(paste(i,". ID stazione =",DBmeteo$IDstazione[i],DBmeteo$NOMEstazione[i]),"\n",file=fileout_ATT,append=T)
        cat(paste("DBmeteo: ",DBmeteo$Attributo[i]),"\n",file=fileout_ATT,append=T)
        cat(paste("DBunico: ",REM2_Attributo[j]),"\n",file=fileout_ATT,append=T)
      }
    } else {
      if (DBmeteo$Attributo[i]==REM2_Attributo[j]){
  #      cat("NOMEstazione...OK\n",file=fileout,append=T)
      } else {
  cat("-----------------------------------------------------------------\n",file=fileout_ATT,append=T)
  cat(paste(i,". ID stazione =",DBmeteo$IDstazione[i],DBmeteo$NOMEstazione[i]),"\n",file=fileout_ATT,append=T)
        cat(paste("DBmeteo: ",DBmeteo$Attributo[i]),"\n",file=fileout_ATT,append=T)
        cat(paste("DBunico: ",REM2_Attributo[j]),"\n",file=fileout_ATT,append=T)
      }
    }
######################    controllo sui COMUNI delle stazioni     ##################################
#Encoding(DBmeteo$Comune[i])<-"latin-1"
#Encoding(REM2_Comune[j])<-"latin-1"
DBmeteo$Comune[i] <- iconv(DBmeteo$Comune[i], to="ASCII//TRANSLIT")
REM2_Comune[j] <- iconv(REM2_Comune[j], to="ASCII//TRANSLIT")
    # converto in upper case 
    DBmeteo$Comune[i]=toupper(DBmeteo$Comune[i])
    REM2_Comune[j]=toupper(REM2_Comune[j])
    if(is.na(REM2_Comune[j])==FALSE &  REM2_Comune[j]=="ND")REM2_Comune[j]="--"
    # tolgo virgolette e spazi
    REM2_Comune[j]=gsub(" ","",REM2_Comune[j])
    REM2_Comune[j]=gsub("`","",REM2_Comune[j])
    REM2_Comune[j]=gsub("'","",REM2_Comune[j])
    DBmeteo$Comune[i]=gsub(" ","",DBmeteo$Comune[i])
    DBmeteo$Comune[i]=gsub("'","",DBmeteo$Comune[i])
    DBmeteo$Comune[i]=gsub("`","",DBmeteo$Comune[i])

    if ( is.na(DBmeteo$Comune[i]) | is.na(REM2_Comune[j]) ) {
      if ( is.na(DBmeteo$Comune[i]) & is.na(REM2_Comune[j]) ) {
  #      cat("NOMEstazione...OK\n",file=fileout,append=T)
      } else {
  cat("-----------------------------------------------------------------\n",file=fileout_COMUNE,append=T)
  cat(paste(i,". ID stazione =",DBmeteo$IDstazione[i],DBmeteo$NOMEstazione[i]),"\n",file=fileout_COMUNE,append=T)
        cat(paste("DBmeteo: ",DBmeteo$Comune[i]),"\n",file=fileout_COMUNE,append=T)
        cat(paste("DBunico: ",REM2_Comune[j]),"\n",file=fileout_COMUNE,append=T)
      }
    } else {
      if (DBmeteo$Comune[i]==REM2_Comune[j]){
  #      cat("NOMEstazione...OK\n",file=fileout,append=T)
      } else {
  cat("-----------------------------------------------------------------\n",file=fileout_COMUNE,append=T)
  cat(paste(i,". ID stazione =",DBmeteo$IDstazione[i],DBmeteo$NOMEstazione[i]),"\n",file=fileout_COMUNE,append=T)
        cat(paste("DBmeteo: ",DBmeteo$Comune[i]),"\n",file=fileout_COMUNE,append=T)
        cat(paste("DBunico: ",REM2_Comune[j]),"\n",file=fileout_COMUNE,append=T)
      }
    }

######################    controllo sui FIUME delle stazioni     ##################################
    if ( is.na(DBmeteo$Fiume[i]) | is.na(REM2_Fiume[j]) ) {
      if ( is.na(DBmeteo$Fiume[i]) & is.na(REM2_Fiume[j]) ) {
  #      cat("NOMEstazione...OK\n",file=fileout,append=T)
      } else {
  cat("-----------------------------------------------------------------\n",file=fileout_FIUME,append=T)
  cat(paste(i,". ID stazione =",DBmeteo$IDstazione[i],DBmeteo$NOMEstazione[i]),"\n",file=fileout_FIUME,append=T)
        cat(paste("DBmeteo: ",DBmeteo$Fiume[i]),"\n",file=fileout_FIUME,append=T)
        cat(paste("DBunico: ",REM2_Fiume[j]),"\n",file=fileout_FIUME,append=T)
      }
    } else {
      ###if (DBmeteo$Provincia[i]==REM2_Provincia[j]){
      if ((DBmeteo$Fiume[i]==REM2_Fiume[j])|(DBmeteo$Fiume[i]=="--" & REM2_Fiume[j]=="ND")){
  #      cat("NOMEstazione...OK\n",file=fileout,append=T)
      } else {
#  cat("-----------------------------------------------------------------\n",file=fileout_PROVINCIA,append=T)
  cat(paste(i,". ID stazione =",DBmeteo$IDstazione[i],DBmeteo$NOMEstazione[i]),"\n",file=fileout_FIUME,append=T)
        cat(paste("DBmeteo: ",DBmeteo$Fiume[i]),"\n",file=fileout_FIUME,append=T)
        cat(paste("DBunico: ",REM2_Fiume[j]),"\n",file=fileout_FIUME,append=T)
      }
    }

######################    controllo sui BACINI delle stazioni     ##################################
#    if ( is.na(DBmeteo$Bacino[i]) | is.na(REM2_Bacino[j]) ) {
#      if ( is.na(DBmeteo$Bacino[i]) & is.na(REM2_Bacino[j]) ) {
  #      cat("NOMEstazione...OK\n",file=fileout,append=T)
#      } else {
#  cat("-----------------------------------------------------------------\n",file=fileout_BACINO,append=T)
#  cat(paste(i,". ID stazione =",DBmeteo$IDstazione[i],DBmeteo$NOMEstazione[i]),"\n",file=fileout_BACINO,append=T)
#        cat(paste("DBmeteo: ",DBmeteo$Bacino[i]),"\n",file=fileout_BACINO,append=T)
#        cat(paste("DBunico: ",REM2_Bacino[j]),"\n",file=fileout_BACINO,append=T)
#      }
#    } else {
#      ###if (DBmeteo$Provincia[i]==REM2_Provincia[j]){
#      if ((DBmeteo$Bacino[i]==REM2_Bacino[j])|(DBmeteo$Bacino[i]=="--" & REM2_Bacino[j]=="ND")){
#  #      cat("NOMEstazione...OK\n",file=fileout,append=T)
#      } else {
##  cat("-----------------------------------------------------------------\n",file=fileout_PROVINCIA,append=T)
#  cat(paste(i,". ID stazione =",DBmeteo$IDstazione[i],DBmeteo$NOMEstazione[i]),"\n",file=fileout_BACINO,append=T)
#        cat(paste("DBmeteo: ",DBmeteo$Bacino[i]),"\n",file=fileout_BACINO,append=T)
#        cat(paste("DBunico: ",REM2_Bacino[j]),"\n",file=fileout_BACINO,append=T)
#      }
#    }

######################    controllo sulle PROVINCE delle stazioni     ##################################
# converto i NULL  e i "NA" in NA
if(is.null(DBmeteo$Provincia[i])==T)DBmeteo$Provincia[i]=NA
if(is.null(REM2_Provincia[i])==T)REM2_Provincia[i]=NA
    if ( is.na(DBmeteo$Provincia[i]) | is.na(REM2_Provincia[j]) ) {
      if ( is.na(DBmeteo$Provincia[i]) & is.na(REM2_Provincia[j]) ) {
  #      cat("NOMEstazione...OK\n",file=fileout,append=T)
      } else {
  cat("-----------------------------------------------------------------\n",file=fileout_PROVINCIA,append=T)
  cat(paste(i,". ID stazione =",DBmeteo$IDstazione[i],DBmeteo$NOMEstazione[i]),"\n",file=fileout_PROVINCIA,append=T)
        cat(paste("DBmeteo: ",DBmeteo$Provincia[i]),"\n",file=fileout_PROVINCIA,append=T)
        cat(paste("DBunico: ",REM2_Provincia[j]),"\n",file=fileout_PROVINCIA,append=T)
      }
    } else {
      ###if (DBmeteo$Provincia[i]==REM2_Provincia[j]){
      if ((DBmeteo$Provincia[i]==REM2_Provincia[j])|(DBmeteo$Provincia[i]=="--" & REM2_Provincia[j]=="ND")){
  #      cat("NOMEstazione...OK\n",file=fileout,append=T)
      } else {
#  cat("-----------------------------------------------------------------\n",file=fileout_PROVINCIA,append=T)
  cat(paste(i,". ID stazione =",DBmeteo$IDstazione[i],DBmeteo$NOMEstazione[i]),"\n",file=fileout_PROVINCIA,append=T)
        cat(paste("DBmeteo: ",DBmeteo$Provincia[i]),"\n",file=fileout_PROVINCIA,append=T)
        cat(paste("DBunico: ",REM2_Provincia[j]),"\n",file=fileout_PROVINCIA,append=T)
      }
    }

######################    controllo sulle RETI delle stazioni     ##################################
## nome            REM   meteo   
## Canton Ticino    1      5 
## CGP              2      6 
## INM              3      4 
## AIPO             4      6 
## MeteoSvizzera    5      5 
## Aria             6      1 
## CMG              7      2 
## Consorzio Adda   8      6 
## Consorzio Ticino 9      6 
## Consorzio Oglio  10     6
## Consorzio Chiese 11     6
## Extra Lombardia  12     5
## Miscellanea      16     6
## ETV              21     6
## ARPA Veneto      25     5
## Rete Lampo       24     3
## Provincia Trento 23     5
## Valle dAosta     26     5

REM2Rete <- 0
if (REM2_IdReteVis[j]==6) REM2Rete <- 1 #aria
if (REM2_IdReteVis[j]==7) REM2Rete <- 2 #cmg
if (REM2_IdReteVis[j]==3) REM2Rete <- 4 #inm
if (REM2_IdReteVis[j]==24) REM2Rete <- 3 #lampo
if (REM2_IdReteVis[j]==1 | REM2_IdReteVis[j]==5 | REM2_IdReteVis[j]==12 | REM2_IdReteVis[j]==25 | REM2_IdReteVis[j]==23 | REM2_IdReteVis[j]==26 | REM2_IdReteVis[j]==28) REM2Rete <- 5 # extra lombardia
if (REM2_IdReteVis[j]==2 | REM2_IdReteVis[j]==4 | REM2_IdReteVis[j]==8 | REM2_IdReteVis[j]==9 | REM2_IdReteVis[j]==10 | REM2_IdReteVis[j]==11 | REM2_IdReteVis[j]==16| REM2_IdReteVis[j]==21) REM2Rete <- 6 # altro 
if ((REM2_IdReteVis[j]==12 | REM2_IdReteVis[j]==4) && (REM2_Provincia[j] %in% c('CR','PV','MN'))) REM2Rete <- 6 # altro 
if ((REM2_IdReteVis[j]==12 | REM2_IdReteVis[j]==4) && (REM2_Provincia[j] %in% c('VR','VC','FE','RO','AL','PR'))) REM2Rete <- 5 # Extra Lombardia 
 
     if ( is.na(DBmeteo$IDrete[i]) | is.na(REM2_IdReteVis[j]) ) {
       if ( is.na(DBmeteo$IDrete[i]) & is.na(REM2_IdReteVis[j]) ) {
   #      cat("NOMEstazione...OK\n",file=fileout,append=T)
       } else {
   cat("-----------------------------------------------------------------\n",file=fileout_RETE,append=T)
   cat(paste(i,". ID stazione =",DBmeteo$IDstazione[i],DBmeteo$NOMEstazione[i]),"\n",file=fileout_RETE,append=T)
         cat(paste("DBmeteo: ",DBmeteo$IDrete[i]),"\n",file=fileout_RETE,append=T)
         cat(paste("DBunico: ",REM2_NomeReteVis[j]),"\n",file=fileout_RETE,append=T)
       }
     } else {
#       if (DBmeteo$IDrete[i]==REM2_IdReteVis[j]){
       if (DBmeteo$IDrete[i]==REM2Rete){
   #      cat("NOMEstazione...OK\n",file=fileout,append=T)
       } else {
   cat("-----------------------------------------------------------------\n",file=fileout_RETE,append=T)
   cat(paste(i,". ID stazione =",DBmeteo$IDstazione[i],DBmeteo$NOMEstazione[i]),"\n",file=fileout_RETE,append=T)
         cat(paste("DBmeteo: ",DBmeteo$IDrete[i]),"\n",file=fileout_RETE,append=T)
         cat(paste("DBunico: ",REM2_NomeReteVis[j]),"\n",file=fileout_RETE,append=T)
       }
     }
######################    controllo sulle QUOTE delle stazioni     ##################################
    if ( is.na(DBmeteo$QuotaSensore[i]) | is.na(REM2_Quota[j]) ) {
      if ( is.na(DBmeteo$QuotaSensore[i]) & is.na(REM2_Quota[j]) ) {
  #      cat("NOMEstazione...OK\n",file=fileout,append=T)
      } else {
  cat("-----------------------------------------------------------------\n",file=fileout_QUOTA,append=T)
  cat(paste(i,". ID stazione =",DBmeteo$IDstazione[i],DBmeteo$NOMEstazione[i]),"\n",file=fileout_QUOTA,append=T)
        cat(paste("DBmeteo: ",DBmeteo$QuotaSensore[i]),"\n",file=fileout_QUOTA,append=T)
        cat(paste("DBunico: ",REM2_Quota[j]),"\n",file=fileout_QUOTA,append=T)
      }
    } else {
      if (DBmeteo$QuotaSensore[i]==REM2_Quota[j]){
  #      cat("NOMEstazione...OK\n",file=fileout,append=T)
      } else {
  cat("-----------------------------------------------------------------\n",file=fileout_QUOTA,append=T)
  cat(paste(i,". ID stazione =",DBmeteo$IDstazione[i],DBmeteo$NOMEstazione[i]),"\n",file=fileout_QUOTA,append=T)
        cat(paste("DBmeteo: ",DBmeteo$QuotaSensore[i]),"\n",file=fileout_QUOTA,append=T)
        cat(paste("DBunico: ",REM2_Quota[j]),"\n",file=fileout_QUOTA,append=T)
      }
    }
#######################    controllo sulle COORDINATE dei sensori/stazioni     ##################################
######
# UTM Nord dei sensori
# converto i NULL in NA
if(is.na(DBmeteo$UTM_Nord[i])==F &&  DBmeteo$UTM_Nord[i]==-9999)DBmeteo$UTM_Nord[i]=NA
if(is.null(DBmeteo$UTM_Nord[i])==T)DBmeteo$UTM_Nord[i]=NA
if(is.null(REM2_UTM_Nord[j])==T)REM2_UTM_Nord[j]=NA
#
    if ( is.na(DBmeteo$UTM_Nord[i]) | is.na(REM2_UTM_Nord[j]) ) {
      if ( is.na(DBmeteo$UTM_Nord[i]) & is.na(REM2_UTM_Nord[j]) ) {
  #      cat("NOMEstazione...OK\n",file=fileout,append=T)
      } else {
  cat("-----------------------------------------------------------------\n",file=fileout_COORD,append=T)
  cat(paste(i,". ID stazione =",DBmeteo$IDstazione[i],DBmeteo$NOMEstazione[i]),"\n",file=fileout_COORD,append=T)
        cat(paste("DBmeteo UTM_Nord sensore: ",DBmeteo$UTM_Nord[i]),"\n",file=fileout_COORD,append=T)
        cat(paste("DBunico UTM_Nord sensore: ",round(REM2_UTM_Nord[j])),"\n",file=fileout_COORD,append=T)
      }
    } else {
      #cat("j ", j,"\n")
      #cat("REM2_UTM_Nord[j] ", REM2_UTM_Nord[j],"\n")
      #prova<-as.numeric(REM2_UTM_Nord[j])
      #cat("prova ", prova,"\n")
      #cat("round ", round(REM2_UTM_Nord[j]),"\n")
      if (DBmeteo$UTM_Nord[i]==round(REM2_UTM_Nord[j])){
  #      cat("NOMEstazione...OK\n",file=fileout,append=T)
      } else {
  cat("-----------------------------------------------------------------\n",file=fileout_COORD,append=T)
  cat(paste(i,". ID stazione =",DBmeteo$IDstazione[i],DBmeteo$NOMEstazione[i]),"\n",file=fileout_COORD,append=T)
        cat(paste("DBmeteo UTM_Nord sensore: ",DBmeteo$UTM_Nord[i]),"\n",file=fileout_COORD,append=T)
        cat(paste("DBunico UTM_Nord sensore: ",round(REM2_UTM_Nord[j])),"\n",file=fileout_COORD,append=T)
      }
    }
##
# UTM Est dei sensori
# converto i NULL in NA
if(is.na(DBmeteo$UTM_Est[i])==F &&  DBmeteo$UTM_Est[i]==-9999)DBmeteo$UTM_Est[i]=NA
if(is.null(DBmeteo$UTM_Est[i])==T)DBmeteo$UTM_Est[i]=NA
if(is.null(REM2_UTM_Est[j])==T)REM2_UTM_Est[j]=NA
#
    if ( is.na(DBmeteo$UTM_Est[i]) | is.na(REM2_UTM_Est[j]) ) {
      if ( is.na(DBmeteo$UTM_Est[i]) & is.na(REM2_UTM_Est[j]) ) {
  #      cat("NOMEstazione...OK\n",file=fileout,append=T)
      } else {
  cat("-----------------------------------------------------------------\n",file=fileout_COORD,append=T)
  cat(paste(i,". ID stazione =",DBmeteo$IDstazione[i],DBmeteo$NOMEstazione[i]),"\n",file=fileout_COORD,append=T)
        cat(paste("DBmeteo UTM_Est sensore: ",DBmeteo$UTM_Est[i]),"\n",file=fileout_COORD,append=T)
        cat(paste("DBunico UTM_Est sensore: ",round(REM2_UTM_Est[j])),"\n",file=fileout_COORD,append=T)
      }
    } else {
      if (DBmeteo$UTM_Est[i]==round(REM2_UTM_Est[j])){
  #      cat("NOMEstazione...OK\n",file=fileout,append=T)
      } else {
  cat("-----------------------------------------------------------------\n",file=fileout_COORD,append=T)
  cat(paste(i,". ID stazione =",DBmeteo$IDstazione[i],DBmeteo$NOMEstazione[i]),"\n",file=fileout_COORD,append=T)
        cat(paste("DBmeteo UTM_Est sensore: ",DBmeteo$UTM_Est[i]),"\n",file=fileout_COORD,append=T)
        cat(paste("DBunico UTM_Est sensore: ",round(REM2_UTM_Est[j])),"\n",file=fileout_COORD,append=T)
      }
    }
######
# UTM Nord delle stazioni
# converto i NULL in NA
if(is.na(DBmeteo$UTM_Nord_Staz[i])==F &&  DBmeteo$UTM_Nord_Staz[i]==-9999)DBmeteo$UTM_Nord_Staz[i]=NA
if(is.null(DBmeteo$UTM_Nord_Staz[i])==T)DBmeteo$UTM_Nord_Staz[i]=NA
if(is.null(REM2_UTM_Nord_Staz[j])==T)REM2_UTM_Nord_Staz[j]=NA
#
    if ( is.na(DBmeteo$UTM_Nord_Staz[i]) | is.na(REM2_UTM_Nord_Staz[j]) ) {
      if ( is.na(DBmeteo$UTM_Nord_Staz[i]) & is.na(REM2_UTM_Nord_Staz[j]) ) {
      } else {
  cat("-----------------------------------------------------------------\n",file=fileout_COORD,append=T)
  cat(paste(i,". ID stazione =",DBmeteo$IDstazione[i],DBmeteo$NOMEstazione[i]),"\n",file=fileout_COORD,append=T)
        cat(paste("DBmeteo UTM_Nord stazione: ",DBmeteo$UTM_Nord_Staz[i]),"\n",file=fileout_COORD,append=T)
        cat(paste("DBunico UTM_Nord stazione: ",round(REM2_UTM_Nord_Staz[j])),"\n",file=fileout_COORD,append=T)
      }
    } else {
      if (DBmeteo$UTM_Nord_Staz[i]==round(REM2_UTM_Nord_Staz[j])){
      } else {
  cat("-----------------------------------------------------------------\n",file=fileout_COORD,append=T)
  cat(paste(i,". ID stazione =",DBmeteo$IDstazione[i],DBmeteo$NOMEstazione[i]),"\n",file=fileout_COORD,append=T)
        cat(paste("DBmeteo UTM_Nord stazione: ",DBmeteo$UTM_Nord_Staz[i]),"\n",file=fileout_COORD,append=T)
        cat(paste("DBunico UTM_Nord stazione: ",round(REM2_UTM_Nord_Staz[j])),"\n",file=fileout_COORD,append=T)
      }
    }
##
# UTM Est delle stazioni
# converto i NULL in NA
if(is.na(DBmeteo$UTM_Est_Staz[i])==F &&  DBmeteo$UTM_Est_Staz[i]==-9999)DBmeteo$UTM_Est_Staz[i]=NA
if(is.null(DBmeteo$UTM_Est_Staz[i])==T)DBmeteo$UTM_Est_Staz[i]=NA
if(is.null(REM2_UTM_Est_Staz[j])==T)REM2_UTM_Est_Staz[j]=NA
#
    if ( is.na(DBmeteo$UTM_Est_Staz[i]) | is.na(REM2_UTM_Est_Staz[j]) ) {
      if ( is.na(DBmeteo$UTM_Est_Staz[i]) & is.na(REM2_UTM_Est_Staz[j]) ) {
      } else {
  cat("-----------------------------------------------------------------\n",file=fileout_COORD,append=T)
  cat(paste(i,". ID stazione =",DBmeteo$IDstazione[i],DBmeteo$NOMEstazione[i]),"\n",file=fileout_COORD,append=T)
        cat(paste("DBmeteo UTM_Est stazione: ",DBmeteo$UTM_Est_Staz[i]),"\n",file=fileout_COORD,append=T)
        cat(paste("DBunico UTM_Est stazione: ",round(REM2_UTM_Est_Staz[j])),"\n",file=fileout_COORD,append=T)
      }
    } else {
      if (DBmeteo$UTM_Est_Staz[i]==round(REM2_UTM_Est_Staz[j])){
      } else {
  cat("-----------------------------------------------------------------\n",file=fileout_COORD,append=T)
  cat(paste(i,". ID stazione =",DBmeteo$IDstazione[i],DBmeteo$NOMEstazione[i]),"\n",file=fileout_COORD,append=T)
        cat(paste("DBmeteo UTM_Est stazione: ",DBmeteo$UTM_Est_Staz[i]),"\n",file=fileout_COORD,append=T)
        cat(paste("DBunico UTM_Est stazione: ",round(REM2_UTM_Est_Staz[j])),"\n",file=fileout_COORD,append=T)
      }
    }
######################    controllo sulla flag di STORICO     ##################################
storico=NULL
if(DBmeteo$Storico[i]=="Yes"){
storico=1
}else if(DBmeteo$Storico[i]=="No"){
storico=0
}else{
}

    if ( is.na(storico) | is.na(REM2_Storico[j]) ) {
      if ( is.na(storico) & is.na(REM2_Storico[j]) ) {
  #      cat("NOMEstazione...OK\n",file=fileout,append=T)
      } else {
  cat("-----------------------------------------------------------------\n",file=fileout_STORICO,append=T)
  cat(paste(i,". ID sensore =",DBmeteo$IDsensore[i],DBmeteo$NOMEtipologia[i],DBmeteo$NOMEstazione[i]),"\n",file=fileout_STORICO,append=T)
        cat(paste("DBmeteo: ",storico),"\n",file=fileout_STORICO,append=T)
        cat(paste("DBunico: ",REM2_Storico[j]),"\n",file=fileout_STORICO,append=T)
      }
    } else {
      if (storico==REM2_Storico[j]){
  #      cat("NOMEstazione...OK\n",file=fileout,append=T)
      } else {
  cat("-----------------------------------------------------------------\n",file=fileout_STORICO,append=T)
  cat(paste(i,". ID sensore =",DBmeteo$IDsensore[i],DBmeteo$NOMEtipologia[i],DBmeteo$NOMEstazione[i]),"\n",file=fileout_STORICO,append=T)
        cat(paste("DBmeteo: ",storico),"\n",file=fileout_STORICO,append=T)
        cat(paste("DBunico: ",REM2_Storico[j]),"\n",file=fileout_STORICO,append=T)
      }
    }
#######################    controllo su DATA MINIMA    ##################################
# converto DataMinimaRT del REM al formato del DBmeteo
dataminima<-as.Date(REM2_DataMinimaRT[j], format="%d/%m/%Y")
#
    if ( is.na(DBmeteo$DataInizio[i]) | is.na(dataminima) ) {
      if ( is.na(DBmeteo$DataInizio[i]) & is.na(dataminima) ) {
  #      cat("NOMEstazione...OK\n",file=fileout,append=T)
      } else {
  cat("-----------------------------------------------------------------\n",file=fileout_DATAI,append=T)
  cat(paste(i,". sensore =",DBmeteo$NOMEtipologia[i],DBmeteo$NOMEstazione[i]),"\n",file=fileout_DATAI,append=T)
        cat(paste("DBmeteo: ",DBmeteo$DataInizio[i]),"\n",file=fileout_DATAI,append=T)
        cat(paste("DBunico: ",dataminima),"\n",file=fileout_DATAI,append=T)
      }
    } else {
      if (DBmeteo$DataInizio[i]==dataminima){
  #      cat("NOMEstazione...OK\n",file=fileout,append=T)
      } else {
  cat("-----------------------------------------------------------------\n",file=fileout_DATAI,append=T)
  cat(paste(i,". sensore =",DBmeteo$NOMEtipologia[i],DBmeteo$NOMEstazione[i]),"\n",file=fileout_DATAI,append=T)
        cat(paste("DBmeteo: ",DBmeteo$DataInizio[i]),"\n",file=fileout_DATAI,append=T)
        cat(paste("DBunico: ",dataminima),"\n",file=fileout_DATAI,append=T)
      }
    }
######################    controllo su DATA MASSIMA    ##################################
# eseguo solo per sensori storici
if(DBmeteo$Storico[i]=="Yes"){
# converto DataMassimaRT del REM al formato del DBmeteo
datamassima<-as.Date(REM2_DataMassimaRT[j], format="%d/%m/%Y")
#
    if ( is.na(DBmeteo$DataFine[i]) | is.na(datamassima) ) {
      if ( is.na(DBmeteo$DataFine[i]) & is.na(datamassima) ) {
  #      cat("NOMEstazione...OK\n",file=fileout,append=T)
      } else {
  cat("-----------------------------------------------------------------\n",file=fileout_DATAF,append=T)
  cat(paste(i,". sensore =",DBmeteo$IDsensore[i],DBmeteo$NOMEtipologia[i],DBmeteo$NOMEstazione[i]),"\n",file=fileout_DATAF,append=T)
        cat(paste("DBmeteo: ",DBmeteo$DataFine[i]),"\n",file=fileout_DATAF,append=T)
        cat(paste("DBunico: ",datamassima),"\n",file=fileout_DATAF,append=T)
      }
    } else {
      if (DBmeteo$DataFine[i]==datamassima){
  #      cat("NOMEstazione...OK\n",file=fileout,append=T)
      } else {
  cat("-----------------------------------------------------------------\n",file=fileout_DATAF,append=T)
  cat(paste(i,". sensore =",DBmeteo$IDsensore[i],DBmeteo$NOMEtipologia[i],DBmeteo$NOMEstazione[i]),"\n",file=fileout_DATAF,append=T)
        cat(paste("DBmeteo: ",DBmeteo$DataFine[i]),"\n",file=fileout_DATAF,append=T)
        cat(paste("DBunico: ",datamassima),"\n",file=fileout_DATAF,append=T)
      }
    }
}
######################    controllo sulle TIPOLOGIE di sensore     ##################################
    if ( is.na(DBmeteo$IDtipologia[i]) | is.na(REM2_IdTipologia[j]) ) {
      if ( is.na(DBmeteo$IDtipologia[i]) & is.na(REM2_IdTipologia[j]) ) {
  #      cat("NOMEstazione...OK\n",file=fileout,append=T)
      } else {
  cat("-----------------------------------------------------------------\n",file=fileout_TIP,append=T)
  cat(paste(i,". ID sensore =",DBmeteo$IDsensore[i],DBmeteo$NOMEstazione[i]),"\n",file=fileout_TIP,append=T)
        cat(paste("DBmeteo: ",DBmeteo$NOMEtipologia[i]),"\n",file=fileout_TIP,append=T)
        cat(paste("DBunico: ",REM2_IdTipologia[j]),"\n",file=fileout_TIP,append=T)
      }
    } else {
      if (DBmeteo$IDtipologia[i]==REM2_IdTipologia[j]){
  #      cat("NOMEstazione...OK\n",file=fileout,append=T)
      } else {
  cat("-----------------------------------------------------------------\n",file=fileout_TIP,append=T)
  cat(paste(i,". ID stazione =",DBmeteo$IDstazione[i],DBmeteo$NOMEstazione[i]),"\n",file=fileout_TIP,append=T)
        cat(paste("DBmeteo: ",DBmeteo$NOMEtipologia[i]),"\n",file=fileout_TIP,append=T)
        cat(paste("DBunico: ",REM2_IdTipologia[j]),"\n",file=fileout_TIP,append=T)
      }
    }
######################    controllo sulle FREQUENZE     ##################################
## nel REM i pluviometri al minuto CAE risultano ai 10 minuti quindi prima di confrontare li porto a 1
## condizioni sul sensore: appartenenza rete INM, non storici, tipologia pluvio
##    if (REM2_Storico[j]==0 & REM2_Frequenza[j]==10 & REM2_IdReteVis[j]==3 & REM2_IdTipologia[j]==2) REM2_Frequenza[j]<-1
  if(DBmeteo$Storico[i]=="No"){
    if ( is.na(DBmeteo$AggregazioneTemporale[i]) | is.na(REM2_Frequenza[j]) ) {
     if ( is.na(DBmeteo$AggregazioneTemporale[i]) & is.na(REM2_Frequenza[j]) ) {
  #      cat("NOMEstazione...OK\n",file=fileout,append=T)
     } else {
  cat("-----------------------------------------------------------------\n",file=fileout_QUOTA,append=T)
  cat(paste(i,". ID sensore =",DBmeteo$IDsensore[i],"  tip=",DBmeteo$NOMEtipologia[i],"  staz=",DBmeteo$IDstazione[i],DBmeteo$NOMEstazione[i]),"\n",file=fileout_FREQUENZA,append=T)
        cat(paste("DBmeteo: ",DBmeteo$AggregazioneTemporale[i]),"\n",file=fileout_FREQUENZA,append=T)
        cat(paste("DBunico: ",REM2_Frequenza[j]),"\n",file=fileout_FREQUENZA,append=T)
      }
    } else {
      if (DBmeteo$AggregazioneTemporale[i]==REM2_Frequenza[j]){
  #      cat("NOMEstazione...OK\n",file=fileout,append=T)
      } else {
  cat("-----------------------------------------------------------------\n",file=fileout_FREQUENZA,append=T)
  cat(paste(i,". ID sensore =",DBmeteo$IDsensore[i],"  tip=",DBmeteo$NOMEtipologia[i],"  staz=",DBmeteo$IDstazione[i],DBmeteo$NOMEstazione[i]),"\n",file=fileout_FREQUENZA,append=T)
        cat(paste("DBmeteo: ",DBmeteo$AggregazioneTemporale[i]),"\n",file=fileout_FREQUENZA,append=T)
        cat(paste("DBunico: ",REM2_Frequenza[j]),"\n",file=fileout_FREQUENZA,append=T)
      }
    }
  }
##################################################################################################
  }
  i<-i+1
} 

# VERIFICO ALLINEAMENTO SUGLI INVII A PROTEZIONE CIVILE E IN FORM WEB
ii <- which(REM2_invioPC==1)
IDsensoreREM2_perPC<-REM2_idSensore[ii]
ComuneREM2_perPC<-REM2_Comune[ii]
AttributoREM2_perPC<-REM2_Attributo[ii]
tipologiaREM2_perPC<-REM2_IdTipologia[ii]
#
yy <- which(REM2_FormWeb=="S")
IDsensoreREM2_FormWeb<-REM2_idSensore[yy]
ComuneREM2_perFormWeb<-REM2_Comune[yy]
AttributoREM2_perFormWeb<-REM2_Attributo[yy]
tipologiaREM2_perFormWeb<-REM2_IdTipologia[yy]

#####################            invio PC         ########################
#
DBmeteo_dest<-try(dbGetQuery(conn, "select A_Sensori.IDsensore, CONCAT(Comune,' ',IFNULL(Attributo,'')) as Nome, NOMEtipologia from A_Sensori2Destinazione, A_Sensori,A_Stazioni where A_Stazioni.IDstazione=A_Sensori.IDstazione and A_Sensori.IDsensore=A_Sensori2Destinazione.IDsensore and Destinazione =1 and A_Sensori2Destinazione.DataFine is null"),silent=TRUE)
#
cat ( "\n\n ------   INVIO a PROTEZIONE CIVILE   -----\n",file=fileout_destinazioni,append=T)
cat ( "\n n. sensori da DBmeteo=", length(DBmeteo_dest$IDsensore),"\n",file=fileout_destinazioni,append=T)
cat ( " n. sensori da REM2=", length(IDsensoreREM2_perPC),"\n",file=fileout_destinazioni,append=T)
#
indice_REM2<-which(!IDsensoreREM2_perPC %in% DBmeteo_dest$IDsensore)
indice_DBmeteo<-which(!DBmeteo_dest$IDsensore %in% IDsensoreREM2_perPC)
#
cat(" \n n. sensori solo nel REM2:= ",length(indice_REM2) ,"\n",file=fileout_destinazioni,append=T)
ii<-1
while(ii<length(indice_REM2)+1){
if(is.na(AttributoREM2_perPC[indice_REM2[ii]])==T)AttributoREM2_perPC[indice_REM2[ii]]<-""
cat(ComuneREM2_perPC[indice_REM2[ii]],AttributoREM2_perPC[indice_REM2[ii]],tipologie_idrometeo_DBmeteo$Nome[tipologie_idrometeo_DBmeteo$IdTipologia==tipologiaREM2_perPC[indice_REM2[ii]]]," IDsens=",IDsensoreREM2_perPC[indice_REM2[ii]],"\n",file=fileout_destinazioni,append=T)
ii<-ii+1
}
#
cat(" \n n. sensori solo nel DBmeteo:= ",length(indice_DBmeteo) ,"\n",file=fileout_destinazioni,append=T)
ii<-1
while(ii<length(indice_DBmeteo)+1){
cat(DBmeteo_dest$IDsensore[indice_DBmeteo[ii]],DBmeteo_dest$Nome[indice_DBmeteo[ii]], DBmeteo_dest$NOMEtipologia[indice_DBmeteo[ii]],"\n",file=fileout_destinazioni,append=T)
ii<-ii+1
}
#####################            FORM WEB         ########################
#
DBmeteo_dest<-try(dbGetQuery(conn, "select A_Sensori.IDsensore, CONCAT(Comune,' ',IFNULL(Attributo,'')) as Nome, NOMEtipologia from A_Sensori2Destinazione, A_Sensori,A_Stazioni where A_Stazioni.IDstazione=A_Sensori.IDstazione and A_Sensori.IDsensore=A_Sensori2Destinazione.IDsensore and Destinazione =13 and A_Sensori2Destinazione.DataFine is null"),silent=TRUE)
#
cat ( "\n\n ------   FORM WEB   -----\n",file=fileout_destinazioni,append=T)
cat ( " \n n. sensori da DBmeteo=", length(DBmeteo_dest$IDsensore),"\n",file=fileout_destinazioni,append=T)
cat ( " n. sensori da REM2=", length(IDsensoreREM2_FormWeb),"\n",file=fileout_destinazioni,append=T)
#
indice_REM2<-which(!IDsensoreREM2_FormWeb %in% DBmeteo_dest$IDsensore)
indice_DBmeteo<-which(!DBmeteo_dest$IDsensore %in% IDsensoreREM2_FormWeb)
#
cat(" \n n. sensori solo nel REM2:= ",length(indice_REM2) ,"\n",file=fileout_destinazioni,append=T)
ii<-1
while(ii<length(indice_REM2)+1){
if(is.na(AttributoREM2_perFormWeb[indice_REM2[ii]])==T)AttributoREM2_perFormWeb[indice_REM2[ii]]<-""
cat(ComuneREM2_perFormWeb[indice_REM2[ii]],AttributoREM2_perFormWeb[indice_REM2[ii]], tipologie_idrometeo_DBmeteo$Nome[tipologie_idrometeo_DBmeteo$IdTipologia==tipologiaREM2_perFormWeb[indice_REM2[ii]]]," IDsens=",IDsensoreREM2_FormWeb[indice_REM2[ii]],"\n",file=fileout_destinazioni,append=T)
ii<-ii+1
}
#
cat(" \n n. sensori solo nel DBmeteo:= ",length(indice_DBmeteo) ,"\n",file=fileout_destinazioni,append=T)
ii<-1
while(ii<length(indice_DBmeteo)+1){
cat(DBmeteo_dest$IDsensore[indice_DBmeteo[ii]],DBmeteo_dest$Nome[indice_DBmeteo[ii]], DBmeteo_dest$NOMEtipologia[indice_DBmeteo[ii]],"\n",file=fileout_destinazioni,append=T)
ii<-ii+1
}

system('cat generico.txt check*.txt> allineamentoREM.out')
system('rm generico.txt check*.txt')

#------------------------------------------------------------------------------
dbDisconnect(conn)
rm(conn)
#dbUnloadDriver(drv)
warnings()
quit(status=0)

