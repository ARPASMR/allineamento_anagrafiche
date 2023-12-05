###############################################################################
#  
library(DBI)
library(RMySQL)
library(foreign)
#library(RODBC)
#
#+ gestione dell'errore
neverstop<-function(){
  print("EE..ERRORE durante l'esecuzione dello script!! Messaggio d'Errore prodotto:\n")
  quit()
}
options(show.error.messages=TRUE,error=neverstop)

#dir_output<-"/home/meteo/programmi/anagrafica/HYDSTRA/"
#dir_anagrafica<-"/home/meteo/programmi/anagrafica/HYDSTRA/"
#fileout<-paste(dir_output,"aa_HYDSTRA.txt",sep="")
dir_output<-"./"
dir_anagrafica<-"./"
fileout<-"aa_HYDSTRA.txt"

cat(">>>>>>>>>>><<<<<<<<<<<<<  \n\n",file=fileout)
#==============================================================================
#   LEGGO INFO DI HYDSTRA - tutte
#==============================================================================
cat( "  > leggi informazioni di Hydstra SITE\n", file=fileout,append=TRUE)

#HYD_SITE <- read.csv ( paste(dir_anagrafica,"SITE.CSV",sep="") , header = TRUE , dec=",", quote="\"", na.string=c("-9999",""),as.is = TRUE, sep=",") 
HYD_SITE<-NULL
HYD_SITE <- try(read.dbf(paste(dir_anagrafica,"SITE.DBF",sep=""),as.is = FALSE),silent=TRUE)
#print("HYD_SITE")
#print(HYD_SITE)
HYD_SITE_staz <- HYD_SITE$CATEGORY8[which(is.na(HYD_SITE$CATEGORY8)==F)]
#HYD_SITE_nome <- HYD_SITE$STNAME[which(is.na(HYD_SITE$CATEGORY8)==F)]
HYD_SITE_nome2 <- HYD_SITE$STATION[which(is.na(HYD_SITE$CATEGORY8)==F)]
print(HYD_SITE_nome2)
#HYD_SITE_nome3 <- HYD_SITE$SHORTNAME[which(is.na(HYD_SITE$CATEGORY8)==F)]
#
cat( "  > leggi informazioni di Hydstra\n", file=fileout,append=TRUE)
HYD_anag <- read.csv ( paste(dir_anagrafica,"Anagrafica.csv",sep="") , header = TRUE , dec=",", quote="\"", as.is = TRUE,na.strings = c("-9999",""), sep=";") 
HYD_staz <- HYD_anag$IdStazione[which(is.na(HYD_anag$IdStazione)==F)]
HYD_nome <- HYD_anag$Nome[which(is.na(HYD_anag$IdStazione)==F)]
#
## termo
HYD_T1 <- HYD_anag$IdTerm[which(is.na(HYD_anag$IdStazione)==F)]
HYD_T2 <- HYD_anag$IdTerm2[which(is.na(HYD_anag$IdStazione)==F)]
HYD_T <- c(HYD_T1,HYD_T2)
#
## idro
HYD_I1 <- HYD_anag$IdIdro[which(is.na(HYD_anag$IdStazione)==F)]
HYD_I2 <- HYD_anag$IdIdro2[which(is.na(HYD_anag$IdStazione)==F)]
HYD_I <- c(HYD_I1,HYD_I2)
#
## pluvio
HYD_PP1 <- HYD_anag$IdPluv[which(is.na(HYD_anag$IdStazione)==F)]
HYD_PP2 <- HYD_anag$IdPluv2[which(is.na(HYD_anag$IdStazione)==F)]
HYD_PP <- c(HYD_PP1,HYD_PP2)


#==============================================================================
#   LEGGO INFO DEL DBmeteo
#==============================================================================
cat( "  > leggi informazioni del DBmeteo\n", file=fileout,append=TRUE)

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
DBmeteo<-try(dbGetQuery(conn,"SET NAMES utf8"), silent=TRUE)
DBmeteo<-try(dbGetQuery(conn, "
SELECT
IDrete,A_Stazioni.IDstazione, A_Sensori.IDsensore, NOMEhydstra,NOMEtipologia,Attributo,IDsensore,Comune, Provincia  
FROM
A_Stazioni,A_Sensori
WHERE A_Stazioni.IDstazione=A_Sensori.IDstazione
AND NOMEtipologia in ('I','T','PP')
AND IDrete in (1,2,4)
AND DataInizio is not null
AND (DataFine>'2018-01-01' OR DataFine is NULL)"), silent=TRUE)

DBmeteo_rete<-DBmeteo$IDrete
DBmeteo_sens<-DBmeteo$IDsensore
DBmeteo_staz<-DBmeteo$IDstazione
DBmeteo_NOMEtipologia<-DBmeteo$NOMEtipologia
DBmeteo_NOMEhydstra<-DBmeteo$NOMEhydstra
DBmeteo$Attributo[which(is.na(DBmeteo$Attributo)==T)]=""
DBmeteo_nome<-paste(DBmeteo$Comune," ",DBmeteo$Attributo)

### tra le altre stazioni RICERCA DI STAZIONI con NOMEhydstra compilato nel DBmeteo 
DBmeteo_check<-try(dbGetQuery(conn, "
SELECT IDstazione ,Comune, NOMEhydstra 
FROM A_Stazioni 
WHERE 
NOMEhydstra is not NULL
AND IDstazione not in (
SELECT
A_Stazioni.IDstazione
FROM
A_Stazioni,A_Sensori
WHERE A_Stazioni.IDstazione=A_Sensori.IDstazione
AND NOMEtipologia in ('I','T','PP')
AND IDrete in (1,2,4)
AND DataInizio is not null
AND (DataFine>'2018-01-01' OR DataFine is NULL)
)
 "), silent=TRUE)

DBmeteo_check_nome<-DBmeteo_check$Comune
DBmeteo_check_nomeH<-DBmeteo_check$NOMEhydstra
DBmeteo_check_idstaz<-DBmeteo_check$IDstazione

#cat("\n altre stazioni in DBmeteo con NOMEhydstra compilato \n",file=fileout,append=T)
#if (length(DBmeteo_check_nome)>0) {
#cat("\n stazioni trovate: ", length(DBmeteo_check_nome),"\n", file=fileout,append=T)
#  cat("\n IDstaz, NomeHydstra\n", file=fileout,append=T)
#  iii<-1
#  while(iii<length(DBmeteo_check_nome)+1){
#   cat(as.vector(DBmeteo_check_idstaz[iii]),
#       ",",
#       as.vector(DBmeteo_check_nomeH[iii])  ,"\n",file=fileout,append=T)
#  iii<-iii+1
#  }
#} else {
#  cat("\nstazioni trovate 0\n",file=fileout,append=T)
#}

## 
kkk <- DBmeteo_check_nomeH %in% HYD_SITE_nome2 
cat("\n\n NOMI Hydstra in DBmeteo che non hanno corrispettivo in Hydstra;\n ",DBmeteo_check_nomeH[!kkk],"\n",file = fileout, append=T )

#### RICERCA STAZIONI IN HYDSTRA SITE MA NON IN DBMETEO O SENZA NOMEHYDSTRA COMPILATO #### 

#cat("\n -------------------------------------------------------\n",file=fileout,append=T)
#cat("\n Ricerca STAZIONI appartenenti ad HYDSTRA SITE ma non al DBmeteo \n",file=fileout,append=T)
#aux<-HYD_SITE_staz %in% DBmeteo_staz
#if (length(HYD_SITE_staz[!aux])>0) {
#cat("\n stazioni trovate: ", length(HYD_SITE_staz[!aux]),"\n", file=fileout,append=T)
#  cat("\n IDstaz, Nome\n", file=fileout,append=T)
#  iii<-1
#  while(iii<length(HYD_SITE_staz[!aux])+1){
#   cat(as.vector(HYD_SITE_staz[!aux][iii]),
#       ",",
#       as.vector(HYD_SITE_nome2[!aux][iii])  ,"\n",file=fileout,append=T)
#  iii<-iii+1
#  }
#} else {
#  cat("\nstazioni trovate 0\n",file=fileout,append=T)
#}

####################### RICERCA STAZIONI NON IN HYDSTRA SITE 

cat("\n -------------------------------------------------------------------\n",file=fileout,append=T)
cat("\n Ricerca STAZIONI appartenenti al DBmeteo ma non appartenenti ad HYDSTRA SITE\n",file=fileout,append=T)
aux<-DBmeteo_staz %in% HYD_SITE_staz 
if (length(DBmeteo_nome[!aux])>0) {
cat("\n stazioni trovate: ", length(DBmeteo_staz[!aux]),"\n", file=fileout,append=T)
  cat("\n Rete, IDstaz, Nome\n", file=fileout,append=T)
  iii<-1
  while(iii<length(DBmeteo_staz[!aux])+1){
   if(DBmeteo_rete[!aux][iii]==1)DBmeteo_rete[!aux][iii]="Aria"
   if(DBmeteo_rete[!aux][iii]==4)DBmeteo_rete[!aux][iii]="INM"
   if(DBmeteo_rete[!aux][iii]==2)DBmeteo_rete[!aux][iii]="CMG"
   if(DBmeteo_rete[!aux][iii]==5)DBmeteo_rete[!aux][iii]="extraLomb"
   if(DBmeteo_rete[!aux][iii]==6)DBmeteo_rete[!aux][iii]="altro"
   cat(as.vector(DBmeteo_rete[!aux][iii]),
       ",",
       as.vector(DBmeteo_staz[!aux][iii]),
       ",",
       as.vector(DBmeteo_nome[!aux][iii])  ,"\n",file=fileout,append=T)
  iii<-iii+1
  }
} else {
  cat("\nstazioni trovate 0\n",file=fileout,append=T)
}

####### RICERCA STAZIONI NON IN HYDSTRA Anagrafica

cat("\n -------------------------------------------------------------------\n",file=fileout,append=T)
cat("\n Ricerca STAZIONI appartenenti al DBmeteo ma non appartenenti ad HYDSTRA Anagrafica\n",file=fileout,append=T)
aux<-DBmeteo_staz %in% HYD_staz
if (length(DBmeteo_nome[!aux])>0) {
cat("\n stazioni trovate : ", length(DBmeteo_staz[!aux]),"\n", file=fileout,append=T)
  cat("\n Rete, IDstaz, Nome,  IDsens\n", file=fileout,append=T)
  iii<-1
  while(iii<length(DBmeteo_staz[!aux])+1){
   if(DBmeteo_rete[!aux][iii]==1)DBmeteo_rete[!aux][iii]="Aria"
   if(DBmeteo_rete[!aux][iii]==4)DBmeteo_rete[!aux][iii]="INM"
   if(DBmeteo_rete[!aux][iii]==2)DBmeteo_rete[!aux][iii]="CMG"
   if(DBmeteo_rete[!aux][iii]==5)DBmeteo_rete[!aux][iii]="extraLomb"
   if(DBmeteo_rete[!aux][iii]==6)DBmeteo_rete[!aux][iii]="altro"
   cat(as.vector(DBmeteo_rete[!aux][iii]),
       ",",
       as.vector(DBmeteo_staz[!aux][iii]),
       ",",
       as.vector(DBmeteo_nome[!aux][iii]),
       ",",
       as.vector(DBmeteo_sens[!aux][iii])  ,"\n",file=fileout,append=T)
  iii<-iii+1
  }
} else {
  cat("\nstazioni trovate 0\n",file=fileout,append=T)
}

####### RICERCA SENSORI NON IN HYDSTRA Anagrafica   

cat("\n -------------------------------------------------------------------\n",file=fileout,append=T)
cat("\n Ricerca sensori appartenenti al DBmeteo ma non appartenenti ad HYDSTRA Anagrafica\n",file=fileout,append=T)
cat("\n Lo script non gestisce i terzi sensori, 7050 e 22000 sono presenti in Hydstra\n",file=fileout,append=T)
cat("\n Il sensore 1799 non è da inserire in Hydstra\n",file=fileout,append=T)
aux<-DBmeteo_sens %in% c(HYD_T,HYD_I,HYD_PP)
if (length(DBmeteo_nome[!aux])>0) {
cat("\n sensori trovati : ", length(DBmeteo_staz[!aux]),"\n", file=fileout,append=T)
  cat("\n Rete, IDstaz, Nome, IDsens\n", file=fileout,append=T)
  iii<-1
  while(iii<length(DBmeteo_staz[!aux])+1){
   if(DBmeteo_rete[!aux][iii]==1)DBmeteo_rete[!aux][iii]="Aria"
   if(DBmeteo_rete[!aux][iii]==4)DBmeteo_rete[!aux][iii]="INM"
   if(DBmeteo_rete[!aux][iii]==2)DBmeteo_rete[!aux][iii]="CMG"
   if(DBmeteo_rete[!aux][iii]==5)DBmeteo_rete[!aux][iii]="extraLomb"
   if(DBmeteo_rete[!aux][iii]==6)DBmeteo_rete[!aux][iii]="altro"
   cat(as.vector(DBmeteo_rete[!aux][iii]),
       ",",
       as.vector(DBmeteo_staz[!aux][iii]),
       ",",
       as.vector(DBmeteo_nome[!aux][iii]),
       ",",
       as.vector(DBmeteo_NOMEtipologia[!aux][iii]),
       ",",
       as.vector(DBmeteo_sens[!aux][iii])  ,"\n",file=fileout,append=T)
  iii<-iii+1
  }
} else {
  cat("\n trovati 0\n",file=fileout,append=T)
}

####### RICERCA STAZIONI SENZA NOMI IN HYDSTRA NEL DBMETEO 

#cat("\n -------------------------------------------------------------------\n",file=fileout,append=T)
#cat("\n Ricerca stazioni senza NOMEhydstra nel DBmeteo\n\n",file=fileout,append=T)
#i<-1
#while(i<=length(DBmeteo_staz)) {
#  j<-which(HYD_SITE_staz==DBmeteo_staz[i])
#
#  if (length(j)!=1) {
#  cat("in HYDSTRA ID=", DBmeteo_staz[i], " non esiste\n", file=fileout,append=T)
#  } else {
#    if(is.na(DBmeteo_NOMEhydstra[i])==TRUE)cat("ATTENZIONE, stazione ", DBmeteo_staz[i]," - ",HYD_SITE_nome2[j], " senza nome HYDSTRA\n", sep="" ,file=fileout, append=T)
#  }
#  i <- i + 1
#}


####### RICERCA DISCREPANZE NEI NOMI HYDSTRA DEL DBMETEO

cat("\n -------------------------------------------------------------------\n",file=fileout,append=T)
cat("\n Nomi HYDSTRA mancanti o da correggere in DBmeteo\n\n",file=fileout,append=T)
i<-1
while(i<=length(DBmeteo_staz)) {
   j<-which(HYD_SITE_nome2==DBmeteo_NOMEhydstra[i])

   if (length(j)!=1) {
   cat("in DBmeteo ID=", DBmeteo_staz[i],"NOME=",DBmeteo_nome[i],"=>"," nome Hydstra in DBmeteo=",DBmeteo_NOMEhydstra[i], "\n", file=fileout,append=T)
  } else {
  }
  i <- i + 1
}

####### RIEPILOGO NOMI IN HYDSTRA SITE

#cat("\n -------------------------------------------------------------------\n",file=fileout,append=T)
#cat("\n Riepilogo dei nomi in HYDSTRA SITE\n",file=fileout,append=T)
#cat("\n IDstaz, IDsens, UTM_Est, UTM_Nord,NOMEhydstra_DBmeteo, NOMEhydstra_HYDSTRA,HYD_SITE_nome, HYD_SITE_nome3,  DBmeteo_nome\n", file=fileout,append=T)

#i<-1
#while(i<=length(DBmeteo_staz)) {
#  j<-which(HYD_SITE_staz==DBmeteo_staz[i])
#
#  if (length(j)!=1) {
#  cat("in HYDSTRA ID=", DBmeteo_staz[i], " non esiste\n", file=fileout,append=T)
#  } else {
#   cat(DBmeteo_staz[i],
#       ",",
#       DBmeteo_sens[i],
#       ",",
#       DBmeteo_UTMX[i],
#       ",",
#       DBmeteo_UTMY[i],
#       ",",
#       DBmeteo_NOMEhydstra[i],
#       ",",
#       HYD_SITE_nome2[j],
#       ",",
#       HYD_SITE_nome[j],
#       ",",
#       HYD_SITE_nome3[j],
#       ",",
#       DBmeteo_nome[i]  ,"\n",file=fileout,append=T)
#  }
#  i <- i + 1
#}

####### RIEPILOGO NOMI E ASSOCIAZIONE STAZIONI/SENSORI IN HYDSTRA Anagrafica
#HYD_staz <- HYD_anag$IdStazione[which(is.na(HYD_anag$IdStazione)==F)]
#HYD_nome <- HYD_anag$Nome[which(is.na(HYD_anag$IdStazione)==F)]
#HYD_T1 <- HYD_anag$IdTerm[which(is.na(HYD_anag$IdStazione)==F)]
#HYD_T2 <- HYD_anag$IdTerm2[which(is.na(HYD_anag$IdStazione)==F)]
#HYD_T <- c(HYD_T1,HYD_T2)
#
#cat("\n -------------------------------------------------------------------\n",file=fileout,append=T)
#cat("\n Riepilogo dei nomi in HYDSTRA Anagrafica\n",file=fileout,append=T)
#cat("\n IDsens, HYD_nome,  DBmeteo_nome, HYD_staz, DBmeteo_staz\n", file=fileout,append=T)
#i<-1
#while(i<=length(DBmeteo_sens)) {
#  j<-which(HYD_T==DBmeteo_sens[i])
#
#  if (length(j)!=1) {
#  cat("in HYDSTRA ID=", DBmeteo_sens[i], " non esiste\n", file=fileout,append=T)
#  } else {
#   cat(DBmeteo_sens[i],
#       ",",
#       HYD_nome[j],
#       ",",
#       DBmeteo_nome[i]  ,
#       ",",
#       HYD_staz[j],
#       ",",
#       DBmeteo_staz[i],"\n",file=fileout,append=T)
#   if(is.na(DBmeteo_staz[i])==F && is.na(HYD_staz[j])==F) {
#    if(DBmeteo_staz[i]!=HYD_staz[j])cat("\n\n\ ATTENZIONE, sensore ",DBmeteo_sens[i]," ha DBmeteo_IDstaz=",DBmeteo_staz[i]," ma HYDSTRA_IDstaz=",HYD_staz[j],"\n\n\n\n",file=fileout,append=T)
#    }
#  }
#  i <- i + 1
#}
#
#comando <- paste ("mv " , fileout," /srv/www/htdocs/applications/controlli/")
#try(system(comando, intern = TRUE))
#------------------------------------------------------------------------------
warnings()
quit(status=0)

