#!/bin/bash
# 1. legge da xxxx  il file di anagrafica estratto dal REM ogni giorno, 
#    scrive su file eventuali discrepanze 
#    e carica il file su xxxxxxxxxx 
#
# 2.  legge da xxxxxxxxx il file con le estrazioni dati dal REM estratto dal REM ogni giorno, 
#     scrive su file eventuali discrepanze con quello che risulta al DBmeteo
#     e carica il file su xxxx
#
# 3   allinemento anagrafiche HYDSTRA
#
#################################################################################

# variabili da env.txt da passare nel configurare il servizio
#MYSQL_USR
#MYSQL_PWD
#MYSQL_DBNAME
#MYSQL_HOST
#MYSQL_PORT
#nas_usr
#NAS
#nas_dir
#fake

FILE_ANAG_CSV='AnagraficaSensori.csv'
FILE_ESTR_CSV='AnagraficaEstrazioni.csv'
FILE_HYD_ANAG_CSV='Anagrafica.csv'
FILE_HYD_SITE_DBF='SITE.DBF'

numsec=3600
SECONDS=$numsec

#endless loop
while [ 1 ]
do
  if [[ $(date +"%H") == "05" || ($SECONDS -ge $numsec) ]]
  then
  
  
  ################# 1 #################################

   # leggo il file di anagrafica 
      smbclient -U $nas_usr $NAS -n $fake -c "prompt; cd ${nas_dir}; mget $FILE_ANAG_CSV; quit"

echo 'IdReteVis;NomeReteVis;idStazione;Nome;IstatProvincia;Provincia;IstatComune;Comune;Attributo;Localita;Fiume;Bacino;IdTipologia;Descrizione;IdSensore;Storico;UnitaMisura;Frequenza;Quota;UTM Nord;UTM Est;CGB_Nord;CGB_Est;lat decimale;Lon decimale;DataMinimaRT;DataMassimaRT;visibilitaweb;invioPC;UTM Nord Staz;UTM Est Staz;Quota Staz;UOTitolare'  >  temp
  awk 'BEGIN{FS=";"}{if(NR>1) print $0}' $FILE_ANAG_CSV  >>  temp
  
  mv temp $FILE_ANAG_CSV


   #eseguo lo script 
   Rscript A_allineamentoREM.R 
   
   # verifico se è andato a buon fine
   STATO=$?
   echo "STATO USCITA SCRIPT ====> "$STATO

   if [ "$STATO" -eq 1 ] # se si sono verificate anomalie esci 
   then
       exit 1
   else # caricamento 

      smbclient -U $nas_usr $NAS -n $fake -c "prompt; cd ${nas_dir}; mput *.out; quit"

       # controllo sul caricamento 
       if [ $? -ne 0 ]
       then
         echo "problema caricamento su nasprevisore"
         exit 1
       fi
   fi

   rm -f allineamentoREM.out
   rm -f PC_e_FormWeb.out
   
    
#   ################# 2 #################################
   
#   # leggo il file di anagrafica
      smbclient -U $nas_usr $NAS -n $fake -c "prompt; cd ${nas_dir}; mget $FILE_ESTR_CSV; quit"
#     
#   # rimuovo caratteri nascosti
   echo $FILE_ESTR_CSV | tr -d '\277' | tr -d '\273' | tr -d '\357' >> $FILE_ESTR_CSV
#
#   #eseguo lo script 
   Rscript estrazioni.R 
#   
#   # verifico se è andato a buon fine
   STATO=$?
   echo "STATO USCITA SCRIPT ====> "$STATO
#
   if [ "$STATO" -eq 1 ] # se si sono verificate anomalie esci 
   then
      exit 1
   else # caricamento 
      smbclient -U $nas_usr $NAS -n $fake -c "prompt; cd ${nas_dir}; mput *.out; quit"
       
       # controllo sul caricamento 
       if [ $? -ne 0 ]
       then
         echo "problema caricamento estrazioni su nasprevisore"
         exit 1
       fi
   fi
    
   rm -f diff_estrazioni.out
   

#   ################# 3 allineamento HYDSTRA  #################################
   
#   # leggo il file di anagrafica
      smbclient -U $nas_usr $NAS -n $fake -c "prompt; cd ${nas_dir}; mget $FILE_HYD_ANAG_CSV; quit"
      smbclient -U $nas_usr $NAS -n $fake -c "prompt; cd ${nas_dir}; mget $FILE_HYD_SITE_DBF; quit"
#     
#   # rimuovo caratteri nascosti
   #echo $FILE_ESTR_CSV | tr -d '\277' | tr -d '\273' | tr -d '\357' >> $FILE_ESTR_CSV
#
#   #eseguo lo script 
   Rscript anag_hydstra.R 
#   
#   # verifico se è andato a buon fine
   STATO=$?
   echo "STATO USCITA SCRIPT ====> "$STATO
#
   if [ "$STATO" -eq 1 ] # se si sono verificate anomalie esci 
   then
      exit 1
   else # caricamento 
      smbclient -U $nas_usr $NAS -n $fake -c "prompt; cd ${nas_dir}; mput aa_HYDSTRA.txt; quit"
       
       # controllo sul caricamento 
       if [ $? -ne 0 ]
       then
         echo "problema caricamento estrazioni su nasprevisore"
         exit 1
       fi
   fi
    
   rm -f aa_HYDSTRA.txt
   


   
   SECONDS=0
   sleep $numsec
  fi
    
done
