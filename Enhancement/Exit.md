<h1>Exit</h1>    
Le exit sono messe a disposizione dalla sap per la modifica di un comportamento standard senza la modifica del codice sorgente. 
Esistono vari tipi di exit ( tipo function, menu, screen, etc... ) attivabili tramite CMOD ( tcode del gruppo di exit ) ed SMOD ( tcode singola exit ). 

*User exit*: La user exit è un include standard dove un utente può inserire del codice per modificare determinati dati di un flusso standard.    
    
*Customer exit*: è una function creata da sap per chiamare il codice custom in un flusso standard. Il codice va inserito nell'include che essa contiene ( quindi la user exit ).

SXX: S standard exits della sap. 
UXX: U user exits definite dagli utenti

Lista Exit:
- EXIT_SAPLV56K_002 ( Exit Idoc DESADV - DELVRY07 )
