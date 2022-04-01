<b>DEFINIZIONE GENERALE</b></br>
La CDS view è un'infrastruttura che definisce un modello dati (simile ad una semplice view). Questo modello dati viene creato nel database e non sul server. Questo vantaggio viene dato dal fatto che una view normale risiede nel dictionary e per l'estrazione dei dati deve creare una connessione a DB passando dal server. La CDS è un semplice 'oggetto' che risiede nel DB e quindi non deve essere gestita una connessione con quest'ultimo. Esistono CDS VIEW con e senza parametri, ABAP CDS e HANA CDS. Quando vengono selezionati i campi da estrarre possono essere decise le chiavi anteponendo al campo la parola key ( le chiavi devono essere contigue e partire dal primo campo ).
</br>
</br>
<b>ODATA SERVICE</b></br>
Le CDS View possono essere utilizzate anche per esporre un servizio OData (altrimenti creabile tramite la transazione SEGW) tramite l'annotazione <i>@OData.publish: true.</i> Una volta creata la view utilizzare la transazione <i>/IWFND/MAINT_SERVICE</i> per registrare il servizio. Testare il servizio tramite la transazione <i>/IWFND/GW_CLIENT</i>. <br>
Possono esserci degli errori: https://sapyard.com/odata-service-from-cds-annotation-not-working-in-browser-mode/
</br>
</br>
<b>S4 CLOUD</b></br>
Sono necessarie per l'estrazione dei dati dalle tabelle in S4 CLOUD. Vengono fornite delle CDS standard e possono essere create delle CDS custom partendo da queste (se possiedi il ruolo <i>BR_ADMINISTRATOR</i>).
</br>
</br>
<b>AUTORIZZAZIONI</b></br>
Quando una CDS viene creata ha l'annotazione <i>@AccessControl.authorizationCheck: #NOT_REQUIRED</i> per indicare che non servono privilegi particolari per poterla utilizzare. Va quindi creato un <i>Access Control</i>. Questo access control può essere utilizzato in 3 modi diversi: </br>  
    - <i>Full access:</i> Non vengono inserite restrizioni. Nel Access control viene inserito solo il nome della view.</br> 
    - <i>Literal Conditions:</i> Vengono imposte delle condizioni di estrazione per limitare i dati che vengono restituiti</br> 
    - <i>PFCG (approfondimento nei link esterni):</i> Viene creato un ruolo che permette agli utenti di utilizzare solo alcune funzioni</br> 
    
Esistono altri metodi come la combinazione tra literal e pfcg, autorizzazioni ereditate e la current user authorization.
</br>
</br>
<b>TRADUZIONI E TESTI</b></br>
Nelle CDS possono essere creati dei testi: label (con un massimo di 60 caratteri) e le quickinfo (stringhe). 
Vengono dichiarate con il simbolo "@":
```abap
@EndUserText.label: 'Test'
@EndUserText.quickInfo: 'Second test'
```
Questi testi possono essere tradotti poi con la transazione <i>SE63</i> ed è possibile inserire queste traduzioni in un trasporto con la transazione <i>SLXT</i>.<br>
E' possibile accedere ai testi e alle traduzioni in modo dinamico utilizzanod la classe <i>CL_DD_DDL_ANNOTATION_SERVICE</i> metodo <i>GET_LABEL_4_ELEMENT</i>. Con questo metodo si può specificare la linga in cui si vogliono avere i testi oppure lasciare che prensa quella di accesso (senza quindi specificarla).
</br>
</br>
<b>SINTASSI BASILARE</b></br>
Le CDS sono parametrizzate, ovvero accettano anche variabili in input (le CDS sono case-sensitive). E' importante ricordare che lavorano anche tramite alias, soprattutto quando vengono fatte delle associazioni di join. Anche qui esistono delle variabili di sistema:

| Variabile          | Descrizione                                                                 |
|--------------------|-----------------------------------------------------------------------------|
| user               | Corrisponde alla variabile sy-uname in abap. Contiene il nome utente        |
| client             | Corrisponde alla variabile sy-mandt in abap. Contiene quindi il mandante    |               
| system_language    | Corrisponde alla variabile sy-langu in abap. Lingua utilizzata dal sistema  |
| system_date        | Corrisponde alla variabile sy-datum in abap. Contiene la data corrente      |

- <i>Case distinction</i>
    - CASE a IS NULL THEN 'err'

- <i>Condition</i>
    - a = b
    - a <> b
    - a > b
    - a < b
    - a >= b
    - a <= b
    - a <= n AND b > n2 (per intervalli)
    - a LIKE b
    - a IS [NOT] NULL
    
- <i>Espressioni aritmetiche</i>

- <i>Espressioni di aggregazioni</i>
    - MAX
    - MIN
    - AVG
    - SUM
    - COUNT
    - GROUP BY (obbligatorio)
    - HAVING
    
- <i>Espressioni di casting</i>

| Variabile               | Descrizione                                                                 |
|-------------------------|-----------------------------------------------------------------------------|
| abap.char( len )        | CHAR with length len                                                        |
| abap.clnt[(3)]          | CLNT                                                                        |               
| abap.cuky( len )        | CHAR with length len                                                        |
| abap.curr(len,decimals) | CURR with length len and decimals decimal places                            |
| abap.dats[(8)]          | DATS                                                                        |
| abap.dec(len,decimals)  | DEC with length len and decimals decimal places                             |
| abap.fltp[(16,16)]	  | FLTP                                                                        |
| abap.int1[(3)]	      | INT1                                                                        |
| abap.int2[(5)]	      | INT2                                                                        |
| abap.int4[(10)]         | INT4                                                                        |
| abap.int8[(19)]         | INT8                                                                        |
| abap.lang[(1)]          | LANG                                                                        |
| abap.numc( len )        | NUMC with length len                                                        |
| abap.quan(len,decimals) | QUAN with length len with decimals decimal places                           |
| abap.raw(len)           | RAW                                                                         |
| abap.sstring(len)       | SSTRING                                                                     |
| abap.tims[(6)]          | TIMS                                                                        |
| abap.unit( len )        | CHAR with length len                                                        |

</br>
</br>
</br>
</br>

 - Note
    - <b>System access:</b> (privilege) per l'utilizzo di certe funzioni
    - <b>Authorization Objects:</b> sono certe transazioni a cui possiamo accedere o meno
    - <b>DCL:</b> Data control language. Utilizzato per controllare i privilegi nelle operazioni su DB
    - <b>GRANT:</b> Usato per indicare ogni privilegio per un utente
    - <b>REVOKE:</b> Usato per rimuovere i privilegi
    - <b>ROLE:</b> Permessi speciali per alcune funzioni specifiche. Mantenute dalla transazione <i>PFCG</i>.
