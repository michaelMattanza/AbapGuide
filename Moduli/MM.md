<h1>Modulo MM</h1>
Il modulo MM (Material Management) è un modulo che consente la gestione di materiale, inventario e magazzino. I componenti più utilizzati sono Master Data, Acquisti e inventario.     
<br>
<br>
     
**Divisione (Plant)**    
Con l’unità organizzativa divisione si identificano generalmente le unità produttive oppure i 
magazzini centrali che abbiano una certa rilevanza ai fini della pianificazione. La divisione 
costituisce, infatti, l’unità organizzativa di riferimento per la gestione dei materiali e la 
pianificazione. A livello divisione vengono definite le anagrafiche di distinte basi, cicli e centri di 
lavoro.    
Una divisione può essere:
- uno stabilimento 
- un deposito centrale di consegna
- un ufficio vendite regionale
- la sede di un’azienda

**Magazzino (storage location)**    
Il magazzino serve per gestire il flusso e lo stoccaggio dei materiali in maniera diversificata 
all’interno dello stesso stabilimento (divisione). Ad un magazzino può corrispondere un’area 
fisica (magazzino esistente) oppure un’area logica (magazzino virtuale).

**Organizzazione acquisti**    
L’organizzazione acquisti è l’unità organizzativa principale degli Acquisti. Provvede all’acquisto 
di materiali e servizi per una o più divisioni e definisce con i fornitori le condizioni generali 
d’acquisto. L’organizzazione acquisti può essere assegnata ad una sola società.
- Se un’organizzazione acquisti è collegata con più divisioni, la determinazione del prezzo 
può essere valida per tutte le divisioni, essendo le condizioni prezzo definite a livello 
dell’organizzazione acquisti.
- L’autorizzazione agli acquisti viene spesso definita a livello di organizzazione acquisti

**Gruppo acquisti**    
L’organizzazione acquisti si articola in gruppi acquisti, identificabili con i buyer, che sono
responsabili delle attività operative quotidiane.
Il Gruppo Acquisti, in combinazione con il Gruppo Merci, costituisce infatti il principale parametro
di selezione per le estrazioni standard che si possono effettuare sul sistema SAP, quali ad
esempio:
- Lista delle richieste di acquisto
- Lista degli ordini di acquisto
- Lista dei record info

**Inforecord**    
Gli inforecord sono le informazioni che legano un determinato materiale ad un fornitore. Il sistema ripropone questa coppia durante gli acquisti. Le informazioni vengono ereditate dall'anagrafica fornitore e materiale mentre nell'inforecord vengono indicati in più i giorni di sollecito ( nel caso ci siano ).  Sono un'indicazione per aiutare a scegliere da che fornitore acquistare e possono essere creati/aggiornati in automatico dall'ordine d'acquisto.

**Source list**    
La source list come un inforecord crea un legame materiale/fornitore. Con una source list il fornitore viene imposto fisso per un determinato materiale durante la creazione automatica di ordini, piani di consegna e schedulazioni. Oltre a indicare da chi comprare un materiale si può utilizzare per indicare anche da chi non acquistare un determinato articolo.

**Flusso acquisto**    
Il flusso d'acquisto può essere:
    - Manuale: viene generata una richiesta d'acquisto, convertita poi in un ordine d'acquisto alla conferma. Tra i due step potrebbe esserci anche la richiesta d'offerta. In caso si conosca gà un fornitore si può procedere direttamente con l'ordine d'acquisto.
    - Automatico: l'MRP crea la richiesta d'acquisto che viene poi convertita in ordine, per poter fare l'EM.

**Richiesta d'acquisto**    
Procedura con la quale utenti o reparti possono richiedere l'acquisto di beni o servizi. Può essere generata dall'MRP. Utile solo pre l'ufficio acquisti. Non è un documento ufficiale.
Devono essere rilasciate

**Richiesta d'offerta**    
E' una ricerca di mercato: viene inviato a uno o più fornitori una richiesta d'offerta. Ha una scadenza entro il quale il fornitore deve riferire prezzo e se riesce a fornire il materiale per tempo indicato. La richiesta d'offerta può essere creata direttamente da richiesta d'acquisto o da un contratto.
Devono essere rilasciate

**Ordine d'acquisto**    
L'ordine d'acquisto è un documento legale e vincolante. Può essere creato da RDA, RDO, Contratto o direttamente in autonomia. L'ufficio acquisti deve essere autorizzato prima del rilascio. L'ordine può essere rilasciato da n persone in sequenza: se richiede conferme da più utenti è possibile vincolarlo dal customizing. Per ogni posizione dell'ordine d'acquisto possono venire configurate le schedulazioni.

**Piano di consegna**    
E' un accordo tra il reparto acquisti e un fornitore. Viene considerato come un ODA vincolante. Ha un inizio e fine di validità indicato per un determinato fornitore. Può contenere schedulazioni create in separata sede. Può essere creato in automatico dal sistema.

**Contratto**    
Il contratto è un accordo tra ufficio acquisti e fornitore. Può essere a valore o qualità, in base al tipo di acquisto che si vuole fare. Può essere creato da una RDA o da una RDO. E' comunque necessario creare un ODA in quanto il contratto non ha valore legale.
Il customizing può essere utilizzato per bloccare l'acquisto in caso di eccedenza. In caso non venga acquistato tutto entro la scadenza viene aperta una nota di debito per risarcire il fornitore del valore mancante.

**Entrata merce**    
La generazione dell'entrata merce richiede una data di registrazione e una bolla di consegna: un codice numerico segnato sul DDT. Se inserisco l'ordine d'acquisto durante la creazione la migo ne rileva tutti i dati. Una volta compilati i dati si apre un tab. posizione per indicare i pezzi entrati ( in quanto potrebbero non arrivare tutti ). Indicare il flag che l'entrata merce venga effettuata.

**Fattura MIRO**    
Fattura creata da ODA o bolla di consegna: nel primo caso propone le posizioni dell'ordine con la quantità complessivamente entrata senza indicare le consegne; nel secondo caso presenta nel dettaglio le diverse consegne con la quantità non ancora fatturata