<h1>Modulo FI</h1>    
Il modulo FI ( contabilità finanziaria ) serve per gestire la contabilità relativa a pagamenti, record contabili in entrata e uscita, il patrimonio e conti bancari. Spesso si collega al modulo CO ( contabilità ). Lo scopo di questi moduli è registrare ogni transazione finanziaria in un determinato periodo.

**Piano dei conti**    
Il piano dei conti riporta i rendiconti della parte gestionale e finanziaria. I record presenti in questo piano dei conti possono essere di credito o debito. 

**Azienda**    
E’ l’unità organizzativa per la quale i rendiconti finanziari possono essere redatti secondo il diritto commerciale pertinente. Un’azienda può comprendere più codici aziendali. 
 
**BTE**   
Le BTE (Business Transaction Event) sono delle tecniche di enhancement sviluppate per il modulo FI. Le BTE sfruttano delle interfaccie 
create dagli sviluppatori collegate ad un codice evento nelle tabelle di configurazione. Vengono utilizzate per esigenze di business 
dove non esistano funzionalità standard e sono raggiungibili tramite la transazione *FIBF*.    

**Implemntare una BTE**
- Andanre in *FIBF -> Environment -> Infosystem(Processes)* troviamo la lista delle interfacce implementabili, 
ognuna con il proprio processo.   
- Selezionare il processo e premere il bottone "Sample Function Module" per poter creare una function module custom basata su una fm 
standard adatta al processo   
- Una volta inserito il codice nella fm tornare nella *FIBF* e, in *Settings*, collegare il processo con la nostra fm nella sezione 
corretta (Process,...).

*Vedere link esterni*

Contabilità clienti
Contabilità clienti in FICO SAP è un sottomodulo che cattura tutte le transazioni con i clienti e gestisce i conti dei clienti. Le transazioni nei crediti includono la registrazione delle fatture, la registrazione delle note di credito, gli acconti, il pagamento delle fatture, il sollecito e l'esecuzione dei rapporti dei clienti.
Contabilità fornitori
Contabilità fornitori è un modulo secondario che acquisisce tutte le transazioni con i fornitori e gestisce gli account dei fornitori.  Vengono mantenuti conti fornitore separati e quando le transazioni vengono registrate nei conti dei clienti, i conti di riconciliazione nella contabilità generale vengono aggiornati con le cifre in tempo reale. Le transazioni nei debiti contabili includono la registrazione delle fatture, la registrazione delle note di credito, gli acconti, il pagamento delle fatture, il programma di pagamento automatico e l'esecuzione dei rapporti dei fornitori.
Contabilità patrimoniale
La contabilità patrimoniale in SAP FICO gestisce tutte le transazioni relative alle attività per un'entità. Quando le transazioni sono registrate nei conti delle attività, i conti di riconciliazione nella contabilità generale vengono aggiornati in tempo reale. Le operazioni nella contabilità patrimoniale includono l'acquisizione, il pensionamento delle attività, la vendita delle attività, il trasferimento delle attività, la rivalutazione delle attività e l'ammortamento delle attività.
Contabilità bancaria
La contabilità bancaria cattura tutte le transazioni con le banche. La riconciliazione bancaria viene eseguita per riconciliare tutte le transazioni registrate sugli estratti conto bancari confrontandole con le transazioni nel sistema.
Tutti i sottomoduli SAP FI sono integrati e le transazioni vengono aggiornate in tempo reale, il che significa che è possibile estrarre dal sistema bilanci accurati in qualsiasi momento.
SAP Funds Management: questo modulo di contabilità SAP supporta tutte le attività correlate nella creazione e gestione dei budget. Il calcolo delle entrate, delle spese e dei fondi è incluso negli elenchi di attività della gestione dei fondi SAP.
SAP Travel Management: Questo modulo SAP tiene conto di tutte le transazioni relative ai viaggi aziendali organizzati all'interno e dall'organizzazione. Approvazioni, prenotazioni, liquidazione e diverse spese di viaggio vengono registrate e gestite utilizzando il modulo di gestione dei viaggi SAP.
