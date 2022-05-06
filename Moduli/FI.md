<h1>Modulo FI</h1>    
Il modulo FI ( contabilità finanziaria ) serve per gestire la contabilità relativa a pagamenti, record contabili in entrata e uscita, il patrimonio e conti bancari. Spesso si collega al modulo CO ( contabilità ). Lo scopo di questi moduli è registrare ogni transazione finanziaria.

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
