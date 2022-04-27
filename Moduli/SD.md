Questo modulo gestisce ogni vendita dalla registrazione del prodotto, info del cliente, dettagli, prezzi, consegna e fatturazione.


**Struttura di vendita**    
La struttura organizzativa è composta da: 
- *Org. commerciale:* definisce a livello logistico l'azienda
- *Canale distributivo:* definisce come vengono distribuiti i prodotti ( al dettaglio, ingrosso, ecc.. )
- *Settore merceologico:* Definisce le macrotipologie di prodotti

Questi tre parametri definiscono l'area vendite. A seconda di quest'ultima possono essere creati o meno determinati documenti.
Per un cliente è necessario aver associata un'area vendite per poter raggiungere un prodotto al quale è stata associata la medesima area vendite in anagrafica materiale.

**Business Partner**    
Anagrafica di clienti/fornitori ( in R3 erano separati ).
Ogni anagrafica è divisa in tre parti: dati generali, dati societari, dati vednite.    
Ogni cliente ha un unico committente ( se stesso ) e poi altri tre ruoli assegnati che sono l'esecutore pagamenti, destinatario merci e destinatario fattura ( relazione 1-N, es. clienti con più sedi )


**Incoterms**     
Obbligatorio per i clienti. Definisce il limite responsabilità fornitore/cliente di un prodotto.
