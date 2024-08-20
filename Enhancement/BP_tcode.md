<h1>BP Enhancement</h1>
Questo documento mostrerà come implementare una schermata custom nella transazione standard BP seguendo un esempio pratico.

- *Creazione Function Group*     
  Per prima cosa andare in SE80 e creare un screen contente i campi da visualizzare.
  ![dynpro setting](https://github.com/user-attachments/assets/7ce21b65-3246-449f-8141-ad785bd35c6c)
  ![2](https://github.com/user-attachments/assets/46c7a13c-f297-45e6-9df7-9f066d56119b)

  Creare poi due function module che costituiranno il PBO e il PAI.

- *Informazioni Transazione BP*      
  Una volta creato lo screen, dobbiamo ottenere delle informazioni dalla transazione standard. Andare in *BP* e trovare la sezione dove si vuole inserire il nuovo tab:
  ![bp](https://github.com/user-attachments/assets/082799f5-3091-4b89-8ccb-7391b0171844)

  Una volta che siamo nella transazione standard lanciamo *BDT_ANALYZER* per capire in quale view dobbiamo agganciarci
  ![bp ana](https://github.com/user-attachments/assets/47e86949-2871-4773-a630-ce1fea2f44fe)

- *Struttura CMSD*     
  Andare in SE11 e cercare la struttura CMDS_EI_CMD_CENTRAL:     
  aggiungere quindi un append (includendo il campo custom) nelle due strutture che contiene ( data e datax )
  ![cmd](https://github.com/user-attachments/assets/60e4a60f-e9ba-424d-a799-4ced8a27eead)
 
   
   
