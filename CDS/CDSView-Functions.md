<b>FUNCTION</b></br>
Esistono delle functions che migliorano la qualità del codice. Possono essere <i>SQL Functions</i> o <i>Built-In Functions</i>.

- <i>SQL Functions</i></br>
Sono funzioni basilari e non flessibili e ritornano un risultato logico o booleano. Questa categoria di funzioni può essere sottodivisa 
in altre due categorie: numeriche e stringhe.
  - <i>Numeriche</i>

| Funzione                  | Definizione                               | Output                            |
|---------------------------|-------------------------------------------|-----------------------------------|
| ABS(arg)                  | Valore assoluto del numero del numero arg | Valore assoluto                   |
| CEIL(arg)                 | Arrotondamento numero con la virgola      | Numero arrotondato per difetto    |
| DIV(arg1, arg2)           | Divisione convenzionale (interi)          | Quoziente                         |
| DIVISION(arg1, arg2, dec) | Divisione con decimali                    | Risultato arrotondato ai decimali |
| MOD(arg1, arg2)           | Operazione modulo convenzionale           | Ottiene il resto della divisione  | 
| FLOOR(arg)                | Arrotondamento numero con la virgola      | Numero arrotondato per eccesso    |
| ROUND(arg, pos)           | Arrotondamento numero con decimali        | Numero arrotondato per decimale   |

  - <i>Stringhe</i>
  
| Funzione                           | Definizione                                                                  |
|------------------------------------|------------------------------------------------------------------------------|
| LENGHT(arg)                        | Lunghezza della stringa                                                      | 
| INSTR(arg1, arg2)                  | Posizione di una stringa in un'altra                                         | 
| CONCATENATE(arg1, arg2)            | Divisione convenzionale (interi)                                             | 
| CONCATENATE WITH SPACE(arg1, arg2) | Divisione con decimali                                                       | 
| LEFT(arg1, arg2)                   | Prende le prime n lettere di arg1                                            |  
| LOWER(arg1)                        | Converte in minuscolo la stringa passata                                     |                       
| LPAD(arg1, arg2, arg3)             | Aggiunge ad arg1 la stringa arg3 per arrivare alla lunghezza arg2 (a sinistra)|
| RPAD(arg1, arg2, arg3)             | Aggiunge ad arg1 la stringa arg3 per arrivare alla lunghezza arg2 (a destra)  |
| LTRIM(arg1, arg2)                  | Rimuove da arg1 la stringa indicata in arg2 (la prima a sinistra)             |
| RTRIM(arg1, arg2)                  | Rimuove da arg1 la stringa indicata in arg2 (la prima a destra)               |
| REPLACE(arg1, arg2, arg3)          | Sostituisce ad arg1 la stringa arg3 al posto della sottostringa arg2          |
| SUBSTRING(arg1, arg2, arg3)        | Leggi nell'arg1 le n lettere (arg3) partendo dalla posizione arg3             |
| UPPER(arg1)                        | Converti tutte le lettere in maiuscolo                                        |
 
- <i>Built-In Functions</i></br>
  - <i>Unit Conversion Functions</i><br>
   <b>Sintassi</b></br>
    unit_conversion( quantity => brgew, 
                   source_unit => meins, 
                   target_unit => gewei 
                   )</br>
    <b>Regole</b></br>
    Se non ci sono relazioni tra il source_unit e il target_unit ci sarà un dump.</br>
    
  - <i>Currency Conversion Functions</i><br>
   <b>Sintassi</b></br>
    currency_conversion( amount => a.price, 
                   source_currency => a.currency, 
                   target_currency => :p_to_curr,
                   exchange_rate_date => :p_conv_date
                   )</br>
    <b>Regole</b></br>
    Non ottiene dei risultati precisi.</br>
    
  - <i>Decimal Shift</i><br>
   <b>Sintassi</b></br>
     decimal_shift( amount => :p_amt, currency => a.currency )</br>
    <b>Regole</b></br>
    Esegue la conversione in base ai dati interni (tab TCURX) -> Approfondire.</br>
    
   - Date
     - Add Days</br>
     <b>Sintassi</b></br>
     dats_add_days(a.fldate, :p_add_days  , 'INITIAL')</br>
    <b>Regole</b></br>
    Il primo parametro è la data, il secondo i giorni da aggiungere, il terzo è per la gestione dell'errore (INITIAL, FAIL, NULL, INITIAL, UNCHANGED)</br>
    
     - Add Month</br>
     <b>Sintassi</b></br>
     dats_add_months(a.fldate, :p_add_months, 'NULL')</br>
     <b>Regole</b></br>
     Come per i giorni il primo parametro è la data, il secondo i mesi da aggiungere, il terzo è la gestione dell'errore
     
     - Days between two dates</br>
     <b>Sintassi</b></br>
     dats_days_between (a.fldate, $parameters.p_curr_date )</br>
     <b>Regole</b></br>
     Il primo parametro è il primo giorno, il secondo è l'ultimo
     
     - Date validation</br>
     <b>Sintassi</b></br>
     dats_is_valid(a.fldate)</br>
     <b>Regole</b></br>
     Controlla se la data è valida (ritorna 1 o 0)
     
> NB<br>
Alcune delle built-in functions contengono bug.
