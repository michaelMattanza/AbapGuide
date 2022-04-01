<b>Definizione</b><br>
Una join è l'unione di due tabelle per un'estrazione combinata di dati. Un'associations è sempre una join ma a livello di data model, non di dati estratti. Può essere utilizzata anche la join se inserita tra i dati (vedere link esterni - associations part 1 ).
<br><br>
<b>Differenze</b><br>
- In un'associazione le chiavi vengono prese direttamete dalla tabella principale (senza dover definire la tabella) e, nel caso dalla tabella associata non vengono estratti campi, questa non verrà coinvolta nella join migliorando le performance. 
- Utilizzando la parola "AS" non creiamo un alias come nelle join, creiamo un nome per l'associazione tra le tabelle (il nome dovrebbe iniziare per "_").
<br><br>

<b>Cardinalità</b><br>
E' la relazione tra le due origini di dati (tabelle o cds). La cardinalità viene definita solo per l'obiettivo.
Di default è " 0...1 " e quindi il valore minimo di deault è 0. Aggiungendo quindi [ 1 ] si intende " 0...1 ", [ 3 ] si intende " 0...3 ", [ * ] si intende " 0...* ". Il valore minimo non può essere * e il valore massimo non può essere 0.<br>
Viene introdotta nella select tramite la parola <i>association</i>.
