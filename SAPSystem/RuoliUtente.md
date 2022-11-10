-	Allineare i ruoli attualmente in DEV a quelli di PRD: ieri abbiamo provato a importare il file che trovi sul teams ma a causa delle transazioni non più presenti 
in S/4 non è stato possibile. La cosa migliore da fare, considerando che DEV è copia di PRD al 19.09, è capire da quella data a oggi cosa è cambiato in PRD:
    -	Importando eventuali ruoli creati da quella data in avanti (li trovi in tabella AGR_DEFINE)
    -	Controllando in SUIM i ruoli modificati a partire da quella data
-	Eseguire nuovamente step SU25 2D in DEV seguendo le decisioni già prese in questo file per i vari ruoli + analizzando il delta dei nuovi ruoli non coperti in SBX
    -	Confrontarsi con i funzionali di modulo su action pending ed eventualmente delta
-	Eseguire step SU25 2C per tutti i ruoli da rimediare, prendendo evidenze delle decisioni prese
-	Regolarmente capire cosa cambia in PRD (ogni 15-20 giorni) e allineare DEV
