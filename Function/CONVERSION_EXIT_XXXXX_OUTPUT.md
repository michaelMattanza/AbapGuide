<h1>Conversion Exit</h1>    

Per creare una exit di conversione valore (input/output ) il nome della fm deve essere CONVERSION_EXIT_XXXXX_INPUT/CONVERSION_EXIT_XXXXX_OUTPUT dove al posto di 
XXXXX va inserito un identificativo.    
  
Per la FM INPUT spostare il valore in -> out se non servono particolari modifiche al valore. 


```abap
FUNCTION CONVERSION_EXIT_XXXXX_OUTPUT
  IMPORTING
    INPUT TYPE ANY ##ADT_PARAMETER_UNTYPED
  EXPORTING
    OUTPUT TYPE ANY ##ADT_PARAMETER_UNTYPED.




 DATA: lt_dd07v TYPE TABLE OF dd07v.
 FIELD-SYMBOLS: <ls_dd07> TYPE dd07v.

  CALL FUNCTION 'DDIF_DOMA_GET'
    EXPORTING
      name      = 'DOMAIN'
      langu     = sy-langu
    TABLES
      dd07v_tab = lt_dd07v.

  READ TABLE lt_dd07v ASSIGNING <ls_dd07> WITH KEY domvalue_l = input.
  CHECK sy-subrc = 0.

  output = <ls_dd07>-ddtext.

ENDFUNCTION.
```
