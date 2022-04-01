<h1>SXV_GET_CLIF_BY_NAME</h1>    
FM per trovare badi coinvolte in un evento
```abap 
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(NAME)
*"     VALUE(PREFIX) TYPE  SEEX_CLIF_PREFIX
*"  EXPORTING
*"     VALUE(CLIF) TYPE  SEOCLSNAME

  CALL FUNCTION 'SXV_ADD_PREFIX'
       EXPORTING
            name     = name
            prefix   = prefix
       IMPORTING
            new_name = clif.
```