<h1>BAPI_MATERIAL_SAVEDATA</h1>

```abap
" Aggiungi o sottrai N giorni con 0 < N < 365+, N mesi o anni

*"*"Lokale Schnittstelle:
*"       IMPORTING
*"             VALUE(ID_DATE) TYPE  BEGDA
*"             VALUE(ID_OPERATOR) TYPE  ADSUB DEFAULT '+'
*"             VALUE(IS_DURATION) TYPE  PSEN_DURATION_DEC
*"       EXPORTING
*"             VALUE(ED_DATE) TYPE  ENDDA
*"       EXCEPTIONS
*"              CONVERSION_NOT_SPECIFIED
*"              CONVERSION_NOT_POSSIBLE

CALL FUNCTION 'HR_SEN_CALE_DAYS_DATE' 
  EXPORTING
    id_date =                   " begda         
*   id_operator = '+'           " adsub         
    is_duration =               " psen_duration_dec 
  IMPORTING
    ed_date =                   " endda        
  EXCEPTIONS
    CONVERSION_NOT_SPECIFIED = 1  "             
    CONVERSION_NOT_POSSIBLE = 2  "             
    .  "  HR_SEN_CALE_DAYS_DATE
```