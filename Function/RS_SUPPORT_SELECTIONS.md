<h1>RS_SUPPORT_SELECTIONS</h1>    
Per caricare una variante specifica al lancio del report, inserire questa function nella sezione *INITIALIZATION* del report: 

```abap
*"*"Lokale Schnittstelle:
*"       IMPORTING
*"             REPORT LIKE RSVAR-REPORT
*"             VARIANT LIKE RSVAR-VARIANT
*"       EXCEPTIONS
*"             VARIANT_NOT_EXISTENT
*"             VARIANT_OBSOLETE

CALL FUNCTION 'RS_SUPPORT_SELECTIONS'
  EXPORTING
    report               = 'nome_report'                 " Report Name
    variant              = 'nome_variante'                 " Variant Name
  EXCEPTIONS
    variant_not_existent = 1                " Variant does not exist
    variant_obsolete     = 2                " Obsolete Variant
    others               = 3
  .
  ```