<h1>CONVERSION_EXIT_CUNIT_INPUT</h1>
Function Per la conversione di udm da formato esterno a interno
```abap
*"*"Lokale Schnittstelle:
*"       IMPORTING
*"              INPUT
*"              LANGUAGE LIKE  SY-LANGU DEFAULT SY-LANGU
*"       EXPORTING
*"              OUTPUT
*"       EXCEPTIONS
*"              UNIT_NOT_FOUND

CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
        EXPORTING
          input  = <fs_excel_data>-meins
        IMPORTING
          output = <fs_excel_data>-meins.
```