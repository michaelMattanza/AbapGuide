# Unit of measure conversion

This function converts a unit of measure from ISO value to internal value 

```abap 
CALL FUNCTION 'UNIT_OF_MEASURE_ISO_TO_SAP'
              EXPORTING
                iso_code  = idoc_sched_agree-menee
              IMPORTING
                sap_code  = <fs_subrow>-meins
              EXCEPTIONS
                not_found = 1
                others    = 2
              .
```
