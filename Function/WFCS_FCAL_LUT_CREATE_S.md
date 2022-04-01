<h1>WFCS_FCAL_LUT_CREATE_S</h1>
Date e tipologia ( lavorativo, feriale, ... ) con riferimento al calendario lavorativo comprese tra due date indicative
```abap
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     REFERENCE(PI_TIME_INTERVAL) LIKE  WFCS_TIME_INTERVAL STRUCTURE
*"        WFCS_TIME_INTERVAL
*"     REFERENCE(PI_FCALID) TYPE  WFCID
*"  TABLES
*"      TE_LUT_DATE_FACDATE STRUCTURE  WFCS_LUT_DATE_FACDATE
*"  EXCEPTIONS
*"      ERROR_INTERFACE
*"      ERROR_LUT_DATE_FACDATE_CREATE

CALL FUNCTION 'WFCS_FCAL_LUT_CREATE_S'
      EXPORTING
        pi_time_interval    = VALUE wfcs_time_interval( dat_from = '20190404' dat_to = '20210404' )
        pi_fcalid           = 'IT'
      TABLES
        te_lut_date_facdate = lt_date_facdate.
```