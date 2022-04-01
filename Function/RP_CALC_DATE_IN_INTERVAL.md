Funcion per aggiungere o togliere n tempo ad una data di partenza e calcolare correttamente la data di arrivo
``` abap
" Inserire i giorni da 0-31, mese 0-12, anni

*"Lokale Schnittstelle:
*"       IMPORTING
*"             DATE LIKE P0001-BEGDA
*"             DAYS LIKE T5A4A-DLYDY
*"             MONTHS LIKE T5A4A-DLYMO
*"             SIGNUM LIKE T5A4A-SPLIT DEFAULT '+'
*"             YEARS LIKE T5A4A-DLYYR
*"       EXPORTING
*"             CALC_DATE LIKE P0001-BEGDA

CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        date      = sy-datum
        days      = CONV dlydy( lv_giorni )
        months    = 0
        years     = 0
        signum    = '-'
      IMPORTING
        calc_date = lv_past_date.
```