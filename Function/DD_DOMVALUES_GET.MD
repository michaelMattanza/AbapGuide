<h1>DD_DOMVALUES_GET</h1>    
FM per estrarre i valori definiti nel dominio dictionary

```abap
CALL FUNCTION 'DD_DOMVALUES_GET'
    EXPORTING
      domname        = 'RFBSK'   "<-- Your Domain Here
      text           = 'X'
      langu          = sy-langu
    TABLES
      dd07v_tab      = idd07v
    EXCEPTIONS
      wrong_textflag = 1
      OTHERS         = 2.
    ```
