<h1>CACS_GET_TABLE_FIELDS</h1>
Ottnere i campi di una tabella distinti tra campi chiave e non chiave
``` abap
*"*"Lokale Schnittstelle:
*"       IMPORTING
*"             VALUE(I_TABNAME) LIKE  DCOBJDEF-NAME
*"       TABLES
*"              T_KEYFIELD STRUCTURE  CACS_S_COND_KEYFIELDS OPTIONAL
*"              T_NONKEYFIELD STRUCTURE  CACS_S_COND_KEYFIELDS
*"                             OPTIONAL

 CALL FUNCTION 'CACS_GET_TABLE_FIELDS'
      EXPORTING
        i_tabname     = p_table    " Table Name
      TABLES
        t_keyfield    = lt_keyfield   " Key Fields
        t_nonkeyfield = lt_nonkeyfield. " Non Key Fields
``` 