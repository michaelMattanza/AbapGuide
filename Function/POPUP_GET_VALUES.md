<h1>POPUP_GET_VALUES</h1>
Inserimento dati tramite popup (non controllati)   
```abap
*"Lokale Schnittstelle:
*"       IMPORTING
*"             NO_VALUE_CHECK DEFAULT SPACE
*"             POPUP_TITLE
*"             START_COLUMN DEFAULT '5'
*"             START_ROW DEFAULT '5'
*"       EXPORTING
*"             RETURNCODE
*"       TABLES
*"             FIELDS STRUCTURE SVAL
*"       EXCEPTIONS
*"             ERROR_IN_FIELDS

  APPEND: VALUE #( tabname = 'LFA1' fieldname = 'LIFNR' field_obl = 'X' fieldtext = 'Fornitore' value = '001234' ) TO lt_fields_popup,
          VALUE #( tabname = 'EBAN' fieldname = 'EKGRP' field_obl = 'X' fieldtext = 'Gruppo acquisti' ) TO lt_fields_popup.

      TRY.
        CALL FUNCTION 'POPUP_GET_VALUES'
          EXPORTING
            popup_title           = 'Creare RDA'
         IMPORTING
           RETURNCODE            = lv_subrc
          tables
            fields                = lt_fields_popup
         EXCEPTIONS
           ERROR_IN_FIELDS       = 1
           OTHERS                = 2.
        CATCH cx_root.
      ENDTRY.
```