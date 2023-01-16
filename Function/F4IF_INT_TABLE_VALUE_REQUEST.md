<h1>F4IF_INT_TABLE_VALUE_REQUEST</h1>     
Questa function viene utilizzata per la compilazione custom del search help su parametri di input di un report.     

```abap 
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_juri.
  DATA: lt_value_selected TYPE TABLE OF ddshretval.

  SELECT DISTINCT ( taxjurcode ) , ( name1 ) FROM adrc
    WHERE country EQ 'IT' AND taxjurcode IS NOT INITIAL INTO TABLE @DATA(lt_adrc).

  SORT lt_adrc BY name1.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield         = 'TAXJURCODE'                 " Name of return field in FIELD_TAB
      dynpprog         = sy-repid            " Current program
      dynpnr           = sy-dynnr            " Screen number
      dynprofield      = 'P_JURI'            " Name of screen field for value return
      value_org        = 'S'              " Value return: C: cell by cell, S: structured
      display          = 'F'            " Override readiness for input
    TABLES
      value_tab        = lt_adrc                 " Table of values: entries cell by cell
      return_tab       = lt_value_selected                 " Return the selected value
    .
```
