# Plant auth
```abap
AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
        FOR USER i_user
        ID 'ACTVT' FIELD '03'
        ID 'WERKS' FIELD ls_werks.
      CHECK sy-subrc  EQ 0.
```
