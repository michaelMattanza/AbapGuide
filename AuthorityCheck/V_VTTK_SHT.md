# Shipment type auth

```abap
SELECT DISTINCT shtyp FROM tvtk INTO TABLE @DATA(lt_shtyp).
LOOP AT lt_shtyp ASSIGNING FIELD-SYMBOL(<fs_shtyp>).
    AUTHORITY-CHECK OBJECT 'V_VTTK_SHT'
            ID 'ACTVT' FIELD '02'
            ID 'SHTYP' FIELD <fs_shtyp>-shtyp.

     IF sy-subrc NE 0.
        DELETE lt_shtyp.
        CONTINUE.
     ENDIF.
ENDLOOP.
```
