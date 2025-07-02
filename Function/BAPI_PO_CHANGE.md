# BAPI_PO_CHANGE
Function per l'update dell'ordine di acquisto
```abap
CALL FUNCTION 'BAPI_PO_CHANGE'
      EXPORTING
        purchaseorder = <fs_porder>-ebeln
      TABLES
        return        = lt_return
        extensionin   = lt_extension.
```
