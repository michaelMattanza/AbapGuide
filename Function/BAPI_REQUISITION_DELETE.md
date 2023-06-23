<h1>BAPI_REQUISITION_DELETE</h1>     
Cancella una richiesta di acquisto. Non la elimina dal sistema ma imposta il flag di cancellazione

```abap
DATA: lt_bapieband TYPE TABLE OF bapieband,
      lt_bapireturn TYPE TABLE OF bapireturn.

APPEND VALUE #( preq_item = <fs_ptp_purreq01>-bnfpo delete_ind = abap_true ) TO lt_bapieband.

CALL FUNCTION 'BAPI_REQUISITION_DELETE'
   EXPORTING
     number                      = iv_banfn
   TABLES
     requisition_items_to_delete = lt_bapieband
     return                      = lt_bapireturn.
```
