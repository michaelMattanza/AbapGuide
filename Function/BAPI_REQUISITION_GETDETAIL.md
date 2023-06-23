<h1>BAPI_REQUISITION_GETDETAIL</h1>     
Bapi per leggere i dati principali della richiesta di acquisto     

```abap
DATA: lt_req_account_old_info TYPE TABLE OF bapiebkn,
      lt_old_items_req TYPE TABLE OF bapieban.

CALL FUNCTION 'BAPI_REQUISITION_GETDETAIL'
  EXPORTING
    number = <fs_ptp_purreq01>-banfn
    account_assignment = 'X'
  TABLES
    requisition_items = lt_old_items_req
    requisition_account_assignment = lt_req_account_old_info.
```
