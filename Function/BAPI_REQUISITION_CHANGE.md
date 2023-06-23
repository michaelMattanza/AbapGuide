<h1>BAPI_REQUISITION_CHANGE</h1>     
Bapi per aggiornare la richiesta di acquisto

```abap
DATA: lt_old_items TYPE /smerp/mm_bapiebanv_tab,
      lt_new_items TYPE /smerp/mm_bapiebanv_tab,
      lt_old_acc TYPE /SMERP/MM_BAPIEBKNV_TAB,
      lt_new_acc TYPE /SMERP/MM_BAPIEBKNV_TAB,
      lt_return TYPE TABLE OF bapireturn.

CALL FUNCTION 'BAPI_REQUISITION_CHANGE'
         EXPORTING
          number = cv_banfn
         TABLES
          requisition_items_old = lt_old_items
          requisition_items_new = lt_new_items
          requisition_account_old = lt_old_acc
          requisition_account_new = lt_new_acc
          return                = lt_return.
```
