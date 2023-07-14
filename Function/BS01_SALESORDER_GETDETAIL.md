<h1>BS01_SALESORDER_GETDETAIL</h1>     

```abap
CALL FUNCTION 'BS01_SALESORDER_GETDETAIL'
      EXPORTING
        salesdocument        = <fs_orders>-vbeln
      IMPORTING
        order_header         = ls_order_head
      TABLES
        return               = lt_bapiret
        order_items          = lt_order_items
        order_conditions    = lt_order_cond
        order_partners       = lt_order_part
*        order_cond_item      = lt_order_cond
      .
```
