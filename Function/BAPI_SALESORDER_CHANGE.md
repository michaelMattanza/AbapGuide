<h1>BAPI_SALESORDER_CHANGE</h1>     
Bapi per modificare un ordine di vendita già esistente     

```abap
  IMPORTING   iv_vbeln  TYPE vbeln
              is_order_header_in  TYPE bapisdh1
              is_order_header_inx TYPE bapisdh1x
              is_logic_switch     TYPE bapisdls optional
    CHANGING  ct_bapi_order_text  TYPE bapisdtext_t OPTIONAL
              ct_bapi_items       TYPE bapisditm_tt
              ct_bapi_itemsx      TYPE bapisditmx_tt
              ct_conditions       TYPE cod_tt_bapicond OPTIONAL
              ct_conditionsx      TYPE cod_t_bapicondx OPTIONAL
              ct_return           TYPE bapirettab.

    CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
    EXPORTING
      salesdocument    = iv_vbeln
      order_header_in  = is_order_header_in
      order_header_inx = is_order_header_inx
      logic_switch     = is_logic_switch
    TABLES
      return           = ct_return
      order_item_in    = ct_bapi_items
      order_item_inx   = ct_bapi_itemsx
      conditions_in    = ct_conditions
      conditions_inx   = ct_conditionsx
      order_text       = ct_bapi_order_text.
```
