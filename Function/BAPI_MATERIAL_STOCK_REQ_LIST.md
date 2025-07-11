<h1>BAPI_MATERIAL_STOCK_REQ_LIST</h1>     
Bapi for extracting available and used stock quantities.  


```abap
DATA: lt_mrp_ind_lines TYPE TABLE OF bapi_mrp_ind_lines,
      ls_mrp_list TYPE bapi_mrp_list.

CALL FUNCTION 'BAPI_MATERIAL_STOCK_REQ_LIST'
   EXPORTING
     material          = CONV bapi_mrp_mat_param-material( iv_matnr )
     plant             = iv_werks
     mrp_area          = iv_berid
   IMPORTING
     mrp_list          = ls_mrp_list " ->reord_pt = stock presente
   TABLES
     mrp_ind_lines     = lt_mrp_ind_lines.
```abap 
