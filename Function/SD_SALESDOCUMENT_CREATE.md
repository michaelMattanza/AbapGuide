# abap.zcl_alv_manager
Function per simulare o creare gli ordini di vendita

```abap
importing is_sales_header_in  TYPE bapisdhd1
                  is_sales_header_inx TYPE bapisdhd1x
                  iv_testrun          TYPE bapiflag-bapiflag
         changing ct_sales_items_inx  TYPE bapisditmx_tt
                  ct_sales_partners   TYPE cmp_t_parnr
                  ct_sales_sched_in   TYPE cod_t_bapischdl
                  ct_sales_sched_inx  TYPE cod_t_bapischdlx
                  ct_schedule_ex      TYPE bapisdhedutab
                  ct_sales_items_in   TYPE bapisditm_tt
                  ct_return TYPE bapiret2_t.

    CALL FUNCTION 'SD_SALESDOCUMENT_CREATE'
        EXPORTING
          sales_header_in     = is_sales_header_in
          sales_header_inx    = is_sales_header_inx
          logic_switch        = VALUE bapisdls( scheduling = 'X' )
          testrun             = iv_testrun
        TABLES
          return              = ct_return
          sales_items_in      = ct_sales_items_in
          sales_items_inx     = ct_sales_items_inx
          sales_partners      = ct_sales_partners
          sales_schedules_in  = ct_sales_sched_in
          sales_schedules_inx = ct_sales_sched_inx
          schedule_ex         = ct_schedule_ex.
```
