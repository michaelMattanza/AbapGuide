```abap     
class ZWW_CAT_CL_SALESORD definition
  public
  final
  create public .

public section.

   class-methods UPDATE_SALESORD
    importing
              iv_vbeln            TYPE vbeln
              is_order_header_in  TYPE bapisdh1
              is_order_header_inx TYPE bapisdh1x
              is_logic_switch     TYPE bapisdls optional
    CHANGING  ct_bapi_order_text  TYPE bapisdtext_t OPTIONAL
              ct_bapi_items       TYPE bapisditm_tt
              ct_bapi_itemsx      TYPE bapisditmx_tt
              ct_conditions       TYPE cod_tt_bapicond OPTIONAL
              ct_conditionsx      TYPE cod_t_bapicondx OPTIONAL
              ct_return           TYPE bapirettab.


    class-methods create_salesdoc
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

    class-methods get_detailed_list
     importing  is_document_view        TYPE bos_order_view
     exporting  et_order_document_key   TYPE shp_sales_key_t
                et_order_headers_out    TYPE wiso_t_sdhd
                et_order_items_out      TYPE zww_dtel_bapisditbos
                et_order_cond_out       TYPE bapisdcondt
                et_order_cond_head      TYPE bbpt_cnd_mm_condhd
                et_order_cond_item      TYPE bbpt_cnd_mm_condit.

   class-methods bapi_commit
    importing iv_wait TYPE abap_bool DEFAULT abap_true
    returning value(rs_result) TYPE bapiret2.

  class-methods bapi_rollback
    returning value(rs_result) TYPE bapiret2.

  class-methods LOCK_ORDER
    importing !iv_vbeln TYPE vbeln
    changing ct_return TYPE bapirettab.

  class-methods UNLOCK_ORDER
    importing !iv_vbeln TYPE vbeln.

protected section.
private section.
ENDCLASS.



CLASS ZWW_CAT_CL_SALESORD IMPLEMENTATION.


  method LOCK_ORDER.
    CALL FUNCTION 'ENQUEUE_EVVBAKE'
    EXPORTING
      vbeln          = iv_vbeln
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2.

    IF sy-subrc <> 0.
        CASE sy-subrc.
          WHEN 1. "Foreign lock of sales document
            MESSAGE e042(v1) WITH iv_vbeln sy-msgv1 INTO DATA(l_dummy).

          WHEN 2. "System error in enqueue server
            MESSAGE e043(v1) INTO l_dummy.
        ENDCASE.
    ENDIF.

    APPEND VALUE #( id = sy-msgid number = sy-msgno type = sy-msgty
                  message_v1 = sy-msgv1 message_v2 = sy-msgv2
                  message_v3 = sy-msgv3 message_v4 = sy-msgv4
                  message = l_dummy ) TO ct_return.
  endmethod.


  method UNLOCK_ORDER.
    CALL FUNCTION 'DEQUEUE_EVVBAKE'
    EXPORTING
      vbeln = iv_vbeln.
  endmethod.


  method UPDATE_SALESORD.

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

  endmethod.
  METHOD BAPI_COMMIT.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait   = iv_wait
      IMPORTING
        return = rs_result
      .
  ENDMETHOD.

  METHOD BAPI_ROLLBACK.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
      IMPORTING
        return = rs_result
      .
  ENDMETHOD.

  METHOD CREATE_SALESDOC.

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

  ENDMETHOD.


  METHOD GET_DETAILED_LIST.
    CALL FUNCTION 'BS01_SALESDOCU_GETDETAILEDLIST'
    EXPORTING
      i_bapi_view          = is_document_view
      internal_use         = space
      i_memory_read        = space "'A'
    TABLES
      sales_documents      = et_order_document_key
      order_headers_out    = et_order_headers_out
      order_items_out      = et_order_items_out
      order_conditions_out = et_order_cond_out
      order_cond_head      = et_order_cond_head
      order_cond_item      = et_order_cond_item.
  ENDMETHOD.

ENDCLASS.
```
