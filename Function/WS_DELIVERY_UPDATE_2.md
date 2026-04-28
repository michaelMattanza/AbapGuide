```abap
DATA: lt_prot            TYPE TABLE OF prot, " Return messages/log.
      lt_handling_units_1 TYPE TABLE OF wshun_t. " Table for HUs in WS_DELIVERY_UPDATE_2    
  
  " Prepare VBKOK_WA (Delivery Header Control)
  " This is essential. We need to specify the delivery number to be updated.
  DATA(ls_vbkok) = VALUE VBKOK(
        vbeln_vl = <fs_likp>-vbeln
        vbtyp_vl = <fs_likp>-vbtyp
        wabuc    = 'X' " Post good issue
      ).

  " Prepare IT_HANDLING_UNITS_1 table
  " This table holds the details of the HUs to be assigned or changed.

  APPEND VALUE #(
    exidv   = lv_hu_internal_num " Internal HU Number (VENUM)
    rfbel   = lv_delivery_number " Reference Delivery Number
    rfpos   = lv_delivery_item   " Reference Delivery Item
    kzpkz   = 'X'                " 'X' to pack/assign the HU If you want to unpack, set kzpkz = SPACE.
) TO lt_handling_units_1.

lt_sernr = VALUE #(
  FOR wa_srnum IN gt_sernum
  WHERE ( vbeln = <fs_likp>-vbeln )
  ( rfbel = wa_srnum-vbeln rfpos = wa_srnum-posnr sernr = wa_srnum-srnum )
).

CALL FUNCTION 'WS_DELIVERY_UPDATE_2'
  EXPORTING
    delivery          = <fs_likp>-vbeln " Delivery Number
    vbkok_wa          = ls_vbkok        " Header Control Data
    synchron          = 'X'                " Synchronous update
    commit            = 'X'                " Commit changes
    it_sernr_update   = lt_sernr
  TABLES
    it_handling_units_1 = lt_handling_units_1 " Table of Handling Units
    prot           = lt_prot.             " Return Messages/Log
```
