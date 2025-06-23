DATA:
  ls_vbak_wa         TYPE vbak. " Not strictly needed for WS_DELIVERY_UPDATE_2 directly but useful
DATA:
  ls_vbkok_wa        TYPE vbok_wa, " Header data for delivery update
  lt_prot            TYPE TABLE OF prot. " Return messages/log

DATA:
  lt_handling_units_1 TYPE TABLE OF wshun_t. " Table for HUs in WS_DELIVERY_UPDATE_2
DATA:
  ls_handling_unit_1  TYPE wshun_t.

*&---------------------------------------------------------------------*
*& Start of Selection
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  " Prepare VBKOK_WA (Delivery Header Control)
  " This is essential. We need to specify the delivery number to be updated.
  ls_vbkok_wa-vbeln_vl = lv_delivery_number.
  ls_vbkok_wa-vbtyp_vl = 'J'. " Delivery category (J for outbound delivery)
  ls_vbkok_wa-updkz    = 'U'. " Update indicator: U for Update

  " Prepare IT_HANDLING_UNITS_1 table
  " This table holds the details of the HUs to be assigned or changed.
  CLEAR ls_handling_unit_1.
  ls_handling_unit_1-exidv   = lv_hu_internal_num. " Internal HU Number (VENUM)
  ls_handling_unit_1-rfbel   = lv_delivery_number. " Reference Delivery Number
  ls_handling_unit_1-rfpos   = lv_delivery_item.   " Reference Delivery Item (Optional but good practice to link to item)
  ls_handling_unit_1-kzpkz   = 'X'.                " 'X' to pack/assign the HU
  " If you want to unpack, set kzpkz = SPACE.
  " For other actions like creating an HU via this FM, other fields would be relevant.

  APPEND ls_handling_unit_1 TO lt_handling_units_1.

  " Call the Function Module
  CALL FUNCTION 'WS_DELIVERY_UPDATE_2'
    EXPORTING
      i_vbeln_vl          = lv_delivery_number " Delivery Number
      is_vbkok_wa         = ls_vbkok_wa        " Header Control Data
      i_synchron          = 'X'                " Synchronous update
      i_commit            = 'X'                " Commit changes
      i_ubukz             = ' '                " Set 'X' for Goods Issue if needed
    TABLES
      it_handling_units_1 = lt_handling_units_1 " Table of Handling Units
      et_prot             = lt_prot.             " Return Messages/Log
