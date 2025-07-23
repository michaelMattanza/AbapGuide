DATA: ls_headerproposal TYPE bapihuhdrproposal,
      ls_itemproposal   TYPE bapihuitmproposal,
      lt_itemproposal   TYPE STANDARD TABLE OF bapihuitmproposal,
      ls_huheader       TYPE bapihuheader,
      gv_hukey          TYPE bapihukey,
      lt_return         TYPE STANDARD TABLE OF bapiret2.

*   1. Populate the header proposal
ls_headerproposal-pack_mat = 'PALLET'.    " Packaging material
ls_headerproposal-plant = '1000'.          " Plant
ls_headerproposal-stge_loc = '0001'.       " Storage location
ls_headerproposal-hu_status_init = 'A'.  " Initial status of HU. 'A' for active.

*   2. Populate the item proposal
ls_itemproposal-hu_item_type = '1'.        " Item type: Material.
ls_itemproposal-material = 'MAT001'.      " Material number
ls_itemproposal-pack_qty = 10.            " Quantity to pack
ls_itemproposal-base_unit_qty = 'PC'.      " Base unit of measure.
ls_itemproposal-plant = '1000'.          " Plant
ls_itemproposal-stge_loc = '0001'.       " Storage location

*   3. Add the item proposal to the table
APPEND ls_itemproposal TO lt_itemproposal.

*   4. Call the BAPI
CALL FUNCTION 'BAPI_HU_CREATE'
  EXPORTING
    headerproposal = ls_headerproposal
  IMPORTING
    huheader       = ls_huheader
    hukey          = gv_hukey
  TABLES
    itemsproposal  = lt_itemsproposal
    return         = lt_return.
