<h1>CSAP_MAT_BOM_MAINTAIN</h1>    

```abap
gs_stpo_bom1-item_categ = gs_stpo_bom-item_categ.
gs_stpo_bom1-item_no    = gs_stpo_bom-item_no.
gs_stpo_bom1-component  = gs_stpo_bom-component.
gs_stpo_bom1-comp_qty   = gs_stpo_bom-comp_qty .
gs_stpo_bom1-comp_unit  = gs_stpo_bom-comp_unit.
gs_stpo_bom1-rel_cost   = gs_stpo_bom-rel_cost.
gs_stpo_bom1-rel_prod   = gs_stpo_bom-rel_prod.
gs_stpo_bom1-itm_ident  = gs_stpo_bom-itm_ident.
gs_stpo_bom1-item_guid  = gs_stpo_bom-item_guid.
gs_stpo_bom1-valid_from = gs_stpo_bom-valid_from.
gs_stpo_bom1-change_no  = gd_change_no.
gs_stpo_bom1-bom_no     = gs_stpo_bom-bom_no.
gs_stpo_bom1-item_node  = gs_stpo_bom-item_node.
gs_stpo_bom1-item_count = gs_stpo_bom-item_count.
gs_stpo_bom1-fldelete   = 'X'.

APPEND gs_stpo_bom1 TO gt_stpo_bom1.

CLEAR: gs_stpo_bom1-comp_unit, gs_stpo_bom1-rel_cost, gs_stpo_bom1-fldelete,
     gs_stpo_bom1-itm_ident, gs_stpo_bom1-item_guid, gs_stpo_bom1-valid_from,
     gs_stpo_bom1-bom_no, gs_stpo_bom1-item_node, gs_stpo_bom1-item_count.

gs_stpo_bom1-item_categ = gs_stpo_bom-item_categ.
gs_stpo_bom1-item_no    = gs_stpo_bom-item_no.
gs_stpo_bom1-component  = gs_material-smatn.
gs_stpo_bom1-comp_qty   = gs_stpo_bom-comp_qty .
gs_stpo_bom1-rel_prod   = gs_stpo_bom-rel_prod.
gs_stpo_bom1-change_no  = gd_change_no.

APPEND gs_stpo_bom1 TO gt_stpo_bom1.

CSAP_MAT_BOM_MAINTAINCALL FUNCTION 'CSAP_MAT_BOM_MAINTAIN'
    EXPORTING
      material           = ud_matnr
      plant              = p_werks
      bom_usage          = '1'
      alternative        = '1'
      valid_from         = ud_valid_form
      change_no          = ud_bom
      i_stko             = gt_stko1_i
      fl_commit_and_wait = 'X'
      fl_default_values  = 'X'
    IMPORTING
      fl_warning         = gd_fl_warning1
      o_stko             = gt_stko1_o
    TABLES
      t_stpo             = gt_stpo_bom1
    EXCEPTIONS
      error              = 1
      OTHERS             = 2.
```
