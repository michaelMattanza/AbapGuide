<h1>Salv</h1>

The SALV is a class used for displaying data on screen (similar to ALV), offering the advantage of being fast and performant, but with limited customization options.

```abap
DATA: lo_functions TYPE REF TO cl_salv_functions_list,
      lo_cols      TYPE REF TO cl_salv_columns,
      lo_alv       TYPE REF TO cl_salv_table,
      lo_column    TYPE REF TO cl_salv_column,

      lref_layout_settings TYPE REF TO cl_salv_layout,

      ls_layout_key TYPE salv_s_layout_key.
TRY.
    cl_salv_table=>factory(
        IMPORTING r_salv_table = lo_alv
        CHANGING  t_table      = gt_outtab
    ).

    DATA(lt_comp) = CAST cl_abap_structdescr(
        CAST cl_abap_tabledescr(
          cl_abap_tabledescr=>describe_by_data( gt_outtab )
        )->get_table_line_type( )
    )->get_components( ).

  CATCH cx_salv_msg.
    "handle exception
ENDTRY.

" PF Status
lo_functions = lo_alv->get_functions( ).
lo_functions->set_all( abap_true ).

lo_cols = lo_alv->get_columns( ).
lo_cols->set_optimize( 'X' ).

layout_settings = lo_alv->get_layout( ).

layout_key-report = sy-repid.
layout_settings->set_key( layout_key ).

layout_settings->set_save_restriction( if_salv_c_layout=>restrict_none ).
```
