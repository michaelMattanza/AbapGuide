<h1>REUSE_ALV_GRID_DISPLAY</h1>    
This function allows you to create an ALV. It can be used as an alternative to SALV and ALV OO (Object-Oriented ALV). It's particularly useful when you need to simply manage calls to other external processes that should overwrite the current screen (e.g., calling a PDF form).

```abap
DATA: lt_fcat TYPE slis_t_fieldcat_alv,
      ls_layout TYPE slis_layout_alv.
        
CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program      = sy-repid
      it_fieldcat             = lt_fcat
      is_layout               = ls_layout
      i_callback_user_command = 'USER_COMMAND'
      I_CALLBACK_PF_STATUS_SET = 'SET_PF_STATUS'.
      
 FORM user_command USING r_ucomm LIKE sy-ucomm
                        rs_selfield TYPE slis_selfield.
                        
  DATA: lref_grid TYPE REF TO cl_gui_alv_grid,
        lt_rows TYPE lvc_t_row.
        
   IF lref_grid IS INITIAL.
    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        e_grid = lref_grid.
   ENDIF.
   
   IF NOT lref_grid IS INITIAL.
    lref_grid->check_changed_data( ).
    lref_grid->get_selected_rows(
      IMPORTING
        et_index_rows = lt_rows                 " Indexes of Selected Rows
    ).
   ENDIF.
 ENDFORM.
 
 FORM set_pf_status using rt_extab type slis_t_extab.
  SET PF-STATUS 'STAT100'.
 ENDFORM.
```
