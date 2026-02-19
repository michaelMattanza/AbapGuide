CLASS zcl_alv_manager DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES ty_string_tab TYPE STANDARD TABLE OF string WITH EMPTY KEY.
    TYPES: BEGIN OF ty_fc_custom,
             fieldname    TYPE string,
             fc_component TYPE string,
             value        TYPE string,
           END OF ty_fc_custom,
           tty_fc_custom TYPE STANDARD TABLE OF ty_fc_custom WITH EMPTY KEY.

    CLASS-METHODS get_instance
      IMPORTING iv_program_name TYPE string
                io_parent       TYPE REF TO cl_gui_container OPTIONAL
                it_custom_fc    TYPE tty_fc_custom OPTIONAL
      CHANGING  ct_outtab       TYPE ANY TABLE
      RETURNING VALUE(ro_manager) TYPE REF TO zcl_alv_manager.

    METHODS constructor
      IMPORTING iv_program_name TYPE string
                ir_outtab       TYPE REF TO data
                io_parent       TYPE REF TO cl_gui_container OPTIONAL
                it_custom_fc    TYPE tty_fc_custom OPTIONAL.

    " UI Configuration
    METHODS set_container_name     IMPORTING iv_container_name TYPE scrfname.
    METHODS set_event_handler      IMPORTING io_handler        TYPE REF TO zif_alv_event_handler.
    METHODS set_toolbar_exclusions IMPORTING it_toolbar_excl   TYPE ui_functions.
    METHODS set_status_and_title
      IMPORTING iv_status          TYPE string OPTIONAL
                iv_title           TYPE string OPTIONAL
                it_fcode_excluding TYPE ty_string_tab OPTIONAL.

    " Core Actions
    METHODS display_data
      IMPORTING is_variant       TYPE disvariant OPTIONAL
                iv_save          TYPE char01 DEFAULT 'A'
                is_layout        TYPE lvc_s_layo OPTIONAL
                iv_auto_optimize TYPE abap_bool DEFAULT abap_true.

    METHODS handle_resize
      IMPORTING iv_optimize_columns TYPE abap_bool DEFAULT abap_true.

    METHODS refresh_with_state
      IMPORTING iv_soft_refresh TYPE abap_bool DEFAULT abap_false.

  PRIVATE SECTION.
    CLASS-DATA go_instance        TYPE REF TO zcl_alv_manager.

    DATA mo_alv                   TYPE REF TO cl_gui_alv_grid.
    DATA mo_parent                TYPE REF TO cl_gui_container.
    DATA mo_handler               TYPE REF TO zif_alv_event_handler.
    DATA mr_outtab                TYPE REF TO data.
    DATA mt_fcat                  TYPE lvc_t_fcat.
    DATA mt_toolbar_excl          TYPE ui_functions.
    DATA mv_program_name          TYPE string.
    DATA mv_container_name        TYPE scrfname.
    
    " State Persistence
    DATA mt_selected_rows         TYPE lvc_t_row.
    DATA ms_row_no                TYPE lvc_s_roid.

    METHODS set_handlers.
    METHODS create_dyn_fc
      IMPORTING is_outtab    TYPE data
                it_custom_fc TYPE tty_fc_custom OPTIONAL
      RETURNING VALUE(rt_fcat) TYPE lvc_t_fcat.

    METHODS normalize_fieldcat
      IMPORTING iv_tabname   TYPE string
                it_custom_fc TYPE tty_fc_custom OPTIONAL
      CHANGING  ct_fieldcat  TYPE lvc_t_fcat.

    " Event Bridges
    METHODS _on_toolbar      FOR EVENT toolbar       OF cl_gui_alv_grid IMPORTING e_object e_interactive.
    METHODS _on_user_command FOR EVENT user_command  OF cl_gui_alv_grid IMPORTING e_ucomm.
    METHODS _on_double_click FOR EVENT double_click  OF cl_gui_alv_grid IMPORTING e_row e_column.
    METHODS _on_hotspot      FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_row_id e_column_id.
    METHODS _on_data_changed FOR EVENT data_changed  OF cl_gui_alv_grid IMPORTING er_data_changed.
ENDCLASS.

CLASS zcl_alv_manager IMPLEMENTATION.

  METHOD get_instance.
    IF go_instance IS NOT BOUND.
      DATA lr_outtab TYPE REF TO data.
      GET REFERENCE OF ct_outtab INTO lr_outtab.
      go_instance = NEW #( iv_program_name = iv_program_name ir_outtab = lr_outtab
                           io_parent = io_parent it_custom_fc = it_custom_fc ).
    ENDIF.
    ro_manager = go_instance.
  ENDMETHOD.

  METHOD constructor.
    FIELD-SYMBOLS <lt_out> TYPE ANY TABLE.
    mv_program_name = iv_program_name.
    mr_outtab       = ir_outtab.
    mo_parent       = io_parent.

    ASSIGN mr_outtab->* TO <lt_out>.
    DATA(lr_row) = NEW data.
    CREATE DATA lr_row LIKE LINE OF <lt_out>.
    ASSIGN lr_row->* TO FIELD-SYMBOL(<ls_row>).

    mt_fcat = create_dyn_fc( is_outtab = <ls_row> it_custom_fc = it_custom_fc ).
  ENDMETHOD.

  METHOD display_data.
    FIELD-SYMBOLS <lt_out> TYPE ANY TABLE.
    ASSIGN mr_outtab->* TO <lt_out>.

    IF mo_parent IS INITIAL AND mv_container_name IS NOT INITIAL.
      mo_parent = NEW cl_gui_custom_container( container_name = mv_container_name ).
    ENDIF.

    IF mo_alv IS INITIAL.
      DATA(lo_p) = COND #( WHEN mo_parent IS BOUND THEN mo_parent ELSE cl_gui_container=>default_screen ).
      mo_alv = NEW cl_gui_alv_grid( i_parent = lo_p ).
      set_handlers( ).
    ENDIF.

    mo_alv->set_table_for_first_display(
      EXPORTING is_layout = is_layout is_variant = is_variant i_save = iv_save
                it_toolbar_excluding = mt_toolbar_excl
      CHANGING  it_outtab = <lt_out> it_fieldcatalog = mt_fcat ).

    handle_resize( iv_optimize_columns = iv_auto_optimize ).
  ENDMETHOD.

  METHOD handle_resize.
    CHECK mo_alv IS BOUND.
    cl_gui_cfw=>flush( ).
    IF iv_optimize_columns = abap_true.
      DATA ls_layo TYPE lvc_s_layo.
      mo_alv->get_frontend_layout( IMPORTING es_layout = ls_layo ).
      ls_layo-col_opt = abap_true.
      mo_alv->set_frontend_layout( is_layout = ls_layo ).
    ENDIF.
    mo_alv->refresh_table_display( is_stable = VALUE #( row = 'X' col = 'X' ) ).
  ENDMETHOD.

  METHOD refresh_with_state.
    CHECK mo_alv IS BOUND.
    mo_alv->get_selected_rows( IMPORTING et_index_rows = mt_selected_rows ).
    mo_alv->get_scroll_info_via_id( IMPORTING es_row_no = ms_row_no ).
    mo_alv->refresh_table_display( is_stable = VALUE #( row = 'X' col = 'X' ) i_soft_refresh = iv_soft_refresh ).
    mo_alv->set_selected_rows( it_index_rows = mt_selected_rows ).
    mo_alv->set_scroll_info_via_id( is_row_no = ms_row_no ).
  ENDMETHOD.

  METHOD set_status_and_title.
    IF iv_title  IS NOT INITIAL. SET TITLEBAR  iv_title.  ENDIF.
    IF iv_status IS NOT INITIAL. SET PF-STATUS iv_status EXCLUDING it_fcode_excluding. ENDIF.
  ENDMETHOD.

  METHOD set_event_handler.      mo_handler = io_handler.        ENDMETHOD.
  METHOD set_container_name.     mv_container_name = iv_container_name. ENDMETHOD.
  METHOD set_toolbar_exclusions. mt_toolbar_excl = it_toolbar_excl.   ENDMETHOD.

  METHOD set_handlers.
    SET HANDLER me->_on_toolbar me->_on_user_command me->_on_double_click 
                me->_on_hotspot me->_on_data_changed FOR mo_alv.
  ENDMETHOD.

  METHOD create_dyn_fc.
    DATA(lo_struct) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( is_outtab ) ).
    DATA(lt_comp)   = lo_struct->components.

    LOOP AT lt_comp ASSIGNING FIELD-SYMBOL(<ls_c>).
      IF <ls_c>-name = 'T_STYL' OR <ls_c>-name = 'T_SCOL'. CONTINUE. ENDIF.
      APPEND VALUE lvc_s_fcat( fieldname = <ls_c>-name inttype = <ls_c>-type_kind 
                               intlen = <ls_c>-length decimals = <ls_c>-decimals col_opt = abap_true ) TO rt_fcat.
    ENDLOOP.
    normalize_fieldcat( EXPORTING iv_tabname = lo_struct->absolute_name+6(30)
                                  it_custom_fc = it_custom_fc
                        CHANGING  ct_fieldcat = rt_fcat ).
  ENDMETHOD.

  METHOD normalize_fieldcat.
    LOOP AT ct_fieldcat ASSIGNING FIELD-SYMBOL(<fs_f>).
      <fs_f>-col_pos = sy-tabix.
      LOOP AT it_custom_fc ASSIGNING FIELD-SYMBOL(<ls_cust>) WHERE fieldname = <fs_f>-fieldname OR fieldname = '*'.
        ASSIGN COMPONENT <ls_cust>-fc_component OF STRUCTURE <fs_f> TO FIELD-SYMBOL(<fs_val>).
        IF sy-subrc = 0. <fs_val> = <ls_cust>-value. ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD _on_toolbar.      IF mo_handler IS BOUND. mo_handler->on_toolbar( e_object = e_object e_interactive = e_interactive ). ENDIF. ENDMETHOD.
  METHOD _on_double_click. IF mo_handler IS BOUND. mo_handler->on_double_click( e_row = e_row e_column = e_column ). ENDIF. ENDMETHOD.
  METHOD _on_hotspot.      IF mo_handler IS BOUND. mo_handler->on_hotspot_click( e_row_id = e_row_id e_column_id = e_column_id ). ENDIF. ENDMETHOD.
  METHOD _on_data_changed. IF mo_handler IS BOUND. mo_handler->on_data_changed( er_data_changed = er_data_changed ). ENDIF. ENDMETHOD.
  METHOD _on_user_command.
    DATA lt_r TYPE lvc_t_row. mo_alv->get_selected_rows( IMPORTING et_index_rows = lt_r ).
    IF mo_handler IS BOUND. mo_handler->on_user_command( e_ucomm = e_ucomm it_rows = lt_r ). ENDIF.
  ENDMETHOD.
ENDCLASS.
