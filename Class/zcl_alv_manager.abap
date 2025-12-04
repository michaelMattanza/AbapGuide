CLASS zcl_alv_manager DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    " Simple helper types
    TYPES ty_string_tab       TYPE STANDARD TABLE OF string WITH EMPTY KEY.
    TYPES ty_salv_colname     TYPE c LENGTH 30.
    TYPES ty_fieldname_range  TYPE RANGE OF dd03l-fieldname.

    " Customizations and SALV popup metadata
    TYPES:
      BEGIN OF ty_fc_custom,
        fieldname    TYPE string,
        fc_component TYPE string,
        value        TYPE string,
      END OF ty_fc_custom,
      tty_fc_custom TYPE STANDARD TABLE OF ty_fc_custom WITH EMPTY KEY,

      BEGIN OF ty_fc_popup,
        col_name TYPE ty_salv_colname,
        out_len  TYPE lvc_outlen,
        long_txt TYPE scrtext_l,
      END OF ty_fc_popup,
      tyt_fc_popup TYPE STANDARD TABLE OF ty_fc_popup WITH EMPTY KEY.

    " Factory: Note the CHANGING parameter for the table
    CLASS-METHODS create
      IMPORTING iv_program_name TYPE string
                io_alv          TYPE REF TO cl_gui_alv_grid OPTIONAL
                io_parent       TYPE REF TO cl_gui_container OPTIONAL
                it_custom_fc    TYPE tty_fc_custom OPTIONAL
      CHANGING  ct_outtab       TYPE ANY TABLE
      RETURNING VALUE(ro_manager) TYPE REF TO zcl_alv_manager.

    " Constructor: Accepts a raw Reference to Data
    METHODS constructor
      IMPORTING iv_program_name TYPE string
                ir_outtab       TYPE REF TO data
                io_alv          TYPE REF TO cl_gui_alv_grid OPTIONAL
                io_parent       TYPE REF TO cl_gui_container OPTIONAL
                it_custom_fc    TYPE tty_fc_custom OPTIONAL.

    " Bind by screen custom control name
    METHODS set_container_name
      IMPORTING iv_container_name TYPE scrfname.

    METHODS set_event_handler
      IMPORTING io_handler TYPE REF TO zif_alv_event_handler.

    METHODS get_fcat
      RETURNING VALUE(rt_fcat) TYPE lvc_t_fcat.

    " Display single ALV
    METHODS display_data
      IMPORTING is_variant_ft TYPE disvariant OPTIONAL
                iv_save_ft    TYPE char01 DEFAULT 'A'
                is_layout_ft  TYPE lvc_s_layo OPTIONAL.

    " SALV popup
    CLASS-METHODS display_data_popup
      IMPORTING i_start_column TYPE i DEFAULT 25
                i_start_line   TYPE i DEFAULT 6
                i_end_column   TYPE i DEFAULT 100
                i_end_line     TYPE i DEFAULT 10
                it_fc          TYPE tyt_fc_popup OPTIONAL
                iv_show_header TYPE abap_bool OPTIONAL
      CHANGING  ct_data        TYPE ANY TABLE.

    " Table ref access + refresh
    METHODS get_output_table
      RETURNING VALUE(er_table) TYPE REF TO data.

    METHODS update_table
      IMPORTING it_table TYPE data OPTIONAL.

    " Field help & drop-downs
    METHODS set_register_fld
      IMPORTING it_f4 TYPE lvc_t_f4.

    METHODS set_drop_down_table
      IMPORTING it_drop_down       TYPE lvc_t_drop OPTIONAL
                it_drop_down_alias TYPE lvc_t_dral OPTIONAL.

    " Helpers using RANGE type
    METHODS set_editable_columns
      IMPORTING it_fields_rng TYPE ty_fieldname_range
      CHANGING  ct_fcat       TYPE lvc_t_fcat OPTIONAL.

    METHODS set_sum_columns
      IMPORTING it_fields_rng TYPE ty_fieldname_range
      CHANGING  ct_fcat       TYPE lvc_t_fcat OPTIONAL.

    METHODS set_checkbox_columns
      IMPORTING it_fields_rng TYPE ty_fieldname_range
      CHANGING  ct_fcat       TYPE lvc_t_fcat OPTIONAL.

  PROTECTED SECTION.
    DATA gref_outtab       TYPE REF TO data.
    DATA go_alv            TYPE REF TO cl_gui_alv_grid.
    DATA mo_parent         TYPE REF TO cl_gui_container.
    DATA mv_container_name TYPE scrfname.

    DATA gt_fcat           TYPE lvc_t_fcat.
    DATA gt_f4             TYPE lvc_t_f4.
    DATA at_ddown          TYPE lvc_t_drop.
    DATA at_ddown_alias    TYPE lvc_t_dral.

    DATA gv_program_name   TYPE string.
    DATA mo_handler        TYPE REF TO zif_alv_event_handler.

  PRIVATE SECTION.

    " Lightweight DDIC cache
    TYPES: BEGIN OF ty_dd03t_s,
             fieldname TYPE dd03t-fieldname,
             ddtext    TYPE dd03t-ddtext,
           END OF ty_dd03t_s.
    TYPES: BEGIN OF ty_dd03l_s,
             fieldname TYPE dd03l-fieldname,
             domname   TYPE dd03l-domname,
             rollname  TYPE dd03l-rollname,
           END OF ty_dd03l_s.
    TYPES: BEGIN OF ty_dd04t_s,
             rollname  TYPE dd04t-rollname,
             scrtext_m TYPE dd04t-scrtext_m,
           END OF ty_dd04t_s.
    TYPES: BEGIN OF ty_dd01l_s,
             domname  TYPE dd01l-domname,
             convexit TYPE dd01l-convexit,
           END OF ty_dd01l_s.
    TYPES: BEGIN OF ty_ddic_cache,
             tabname TYPE tabname,
             dd03t   TYPE STANDARD TABLE OF ty_dd03t_s WITH EMPTY KEY,
             dd03l   TYPE STANDARD TABLE OF ty_dd03l_s WITH EMPTY KEY,
             dd04t   TYPE STANDARD TABLE OF ty_dd04t_s WITH EMPTY KEY,
             dd01l   TYPE STANDARD TABLE OF ty_dd01l_s WITH EMPTY KEY,
           END OF ty_ddic_cache.
    CLASS-DATA gt_ddic_cache TYPE HASHED TABLE OF ty_ddic_cache
                              WITH UNIQUE KEY tabname.

    " Field-catalog builders
    METHODS create_dyn_fc
      IMPORTING is_outtab    TYPE data
                it_custom_fc TYPE tty_fc_custom OPTIONAL
      RETURNING VALUE(ct_fieldcat) TYPE lvc_t_fcat.

    METHODS normalize_fieldcat
      IMPORTING iv_tabname   TYPE tabname
                it_custom_fc TYPE tty_fc_custom OPTIONAL
      CHANGING  ct_fieldcat  TYPE lvc_t_fcat.

    METHODS get_ddic_metadata
      IMPORTING iv_tabname TYPE tabname
      EXPORTING et_dd03t   TYPE ty_ddic_cache-dd03t
                et_dd03l   TYPE ty_ddic_cache-dd03l
                et_dd04t   TYPE ty_ddic_cache-dd04t
                et_dd01l   TYPE ty_ddic_cache-dd01l.

    METHODS set_handlers.

    " ALV events
    METHODS handle_toolbar
      FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING e_object e_interactive.
    METHODS handle_user_command
      FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.
    METHODS handle_hotspot_click
      FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING e_row_id e_column_id es_row_no.
    METHODS handle_data_changed
      FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING er_data_changed.
    METHODS handle_double_click
      FOR EVENT double_click OF cl_gui_alv_grid
      IMPORTING e_row e_column.
    METHODS handle_on_f4
      FOR EVENT onf4 OF cl_gui_alv_grid
      IMPORTING e_fieldname es_row_no er_event_data.
    METHODS handle_data_changed_finished
      FOR EVENT data_changed_finished OF cl_gui_alv_grid
      IMPORTING e_modified et_good_cells.

ENDCLASS.


CLASS zcl_alv_manager IMPLEMENTATION.

  METHOD create.
    " FIX: Explicitly define the reference variable to avoid generic declaration errors
    DATA lr_outtab TYPE REF TO data.
    GET REFERENCE OF ct_outtab INTO lr_outtab.

    CREATE OBJECT ro_manager
      EXPORTING iv_program_name = iv_program_name
                ir_outtab       = lr_outtab
                io_alv          = io_alv
                io_parent       = io_parent
                it_custom_fc    = it_custom_fc.
  ENDMETHOD.

  METHOD constructor.
    DATA lref_row TYPE REF TO data.
    FIELD-SYMBOLS <ls_row> TYPE any.
    FIELD-SYMBOLS <lt_outtab> TYPE ANY TABLE.

    gv_program_name = iv_program_name.

    " Store the writable reference
    gref_outtab = ir_outtab.

    " Dereference the handle to build the field catalog
    ASSIGN gref_outtab->* TO <lt_outtab>.
    CREATE DATA lref_row LIKE LINE OF <lt_outtab>.
    ASSIGN lref_row->* TO <ls_row>.

    IF io_alv IS BOUND.    go_alv    = io_alv.    ENDIF.
    IF io_parent IS BOUND. mo_parent = io_parent. ENDIF.

    gt_fcat = create_dyn_fc( is_outtab = <ls_row>
                             it_custom_fc = it_custom_fc ).
  ENDMETHOD.

  METHOD set_container_name.
    mv_container_name = iv_container_name.
  ENDMETHOD.

  METHOD set_event_handler.
    mo_handler = io_handler.
  ENDMETHOD.

  METHOD get_fcat.
    rt_fcat = gt_fcat.
  ENDMETHOD.

  METHOD display_data.
    FIELD-SYMBOLS <lt_out> TYPE ANY TABLE.

    " This now points to the original, writable table
    ASSIGN gref_outtab->* TO <lt_out>.
    IF <lt_out> IS NOT ASSIGNED.
      RETURN.
    ENDIF.

    IF mo_parent IS INITIAL AND mv_container_name IS NOT INITIAL.
      mo_parent = NEW cl_gui_custom_container( container_name = mv_container_name ).
    ENDIF.

    IF go_alv IS NOT BOUND.
      DATA lo_parent TYPE REF TO cl_gui_container.
      IF mo_parent IS BOUND.
        lo_parent = mo_parent.
      ELSE.
        lo_parent = cl_gui_container=>default_screen.
      ENDIF.
      go_alv = NEW cl_gui_alv_grid( i_parent = lo_parent ).
    ENDIF.

    IF sy-batch = abap_false.
      set_handlers( ).
      IF at_ddown IS NOT INITIAL OR at_ddown_alias IS NOT INITIAL.
        go_alv->set_drop_down_table(
          it_drop_down       = at_ddown
          it_drop_down_alias = at_ddown_alias ).
      ENDIF.
    ENDIF.

    DATA lt_fcat TYPE lvc_t_fcat.
    lt_fcat = gt_fcat.

    go_alv->set_table_for_first_display(
      EXPORTING
        is_layout       = is_layout_ft
        is_variant      = is_variant_ft
        i_save          = iv_save_ft
      CHANGING
        it_outtab       = <lt_out>
        it_fieldcatalog = lt_fcat ).

    gt_fcat = lt_fcat.
  ENDMETHOD.

  METHOD display_data_popup.
    DATA lo_salv TYPE REF TO cl_salv_table.
    TRY.
        cl_salv_table=>factory(
          IMPORTING r_salv_table = lo_salv
          CHANGING  t_table      = ct_data ).
      CATCH cx_salv_msg.
        RETURN.
    ENDTRY.
    IF lo_salv IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lo_columns) = lo_salv->get_columns( ).
    lo_columns->set_headers_visible( value = iv_show_header ).
    DATA(lr_functions) = lo_salv->get_functions( ).
    lr_functions->set_all( abap_true ).

    LOOP AT it_fc ASSIGNING FIELD-SYMBOL(<fs_fc>).
      DATA lv_col TYPE ty_salv_colname.
      lv_col = <fs_fc>-col_name.
      DATA(lo_column) = lo_columns->get_column( columnname = lv_col ).
      IF <fs_fc>-long_txt IS NOT INITIAL.
        lo_column->set_long_text( value = <fs_fc>-long_txt ).
      ENDIF.
      IF <fs_fc>-out_len IS NOT INITIAL.
        lo_column->set_output_length( value = <fs_fc>-out_len ).
      ENDIF.
    ENDLOOP.

    lo_salv->set_screen_popup(
      start_column = i_start_column
      end_column   = i_end_column
      start_line   = i_start_line
      end_line     = i_end_line ).
    lo_salv->display( ).
  ENDMETHOD.

  METHOD get_output_table.
    er_table = gref_outtab.
  ENDMETHOD.

  METHOD update_table.
    IF it_table IS NOT INITIAL.
      IF go_alv IS BOUND.
        go_alv->refresh_table_display(
          is_stable = VALUE lvc_s_stbl( row = 'X' col = 'X' ) ).
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD set_register_fld.
    APPEND LINES OF it_f4 TO gt_f4.
  ENDMETHOD.

  METHOD set_drop_down_table.
    at_ddown       = CORRESPONDING #( BASE ( at_ddown )       it_drop_down ).
    at_ddown_alias = CORRESPONDING #( BASE ( at_ddown_alias ) it_drop_down_alias ).
  ENDMETHOD.

  METHOD set_editable_columns.
    IF ct_fcat IS INITIAL.
      LOOP AT gt_fcat ASSIGNING FIELD-SYMBOL(<f>)
           WHERE fieldname IN it_fields_rng.
        <f>-edit = abap_true.
      ENDLOOP.
    ELSE.
      LOOP AT ct_fcat ASSIGNING FIELD-SYMBOL(<fc>)
           WHERE fieldname IN it_fields_rng.
        <fc>-edit = abap_true.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD set_sum_columns.
    IF ct_fcat IS INITIAL.
      LOOP AT gt_fcat ASSIGNING FIELD-SYMBOL(<f>)
           WHERE fieldname IN it_fields_rng.
        <f>-do_sum = abap_true.
      ENDLOOP.
    ELSE.
      LOOP AT ct_fcat ASSIGNING FIELD-SYMBOL(<fc>)
           WHERE fieldname IN it_fields_rng.
        <fc>-do_sum = abap_true.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD set_checkbox_columns.
    IF ct_fcat IS INITIAL.
      LOOP AT gt_fcat ASSIGNING FIELD-SYMBOL(<f>)
           WHERE fieldname IN it_fields_rng.
        <f>-checkbox = abap_true.
        <f>-edit     = abap_true.
      ENDLOOP.
    ELSE.
      LOOP AT ct_fcat ASSIGNING FIELD-SYMBOL(<fc>)
           WHERE fieldname IN it_fields_rng.
        <fc>-checkbox = abap_true.
        <fc>-edit     = abap_true.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD create_dyn_fc.
    DATA lo_ref_descr TYPE REF TO cl_abap_structdescr.
    DATA lt_detail    TYPE abap_compdescr_tab.
    FIELD-SYMBOLS <fs_detail>      TYPE abap_compdescr.
    FIELD-SYMBOLS <fs_outtab_comp> TYPE any.

    lo_ref_descr ?= cl_abap_typedescr=>describe_by_data( is_outtab ).
    lt_detail[] = lo_ref_descr->components.

    LOOP AT lt_detail ASSIGNING <fs_detail>.
      ASSIGN COMPONENT <fs_detail>-name OF STRUCTURE is_outtab TO <fs_outtab_comp>.
      IF <fs_outtab_comp> IS ASSIGNED.
        DATA(lref_typedescr) = cl_abap_typedescr=>describe_by_data( <fs_outtab_comp> ).
        IF lref_typedescr->absolute_name+6 = 'LVC_T_SCOL'
        OR lref_typedescr->absolute_name+6 = 'LVC_T_STYL'.
          CONTINUE.
        ENDIF.

        DATA lref_elemdescr TYPE REF TO cl_abap_elemdescr.
        lref_elemdescr ?= cl_abap_typedescr=>describe_by_data( <fs_outtab_comp> ).

        APPEND VALUE lvc_s_fcat(
          ref_field  = lref_typedescr->absolute_name+6
          fieldname  = <fs_detail>-name
          outputlen  = COND #( WHEN lref_typedescr->type_kind = 'P'
                               THEN lref_elemdescr->output_length
                               ELSE lref_typedescr->length )
          decimals_o = lref_typedescr->decimals
          inttype    = <fs_detail>-type_kind
          datatype   = COND #(
                        WHEN <fs_detail>-type_kind = 'D' THEN 'DATS'
                        WHEN <fs_detail>-type_kind = 'T' THEN 'TIMS'
                        WHEN <fs_detail>-type_kind = 'N' THEN 'NUMC'
                        ELSE '' )
          col_opt    = abap_true ) TO ct_fieldcat.
      ENDIF.
    ENDLOOP.

    normalize_fieldcat(
      EXPORTING iv_tabname   = lo_ref_descr->absolute_name+6(30)
                it_custom_fc = it_custom_fc
      CHANGING  ct_fieldcat  = ct_fieldcat ).
  ENDMETHOD.


  METHOD normalize_fieldcat.
    FIELD-SYMBOLS <fs_fcat> TYPE lvc_s_fcat.

    DATA lt_custom_fc TYPE tty_fc_custom.
    lt_custom_fc = it_custom_fc.

    LOOP AT lt_custom_fc ASSIGNING FIELD-SYMBOL(<ls_cust>).
      TRANSLATE <ls_cust>-fieldname TO UPPER CASE.
    ENDLOOP.

    DATA lt_dd03t TYPE ty_ddic_cache-dd03t.
    DATA lt_dd03l TYPE ty_ddic_cache-dd03l.
    DATA lt_dd04t TYPE ty_ddic_cache-dd04t.
    DATA lt_dd01l TYPE ty_ddic_cache-dd01l.

    get_ddic_metadata(
      EXPORTING iv_tabname = iv_tabname
      IMPORTING et_dd03t   = lt_dd03t
                et_dd03l   = lt_dd03l
                et_dd04t   = lt_dd04t
                et_dd01l   = lt_dd01l ).

    DATA lv_counter TYPE i VALUE 0.

    LOOP AT ct_fieldcat ASSIGNING <fs_fcat>.
      READ TABLE lt_dd03t INTO DATA(ls03) WITH KEY fieldname = <fs_fcat>-fieldname.
      IF sy-subrc = 0 AND ls03-ddtext IS NOT INITIAL.
        IF <fs_fcat>-coltext   IS INITIAL. <fs_fcat>-coltext   = ls03-ddtext. ENDIF.
        IF <fs_fcat>-scrtext_m IS INITIAL. <fs_fcat>-scrtext_m = ls03-ddtext. ENDIF.
      ELSE.
        READ TABLE lt_dd03l INTO DATA(ls03l) WITH KEY fieldname = <fs_fcat>-fieldname.
        IF sy-subrc = 0.
          READ TABLE lt_dd04t INTO DATA(ls04) WITH KEY rollname = ls03l-rollname.
          IF sy-subrc = 0 AND ls04-scrtext_m IS NOT INITIAL.
            IF <fs_fcat>-coltext   IS INITIAL. <fs_fcat>-coltext   = ls04-scrtext_m. ENDIF.
            IF <fs_fcat>-scrtext_m IS INITIAL. <fs_fcat>-scrtext_m = ls04-scrtext_m. ENDIF.
          ENDIF.
          READ TABLE lt_dd01l INTO DATA(ls01) WITH KEY domname = ls03l-domname.
          IF sy-subrc = 0 AND ls01-convexit IS NOT INITIAL.
            <fs_fcat>-convexit = ls01-convexit.
          ENDIF.
        ENDIF.
      ENDIF.

      LOOP AT lt_custom_fc ASSIGNING FIELD-SYMBOL(<fs_custom>)
           WHERE fieldname = <fs_fcat>-fieldname OR fieldname = '*'.
        ASSIGN COMPONENT <fs_custom>-fc_component OF STRUCTURE <fs_fcat>
               TO FIELD-SYMBOL(<fs_comp>).
        IF sy-subrc = 0.
          <fs_comp> = <fs_custom>-value.
        ENDIF.
      ENDLOOP.

      IF <fs_fcat>-col_id IS INITIAL.
        lv_counter += 1.
        <fs_fcat>-col_id  = lv_counter.
        <fs_fcat>-col_pos = lv_counter.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_ddic_metadata.
    READ TABLE gt_ddic_cache ASSIGNING FIELD-SYMBOL(<ls_cache>) WITH KEY tabname = iv_tabname.
    IF sy-subrc = 0.
      et_dd03t = <ls_cache>-dd03t.
      et_dd03l = <ls_cache>-dd03l.
      et_dd04t = <ls_cache>-dd04t.
      et_dd01l = <ls_cache>-dd01l.
      RETURN.
    ENDIF.

    DATA ls_new TYPE ty_ddic_cache.
    ls_new-tabname = iv_tabname.

    SELECT fieldname, ddtext
      FROM dd03t
      WHERE tabname    = @iv_tabname
        AND ddlanguage = @sy-langu
        AND as4local   = 'A'
      INTO TABLE @ls_new-dd03t.

    SELECT fieldname, domname, rollname
      FROM dd03l
      WHERE tabname  = @iv_tabname
        AND as4local = 'A'
        AND as4vers  = ' '
      INTO TABLE @ls_new-dd03l.

    IF ls_new-dd03l IS NOT INITIAL.
      SELECT rollname, scrtext_m
        FROM dd04t
        FOR ALL ENTRIES IN @ls_new-dd03l
        WHERE rollname   = @ls_new-dd03l-rollname
          AND ddlanguage = @sy-langu
        INTO TABLE @ls_new-dd04t.

      SELECT domname, convexit
        FROM dd01l
        FOR ALL ENTRIES IN @ls_new-dd03l
        WHERE domname  = @ls_new-dd03l-domname
          AND as4local = 'A'
          AND as4vers  = ' '
        INTO TABLE @ls_new-dd01l.
    ENDIF.

    INSERT ls_new INTO TABLE gt_ddic_cache ASSIGNING <ls_cache>.
    et_dd03t = <ls_cache>-dd03t.
    et_dd03l = <ls_cache>-dd03l.
    et_dd04t = <ls_cache>-dd04t.
    et_dd01l = <ls_cache>-dd01l.
  ENDMETHOD.


  METHOD set_handlers.
    IF go_alv IS INITIAL.
      RETURN.
    ENDIF.
    SET HANDLER me->handle_toolbar               FOR go_alv.
    SET HANDLER me->handle_user_command          FOR go_alv.
    SET HANDLER me->handle_hotspot_click         FOR go_alv.
    SET HANDLER me->handle_double_click          FOR go_alv.
    SET HANDLER me->handle_data_changed          FOR go_alv.
    SET HANDLER me->handle_data_changed_finished FOR go_alv.
    IF gt_f4 IS NOT INITIAL.
      go_alv->register_f4_for_fields( it_f4 = gt_f4 ).
      SET HANDLER me->handle_on_f4 FOR go_alv.
    ENDIF.
  ENDMETHOD.


  METHOD handle_toolbar.
    IF mo_handler IS BOUND.
      mo_handler->on_toolbar( e_object = e_object e_interactive = e_interactive ).
    ELSE.
      PERFORM handle_toolbar IN PROGRAM (gv_program_name) IF FOUND USING e_object e_interactive.
    ENDIF.
  ENDMETHOD.

  METHOD handle_user_command.
    DATA lt_rows TYPE lvc_t_row.
    IF go_alv IS BOUND.
      go_alv->get_selected_rows( IMPORTING et_index_rows = lt_rows ).
    ENDIF.
    IF mo_handler IS BOUND.
      mo_handler->on_user_command( e_ucomm = e_ucomm it_rows = lt_rows ).
    ELSE.
      PERFORM handle_user_command IN PROGRAM (gv_program_name) IF FOUND USING e_ucomm lt_rows.
    ENDIF.
  ENDMETHOD.

  METHOD handle_hotspot_click.
    IF mo_handler IS BOUND.
      mo_handler->on_hotspot_click( e_row_id = e_row_id e_column_id = e_column_id es_row_no = es_row_no ).
    ELSE.
      PERFORM handle_hotspot_click IN PROGRAM (gv_program_name) IF FOUND USING e_row_id e_column_id es_row_no.
    ENDIF.
  ENDMETHOD.

  METHOD handle_double_click.
    IF mo_handler IS BOUND.
      mo_handler->on_double_click( e_row = e_row e_column = e_column ).
    ELSE.
      PERFORM handle_double_click IN PROGRAM (gv_program_name) IF FOUND USING e_row e_column.
    ENDIF.
  ENDMETHOD.

  METHOD handle_data_changed.
    IF mo_handler IS BOUND.
      mo_handler->on_data_changed( er_data_changed = er_data_changed ).
    ELSE.
      PERFORM handle_data_changed IN PROGRAM (gv_program_name) IF FOUND USING er_data_changed.
    ENDIF.
  ENDMETHOD.

  METHOD handle_on_f4.
    IF mo_handler IS BOUND.
      mo_handler->on_f4( e_fieldname = e_fieldname es_row_no = es_row_no er_event_data = er_event_data ).
    ELSE.
      PERFORM handle_on_f4 IN PROGRAM (gv_program_name) IF FOUND USING e_fieldname es_row_no er_event_data.
    ENDIF.
  ENDMETHOD.

  METHOD handle_data_changed_finished.
    IF mo_handler IS BOUND.
      mo_handler->on_data_changed_finished( e_modified = e_modified et_good_cells = et_good_cells ).
    ELSE.
      PERFORM handle_data_changed_finished IN PROGRAM (gv_program_name) IF FOUND USING e_modified et_good_cells.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
