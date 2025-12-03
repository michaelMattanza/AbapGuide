class ZCL_ALV_MANAGER definition
  public
  create public .

public section.

  types:
    BEGIN OF ty_fc_custom,
        fieldname    TYPE char255,
        fc_component TYPE char255,
        value        TYPE char255,
      END OF ty_fc_custom .
  types:
    tty_fc_custom TYPE TABLE OF ty_fc_custom .
  types:
    BEGIN OF ty_fc_popup,
            col_name TYPE lvc_fname,
            out_len  TYPE lvc_outlen,
            long_txt TYPE scrtext_l,
         END OF ty_fc_popup .
  types:
    tyt_fc_popup TYPE TABLE OF ty_fc_popup .

  data GO_ALV type ref to CL_GUI_ALV_GRID .
  data GO_SECOND_ALV type ref to CL_GUI_ALV_GRID .
  data GV_PROGRAM_NAME type STRING .
  data GT_FCAT type LVC_T_FCAT .
  data GT_SECOND_FCAT type LVC_T_FCAT .
  data GT_F4 type LVC_T_F4 .
  data GT_SECOND_F4 type LVC_T_F4 .

  methods CONSTRUCTOR
    importing
      value(IV_PROGRAM_NAME) type STRING
      value(IV_CDS_NAME) type STRING optional
      value(IV_CHARACT_FC) type FLAG optional
      value(IT_OUTTAB) type ANY
      value(IO_ALV) type ref to CL_GUI_ALV_GRID optional
      value(IT_CUSTOM_FC) type TTY_FC_CUSTOM optional .
  methods GET_FCAT
    returning
      value(RT_FCAT) type LVC_T_FCAT .
  methods SET_SECOND_TABLE
    importing
      value(IT_OUTTAB) type ANY
      value(IT_CUSTOM_FC) type TTY_FC_CUSTOM optional
      value(IV_CDS_NAME) type STRING optional
      value(IV_CHARACT_FC) type FLAG optional .
  methods DISPLAY_DATA
    importing
      value(IS_VARIANT_FT) type DISVARIANT optional
      value(IV_SAVE_FT) type CHAR01 default 'A'
      value(IS_LAYOUT_FT) type LVC_S_LAYO optional
      value(IS_VARIANT_ST) type DISVARIANT optional
      value(IV_SAVE_ST) type CHAR01 optional
      value(IS_LAYOUT_ST) type LVC_S_LAYO optional
      value(IV_VERTICAL) type FLAG optional .
  class-methods DISPLAY_DATA_POPUP
    importing
      value(I_START_COLUMN) type I default 25
      value(I_START_LINE) type I default 6
      value(I_END_COLUMN) type I default 100
      value(I_END_LINE) type I default 10
      value(IT_DATA) type STANDARD TABLE
      value(IT_FC) type TYT_FC_POPUP optional
      !IV_SHOW_HEADER type ABAP_BOOL optional .
  methods GET_OUTPUT_TABLES
    exporting
      !ET_TABLE_FT type ref to DATA
      !ET_TABLE_ST type ref to DATA .
  methods UPDATE_TABLES
    importing
      value(IT_TABLE_FT) type DATA optional
      value(IT_TABLE_ST) type DATA optional .
  methods SET_REGISTER_FLD_FIRST_ALV
    importing
      value(IT_F4) type LVC_T_F4 .
  methods SET_REGISTER_FLD_SECOND_ALV
    importing
      value(IT_F4) type LVC_T_F4 .
  methods SET_DROP_DOWN_TABLE_FIRST_ALV
    importing
      value(IT_DROP_DOWN) type LVC_T_DROP optional
      value(IT_DROP_DOWN_ALIAS) type LVC_T_DRAL optional .
  methods SET_DROP_DOWN_TABLE_SECOND_ALV
    importing
      value(IT_DROP_DOWN) type LVC_T_DROP optional
      value(IT_DROP_DOWN_ALIAS) type LVC_T_DRAL optional .
  PROTECTED SECTION.
    DATA gref_outtab TYPE REF TO data .
    DATA gref_second_outtab TYPE REF TO data .

    DATA at_ddown_first_alv TYPE lvc_t_drop.
    DATA at_ddown_second_alv TYPE lvc_t_drop.

    DATA at_ddown_alias_first_alv TYPE lvc_t_dral.
    DATA at_ddown_alias_second_alv TYPE lvc_t_dral.

private section.

  methods CREATE_DYN_FC
    importing
      value(IS_OUTTAB) type DATA
      value(IT_CUSTOM_FC) type TTY_FC_CUSTOM optional
    returning
      value(CT_FIELDCAT) type LVC_T_FCAT .
  methods CREATE_FC_FROM_CDS
    importing
      value(IV_ENTITY_NAME) type STRING
      value(IS_OUTTAB) type DATA
      value(IT_CUSTOM_FC) type TTY_FC_CUSTOM optional
    returning
      value(CT_FIELDCAT) type LVC_T_FCAT .
  methods CREATE_FC_FROM_CHARACT
    importing
      value(IS_OUTTAB) type DATA
      value(IT_CUSTOM_FC) type TTY_FC_CUSTOM optional
    returning
      value(CT_FIELDCAT) type LVC_T_FCAT .
  methods SET_HANDLER_FIRST_ALV .
  methods SET_HANDLER_SECOND_ALV .
  " Handler Prima alv
  methods HANDLE_TOOLBAR
    for event TOOLBAR of CL_GUI_ALV_GRID
    importing
      !E_OBJECT
      !E_INTERACTIVE .
  methods HANDLE_USER_COMMAND
    for event USER_COMMAND of CL_GUI_ALV_GRID
    importing
      !E_UCOMM .
  methods HANDLE_HOTSPOT_CLICK
    for event HOTSPOT_CLICK of CL_GUI_ALV_GRID
    importing
      !E_ROW_ID
      !E_COLUMN_ID
      !ES_ROW_NO .
  methods HANDLE_DATA_CHANGED
    for event DATA_CHANGED of CL_GUI_ALV_GRID
    importing
      !ER_DATA_CHANGED .
  methods HANDLE_DOUBLE_CLICK
    for event DOUBLE_CLICK of CL_GUI_ALV_GRID
    importing
      !E_ROW
      !E_COLUMN .
  methods HANDLE_ON_F4
    for event ONF4 of CL_GUI_ALV_GRID
    importing
      !E_FIELDNAME
      !ES_ROW_NO
      !ER_EVENT_DATA .
  " Handler Seconda alv
  methods HANDLE_TOOLBAR_ST
    for event TOOLBAR of CL_GUI_ALV_GRID
    importing
      !E_OBJECT
      !E_INTERACTIVE .
  methods HANDLE_USER_COMMAND_ST
    for event USER_COMMAND of CL_GUI_ALV_GRID
    importing
      !E_UCOMM .
  methods HANDLE_HOTSPOT_CLICK_ST
    for event HOTSPOT_CLICK of CL_GUI_ALV_GRID
    importing
      !E_ROW_ID
      !E_COLUMN_ID
      !ES_ROW_NO .
  methods HANDLE_DATA_CHANGED_ST
    for event DATA_CHANGED of CL_GUI_ALV_GRID
    importing
      !ER_DATA_CHANGED .
  methods HANDLE_DOUBLE_CLICK_ST
    for event DOUBLE_CLICK of CL_GUI_ALV_GRID
    importing
      !E_ROW
      !E_COLUMN .
  methods HANDLE_ON_F4_ST
    for event ONF4 of CL_GUI_ALV_GRID
    importing
      !E_FIELDNAME
      !ES_ROW_NO
      !ER_EVENT_DATA .
  methods HANDLE_DATA_CHANGED_FINISHED
    for event DATA_CHANGED_FINISHED of CL_GUI_ALV_GRID
    importing
      !E_MODIFIED
      !ET_GOOD_CELLS .
ENDCLASS.



CLASS ZCL_ALV_MANAGER IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ALV_MANAGER->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_PROGRAM_NAME                TYPE        STRING
* | [--->] IV_CDS_NAME                    TYPE        STRING(optional)
* | [--->] IV_CHARACT_FC                  TYPE        FLAG(optional)
* | [--->] IT_OUTTAB                      TYPE        ANY
* | [--->] IO_ALV                         TYPE REF TO CL_GUI_ALV_GRID(optional)
* | [--->] IT_CUSTOM_FC                   TYPE        TTY_FC_CUSTOM(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.

    DATA: lref_row_outtab     TYPE REF TO data.

    FIELD-SYMBOLS: <fs_outtab_row> TYPE any,
                   <fs_outtab>     TYPE INDEX TABLE.

    gv_program_name = iv_program_name.

    " Creo una tabella indicizzata
    CREATE DATA gref_outtab LIKE it_outtab.
    ASSIGN gref_outtab->* TO <fs_outtab>.

    " Creo una struttura basata sulla tabella
    CREATE DATA lref_row_outtab LIKE LINE OF <fs_outtab>.
    ASSIGN lref_row_outtab->* TO <fs_outtab_row>.

    " Passo i dati in input alla tabella
    MOVE-CORRESPONDING it_outtab TO <fs_outtab>.

    " Creo il fc della tabella ricevuta
    IF iv_cds_name IS NOT INITIAL.
      gt_fcat = create_fc_from_cds( iv_entity_name = iv_cds_name is_outtab = <fs_outtab_row> it_custom_fc = it_custom_fc ).
    ELSE.
      gt_fcat = create_dyn_fc( EXPORTING is_outtab = <fs_outtab_row> it_custom_fc = it_custom_fc ).
    ENDIF.

    IF iv_charact_fc EQ abap_true.
      gt_fcat = create_fc_from_charact( is_outtab = <fs_outtab_row> it_custom_fc = it_custom_fc ).
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ALV_MANAGER->CREATE_DYN_FC
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_OUTTAB                      TYPE        DATA
* | [--->] IT_CUSTOM_FC                   TYPE        TTY_FC_CUSTOM(optional)
* | [<-()] CT_FIELDCAT                    TYPE        LVC_T_FCAT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_dyn_fc.
    TYPES: BEGIN OF ty_dd04t,
             rollname  TYPE rollname,
             scrtext_m TYPE scrtext_m,
           END OF ty_dd04t,
           BEGIN OF ty_dd01l,
             domname  TYPE dd01l-domname,
             convexit TYPE dd01l-convexit,
           END OF ty_dd01l.

    DATA : lo_ref_descr   TYPE REF TO cl_abap_structdescr,
           lt_detail      TYPE abap_compdescr_tab,
           lt_field_det   TYPE REF TO cl_abap_structdescr,
           lt_coldescr    TYPE TABLE OF ty_dd04t,
           lt_dd01l       TYPE TABLE OF ty_dd01l,
           lref_typedescr TYPE REF TO cl_abap_typedescr,
           lref_elemdescr TYPE REF TO cl_abap_elemdescr,
           lv_counter     TYPE i VALUE 0.

    FIELD-SYMBOLS: <fs_dref>  TYPE any,
                   <fs_fname> TYPE any.

    lo_ref_descr ?= cl_abap_typedescr=>describe_by_data( is_outtab ). "Chiamare metodo statico su una struttura
    lt_detail[] = lo_ref_descr->components.

    " Loop sui componenti della struttura - Creo fc
    LOOP AT lt_detail ASSIGNING FIELD-SYMBOL(<fs_detail>).
      ASSIGN COMPONENT <fs_detail>-name OF STRUCTURE is_outtab TO FIELD-SYMBOL(<fs_outtab_comp>).

      IF <fs_outtab_comp> IS ASSIGNED.

        lref_typedescr = cl_abap_typedescr=>describe_by_data( <fs_outtab_comp> ) .

        IF lref_typedescr->absolute_name+6 EQ 'LVC_T_SCOL' OR lref_typedescr->absolute_name+6 EQ 'LVC_T_STYL'.
          CONTINUE.
        ENDIF.
        lref_elemdescr ?= cl_abap_typedescr=>describe_by_data( <fs_outtab_comp> ) .

        APPEND VALUE #(
          ref_field = lref_typedescr->absolute_name+6
          fieldname = <fs_detail>-name
          outputlen = COND #( WHEN lref_typedescr->type_kind EQ 'P' THEN lref_elemdescr->output_length ELSE lref_typedescr->length )
          decimals_o = lref_typedescr->decimals
          inttype = <fs_detail>-type_kind
          datatype = COND #( WHEN <fs_detail>-type_kind EQ 'D' THEN 'DATS' ELSE '' )
*          intlen = COND #( WHEN <fs_detail>-type_kind EQ 'D' THEN '000008' ELSE '' )
*          dd_outlen  = COND #( WHEN <fs_detail>-type_kind EQ 'D' THEN '000010' ELSE '' )
          col_opt = 'X'
        ) TO ct_fieldcat.

      ENDIF.
    ENDLOOP.

    " Estraggo descrizioni colonne std
    SELECT rollname, scrtext_m
     FROM dd04t
     INTO TABLE @lt_coldescr
     FOR ALL ENTRIES IN @ct_fieldcat
     WHERE rollname EQ @ct_fieldcat-ref_field
     AND ddlanguage EQ @sy-langu.

    SELECT domname, convexit
     FROM dd01l
     INTO CORRESPONDING FIELDS OF TABLE @lt_dd01l
     FOR ALL ENTRIES IN @ct_fieldcat
     WHERE domname EQ @ct_fieldcat-fieldname
     AND as4local EQ 'A'
     AND as4vers EQ ' '.

    SELECT fieldname, ddtext
    FROM dd03t
    WHERE tabname EQ @lo_ref_descr->absolute_name+6(30)
    AND ddlanguage EQ @sy-langu
    AND as4local EQ 'A'
    INTO TABLE @DATA(lt_dd03t).


    " Trasformo i campi inseriti dall'utente in upper case per evitare errori
    LOOP AT it_custom_fc REFERENCE INTO DATA(lr_cust_fc).
      TRANSLATE lr_cust_fc->fieldname TO UPPER CASE.
    ENDLOOP.

    " Applico le modifiche custom ai campi del fc
    LOOP AT ct_fieldcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).

      READ TABLE lt_dd03t INTO DATA(ls_dd03t) WITH KEY fieldname = <fs_fcat>-fieldname.
      IF sy-subrc EQ 0.
        <fs_fcat>-coltext = <fs_fcat>-scrtext_m = ls_dd03t-ddtext.
      ELSE.
        " Inserisco la descrizione del dominio nella colonna
        READ TABLE lt_coldescr INTO DATA(ls_coldescr) WITH KEY rollname = <fs_fcat>-ref_field.
        <fs_fcat>-coltext = <fs_fcat>-scrtext_m = ls_coldescr-scrtext_m.
      ENDIF.

      READ TABLE lt_dd01l INTO DATA(ls_dd01l) WITH KEY domname = <fs_fcat>-fieldname.
      <fs_fcat>-convexit = ls_dd01l-convexit.

      LOOP AT it_custom_fc ASSIGNING FIELD-SYMBOL(<fs_custom_fc>)
        WHERE fieldname EQ <fs_fcat>-fieldname OR fieldname EQ '*'.

        ASSIGN COMPONENT <fs_custom_fc>-fc_component OF STRUCTURE <fs_fcat> TO FIELD-SYMBOL(<fs_comp>).
        IF sy-subrc EQ 0.
          <fs_comp> =  <fs_custom_fc>-value.
        ENDIF.
      ENDLOOP.

      IF <fs_fcat>-col_id IS INITIAL.
        lv_counter += 1.
        <fs_fcat>-col_id = <fs_fcat>-col_pos = lv_counter.
      ENDIF.

      CLEAR: ls_dd01l, ls_coldescr.
    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ALV_MANAGER->CREATE_FC_FROM_CDS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ENTITY_NAME                 TYPE        STRING
* | [--->] IS_OUTTAB                      TYPE        DATA
* | [--->] IT_CUSTOM_FC                   TYPE        TTY_FC_CUSTOM(optional)
* | [<-()] CT_FIELDCAT                    TYPE        LVC_T_FCAT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_fc_from_cds.

    DATA : lo_ref_descr   TYPE REF TO cl_abap_structdescr,
           lt_detail      TYPE abap_compdescr_tab,
           lt_field_det   TYPE REF TO cl_abap_structdescr,
           lref_typedescr TYPE REF TO cl_abap_typedescr,
           lref_elemdescr TYPE REF TO cl_abap_elemdescr.

    cl_dd_ddl_annotation_service=>get_annos(
      EXPORTING
        entityname      = CONV #( iv_entity_name )
      IMPORTING
        element_annos   = DATA(element_annos)
        entity_annos    = DATA(entity_annos)
        parameter_annos = DATA(parameter_annos)
        annos_tstmp     = DATA(annos_tstmp)
    ).

    DATA(lv_counter) = 0.
    lo_ref_descr ?= cl_abap_typedescr=>describe_by_data( is_outtab ). "Chiamare metodo statico su una struttura
    lt_detail[] = lo_ref_descr->components.

    LOOP AT lt_detail ASSIGNING FIELD-SYMBOL(<fs_detail>).
      ASSIGN COMPONENT <fs_detail>-name OF STRUCTURE is_outtab TO FIELD-SYMBOL(<fs_comp>).
      DATA(ls_col_text) = VALUE #(  element_annos[ elementname = <fs_detail>-name annoname = 'ENDUSERTEXT.LABEL' ] OPTIONAL ).

      IF <fs_comp> IS ASSIGNED.

        lref_typedescr = cl_abap_typedescr=>describe_by_data( <fs_comp> ) .
        lref_elemdescr ?= cl_abap_typedescr=>describe_by_data( <fs_comp> ) .

        APPEND VALUE #(
          ref_field = lref_typedescr->absolute_name+6
          fieldname = <fs_detail>-name
          outputlen = COND #( WHEN lref_typedescr->type_kind EQ 'P' THEN lref_elemdescr->output_length ELSE lref_typedescr->length )
          decimals_o = lref_typedescr->decimals
          col_opt = 'X'
        ) TO ct_fieldcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).

        <fs_fcat>-coltext = <fs_fcat>-scrtext_m = ls_col_text-value.
      ENDIF.
      LOOP AT it_custom_fc ASSIGNING FIELD-SYMBOL(<fs_custom_fc>)
        WHERE fieldname EQ <fs_fcat>-fieldname OR fieldname EQ '*'.

        ASSIGN COMPONENT <fs_custom_fc>-fc_component OF STRUCTURE <fs_fcat> TO FIELD-SYMBOL(<fs_comp_fcat>).
        IF sy-subrc EQ 0.
          <fs_comp_fcat> =  <fs_custom_fc>-value.
        ENDIF.
      ENDLOOP.

      IF <fs_fcat>-col_id IS INITIAL.
        lv_counter += 1.
        <fs_fcat>-col_id = <fs_fcat>-col_pos = lv_counter.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ALV_MANAGER->CREATE_FC_FROM_CHARACT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_OUTTAB                      TYPE        DATA
* | [--->] IT_CUSTOM_FC                   TYPE        TTY_FC_CUSTOM(optional)
* | [<-()] CT_FIELDCAT                    TYPE        LVC_T_FCAT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_fc_from_charact.
    DATA : lo_ref_descr         TYPE REF TO cl_abap_structdescr,
           lref_typedescr       TYPE REF TO cl_abap_typedescr,
           lt_detail            TYPE abap_compdescr_tab,
           lt_chardescr         TYPE TABLE OF bapicharactdescr,
           lt_bapiret           TYPE TABLE OF bapiret2,
           ls_bapicharactdetail TYPE bapicharactdetail.

    DATA(lv_counter) = 0.
    lo_ref_descr ?= cl_abap_typedescr=>describe_by_data( is_outtab ). "Chiamare metodo statico su una struttura
    lt_detail[] = lo_ref_descr->components.

    LOOP AT lt_detail ASSIGNING FIELD-SYMBOL(<fs_detail>).
      lv_counter += 1.

      CALL FUNCTION 'BAPI_CHARACT_GETDETAIL'
        EXPORTING
          charactname   = <fs_detail>-name
          keydate       = sy-datum
        IMPORTING
          charactdetail = ls_bapicharactdetail
        TABLES
          charactdescr  = lt_chardescr
          return        = lt_bapiret.

      CHECK ls_bapicharactdetail IS NOT INITIAL.
      ASSIGN COMPONENT <fs_detail>-name OF STRUCTURE is_outtab TO FIELD-SYMBOL(<fs_comp>).

      IF <fs_comp> IS ASSIGNED.
        lref_typedescr = cl_abap_typedescr=>describe_by_data( <fs_comp> ) .

        DATA(ls_chardescr) = VALUE #( lt_chardescr[ 1 ] OPTIONAL ).
        APPEND VALUE #(
            inttype = COND #( WHEN ls_bapicharactdetail-data_type EQ 'CHAR' THEN 'C' ELSE 'P' ) "lref_typedescr->absolute_name+6
            fieldname = <fs_detail>-name
            outputlen = ls_bapicharactdetail-length
            col_id    = lv_counter
            decimals_o = ls_bapicharactdetail-decimals
            coltext = ls_chardescr-description
            scrtext_m = ls_chardescr-description
          ) TO ct_fieldcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).

        LOOP AT it_custom_fc ASSIGNING FIELD-SYMBOL(<fs_custom_fc>)
          WHERE fieldname EQ <fs_fcat>-fieldname OR fieldname EQ '*'.

          ASSIGN COMPONENT <fs_custom_fc>-fc_component OF STRUCTURE <fs_fcat> TO FIELD-SYMBOL(<fs_comp_fcat>).
          IF sy-subrc EQ 0.
            <fs_comp_fcat> =  <fs_custom_fc>-value.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ALV_MANAGER->DISPLAY_DATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_VARIANT_FT                  TYPE        DISVARIANT(optional)
* | [--->] IV_SAVE_FT                     TYPE        CHAR01 (default ='A')
* | [--->] IS_LAYOUT_FT                   TYPE        LVC_S_LAYO(optional)
* | [--->] IS_VARIANT_ST                  TYPE        DISVARIANT(optional)
* | [--->] IV_SAVE_ST                     TYPE        CHAR01(optional)
* | [--->] IS_LAYOUT_ST                   TYPE        LVC_S_LAYO(optional)
* | [--->] IV_VERTICAL                    TYPE        FLAG(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD display_data.
    FIELD-SYMBOLS: <fs_outtab>        TYPE ANY TABLE,
                   <fs_second_outtab> TYPE ANY TABLE.

    IF gt_second_fcat IS NOT INITIAL.

      DATA(lo_cont_docking) = NEW cl_gui_docking_container(
        parent = cl_gui_container=>screen0
        ratio  = 90
      ).
      lo_cont_docking->set_extension( EXPORTING extension = 99999 EXCEPTIONS cntl_error = 1 OTHERS = 2 ).

      DATA(lo_split_container) = NEW cl_gui_splitter_container(
        parent  = lo_cont_docking
        rows    = COND #( WHEN iv_vertical EQ abap_true THEN '1' ELSE '2' )
        columns = COND #( WHEN iv_vertical EQ abap_true THEN '2' ELSE '1' )
      ).

      lo_split_container->get_container(
        EXPORTING
          row       = 1                 " Row
          column    = 1                " Column
        RECEIVING
          container = DATA(lo_first_container)                 " Container
      ).

      lo_split_container->get_container(
        EXPORTING
          row       = COND #( WHEN iv_vertical EQ abap_true THEN '1' ELSE '2' )                 " Row
          column    = COND #( WHEN iv_vertical EQ abap_true THEN '2' ELSE '1' )                " Column
        RECEIVING
          container = DATA(lo_second_container)                 " Container
      ).

      go_alv = NEW cl_gui_alv_grid( i_parent = lo_first_container ).
      go_second_alv = NEW cl_gui_alv_grid( i_parent = lo_second_container ).

      IF sy-batch EQ abap_false.
      set_handler_first_alv( ).
      set_handler_second_alv( ).

      go_alv->set_drop_down_table(
        EXPORTING
          it_drop_down       = at_ddown_first_alv
          it_drop_down_alias = at_ddown_alias_first_alv
      ).

      go_second_alv->set_drop_down_table(
        EXPORTING
          it_drop_down       = at_ddown_second_alv
          it_drop_down_alias = at_ddown_alias_second_alv
      ).
      ENDIF.

      ASSIGN gref_outtab->* TO <fs_outtab>.
      ASSIGN gref_second_outtab->* TO <fs_second_outtab>.

      go_alv->set_table_for_first_display(
        EXPORTING
          is_layout       = is_layout_ft
          is_variant      = is_variant_ft
          i_save          = iv_save_ft
        CHANGING
          it_outtab       = <fs_outtab>
          it_fieldcatalog = gt_fcat
      ).

      go_second_alv->set_table_for_first_display(
        EXPORTING
          is_layout       = is_layout_st
          is_variant      = is_variant_st
          i_save          = iv_save_st
        CHANGING
          it_outtab       = <fs_second_outtab>
          it_fieldcatalog = gt_second_fcat
      ).

      DATA(lt_empty_cell) = VALUE lvc_t_cell( ( row_id = VALUE #( index = 0 ) ) ).
      go_second_alv->set_selected_cells( it_cells = lt_empty_cell ).

    ELSE.
      go_alv = NEW cl_gui_alv_grid( i_parent = cl_gui_container=>default_screen ).
      IF sy-batch EQ abap_false.
      set_handler_first_alv( ).
      go_alv->set_drop_down_table(
        EXPORTING
          it_drop_down       = at_ddown_first_alv
          it_drop_down_alias = at_ddown_alias_first_alv
      ).
      ENDIF.
      ASSIGN gref_outtab->* TO <fs_outtab>.

      go_alv->set_table_for_first_display(
        EXPORTING
          is_layout       = is_layout_ft
          is_variant      = is_variant_ft
          i_save          = iv_save_ft
        CHANGING
          it_outtab       = <fs_outtab>
          it_fieldcatalog = gt_fcat
      ).
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_ALV_MANAGER=>DISPLAY_DATA_POPUP
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_START_COLUMN                 TYPE        I (default =25)
* | [--->] I_START_LINE                   TYPE        I (default =6)
* | [--->] I_END_COLUMN                   TYPE        I (default =100)
* | [--->] I_END_LINE                     TYPE        I (default =10)
* | [--->] IT_DATA                        TYPE        STANDARD TABLE
* | [--->] IT_FC                          TYPE        TYT_FC_POPUP(optional)
* | [--->] IV_SHOW_HEADER                 TYPE        ABAP_BOOL(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD display_data_popup.
    DATA: lo_salv TYPE REF TO cl_salv_table.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = lo_salv
          CHANGING
            t_table      = it_data[] ).

      CATCH cx_salv_msg.
    ENDTRY.

    DATA: lr_functions TYPE REF TO cl_salv_functions_list.

    lr_functions = lo_salv->get_functions( ).
    lr_functions->set_all( 'X' ).
    DATA(lo_columns) = lo_salv->get_columns( ).
    lo_columns->set_headers_visible(
                value = iv_show_header
            ).

    LOOP AT it_fc ASSIGNING FIELD-SYMBOL(<fs_fc>).
        DATA(lo_column) = lo_columns->get_column( columnname = <fs_fc>-col_name ).
        IF <fs_fc>-long_txt IS NOT INITIAL.
            lo_column->set_long_text( value = <fs_fc>-long_txt ).
        ENDIF.
        IF <fs_fc>-out_len IS NOT INITIAL.
            lo_column->set_output_length( value = <fs_fc>-out_len ).
        ENDIF.
    ENDLOOP.

    IF lo_salv IS BOUND.

      lo_salv->set_screen_popup(
        start_column = i_start_column
        end_column  = i_end_column
        start_line  = i_start_line
        end_line    = i_end_line

      ).
      lo_salv->display( ).

    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ALV_MANAGER->GET_FCAT
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RT_FCAT                        TYPE        LVC_T_FCAT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_fcat.
    " Ritorno all'utente il fc creato nel costruttore
    rt_fcat = gt_fcat.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ALV_MANAGER->GET_OUTPUT_TABLES
* +-------------------------------------------------------------------------------------------------+
* | [<---] ET_TABLE_FT                    TYPE REF TO DATA
* | [<---] ET_TABLE_ST                    TYPE REF TO DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_output_tables.
    et_table_ft = gref_outtab.
    et_table_st = gref_second_outtab.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ALV_MANAGER->HANDLE_DATA_CHANGED
* +-------------------------------------------------------------------------------------------------+
* | [--->] ER_DATA_CHANGED                LIKE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD handle_data_changed.
    PERFORM handle_data_changed IN PROGRAM (gv_program_name) IF FOUND USING er_data_changed.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ALV_MANAGER->HANDLE_DATA_CHANGED_FINISHED
* +-------------------------------------------------------------------------------------------------+
* | [--->] E_MODIFIED                     LIKE
* | [--->] ET_GOOD_CELLS                  LIKE
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD handle_data_changed_finished.
  PERFORM handle_data_changed_finished
       IN PROGRAM (gv_program_name) IF FOUND USING e_modified
                                                   et_good_cells.
ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ALV_MANAGER->HANDLE_DATA_CHANGED_ST
* +-------------------------------------------------------------------------------------------------+
* | [--->] ER_DATA_CHANGED                LIKE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD handle_data_changed_st.
    PERFORM handle_data_changed_st IN PROGRAM (gv_program_name) IF FOUND USING er_data_changed.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ALV_MANAGER->HANDLE_DOUBLE_CLICK
* +-------------------------------------------------------------------------------------------------+
* | [--->] E_ROW                          LIKE
* | [--->] E_COLUMN                       LIKE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD handle_double_click.
    PERFORM handle_double_click IN PROGRAM (gv_program_name) IF FOUND USING e_row e_column.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ALV_MANAGER->HANDLE_DOUBLE_CLICK_ST
* +-------------------------------------------------------------------------------------------------+
* | [--->] E_ROW                          LIKE
* | [--->] E_COLUMN                       LIKE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD handle_double_click_st.
    PERFORM handle_double_click_st IN PROGRAM (gv_program_name) IF FOUND USING e_row e_column.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ALV_MANAGER->HANDLE_HOTSPOT_CLICK
* +-------------------------------------------------------------------------------------------------+
* | [--->] E_ROW_ID                       LIKE
* | [--->] E_COLUMN_ID                    LIKE
* | [--->] ES_ROW_NO                      LIKE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD handle_hotspot_click.
    PERFORM handle_hotspot_click IN PROGRAM (gv_program_name) IF FOUND USING e_row_id e_column_id es_row_no.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ALV_MANAGER->HANDLE_HOTSPOT_CLICK_ST
* +-------------------------------------------------------------------------------------------------+
* | [--->] E_ROW_ID                       LIKE
* | [--->] E_COLUMN_ID                    LIKE
* | [--->] ES_ROW_NO                      LIKE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD handle_hotspot_click_st.
    PERFORM handle_hotspot_click_st IN PROGRAM (gv_program_name) IF FOUND USING  e_row_id e_column_id es_row_no.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ALV_MANAGER->HANDLE_ON_F4
* +-------------------------------------------------------------------------------------------------+
* | [--->] E_FIELDNAME                    LIKE
* | [--->] ES_ROW_NO                      LIKE
* | [--->] ER_EVENT_DATA                  LIKE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD handle_on_f4.
    PERFORM handle_on_f4 IN PROGRAM (gv_program_name) IF FOUND USING e_fieldname es_row_no er_event_data.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ALV_MANAGER->HANDLE_ON_F4_ST
* +-------------------------------------------------------------------------------------------------+
* | [--->] E_FIELDNAME                    LIKE
* | [--->] ES_ROW_NO                      LIKE
* | [--->] ER_EVENT_DATA                  LIKE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD handle_on_f4_st.
    PERFORM handle_on_f4_st IN PROGRAM (gv_program_name) IF FOUND USING e_fieldname es_row_no er_event_data.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ALV_MANAGER->HANDLE_TOOLBAR
* +-------------------------------------------------------------------------------------------------+
* | [--->] E_OBJECT                       LIKE CL_ALV_EVENT_TOOLBAR_SET
* | [--->] E_INTERACTIVE                  LIKE CHAR01
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD handle_toolbar.
    PERFORM handle_toolbar IN PROGRAM (gv_program_name) IF FOUND USING e_object e_interactive.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ALV_MANAGER->HANDLE_TOOLBAR_ST
* +-------------------------------------------------------------------------------------------------+
* | [--->] E_OBJECT                       LIKE
* | [--->] E_INTERACTIVE                  LIKE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD handle_toolbar_st.
    PERFORM handle_toolbar_st IN PROGRAM (gv_program_name) IF FOUND USING e_object e_interactive.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ALV_MANAGER->HANDLE_USER_COMMAND
* +-------------------------------------------------------------------------------------------------+
* | [--->] E_UCOMM                        LIKE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD handle_user_command.
    DATA: lt_rows     TYPE lvc_t_row.
    go_alv->get_selected_rows(
      IMPORTING
        et_index_rows = lt_rows
    ).
    PERFORM handle_user_command IN PROGRAM (gv_program_name) IF FOUND USING e_ucomm lt_rows.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ALV_MANAGER->HANDLE_USER_COMMAND_ST
* +-------------------------------------------------------------------------------------------------+
* | [--->] E_UCOMM                        LIKE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD handle_user_command_st.
    DATA: lt_rows     TYPE lvc_t_row.
    go_second_alv->get_selected_rows(
      IMPORTING
        et_index_rows = lt_rows
    ).
    PERFORM handle_user_command_st IN PROGRAM (gv_program_name) IF FOUND USING  e_ucomm lt_rows.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ALV_MANAGER->SET_DROP_DOWN_TABLE_FIRST_ALV
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_DROP_DOWN                   TYPE        LVC_T_DROP(optional)
* | [--->] IT_DROP_DOWN_ALIAS             TYPE        LVC_T_DRAL(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_drop_down_table_first_alv.
    at_ddown_first_alv = CORRESPONDING #( BASE ( at_ddown_first_alv ) it_drop_down ).
    at_ddown_alias_first_alv = CORRESPONDING #( BASE ( at_ddown_alias_first_alv ) it_drop_down_alias ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ALV_MANAGER->SET_DROP_DOWN_TABLE_SECOND_ALV
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_DROP_DOWN                   TYPE        LVC_T_DROP(optional)
* | [--->] IT_DROP_DOWN_ALIAS             TYPE        LVC_T_DRAL(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_drop_down_table_second_alv.
    at_ddown_second_alv = CORRESPONDING #( BASE ( at_ddown_second_alv ) it_drop_down ).
    at_ddown_alias_second_alv = CORRESPONDING #( BASE ( at_ddown_alias_second_alv ) it_drop_down_alias ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ALV_MANAGER->SET_HANDLER_FIRST_ALV
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_handler_first_alv.

    SET HANDLER me->handle_toolbar FOR go_alv.
    SET HANDLER me->handle_user_command FOR go_alv.
    SET HANDLER me->handle_hotspot_click FOR go_alv.
    SET HANDLER me->handle_double_click FOR go_alv.
    SET HANDLER me->handle_data_changed FOR go_alv.
    SET HANDLER me->handle_data_changed_finished FOR go_alv.

    IF gt_f4 IS NOT INITIAL.
      IF go_alv IS NOT INITIAL.
        go_alv->register_f4_for_fields( it_f4 = gt_f4 ).
      ENDIF.
      SET HANDLER me->handle_on_f4        FOR go_alv.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ALV_MANAGER->SET_HANDLER_SECOND_ALV
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_handler_second_alv.
    SET HANDLER me->handle_toolbar_st FOR go_second_alv.
    SET HANDLER me->handle_user_command_st FOR go_second_alv.
    SET HANDLER me->handle_hotspot_click_st FOR go_second_alv.
    SET HANDLER me->handle_double_click_st FOR go_second_alv.
    SET HANDLER me->handle_data_changed_st FOR go_second_alv.
    IF gt_f4 IS NOT INITIAL.
      IF go_second_alv IS NOT INITIAL.
        go_second_alv->register_f4_for_fields( it_f4 = gt_second_f4 ).
      ENDIF.
      SET HANDLER me->handle_on_f4_st       FOR go_second_alv.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ALV_MANAGER->SET_REGISTER_FLD_FIRST_ALV
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_F4                          TYPE        LVC_T_F4
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_register_fld_first_alv.
    APPEND LINES OF it_f4 TO gt_f4.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ALV_MANAGER->SET_REGISTER_FLD_SECOND_ALV
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_F4                          TYPE        LVC_T_F4
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_register_fld_second_alv.
    APPEND LINES OF it_f4 TO gt_second_f4.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ALV_MANAGER->SET_SECOND_TABLE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_OUTTAB                      TYPE        ANY
* | [--->] IT_CUSTOM_FC                   TYPE        TTY_FC_CUSTOM(optional)
* | [--->] IV_CDS_NAME                    TYPE        STRING(optional)
* | [--->] IV_CHARACT_FC                  TYPE        FLAG(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_second_table.
    DATA: lref_row_outtab TYPE REF TO data.

    FIELD-SYMBOLS: <fs_outtab_row> TYPE any,
                   <fs_outtab>     TYPE INDEX TABLE.

    " Creo una tabella indicizzata
    CREATE DATA gref_second_outtab LIKE it_outtab.
    ASSIGN gref_second_outtab->* TO <fs_outtab>.

    " Creo una struttura basata sulla tabella
    CREATE DATA lref_row_outtab LIKE LINE OF <fs_outtab>.
    ASSIGN lref_row_outtab->* TO <fs_outtab_row>.

    MOVE-CORRESPONDING it_outtab TO <fs_outtab>.

    IF iv_cds_name IS NOT INITIAL.
      gt_second_fcat = create_fc_from_cds( iv_entity_name = iv_cds_name is_outtab = <fs_outtab_row> it_custom_fc = it_custom_fc ).
    ELSE.
      gt_second_fcat = create_dyn_fc( EXPORTING is_outtab = <fs_outtab_row> it_custom_fc = it_custom_fc ).
    ENDIF.

    IF iv_charact_fc EQ abap_true.
      gt_second_fcat = create_fc_from_charact( is_outtab = <fs_outtab_row> it_custom_fc = it_custom_fc ).
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ALV_MANAGER->UPDATE_TABLES
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_TABLE_FT                    TYPE        DATA(optional)
* | [--->] IT_TABLE_ST                    TYPE        DATA(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD update_tables.
    IF it_table_ft IS NOT INITIAL.
      gref_outtab = it_table_ft.

      go_alv->refresh_table_display(
        EXPORTING
          is_stable      = VALUE lvc_s_stbl( row = 'X' col = 'X' )
      ).

    ENDIF.
    IF it_table_st IS NOT INITIAL.
      gref_second_outtab = it_table_st.
      go_second_alv->refresh_table_display(
        EXPORTING
          is_stable      = VALUE lvc_s_stbl( row = 'X' col = 'X' )
      ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
