class ZWW_CAT_CL_CDS_UTILS definition
  public
  final
  create public .

public section.
      TYPES: BEGIN OF ty_filter_fields,
            cond_field TYPE fieldname,
            cond_operator TYPE zww_dtel_where_operator,
            cond_value TYPE string,
           END OF ty_filter_fields,
           tyt_filter_fields TYPE TABLE OF ty_filter_fields.

  interfaces IF_SADL_EXIT .
  interfaces IF_SADL_EXIT_CALC_ELEMENT_READ .

  class-methods get_cds_data
    importing it_filter_fields TYPE tyt_filter_fields
              iv_cdsview_name TYPE string.

   class-methods call_odata_service
    importing iv_service_name TYPE string.

protected section.
private section.

ENDCLASS.



CLASS ZWW_CAT_CL_CDS_UTILS IMPLEMENTATION.


  METHOD CALL_ODATA_SERVICE.

  ENDMETHOD.


  METHOD GET_CDS_DATA.
    DATA: lv_where_clause TYPE string.
    DATA: lv_tabname TYPE tabname.

DATA: lo_tabtype     TYPE REF TO cl_abap_tabledescr,
      lo_struct_type TYPE REF TO cl_abap_structdescr,
      lr_data        TYPE REF TO data,
      lt_comp_tab    TYPE cl_abap_structdescr=>component_table,
      ls_comp_fld    TYPE cl_abap_structdescr=>component.

    FIELD-SYMBOLS: <fs_tab> TYPE ANY TABLE,
               <fs_struct> TYPE ANY.

    lv_tabname = iv_cdsview_name.  " Give tab name from your dropdown select

    lo_struct_type ?= cl_abap_typedescr=>describe_by_name( lv_tabname ).
    lt_comp_tab  = lo_struct_type->get_components( ).

    lo_struct_type = cl_abap_structdescr=>create( lt_comp_tab ).
    lo_tabtype     = cl_abap_tabledescr=>create( lo_struct_type ).

    CREATE DATA lr_data TYPE HANDLE lo_tabtype.
    ASSIGN lr_data->* TO <fs_tab>.


    LOOP AT it_filter_fields ASSIGNING FIELD-SYMBOL(<fs_filter_fields>).
        lv_where_clause = |{ lv_where_clause }{ <fs_filter_fields>-cond_field } { <fs_filter_fields>-cond_operator } '{ <fs_filter_fields>-cond_value }'|.
        IF sy-tabix LT lines( it_filter_fields ).
            lv_where_clause = |{ lv_where_clause } AND|.
        ENDIF.
    ENDLOOP.

    TRY.
    SELECT * FROM (iv_cdsview_name) WHERE (lv_where_clause) INTO TABLE @<fs_tab>.
    CATCH CX_SY_DYNAMIC_OSQL_SEMANTICS.

    ENDTRY.

    zww_cat_cl_excel=>create_excel_from_table(
*      EXPORTING
*        it_fcat_column =
      IMPORTING
        rv_bin_file    = DATA(lv_bin_file)
      CHANGING
        ct_table       = <fs_tab>
    ).

    data: str TYPE string.

  CALL METHOD cl_gui_frontend_services=>directory_browse
    CHANGING
      selected_folder      = str
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.

    str = |{ str }/{ iv_cdsview_name }_test_data.csv|.

    zww_cat_cl_excel=>download_excel(
      EXPORTING
        iv_path       = str
        iv_excel_data = lv_bin_file
    ).
  ENDMETHOD.


  method IF_SADL_EXIT_CALC_ELEMENT_READ~CALCULATE.

      DATA: lt_cdsviewdata TYPE REF TO DATA,
            lt_textname_fields TYPE TABLE OF string,
            lt_lines      TYPE TABLE OF tline,
            lv_tdname TYPE thead-tdname.

      FIELD-SYMBOLS: <fs_cdsviewdata> TYPE ANY TABLE.

      LOOP AT it_original_data ASSIGNING FIELD-SYMBOL(<fs_orig_data>).
        ASSIGN COMPONENT 'CDS_NAME' OF STRUCTURE <fs_orig_data> TO FIELD-SYMBOL(<fs_component>).
        IF <fs_component> IS ASSIGNED.
            DATA(lv_cdsname) = CONV string( <fs_component> ).
        ENDIF.
        EXIT.
      ENDLOOP.

      CHECK lv_cdsname IS NOT INITIAL.
      CREATE DATA lt_cdsviewdata TYPE TABLE OF (lv_cdsname).
      ASSIGN lt_cdsviewdata->* TO <fs_cdsviewdata>.

      CHECK <fs_cdsviewdata> IS ASSIGNED.
      <fs_cdsviewdata> = CORRESPONDING #( it_original_data ).

      SELECT tdobject, tdid, namekey, cds_name FROM zww_cat_cdstext
        WHERE cds_name EQ @lv_cdsname INTO TABLE @DATA(lt_cdstext).

      LOOP AT <fs_cdsviewdata> ASSIGNING FIELD-SYMBOL(<fs_cdsrow>).
        LOOP AT lt_cdstext ASSIGNING FIELD-SYMBOL(<fs_cdstext>).
            SPLIT <fs_cdstext>-namekey AT '&' INTO TABLE lt_textname_fields.

            LOOP AT lt_textname_fields ASSIGNING FIELD-SYMBOL(<fs_textname>).
                ASSIGN COMPONENT <fs_textname> OF STRUCTURE <fs_cdstext> TO FIELD-SYMBOL(<fs_tdnamefield>).
                IF <fs_tdnamefield> IS ASSIGNED.
                    lv_tdname = |{ lv_tdname }{ <fs_tdnamefield> }|.
                ENDIF.
            ENDLOOP.
            CLEAR lt_textname_fields.

            CALL FUNCTION 'READ_TEXT'
                EXPORTING
                  client                  = sy-mandt
                  id                      = conv thead-tdid( <fs_cdstext>-tdid )
                  language                = conv thead-tdspras( sy-langu )
                  name                    = conv thead-tdname( lv_tdname )
                  object                  = conv thead-tdobject( <fs_cdstext>-tdobject )
                TABLES
                  lines                   = lt_lines
                EXCEPTIONS
                  id                      = 1
                  language                = 2
                  name                    = 3
                  not_found               = 4
                  object                  = 5
                  reference_check         = 6
                  wrong_access_to_archive = 7
                  OTHERS                  = 8.

             IF sy-subrc NE 0.
                    CALL FUNCTION 'READ_TEXT'
                    EXPORTING
                      client                  = sy-mandt
                      id                      = conv thead-tdid( <fs_cdstext>-tdid )
                      language                = conv thead-tdspras( 'E' )
                      name                    = conv thead-tdname( lv_tdname )
                      object                  = conv thead-tdobject( <fs_cdstext>-tdobject )
                    TABLES
                      lines                   = lt_lines
                    EXCEPTIONS
                      id                      = 1
                      language                = 2
                      name                    = 3
                      not_found               = 4
                      object                  = 5
                      reference_check         = 6
                      wrong_access_to_archive = 7
                      OTHERS                  = 8.

             ENDIF.


             DATA(lv_text) = REDUCE #( INIT wa_string TYPE string FOR wa_tline IN lt_lines NEXT  wa_string = |{ wa_string }{ wa_tline-tdline }| ) .
             DATA(lv_cdstextfield) = |TXT{ <fs_cdstext>-tdobject }{ <fs_cdstext>-tdid }|.

             ASSIGN COMPONENT lv_cdstextfield OF STRUCTURE <fs_cdsrow> TO FIELD-SYMBOL(<fs_txtfield>).
             IF <fs_txtfield> IS NOT INITIAL.
                <fs_txtfield> = lv_text.
             ENDIF.
        ENDLOOP.
      ENDLOOP.

    ct_calculated_data = CORRESPONDING #(  <fs_cdsviewdata> ).

  endmethod.


  method IF_SADL_EXIT_CALC_ELEMENT_READ~GET_CALCULATION_INFO.

  endmethod.
ENDCLASS.
