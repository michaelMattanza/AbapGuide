```abap
CLASS zreg_cl_excel_manager DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS: lc_value_separator VALUE ';'.
    TYPES:
      BEGIN OF ty_excel_row,
        index TYPE i,
        row   TYPE string,
      END OF ty_excel_row.
    TYPES:
      tty_excel_row TYPE TABLE OF ty_excel_row.

    CLASS-METHODS create_excel_from_table
      EXPORTING
        VALUE(rv_bin_file) TYPE xstring
      CHANGING
        !ct_table          TYPE any .

    CLASS-METHODS create_table_from_excel
      EXPORTING
        !et_excel_tab TYPE tty_excel_row
      CHANGING
        !cv_excel     TYPE xstring.

    CLASS-METHODS get_excel_from_al11
        IMPORTING
            !iv_file_path TYPE string
        EXPORTING
            !ev_excel     TYPE xstring
            !et_excel_tab TYPE tty_excel_row.
    CLASS-METHODS upload_excel
      IMPORTING
        !iv_file_path  TYPE string OPTIONAL
        !iv_excel_data TYPE xstring OPTIONAL
      EXPORTING
        !et_excel      TYPE tty_excel_row.

    CLASS-METHODS download_excel
        IMPORTING
        !iv_path TYPE string OPTIONAL
        !iv_excel_data TYPE xstring.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zreg_cl_excel_manager IMPLEMENTATION.

  METHOD create_excel_from_table.
    DATA(lref_excel_data) = REF #( ct_table ).

    ASSIGN lref_excel_data->* TO FIELD-SYMBOL(<fs_excel_data>).

    TRY.
        cl_salv_table=>factory(
        EXPORTING
          list_display = abap_false
        IMPORTING
          r_salv_table = DATA(lt_salv_table)
        CHANGING
          t_table      = <fs_excel_data> ).

        DATA(lt_fcat) = cl_salv_controller_metadata=>get_lvc_fieldcatalog(
                                 r_columns      = lt_salv_table->get_columns( )
                                 r_aggregations = lt_salv_table->get_aggregations( )
                              ).
      CATCH cx_salv_msg.
        RETURN.
    ENDTRY.

    cl_salv_bs_lex=>export_from_result_data_table(
   EXPORTING
     is_format            = if_salv_bs_lex_format=>mc_format_xlsx
     ir_result_data_table =  cl_salv_ex_util=>factory_result_data_table(
                                             r_data                      = lref_excel_data
*                                                s_layout                    = is_layout
                                             t_fieldcatalog              = lt_fcat
*                                                t_sort                      = it_sort
*                                                t_filter                    = it_filt
*                                                t_hyperlinks                = it_hyperlinks
                                             )
   IMPORTING
     er_result_file       = rv_bin_file ).
  ENDMETHOD.

  METHOD create_table_from_excel.

    DATA: lt_string_rows TYPE TABLE OF string,
          lt_rows        TYPE STANDARD TABLE OF string,
          lt_detail      TYPE abap_compdescr_tab,

          lref_data      TYPE REF TO data,

          lo_ref_descr   TYPE REF TO cl_abap_structdescr.

    FIELD-SYMBOLS : <gt_data>       TYPE STANDARD TABLE .

    DATA(lv_doc_name) = 'Data'.
    TRY .
        DATA(lo_excel_ref) = NEW cl_fdt_xl_spreadsheet(
                                document_name = CONV #( lv_doc_name )
                                xdocument     = cv_excel ) .
      CATCH cx_fdt_excel_core.
        "Implement suitable error handling here
    ENDTRY .

    lo_excel_ref->if_fdt_doc_spreadsheet~get_worksheet_names(
      IMPORTING
        worksheet_names = DATA(lt_worksheets) ).

    IF NOT lt_worksheets IS INITIAL.
      READ TABLE lt_worksheets INTO DATA(lv_woksheetname) INDEX 1.

      DATA(lo_data_ref) = lo_excel_ref->if_fdt_doc_spreadsheet~get_itab_from_worksheet( lv_woksheetname ).
      "now you have excel work sheet data in dyanmic internal table
      ASSIGN lo_data_ref->* TO <gt_data>.
    ENDIF.

    CHECK <gt_data> IS ASSIGNED.
    READ TABLE <gt_data> INDEX 1 ASSIGNING FIELD-SYMBOL(<fs_row>).

    lo_ref_descr ?= cl_abap_typedescr=>describe_by_data( <fs_row> ). "Chiamare metodo statico su una struttura
    CREATE DATA lref_data TYPE HANDLE lo_ref_descr.
    lt_detail[] = lo_ref_descr->components.


    LOOP AT <gt_data> ASSIGNING <fs_row>.
      APPEND VALUE #( ) TO et_excel_tab ASSIGNING FIELD-SYMBOL(<fs_rows_to_add>).
      LOOP AT lt_detail ASSIGNING FIELD-SYMBOL(<fs_detail>).
        ASSIGN COMPONENT <fs_detail>-name OF STRUCTURE <fs_row> TO FIELD-SYMBOL(<fs_component>).
        <fs_rows_to_add>-row = |{ <fs_rows_to_add>-row }{ <fs_component> };|.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.

  METHOD upload_excel.
    DATA(lo_xlsxhandler) = cl_ehfnd_xlsx=>get_instance( ).
    CHECK lo_xlsxhandler IS NOT INITIAL.

    TRY.
        DATA(lv_xstring_excel) = cl_openxml_helper=>load_local_file( CONV #( iv_file_path ) ).
      CATCH cx_openxml_not_found INTO DATA(openxml_not_found).

    ENDTRY.

    TRY.
        DATA(lo_xlsxdocument) = lo_xlsxhandler->load_doc( iv_file_data = lv_xstring_excel ).
      CATCH cx_openxml_format INTO DATA(openxml_format).

      CATCH cx_openxml_not_allowed INTO DATA(openxml_not_allowed).
        EXIT.
      CATCH cx_dynamic_check INTO DATA(dynamic_check).

    ENDTRY.

    TRY.
        DATA(lo_firstsheet) = lo_xlsxdocument->get_sheet_by_id( iv_sheet_id = 1 ).
      CATCH cx_openxml_format  INTO openxml_format.

      CATCH cx_openxml_not_found  INTO openxml_not_found.

      CATCH cx_dynamic_check  INTO dynamic_check.

    ENDTRY.

    CHECK lo_firstsheet IS NOT INITIAL.

    DATA(lv_columncount) = lo_firstsheet->get_last_column_number_in_row( 1 ).
    DATA(lv_rowcount) = lo_firstsheet->get_last_row_number( ).

    DATA(lv_index_row) = 0.
    DATA(lv_index_col) = 0.

    DO lv_rowcount  TIMES.
      lv_index_row += 1.
      DATA(lv_cellvalue) = lo_firstsheet->get_cell_content(
                          EXPORTING
                                iv_row     = lv_index_row
                                iv_column  = 1
                            ).
      IF lv_cellvalue IS INITIAL.
        CONTINUE.
      ENDIF.

      APPEND VALUE #( index = lv_index_row ) TO et_excel ASSIGNING FIELD-SYMBOL(<fs_excel_row>).
      DO lv_columncount TIMES.
        lv_index_col += 1.

        lv_cellvalue = lo_firstsheet->get_cell_content(
                        EXPORTING
                          iv_row     = lv_index_row
                          iv_column  = lv_index_col ).

        IF lv_index_col EQ 1 AND lv_cellvalue IS INITIAL.
          EXIT.
        ENDIF.

        <fs_excel_row>-row = |{ <fs_excel_row>-row }{ lv_cellvalue };|.

      ENDDO.

      CLEAR lv_index_col.
    ENDDO.

  ENDMETHOD.

  METHOD get_excel_from_al11.
    TYPES:BEGIN OF ty_final,
          var1 TYPE string,
        END OF ty_final.

    DATA: lt_file        TYPE TABLE OF ty_final,
          lt_values_rows TYPE TABLE OF string,
          ls_file        TYPE ty_final,
          lv_index       TYPE i VALUE 0,
          lv_new_row TYPE string.

    DATA(lv_file_path) = iv_file_path.
    CHECK lv_file_path IS NOT INITIAL.

    OPEN DATASET iv_file_path FOR INPUT IN TEXT MODE ENCODING DEFAULT.
    IF sy-subrc EQ 0.
      DO.
        READ DATASET lv_file_path INTO ls_file-var1.
        IF sy-subrc EQ 0.
          APPEND ls_file TO lt_file.
        ELSE.
          EXIT.
        ENDIF.
        CLEAR ls_file-var1.
      ENDDO.

      CLOSE DATASET lv_file_path.
    ENDIF.


    LOOP AT lt_file INTO ls_file.
      SPLIT ls_file-var1 AT lc_value_separator INTO TABLE lt_values_rows.

      LOOP AT lt_values_rows INTO DATA(ls_value_spec).
        lv_new_row = |{ lv_new_row }{ ls_value_spec };|.
      ENDLOOP.

      APPEND VALUE #( index = lv_index row = lv_new_row ) TO et_excel_tab.
      lv_index += 1.
      CLEAR lv_new_row.

    ENDLOOP.

  ENDMETHOD.

  METHOD download_excel.
  TYPES: BEGIN OF ty_data,
            line TYPE x LENGTH 255,
         END OF ty_data.
    DATA(lv_xstring) = iv_excel_data.
    DATA(lv_filename) = iv_path.

    IF lv_filename IS INITIAL.
        lv_filename = 'C:\tmp\download.xlsx'.
    ENDIF.

    DATA : lt_itab TYPE TABLE OF ty_data,
           lv_filesize TYPE i.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
    EXPORTING
      buffer        = lv_xstring
    IMPORTING
      output_length = lv_filesize
    TABLES
      binary_tab    = lt_itab.

    CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      bin_filesize = lv_filesize
      filename     = lv_filename
      filetype     = 'BIN'
    TABLES
      data_tab     = lt_itab
    EXCEPTIONS
      OTHERS       = 1.

  ENDMETHOD.

ENDCLASS.
```
