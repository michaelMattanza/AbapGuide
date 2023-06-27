class ZWW_CAT_CL_DELIVERY definition
  public
  final
  create public .

public section.

  class-methods CREATE_INBOUND_DEL_FROM_PO
    importing
      !VBSK_I type VBSK
      !NO_COMMIT type XFELD optional
      !IF_SYNCHRON type XFELD default 'X'
      !IS_CONTROL type LESHP_DELIVERY_PROC_CONTROL_IN optional
      !XKOMDLGN type SHP_KOMDLGN_T
    exporting
      !VBSK_E type VBSK
      !XVBLS type SHP_VBLS_T
      !XVBFS type SHP_VBFS_T .

   class-methods create_attachment_inb_del
    importing
    !iv_vbeln TYPE vbeln
    !iv_filename TYPE string
    !iv_regio TYPE sofd-folrg
    !it_attachment TYPE soli_tab
    RETURNING VALUE(rv_msg) TYPE bapiret2.

*    class-methods create_vl31_bdc.
protected section.
private section.
ENDCLASS.



CLASS ZWW_CAT_CL_DELIVERY IMPLEMENTATION.


  method CREATE_INBOUND_DEL_FROM_PO.

     CALL FUNCTION 'GN_DELIVERY_CREATE'
        EXPORTING
          vbsk_i   = vbsk_i
          vbls_pos_rueck = 'X'
          if_no_deque = 'X'
          is_borgr_control = VALUE borgr_control( no_init_text = 'X' )
        IMPORTING
          vbsk_e   = vbsk_e
        TABLES
          xkomdlgn = xkomdlgn
          xvbfs    = xvbfs
          xvbls    = xvbls
        EXCEPTIONS
          OTHERS   = 1.
  endmethod.
  METHOD CREATE_ATTACHMENT_INB_DEL.
    DATA: lt_objhead  TYPE TABLE OF soli,
           ls_obj_data TYPE sood1,
           ls_folder_id   TYPE soodk,
           ls_folder_cont TYPE sofmk,
           ls_object_id   TYPE soodk,
           ls_rolea       TYPE borident,
           ls_roleb       TYPE borident.

    CHECK it_attachment IS NOT INITIAL.
    DATA(lt_content) = it_attachment.

    CALL FUNCTION 'SO_CONVERT_CONTENTS_BIN'
        EXPORTING
          it_contents_bin = lt_content
        IMPORTING
          et_contents_bin = lt_content.

      ls_obj_data-objsns   = 'O'.
      ls_obj_data-objla    = sy-langu.
      ls_obj_data-objdes   = iv_filename.
      SPLIT iv_filename AT '.' INTO ls_obj_data-objdes ls_obj_data-file_ext.
      ls_obj_data-objlen   = lines( lt_content ) * 255.
      CONDENSE ls_obj_data-objlen.

       CALL FUNCTION 'SO_FOLDER_ROOT_ID_GET'
        EXPORTING
          region                = iv_regio
        IMPORTING
          folder_id             = ls_folder_id
        EXCEPTIONS
          communication_failure = 1
          owner_not_exist       = 2
          system_failure        = 3
          x_error               = 4
          OTHERS                = 5.


    APPEND VALUE #( line =  |&SO_FILENAME={ iv_filename }|  ) TO lt_objhead.

      CALL FUNCTION 'SO_OBJECT_INSERT'
        EXPORTING
          folder_id             = ls_folder_id
          object_type           = 'EXT'
          object_hd_change      = ls_obj_data
        IMPORTING
          object_id             = ls_object_id
        TABLES
          objhead               = lt_objhead
          objcont               = lt_content
        EXCEPTIONS
          active_user_not_exist = 35
          folder_not_exist      = 6
          object_type_not_exist = 17
          owner_not_exist       = 22
          parameter_error       = 23
          OTHERS                = 1000.

        ls_folder_cont-foltp = ls_folder_id-objtp.
        ls_folder_cont-folyr = ls_folder_id-objyr.
        ls_folder_cont-folno = ls_folder_id-objno.
        ls_folder_cont-doctp = ls_object_id-objtp.
        ls_folder_cont-docyr = ls_object_id-objyr.
        ls_folder_cont-docno = ls_object_id-objno.

        ls_rolea-objkey  = iv_vbeln.
        ls_rolea-objtype = 'BUS2015'.

        ls_roleb-objtype = 'MESSAGE'.
        ls_roleb-objkey  = ls_folder_cont.

        CALL FUNCTION 'BINARY_RELATION_CREATE_COMMIT'
        EXPORTING
          obj_rolea    = ls_rolea
          obj_roleb    = ls_roleb
          relationtype = 'ATTA'
        EXCEPTIONS
          OTHERS       = 1.


  ENDMETHOD.

ENDCLASS.
