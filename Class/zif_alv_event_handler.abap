
INTERFACE zif_alv_event_handler PUBLIC.
  METHODS on_toolbar
    IMPORTING
      e_object       TYPE REF TO cl_alv_event_toolbar_set
      e_interactive  TYPE abap_bool .

  METHODS on_user_command
    IMPORTING
      e_ucomm        TYPE syucomm
      it_rows        TYPE lvc_t_row.

  METHODS on_hotspot_click
    IMPORTING
      e_row_id       TYPE lvc_s_row
      e_column_id    TYPE lvc_s_col
      es_row_no      TYPE lvc_s_roid.

  METHODS on_double_click
    IMPORTING
      e_row          TYPE lvc_s_row
      e_column       TYPE lvc_s_col.

  METHODS on_data_changed
    IMPORTING
      er_data_changed TYPE REF TO cl_alv_changed_data_protocol.

  METHODS on_data_changed_finished
    IMPORTING
      e_modified     TYPE abap_bool
      et_good_cells  TYPE lvc_t_modi.

  METHODS on_f4
    IMPORTING
      e_fieldname     TYPE lvc_fname
      es_row_no       TYPE lvc_s_roid
      er_event_data   TYPE REF TO cl_alv_event_data.
ENDINTERFACE.
