INTERFACE zif_alv_event_handler PUBLIC.
  METHODS on_toolbar
    IMPORTING e_object      TYPE REF TO cl_alv_event_toolbar_set
              e_interactive TYPE char01.

  METHODS on_user_command
    IMPORTING e_ucomm TYPE sy-ucomm
              it_rows TYPE lvc_t_row.

  METHODS on_double_click
    IMPORTING e_row    TYPE lvc_s_row
              e_column TYPE lvc_s_col.

  METHODS on_hotspot_click
    IMPORTING e_row_id    TYPE lvc_s_row
              e_column_id TYPE lvc_s_col.

  METHODS on_data_changed
    IMPORTING er_data_changed TYPE REF TO cl_alv_changed_data_protocol.
ENDINTERFACE.
