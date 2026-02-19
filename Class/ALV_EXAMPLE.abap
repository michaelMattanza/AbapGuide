REPORT z_test_alv_manager.

*--- Data Definition
DATA: gt_mara TYPE STANDARD TABLE OF mara.

*--- Local Event Handler Implementation
CLASS lcl_events DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_alv_event_handler.
ENDCLASS.

CLASS lcl_events IMPLEMENTATION.
  METHOD zif_alv_event_handler~on_user_command.
    " Handle custom buttons here
  ENDMETHOD.

  METHOD zif_alv_event_handler~on_toolbar.     ENDMETHOD.
  METHOD zif_alv_event_handler~on_double_click. ENDMETHOD.
  METHOD zif_alv_event_handler~on_hotspot_click. ENDMETHOD.
  METHOD zif_alv_event_handler~on_data_changed.  ENDMETHOD.
ENDCLASS.

*--- Main Execution
START-OF-SELECTION.

  " 1. Fetch exactly two records
  SELECT * FROM mara UP TO 2 ROWS INTO TABLE gt_mara.

  " 2. Get the Manager Instance
  DATA(lo_alv) = zcl_alv_manager=>get_instance(
    EXPORTING iv_program_name = sy-repid
    CHANGING  ct_outtab       = gt_mara
  ).

  " 3. Optional: Set a Title and Status
  lo_alv->set_status_and_title( iv_title = 'Displaying MARA Data' ).

  " 4. Optional: Hook the event logic
  lo_alv->set_event_handler( NEW lcl_events( ) ).

  " 5. Display (The Manager creates the grid and handles the screen automatically)
  lo_alv->display_data( ).

  " 6. Enter the screen loop (Required to keep the GUI alive)
  WRITE: / 'Loading...'. " This is just a fallback; in a real app, you'd use a Screen
  CALL SCREEN 0100.

*--- Screen PBO
MODULE status_0100 OUTPUT.
  " If you are using a real screen, just call display_data here
  lo_alv->display_data( ).
ENDMODULE.
