<h2>ALV</h2>

 - [Dynpro](#Dynpro)
 - [FieldCatalog](#FieldCatalog)
 - [Chiamare ALV](#ALV)
 - [Toolbar](#Toolbar)
 - [Moduli](#Moduli)

## Dynpro

A **dynpro** is a combination of logic and view. It includes modules for managing input (**PAI** - Process After Input) and output (**PBO** - Process Before Output). You can insert custom code into these modules to modify data or the behavior of specific elements as needed. Each dynpro is identified by a 4-digit number and is called from a report using the `CALL SCREEN XXXX` command.

### PAI (Process After Input)

This module handles input data and extracts new data, among other things. You can modify the values displayed on the screen here. To ensure the screen stays updated, insert the method `cl_gui_cfw=>set_new_ok_code( new_code = 'REFR' )` at the end of the module to refresh the display.

If your ALV grid has editable fields, you can intercept changes using the method `lo_alv->check_changed_data( )`.

### PBO (Process Before Output)

In the PBO module, you can hide or display various elements within the dynpro. You can loop through these elements using `LOOP AT SCREEN` to inspect the name (or group) of each displayed element. This applies only to elements created through the dynpro's layout, not those within an ALV grid.

To determine the current status of the dynpro, check the `aktyp` field:

* If **A**: Display mode
* If **H**: Creation mode
* If **V**: Modification mode

To determine if a transaction is in read, write, or modify mode, refer to table `T180`.

---

## Field Catalog

The **field catalog** is a table that defines the properties of the data to be displayed by an ALV grid. In many cases, the field catalog is created dynamically using the table descriptor. Since it retrieves text descriptions from table `dd04t`, it's possible that not all fields will be populated, or you might encounter multiple fields of the same type. In such scenarios, these entries must be added manually.

```abap
value( IS_OUTTAB )	TYPE DATA	 " Riga tabella di output
value( CT_FIELDCAT )	TYPE LVC_T_FCAT	Tabella finale fieldcatalog


      DATA : lo_ref_descr TYPE REF TO cl_abap_structdescr,
            lt_detail    TYPE abap_compdescr_tab,
            ls_detail    LIKE LINE OF lt_detail,
            lr_typedescr TYPE REF TO cl_abap_typedescr,
            lv_counter   TYPE i VALUE 0.

  FIELD-SYMBOLS: <fs_dref>  TYPE any,
                 <fs_fname> TYPE any.

  lo_ref_descr ?= cl_abap_typedescr=>describe_by_data( is_outtab ). "Chiamare metodo statico su una struttura
  lt_detail[] = lo_ref_descr->components.

  LOOP AT lt_detail INTO ls_detail.
    ASSIGN COMPONENT ls_detail-name OF STRUCTURE is_outtab TO FIELD-SYMBOL(<ls_comp>).

    IF <ls_comp> IS ASSIGNED.
      ADD 1 TO lv_counter.
      lr_typedescr = cl_abap_typedescr=>describe_by_data( <ls_comp> ) .

      APPEND VALUE #(
        ref_field = lr_typedescr->absolute_name+6
        fieldname = ls_detail-name
        outputlen = lr_typedescr->length
        col_id    = lv_counter
      ) TO ct_fieldcat.

    ENDIF.
  ENDLOOP.

   SELECT rollname, scrtext_m
    FROM dd04t
    INTO TABLE @DATA(lt_coldescr)
    FOR ALL ENTRIES IN @ct_fieldcat
    WHERE rollname EQ @ct_fieldcat-ref_field
    AND ddlanguage EQ @sy-langu.
    
  LOOP AT ct_fieldcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).
    <fs_fcat>-scrtext_m = VALUE #( lt_coldescr[ rollname = <fs_fcat>-ref_field ]-scrtext_m OPTIONAL ).

    LOOP AT it_custom_fc ASSIGNING FIELD-SYMBOL(<fs_custom_fc>) WHERE fieldname EQ <fs_fcat>-fieldname.
      TRANSLATE <fs_custom_fc> TO LOWER CASE.
      ASSIGN COMPONENT <fs_custom_fc>-fc_component OF STRUCTURE <fs_fcat> TO FIELD-SYMBOL(<fs_comp>).
      IF sy-subrc EQ 0.
        <fs_comp> =  CONV #( <fs_custom_fc>-value ).
      ENDIF.
    ENDLOOP.

  ENDLOOP.
```

### ALV (ABAP List Viewer)

An ALV is an output method used to display tables or data on the screen after processing. Various classes exist (e.g., ALV, SALV), but they generally function in a similar way. To use an ALV, you need a **container** (which defines the ALV's display area on the screen), a **field catalog** (which describes the table to be shown), and a **layout** (which holds optional components like color, row selection type, and more).
Before generating the ALV, the **field catalog** must be created (refer to its dedicated page for details).
Insert the "Custom Control" component into the Dynpro used to display the ALV, giving it the same name as your container (in this example, "CONT").

```abap
DATA: r_cont TYPE REF TO cl_gui_custom_container,
      r_alv  TYPE REF TO cl_gui_alv_grid.
      
DATA: it_fcat TYPE TABLE OF lvc_s_fcat,
      wa_fcat LIKE LINE OF it_fcat.
 
 
" Container custom
* CREATE OBJECT r_cont
*   EXPORTING
*     container_name = 'CONT'.

* CREATE OBJECT r_alv
*   EXPORTING
*     i_parent = r_cont.
"

" Container std -> si adatta allo schermo di chi usa l'alv
 CREATE OBJECT r_alv
    EXPORTING i_parent = cl_gui_container=>default_screen

  CALL METHOD r_alv->set_table_for_first_display
    EXPORTING
      is_layout                     = wa_layo
    CHANGING
      it_fieldcatalog               = it_fcat
      it_outtab                     = lt_s_oda
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc <> 0.

  ENDIF.
```
## ALV Events

An ALV contains **Events** that can be implemented in the local program. These events must be implemented through a **Handler method**.

```abap
*lo_event is a event class
CREATE OBJECT lo_event.
SET HANDLER lo_event->handle_hotspot_click FOR r_alv.
```
## Displaying Multiple ALVs

Multiple ALVs can be displayed on the same screen using a splitter container.

```abap
* Create the container that defines the print extension on the screen
  CREATE OBJECT lo_cont_docking
    EXPORTING
      parent = cl_gui_container=>screen0
      ratio  = 95
    EXCEPTIONS
      OTHERS = 6.

* Set the extension of the print area on the screen
  CALL METHOD lo_cont_docking->set_extension
    EXPORTING
      extension  = 99999
    EXCEPTIONS
      cntl_error = 1
      OTHERS     = 2.

* Create the first split container with the docking container as parent. Set three rows for the three ALVs used
    CREATE OBJECT lo_split_co
      EXPORTING
        parent  = lo_cont_docking
        rows    = 3
        columns = 1
        align   = 15.

* Assign the container graphic_parent_hd -> header to the first row of the split container
    CALL METHOD lo_split_co->get_container
      EXPORTING
        row        = 1
        column     = 1
      RECEIVING
        container = graphic_parent_hd.

    CALL METHOD lo_split_co->set_row_height
      EXPORTING
        id          = 1
        height      = 4
      .
* Assign the container graphic_parent1 -> Coil to the first row of the split container
    CALL METHOD lo_split_co->get_container
      EXPORTING
        row        = 2
        column     = 1
      RECEIVING
        container = graphic_parent1.

* Assign the container graphic_parent2 -> Orders to the first row of the split container
    CALL METHOD lo_split_co->get_container
      EXPORTING
        row        = 3
        column     = 1
      RECEIVING
        container = graphic_parent2.

* Create the ALV for coils with graphic_parent1 as parent
    CREATE OBJECT lo_alv_up
      EXPORTING
        i_parent = graphic_parent1.

* Create the ALV for orders with graphic_parent2 as parent
    CREATE OBJECT lo_alv_dw
      EXPORTING
        i_parent = graphic_parent2.

* Set an action handler for the two ALVs that calls functions based on the method called
    SET HANDLER me->handle_user_command FOR lo_alv_up.
    SET HANDLER me->handle_user_command FOR lo_alv_dw.

    wa_layout_1-cwidth_opt    = 'X'.
    wa_layout_2-cwidth_opt  = 'X'.
    wa_layout_1-sel_mode     = 'D'.
    wa_layout_2-sel_mode    = 'D'.
    wa_layout_2-info_fname  = 'ROWCOLOR'.
    wa_layout_1-info_fname   = 'ROWCOLOR'.

    CALL METHOD lo_alv_up->set_table_for_first_display
      EXPORTING
        is_layout             = wa_layout_1
        is_variant            = lv_repname
        i_save                = 'A'
      CHANGING
        it_fieldcatalog       = it_fcat_1[]
        it_outtab             = out_grid_1
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error         = 2
        too_many_lines        = 3
        OTHERS                = 4.
    IF sy-subrc <> 0.
    ENDIF.

    CALL METHOD lo_alv_dw->set_table_for_first_display
      EXPORTING
        is_layout             = wa_layout_2
        is_variant            = lv_repnam2
        i_save                = 'A'
      CHANGING
        it_fieldcatalog       = it_fcat_2
        it_outtab             = out_grid_2
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error         = 2
        too_many_lines        = 3
        OTHERS                = 4.
    IF sy-subrc <> 0.
    ENDIF.
```

To create a header above a table, use the splitter to create two containers. Then insert the text:

```abap
DATA:  lo_doc_header         TYPE REF TO cl_dd_document.
  lo_doc_header->initialize_document( ).

  lo_doc_header->add_text( text =  'Legend' ).
  lo_doc_header->new_line( ).
  lo_doc_header->add_text( text =  'Row 1' ).
  lo_doc_header->add_gap( ). " Tab
  lo_doc_header->add_text( text =  'Row 1' ).


  lo_doc_header->merge_document( ).
  lo_doc_header->display_document( parent = lo_cont_up ).
```

### Modules 
When calling an ALV from a report, you need to use the modules of the dynpro being called. The modules are INPUT and OUTPUT.
The first handles buttons, the second the display of fields.

``` abap
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STAT100'.
  SET TITLEBAR 'Test'.

  ... r_alv->set_table_for_first_display ...
ENDMODULE.

MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
```

<h1>Cell Formatting</h1>      

 ### Dropdown values    
       
``` abap    
" Fieldcatalog Setting
fieldname = 'zzfieldname'
drdn_hndl = '1'
drdn_alias = 'X'

" Value Setting
ls_dropdown-handle = '1'.
ls_dropdown-value = 'G'.
APPEND ls_dropdown TO lt_dropdown.

ls_dropdown-handle = '1'.
ls_dropdown-value = 'H'.
APPEND ls_dropdown TO lt_dropdown.

ls_dropdown-handle = '1'.
ls_dropdown-value = 'I'.
APPEND ls_dropdown TO lt_dropdown.

ls_dropdown-handle = '1'.
ls_dropdown-value = 'M'.
APPEND ls_dropdown TO lt_dropdown.

CALL METHOD go_grid_ctrl->set_drop_down_table
  EXPORTING
    it_drop_down = lt_dropdown.
``` 
