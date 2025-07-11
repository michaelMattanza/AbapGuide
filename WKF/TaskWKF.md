# SAP Workflows

**Workflows** are a series of steps performed to accomplish a complex action (document, information, etc.).
The first step is to create a method for each step the workflow needs to perform. It's recommended to create a class for each workflow so that each implementation is self-contained.
In the class, insert an **event** that will be the workflow's trigger, along with the parameters to be passed to the workflow's container.

Once the methods are created, go to *PFTC* and create a **Workflow Task (TS)** to link the created method.
Then select the object category **Class**, link the created method, perform the **binding of the container** with the method's parameters, and finally check the "Synchronous object method" option.

After creating a task for each workflow step, create a new **Workflow Model** (also from *PFTC*, type WS).
In the workflow trigger section, insert the class and event mentioned above.
Then go to the **workflow builder** section and add the activity node: indicate in the Task field: **TS + created task number**. Caution: if the block node is used, specific parameters must be passed, otherwise the workflow will be interrupted.
Then set the passing data by clicking on the "Data Flow" button, creating the variables you need to work with in the workflow container.

---

**Calling a Workflow**

Create a method in the class that raises the workflow.

```abap
  method RAISE_WKF.
    " WS 900000000
      CONSTANTS: lc_objtype TYPE sibftypeid VALUE '', " Class that contains the workflow event
                 lc_event   TYPE sibfevent  VALUE ''. " Name of the event that starts the workflow

    DATA: lv_param_name       TYPE swfdname,
          lv_objkey           TYPE sweinstcou-objkey,
          lref_event_parameters TYPE REF TO if_swf_ifs_parameter_container.

    TRY.
        CALL METHOD cl_swf_evt_event=>get_event_container
          EXPORTING
            im_objcateg  = cl_swf_evt_event=>mc_objcateg_cl
            im_objtype   = lc_objtype
            im_event     = lc_event
          RECEIVING
            re_reference = lref_event_parameters. " Parameters of the event to be called

        lref_event_parameters->set( EXPORTING name = 'CT_MSEG' value = it_mseg ).

        CALL METHOD cl_swf_evt_event=>raise_in_update_task
          EXPORTING
            im_objcateg        = cl_swf_evt_event=>mc_objcateg_cl
            im_objtype         = lc_objtype
            im_event           = lc_event
            im_objkey          = lv_objkey
            im_event_container = lref_event_parameters.

      CATCH cx_swf_evt_invalid_objtype .
      CATCH cx_swf_evt_invalid_event .
      CATCH cx_swf_cnt_cont_access_denied.
      CATCH cx_swf_cnt_elem_access_denied.
      CATCH cx_swf_cnt_elem_not_found.
      CATCH cx_swf_cnt_elem_type_conflict.
      CATCH cx_swf_cnt_unit_type_conflict.
      CATCH cx_swf_cnt_elem_def_invalid.
      CATCH cx_swf_cnt_container.

    ENDTRY.
    COMMIT WORK.
  endmethod.