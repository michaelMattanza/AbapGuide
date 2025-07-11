**Interface**

In the **interface**, variables are declared for passing data between the report and the form. Here, variables can be further processed (though it's generally not recommended) to pass data different from what originated in the report.

---

**Form**

The **PDF form** contains the graphical layout which, through modules and objects, allows for the creation of a document organized with variables received from the interface. The variables are then connected to the objects through **binding**.
Scripts in JavaScript and FormCalc can be inserted into the form (strongly discouraged, as they function poorly and are difficult to manage). In the section for passing variables from the interface to the form, unused variables can be deactivated. It's also possible to use specific "objects" (like the address object) for specific formatting of certain information.
* Logo
  * https://blogs.sap.com/2014/06/09/how-to-place-an-se78-image-on-an-adobe-form/

When adding the logo, set the MIME type to *'IMAGE/BMP'*.

The most commonly used script is one that allows hiding fields if they are not populated. Position yourself on the field to be conditioned and set the script's event to *Initialize* with the language *FormCalc* executed on the *Client:*

If the print output doesn't start but no dump is created, follow the steps in the link below to debug the print:
* https://blogs.sap.com/2017/11/29/usage-and-system-error-in-sap-adobe-forms/

---

**Script Examples**

FormCalc scripts are usually executed on the *Initialize* event, run on the *Server*.
The JavaScript scripts shown here, however, are used on the *Form:ready* event, run on the *Client*.

### FormCalc

```FormCalc
if ($ eq null) then
$.presence = "hidden"
endif
```

Javascript
```Javascript
if( this.rawValue == "" ){
	this.presence = "hidden";
}
```

Setting the condition on the module containing the data:

FormCalc
```FormCalc
/*Script on the field*/
if ($ eq null) then
$.parent.presence = "hidden"
endif

/*Script on the parent module*/
if($.ZZFLAG == "X") then
	$.presence = "hidden"
endif
```

Javascript
```Javascript
/*Script on the field*/
if( this.rawValue == "" ){
	zzmodule.presence = "hidden";
}
```
    
**Report**</br>
The report contains the code for data extraction and processing. Once the data is processed, the open and close job functions are called.

 ```abap
  DATA: lv_fm_name   TYPE rs38l_fnam,
        ls_outpar    TYPE sfpoutputparams,
        lv_xstring   TYPE xstring.
        
    " Get the logo for the label
    CALL METHOD cl_ssf_xsf_utilities=>get_bds_graphic_as_bmp
      EXPORTING
        p_object       = 'GRAPHICS'
        p_name         = 'ZF_HEADER'
        p_id           = 'BMAP'
        p_btype        = 'BCOL'
      RECEIVING
        p_bmp          = lv_xstring
      EXCEPTIONS
        not_found      = 1
        internal_error = 2
        OTHERS         = 3.
        
    " Try to read the function module name
    TRY .
        CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
          EXPORTING
            i_name     = 'ZFM_NAME'
          IMPORTING
            e_funcname = lv_fm_name.

    ENDTRY.

    CALL FUNCTION 'FP_JOB_OPEN'
      CHANGING
        ie_outputparams = ls_outpar
      EXCEPTIONS
        cancel          = 1
        usage_error     = 2
        system_error    = 3
        internal_error  = 4
        OTHERS          = 5.

    CALL FUNCTION lv_fm_name
      EXPORTING
        is_printdata   = ls_printdata " Structure to pass
        ix_logo        = lv_xstring " Logo to pass
      EXCEPTIONS
        usage_error    = 1
        system_error   = 2
        internal_error = 3
        OTHERS         = 4.


    CALL FUNCTION 'FP_JOB_CLOSE'
      EXCEPTIONS
        usage_error    = 1
        system_error   = 2
        internal_error = 3
        OTHERS         = 4.
 ```
 
The print is triggered with a specific message. The various data for this message are contained in the structure <i>NAST</i>.


**Sending a print via email (N PDF attachments)**

 ```abap
 DATA:
    lv_fm_name      TYPE rs38l_fnam,
    ls_output       TYPE fpformoutput,
    lo_pdf_content  TYPE solix_tab,

    " Gestione mail
    lo_send_request TYPE REF TO cl_bcs,
    lo_document     TYPE REF TO cl_document_bcs,
    lo_recipient    TYPE REF TO if_recipient_bcs,

    lv_sent_to_all  TYPE os_boolean,
    lv_image_xstring TYPE xstring,
    lx_document_bcs TYPE REF TO cx_document_bcs VALUE IS INITIAL.
    
    DATA(ls_outpar) = VALUE sfpoutputparams( nopreview = 'X' getpdf = 'X' nodialog  = 'X' dest = 'LP01' ).

    CALL FUNCTION 'FP_JOB_OPEN'
      CHANGING
        ie_outputparams = ls_outpar
      EXCEPTIONS
        cancel          = 1
        usage_error     = 2
        system_error    = 3
        internal_error  = 4
        OTHERS          = 5.

    CALL FUNCTION lv_fm_name
      EXPORTING
        is_printdata       = ls_print_data
      IMPORTING
        /1bcdwb/formoutput = ls_output
      EXCEPTIONS
        usage_error        = 1
        system_error       = 2
        internal_error     = 3
        OTHERS             = 4.


    TRY.
        " Attach print
        lo_send_request = cl_bcs=>create_persistent( ).
        lo_pdf_content = cl_bcs_convert=>xstring_to_solix( iv_xstring = ls_output-pdf ).

        DATA(lv_size) = CONV so_obj_len( xstrlen( ls_output-pdf ) ).

        lo_document = cl_document_bcs=>create_document(
                        i_type    = CONV so_obj_tp('PDF')
                        i_hex    = lo_pdf_content
                        i_length = lv_size
                        i_subject = CONV so_obj_des( |Mail Object| )
                   ).

        lo_send_request->set_document( lo_document ).
        lo_recipient = cl_cam_address_bcs=>create_internet_address(
             i_address_string = |{ <fs_partner>-smtp_address }|
        ).
        lo_send_request->add_recipient( i_recipient = lo_recipient ).

        lv_sent_to_all = lo_send_request->send(
          i_with_error_screen = 'X'
         ).

        COMMIT WORK.

      CATCH cx_root INTO DATA(lx_root_exception).

        MESSAGE ID   sy-msgid
              TYPE   sy-msgty
              NUMBER sy-msgno
              WITH   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    ENDTRY.

    CALL FUNCTION 'FP_JOB_CLOSE'
      EXCEPTIONS
        usage_error    = 1
        system_error   = 2
        internal_error = 3
        OTHERS         = 4.
 ```
