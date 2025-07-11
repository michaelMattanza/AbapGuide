# Mail

To generate an email in ABAP, which can then be viewed in **SOST**, you need to rely on standard classes. In this example, a PDF file is generated and then sent by email to a distribution list.

```abap
DATA:
  lo_send_request TYPE REF TO cl_bcs,
  lv_sender       TYPE REF TO cl_cam_address_bcs,
  lo_pdf_content  TYPE solix_tab,
  lo_document     TYPE REF TO cl_document_bcs,
  lt_bodymail     TYPE bcsy_text,
  lv_mail         TYPE string,
  lv_sent_to_all  TYPE os_boolean,
  lo_recipient    TYPE REF TO if_recipient_bcs.

TRY.
  DATA(lv_size) = CONV so_obj_len( xstrlen( lv_fpformoutput-pdf ) ).
  " instance of class for mail management
  lo_send_request = cl_bcs=>create_persistent( ).

  "instance of class for sender management
  lv_sender = cl_cam_address_bcs=>create_internet_address(
    i_address_string = CONV adr6-smtp_addr('owner_address')
    i_address_name = CONV ADR6-SMTP_ADDR('sender_mail_address')
    ).

  "conversion from xstring to format required for attachment sending
  lo_pdf_content = cl_document_bcs=>xstring_to_solix( 'pdf_content' ).

  "body creation

  APPEND 'Body of the mail' TO lt_bodymail.

  "mail document creation
  lo_document = cl_document_bcs=>create_document(
    i_type        = 'RAW' "important for correct body display
    i_sensitivity = 'P'
    i_text        = lt_bodymail
    i_sender      = lv_sender
    i_subject     = CONV so_obj_des('Subject of the mail')
    ).

  lv_size = CONV so_obj_len( xstrlen( pdf_xstring ) ).

  "mail attachment handling, in this case PDF
  lo_document->add_attachment(
    EXPORTING
      i_attachment_type   = CONV so_obj_tp('PDF')
      i_attachment_subject = CONV so_obj_des('Attachment Name')
      i_attachment_size   = lv_size
      i_att_content_hex   = lt_solix
  ).

  "the document is set to the request
  lo_send_request->set_document( lo_document ).

  "the sender is set
  lo_send_request->set_sender(
    EXPORTING
      i_sender = lv_sender
    ).
  "the recipient is set
  lo_recipient = cl_cam_address_bcs=>create_internet_address(
    i_address_string = |{ lv_mail }|
  ).

  "the recipient is added to the request
  lo_send_request->add_recipient( i_recipient = lo_recipient ).

  "the request is placed in SOST
  lv_sent_to_all = lo_send_request->send(
    i_with_error_screen = 'X'
  ).

  COMMIT WORK.

CATCH cx_document_bcs INTO lx_document_bcs.
ENDTRY.