<h1>Mail</h1>   
Per generare una mail in abap, visibile poi nella SOST, ci si deve affidare a delle classi standard.
In questo esempio viene generato un file PDF e poi spedito per mail ad una distribution list.   

```abap
  DATA:
    lo_send_request TYPE REF TO cl_bcs,
	lv_sender       TYPE REF TO cl_cam_address_bcs,
	lo_pdf_content  TYPE solix_tab,
	lo_document     TYPE REF TO cl_document_bcs,
	lt_bodymail     TYPE bcsy_text,
	lv_mail         TYPE string,
	lv_sent_to_all  TYPE os_boolean,
	lo_recipient    TYPE REF TO if_recipient_bcs,
  
  TRY.
	DATA(lv_size) = CONV so_obj_len( xstrlen( lv_fpformoutput-pdf ) ).
      " istanza classe per gestione mail
      lo_send_request = cl_bcs=>create_persistent( ).

"istanza classe per gestione mittente
      lv_sender = cl_cam_address_bcs=>create_internet_address(
        i_address_string = CONV adr6-smtp_addr('indirizzo_owner')
        i_address_name = CONV ADR6-SMTP_ADDR('indirizzo_mail_mittente')
        ).

	"converisone da xstring a formato necessario per invio attachmennt	
	  lo_pdf_content = cl_document_bcs=>xstring_to_solix( 'pdf_content' ).
	  
	"creazione body
	
	APPEND 'Body della mail' TO lt_bodymail.
	  
	"creazione document mail
      lo_document = cl_document_bcs=>create_document(
                      i_type    = 'RAW'  "importante per avere il body in modo corretto
                      i_sensitivity = 'P'
                      i_text  = lt_bodymail
                      i_sender = lv_sender
                      i_subject = CONV so_obj_des('Oggetto della mail')
                 ).

      lv_size = CONV so_obj_len( xstrlen( pdf_xstring ) ).

	"gestione allegato mail, in questo caso PDF
      lo_document->add_attachment(
        EXPORTING
          i_attachment_type     = CONV so_obj_tp('PDF')
          i_attachment_subject  = CONV so_obj_des('Nome allegato')
          i_attachment_size     = lv_size
          i_att_content_hex     = lt_solix
      ).

	  "viene settato il documento alla richiesta
      lo_send_request->set_document( lo_document ).
	  
	  "viene settato il mittente
      lo_send_request->set_sender(
      EXPORTING
          i_sender = lv_sender
          ).
      "viene settato il destinatario
      lo_recipient = cl_cam_address_bcs=>create_internet_address(
           i_address_string = |{ lv_mail }|
      ).
	  
	  "viene aggiunto il destinatario alla richiesta
      lo_send_request->add_recipient( i_recipient = lo_recipient ).


	"la richiesta viene messa nella SOST	
      lv_sent_to_all = lo_send_request->send(
        i_with_error_screen = 'X'
       ).

      COMMIT WORK.

    CATCH cx_document_bcs INTO lx_document_bcs.
  ENDTRY
```
