FM per la creazione di una fattura passiva senza riferimento ordine (FB60)

```abap 
DATA: lt_accountgl TYPE TABLE OF BAPIACGL09,
lt_accountpayable TYPE TABLE OF BAPIACAP09,
lt_curram TYPE TABLE OF BAPIACCR09.

DATA(ls_docheader) = VALUE #( 
   OBJ_TYPE = 'BKPFF' 
   username = sy-uname 
   header_text = '--' 
   comp_code = bseg-bukrs 
   doc_date = sy-datum 
   pstng_date = sy-datum 
   fisc_year = bseg-gjahr
   fis_period = bseg-buzei
   doc_type = bseg-blart
   ref_doc_no = bseg-xblnr
 ).
 
 lt_accountgl = value #( 
  (
    itemno_acc = '1'
    gl_account = bseg-hkont
    item_text = '---'
    acct_type = bseg-koart
    alloc_nmbr = bseg-zuonr
    costcenter = bseg-kostl
  )
 ).
 
 lt_accountpayable = VALUE #(
   (
     itemno_acc = '2'
     vendor_no = bseg-lifnr
     gl_account = bseg-hkont
     comp_code = bseg-bukrs 
     pmnttrms =  bseg-zterm
     bline_date = bseg-ZFBDT
     item_text = '---'
   )
 ).
 
 lt_curram = VALUE #(
   (
     itemno_acc = '1'
     currency = bseg-PSWSL
     atm_doccur = bseeg-WRBTR
    )
    (
     itemno_acc = '2'
     currency = bseg-PSWSL
     atm_doccur = bseeg-WRBTR
    )
 ).
 
 CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
    EXPORTING
      DOCUMENTHEADER    = ls_docheader
    IMPORTING
      OBJ_KEY           = OBJTYP
    TABLES
      ACCOUNTGL         = lt_accountgl
      "ACCOUNTRECEIVABLE = CUSTOMER
      ACCOUNTPAYABLE    = lt_accountpayable
      CURRENCYAMOUNT    = lt_curram
      RETURN            = RETURN.

CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      IMPORTING
        RETURN = RETURN.
    COMMIT WORK AND WAIT.
``` 
