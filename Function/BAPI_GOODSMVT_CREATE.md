<h1>BAPI_GOODSMVT_CREATE</h1>
Movements code - GMCODE
 01 - MB01 - Goods Receipts for Purchase Order   
 02 - MB31 - Goods Receipts for Prod Order   
 03 - MB1A - Goods Issue   
 04 - MB1B - Transfer Posting   
 05 - MB1C - Enter Other Goods Receipt   
 06 - MB11   


Tipo movimenti - KZBEW 
      Goods movement w/o reference   
  B - Goods movement for purchase order   
  F - Goods movement for production order   
  L - Goods movement for delivery note   
  K - Goods movement for kanban requirement (WM - internal only)   
  O - Subsequent adjustment of "material-provided" consumption   
  W - Subsequent adjustment of proportion/product unit material   
  
```abap
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(GOODSMVT_HEADER) LIKE  BAPI2017_GM_HEAD_01 STRUCTURE
*"        BAPI2017_GM_HEAD_01
*"     VALUE(GOODSMVT_CODE) LIKE  BAPI2017_GM_CODE STRUCTURE
*"        BAPI2017_GM_CODE
*"     VALUE(TESTRUN) LIKE  BAPI2017_GM_GEN-TESTRUN DEFAULT SPACE
*"     VALUE(GOODSMVT_REF_EWM) LIKE  /SPE/BAPI2017_GM_REF_EWM STRUCTURE
*"        /SPE/BAPI2017_GM_REF_EWM OPTIONAL
*"     VALUE(GOODSMVT_PRINT_CTRL) LIKE  BAPI2017_GM_PRINT STRUCTURE
*"        BAPI2017_GM_PRINT OPTIONAL
*"  EXPORTING
*"     VALUE(GOODSMVT_HEADRET) LIKE  BAPI2017_GM_HEAD_RET STRUCTURE
*"        BAPI2017_GM_HEAD_RET
*"     VALUE(MATERIALDOCUMENT) TYPE  BAPI2017_GM_HEAD_RET-MAT_DOC
*"     VALUE(MATDOCUMENTYEAR) TYPE  BAPI2017_GM_HEAD_RET-DOC_YEAR
*"  TABLES
*"      GOODSMVT_ITEM STRUCTURE  BAPI2017_GM_ITEM_CREATE
*"      GOODSMVT_SERIALNUMBER STRUCTURE  BAPI2017_GM_SERIALNUMBER
*"       OPTIONAL
*"      RETURN STRUCTURE  BAPIRET2
*"      GOODSMVT_SERV_PART_DATA STRUCTURE
*"        /SPE/BAPI2017_SERVICEPART_DATA OPTIONAL
*"      EXTENSIONIN STRUCTURE  BAPIPAREX OPTIONAL
*"      GOODSMVT_ITEM_CWM STRUCTURE  /CWM/BAPI2017_GM_ITEM_CREATE
*"       OPTIONAL

gmhead-pstng_date = sy-datum.
gmhead-doc_date = sy-datum.
gmhead-pr_uname = sy-uname.

gmcode-gm_code = '01'.   "01 - MB01 - Goods Receipts for Purchase Order

CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
  EXPORTING
    goodsmvt_header  = gmhead
    goodsmvt_code    = gmcode
  IMPORTING
    goodsmvt_headret = mthead
  TABLES
    goodsmvt_item    = itab
    return           = errmsg.
```