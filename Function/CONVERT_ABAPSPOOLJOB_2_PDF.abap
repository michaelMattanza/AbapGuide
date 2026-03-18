CALL FUNCTION 'CONVERT_ABAPSPOOLJOB_2_PDF'
  EXPORTING
    src_spoolid   = lv_spool_id
  TABLES
    pdf           = lt_pdf_bin
  EXCEPTIONS
    OTHERS        = 1.
