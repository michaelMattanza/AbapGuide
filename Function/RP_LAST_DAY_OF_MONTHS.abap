" This function gives you the last day of the month

DATA(lv_doc_date) = sy-datum.

  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
      EXPORTING
        day_in            = lv_doc_date
      IMPORTING
        last_day_of_month = lv_doc_date
      EXCEPTIONS
        day_in_no_date    = 1
        others            = 2
      .
    IF SY-SUBRC <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*       WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
