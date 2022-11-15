```abap    
DATA: lv_chr(4) type c,
      lv_num    type p.

CALL FUNCTION 'MOVE_CHAR_TO_NUM'
  EXPORTING
    CHR             = lv_chr
  IMPORTING
    NUM             = lv_num
  EXCEPTIONS
    CONVT_NO_NUMBER = 1
    CONVT_OVERFLOW  = 2
    OTHERS          = 3.
IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.
```
