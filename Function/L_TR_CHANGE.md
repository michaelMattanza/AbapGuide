This function module is used to change transfer requirements
    
    APPEND VALUE #(
      lgnum = <fs_outtab>-lgnum
      tbnum = <fs_outtab>-tbnum
      tbpos = <fs_outtab>-tbpos
      elikz = abap_true
      menga = 0
     ) TO lt_ltbc.

     CALL FUNCTION 'L_TR_CHANGE'
       tables
         T_LTBC                   = lt_ltbc
       exceptions
         ITEM_ERROR               = 1
         NO_UPDATE_ITEM_ERROR     = 2
         NO_UPDATE_NO_ENTRY       = 3
         NO_UPDATE_WITHOUT_COMMIT = 4
         TR_LOCKED                = 5
         OTHERS                   = 6
       .
     if sy-subrc <> 0.
*      message id sy-msgid type sy-msgty number sy-msgno
*                 with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
     endif.
