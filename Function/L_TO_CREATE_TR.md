L_TO_CREATE_TR Is used when you have a transfer requirement for which you need to create a transfer order.

  APPEND VALUE #(
      tbpos = ls_ltbp-btaps
      anfme = lv_qta
      altme = ls_ltbp-meins
      vltyp = ls_ltbp-lgtyp
      vlpla = <fs_outtab>-lgpla
    ) TO lt_trite.

     CALL FUNCTION 'L_TO_CREATE_TR'
       exporting
         I_LGNUM                        = <fs_outtab>-lgnum
         I_TBNUM                        = lv_tbnum
         i_squit                        = 'X'
         i_commit_work                  = 'X'
         i_bname                        = CONV ltak-bname( sy-uname )
         it_trite                       = lt_trite
       exceptions
         FOREIGN_LOCK                   = 1
         QM_RELEVANT                    = 2
         TR_COMPLETED                   = 3
         XFELD_WRONG                    = 4
         LDEST_WRONG                    = 5
         DRUKZ_WRONG                    = 6
         TR_WRONG                       = 7
         SQUIT_FORBIDDEN                = 8
         NO_TO_CREATED                  = 9
         UPDATE_WITHOUT_COMMIT          = 10
         NO_AUTHORITY                   = 11
         PREALLOCATED_STOCK             = 12
         PARTIAL_TRANSFER_REQ_FORBIDDEN = 13
         INPUT_ERROR                    = 14
         OTHERS                         = 15
       .
     if sy-subrc <> 0.
*      message id sy-msgid type sy-msgty number sy-msgno
*                 with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
     endif.
