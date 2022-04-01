<h1>MD_SALES_ORDER_STATUS_REPORT</h1>
Dato un ordine e posizione ne ricava il materiale ed estrae tutti i fabbisogni aperti relativi a quel materiale

```abap 
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(EDELET) LIKE  MDPS-DELET DEFAULT 0000
*"     VALUE(EDELKZ) LIKE  MDPS-DELKZ
*"     VALUE(EDELNR) LIKE  MDPS-DEL12
*"     VALUE(EDELPS) LIKE  MDPS-DELPS DEFAULT 000000
*"     VALUE(EPLSCN) LIKE  MDKP-PLSCN DEFAULT 000
*"     VALUE(AVAILABILITY_CHECK) LIKE  RM61M-DSELK DEFAULT SPACE
*"     VALUE(NO_SAVETY_STOCK) LIKE  RM61O-SSTNO DEFAULT SPACE
*"     VALUE(DATA_IN_MEMORY) LIKE  RM61M-BOOLEAN DEFAULT SPACE
*"     VALUE(MEMORY_ID) TYPE  MEMID4 DEFAULT 'PLHS'
*"     VALUE(EMATNR) LIKE  MT61D-MATNR DEFAULT SPACE
*"     VALUE(EWERKS) LIKE  MT61D-WERKS DEFAULT SPACE
*"     VALUE(EBERID) LIKE  MT61D-BERID DEFAULT SPACE
*"     VALUE(EMDPS) LIKE  MDPS STRUCTURE  MDPS OPTIONAL
*"     VALUE(NODISP) TYPE  XFLAG DEFAULT SPACE
*"     VALUE(I_IGNORE_MTOLD) TYPE  XFLAG DEFAULT SPACE
*"     VALUE(I_PROFID) TYPE  RM61O-PROFID DEFAULT SPACE
*"     VALUE(I_REP_REFRESH) TYPE  XFLAG DEFAULT SPACE
*"     VALUE(IS_PROFILE) TYPE  T464 OPTIONAL
*"     VALUE(IT_VBEP_KEYS) TYPE  MD_T_VBEP_KEY OPTIONAL
*"     VALUE(NO_COMMIT_WORK) TYPE  XFLAG DEFAULT SPACE
*"  EXPORTING
*"     REFERENCE(ET_MLDELAY) TYPE  MD_T_MLDELAY
*"     REFERENCE(ET_RTREE_SEL) TYPE  MD_RTREE_SEL
*"  TABLES
*"      IIOELX STRUCTURE  IOEL OPTIONAL
*"  EXCEPTIONS
*"      ERROR

CALL FUNCTION 'MD_SALES_ORDER_STATUS_REPORT'
          EXPORTING
            edelkz         = 'VC'
            edelnr         = lv_vbeln
            edelps         = lv_posnr
            no_commit_work = 'X'
            nodisp         = 'X'
            is_profile     = ls_profile
          TABLES
            iioelx         = lt_tpm_iioelx
          EXCEPTIONS
            error          = 1
            error_message  = 2
            OTHERS         = 3.
```