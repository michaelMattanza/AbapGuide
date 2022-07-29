<h1>SD_SELECT_SALES_DOCUMENTS</h1>
Estrazione ordini di vendita e piani di consegna aperti

Il lancio della funzione va fatta una prima volta per gli ordini di vendita con:    
iv_trvog = ‘0’;    
iv_vboff = ‘X’;    
i_vkorg_rt = IT01;    
i_werks_rt = IT01;     
i_auart_rt = ---.     
Va fatto un secondo lancio per i piani di consegna:     
iv_trvog = ‘3’;     
i_vkorg_rt = IT01;       
i_werks_rt = IT01;    
i_auart_rt = ---;    
iv_vboff = ‘X’; da verificare.    


```abap 
*"*"Global interface:
*"  IMPORTING
*"     VALUE(IV_TRVOG) TYPE  TRVOG
*"     VALUE(IV_BSTKD) TYPE  BSTKD OPTIONAL
*"     VALUE(IV_ZPAVW) TYPE  PARVW OPTIONAL
*"     VALUE(IV_VBOFF) TYPE  CHAR1 OPTIONAL
*"     VALUE(IV_VBALL) TYPE  CHAR1 OPTIONAL
*"  TABLES
*"      T_VBMTV STRUCTURE  SVBMTV_TRVOG
*"      LVBMTV STRUCTURE  VBMTV OPTIONAL
*"      I_VKORG_RT STRUCTURE  BAPI_RANGESVKORG OPTIONAL
*"      I_VTWEG_RT STRUCTURE  BAPI_RANGESVTWEG OPTIONAL
*"      I_SPART_RT STRUCTURE  BAPI_RANGESSPART OPTIONAL
*"      I_MATNR_RT STRUCTURE  RDM_SR_MATNR OPTIONAL
*"      I_KUNNR1_RT STRUCTURE  BAPI_RANGESKUNNR OPTIONAL
*"      I_VKBUR_RT STRUCTURE  BAPI_RANGESVKBUR OPTIONAL
*"      I_VKGRP_RT STRUCTURE  BAPI_RANGESVKGRP OPTIONAL
*"      I_AUART_RT STRUCTURE  SHP_AUART_RANGE OPTIONAL
*"      I_ERNAM_RT STRUCTURE  ERNAM_RAN OPTIONAL
*"      I_VBELN_RT STRUCTURE  SHP_VBELN_RANGE OPTIONAL
*"      I_AUDAT_RT STRUCTURE  BAPI_RANGESAUDAT OPTIONAL
*"      I_WERKS_RT STRUCTURE  WERKS_RANG OPTIONAL
*"      I_ZPERS_RT STRUCTURE  SHP_PERNR_RANGE OPTIONAL
*"      I_DATAB_RT STRUCTURE  DATAB_RAN OPTIONAL
*"      I_ERDAT_RT STRUCTURE  BAPI_RANGESAUDAT OPTIONAL

CALL FUNCTION 'SD_SELECT_SALES_DOCUMENTS'
      EXPORTING
        iv_trvog   = ls_config_param-s_sales_doc-v_trvog
        iv_vboff   = ls_config_param-s_sales_doc-v_vboff
      TABLES
        t_vbmtv    = lt_vtmtv
        i_vkorg_rt = ls_config_param-s_sales_doc-r_vkorg
        i_auart_rt = ls_config_param-s_sales_doc-r_auart
        i_werks_rt = ls_config_param-s_sales_doc-r_werks.
```
