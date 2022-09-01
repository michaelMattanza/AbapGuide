<h2>ALV</h2>

 - [Dynpro](#Dynpro)
 - [FieldCatalog](#FieldCatalog)
 - [Chiamare ALV](#ALV)
 - [Toolbar](#Toolbar)
 - [Moduli](#Moduli)

 ### Dynpro
 La dynpro è la combinazione della logica e della view. Possiede dei moduli per la gestione di input (PAI) e di output (PBO). In questi moduli è possibile inserire del codice custom per modificare a piacimento dati o comportamenti di determinati elementi. La dynpro è identificata da un numero di 4 cifre e viene chiamata dal report tramite la chiamata <i>CALL SCREEN XXXX</i>.

**PAI** </br>
Vengono gestiti i dati di input, estratti nuovi dati, ecc... Qui possono essere modificati i valori mostrati a schermo. Per far si che lo schermo rimanga ggiornato inserire il metodo <i>cl_gui_cfw=>set_new_ok_code( new_code = 'REFR' )</i> alla fine del modulo, affinchè lo schermo venga aggiornato.   
   
Se l'alv ha campi editabili è possibile intercettare le modifiche con il metodo *lo_alv->check_changed_data( ).*.

**PBO**</br>
In questo modulo è possibile nascondere o mostrare i vari elementi contenuti nella dynpro. E' infatti possibile ciclare sui vari elementi tramite il <i>LOOP AT SCREEN</i> e vedere il nome (o il gruppo) dell'elemento stampato (questo riguarda solo gli elementi creati tramite layout della dynpro, non coinvolge cose nell'alv).

Per sapere lo stato della dynpro si deve controllare il campo aktyp:
- Se A: visualizzazione
- Se H: creazione
- Se V: Modifica

Per sapere se una transazione è in lettura/scrittura/modifica leggere dalla tabella T180.

 ### FieldCatalog
 Il field catalog è la tabella che contiene le proprietà dei dati che verranno stampati tramite la ALV. In questo caso il field catalog viene creato dinamicamente, utilizzando il descrittore di tabelle. Siccome prende i testi dei dati dalla tabella dd04t è possibile che non tutti i campi vengano valorizzati (oppure con la presenza di più campi dello stesso tipo). In quel caso sono da inserire a mano. 
 
 Nel seguente esempio la tabella *IT_CUSTOM_FC* viene utilizzata per modificare valori particolari del fieldcatalog.
 
 FIELDNAME = 'matnr' fc_component = 'scrtext_l' value = 'Descr. colonna matnr custom'

```abap
value( IS_OUTTAB )	TYPE DATA	 " Riga tabella di output
value( IT_CUSTOM_FC )	TYPE ZT_FC_CUSTOM OPTIONAL	table " Valori speciali per il fieldcatalog
value( CT_FIELDCAT )	TYPE LVC_T_FCAT	Tabella finale fieldcatalog


      DATA : lo_ref_descr TYPE REF TO cl_abap_structdescr,
            lt_detail    TYPE abap_compdescr_tab,
            ls_detail    LIKE LINE OF lt_detail,
            lr_typedescr TYPE REF TO cl_abap_typedescr,
            lv_counter   TYPE i VALUE 0.

  FIELD-SYMBOLS: <fs_dref>  TYPE any,
                 <fs_fname> TYPE any.

  lo_ref_descr ?= cl_abap_typedescr=>describe_by_data( is_outtab ). "Chiamare metodo statico su una struttura
  lt_detail[] = lo_ref_descr->components.

  LOOP AT lt_detail INTO ls_detail.
    ASSIGN COMPONENT ls_detail-name OF STRUCTURE is_outtab TO FIELD-SYMBOL(<ls_comp>).

    IF <ls_comp> IS ASSIGNED.
      ADD 1 TO lv_counter.
      lr_typedescr = cl_abap_typedescr=>describe_by_data( <ls_comp> ) .

      APPEND VALUE #(
        ref_field = lr_typedescr->absolute_name+6
        fieldname = ls_detail-name
        outputlen = lr_typedescr->length
        col_id    = lv_counter
      ) TO ct_fieldcat.

    ENDIF.
  ENDLOOP.

   SELECT rollname, scrtext_m
    FROM dd04t
    INTO TABLE @DATA(lt_coldescr)
    FOR ALL ENTRIES IN @ct_fieldcat
    WHERE rollname EQ @ct_fieldcat-ref_field
    AND ddlanguage EQ @sy-langu.
    
  LOOP AT it_custom_fc REFERENCE INTO DATA(lr_cust_fc).
    TRANSLATE lr_cust_fc->fieldname TO UPPER CASE.
  ENDLOOP.

  LOOP AT ct_fieldcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).
    <fs_fcat>-scrtext_m = VALUE #( lt_coldescr[ rollname = <fs_fcat>-ref_field ]-scrtext_m OPTIONAL ).

    LOOP AT it_custom_fc ASSIGNING FIELD-SYMBOL(<fs_custom_fc>) WHERE fieldname EQ <fs_fcat>-fieldname.
      TRANSLATE <fs_custom_fc> TO LOWER CASE.
      ASSIGN COMPONENT <fs_custom_fc>-fc_component OF STRUCTURE <fs_fcat> TO FIELD-SYMBOL(<fs_comp>).
      IF sy-subrc EQ 0.
        <fs_comp> =  CONV #( <fs_custom_fc>-value ).
      ENDIF.
    ENDLOOP.

  ENDLOOP.
```

 ### Chiamare ALV
 Un'ALV è un metodo di output per mostrare tabelle o dati a schermo, dopo un'elaborazione. Esistino varie classi (alv, salv, ...) ma funzionano più o meno allo stesso modo. Per utilizzare un'alv serve un container (che dia all'alv un'estensione sullo schermo), il fieldcatalog (ovvero la descrizione della tabella che verrà mostrata) e un layout (conterrà eventuali componenti come il colore, il tipo di selezione righe e altro).

> Prima di generare la alv deve essere creato il field-catalog (vedi la pagina apposita).
> Inserire il componente "Custom Control" nella Dynpro utilizzata per stampare la Alv dandogli il nome del Container come nome (in questo caso "CONT")

```abap
DATA: r_cont TYPE REF TO cl_gui_custom_container,
      r_alv  TYPE REF TO cl_gui_alv_grid.
      
DATA: it_fcat TYPE TABLE OF lvc_s_fcat,
      wa_fcat LIKE LINE OF it_fcat.
 
 
" Container custom
* CREATE OBJECT r_cont
*   EXPORTING
*     container_name = 'CONT'.

* CREATE OBJECT r_alv
*   EXPORTING
*     i_parent = r_cont.
"

" Container std -> si adatta allo schermo di chi usa l'alv
 CREATE OBJECT r_alv
    EXPORTING i_parent = cl_gui_container=>default_screen

  CALL METHOD r_alv->set_table_for_first_display
    EXPORTING
      is_layout                     = wa_layo
    CHANGING
      it_fieldcatalog               = it_fcat
      it_outtab                     = lt_s_oda
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc <> 0.

  ENDIF.
```
Un'alv contiene degli <i>Eventi</i> che possono essere implementati nel programma locale. Gli eventi devono essere implementati attraverso un metodo Handler:
```abap
*lo_event è una classe handler
CREATE OBJECT lo_event.
SET HANDLER lo_event->handle_hotspot_click FOR r_alv.
```
Possono essere mostrate piu alv in uno stesso screen utilizzando uno splitter container.
```abap
*  Creo il contenitore che definisce l'estensione di stampa su schermo
    CREATE OBJECT lo_cont_docking
      EXPORTING
        parent = cl_gui_container=>screen0
        ratio  = 95
      EXCEPTIONS
        OTHERS = 6.

*   Imposto l'estensione della stampa su schermo
    CALL METHOD lo_cont_docking->set_extension
      EXPORTING
        extension  = 99999
      EXCEPTIONS
        cntl_error = 1
        OTHERS     = 2.

*     Creo il primo split container con parente il contenitore docking. Imposto tre righe per le tre alv utilizzate
      CREATE OBJECT lo_split_co
        EXPORTING
          parent  = lo_cont_docking
          rows    = 3
          columns = 1
          align   = 15.

*     Assegno alla prima riga del container splittato il container graphic_parent_hd -> header
      CALL METHOD lo_split_co->get_container
        EXPORTING
          row       = 1
          column    = 1
        RECEIVING
          container = graphic_parent_hd.

      CALL METHOD lo_split_co->set_row_height
        EXPORTING
          id                = 1
          height            = 4
        .
*     Assegno alla prima riga del container splittato il container graphic_parent1 -> Coil
      CALL METHOD lo_split_co->get_container
        EXPORTING
          row       = 2
          column    = 1
        RECEIVING
          container = graphic_parent1.

*     Assegno alla prima riga del container splittato il container graphic_parent2 -> Ordini
      CALL METHOD lo_split_co->get_container
        EXPORTING
          row       = 3
          column    = 1
        RECEIVING
          container = graphic_parent2.

*     Creo l'alv per i coil con parente graphic_parent1
      CREATE OBJECT lo_alv_up
        EXPORTING
          i_parent = graphic_parent1.

*     Creo l'alv per gli ordini con parente graphic_parent2
      CREATE OBJECT lo_alv_dw
        EXPORTING
          i_parent = graphic_parent2.

*     Imposto per le due alv un gestore di azioni che richiama funzioni in base al metodo chiamato
      SET HANDLER me->handle_user_command FOR lo_alv_up.
      SET HANDLER me->handle_user_command FOR lo_alv_dw.
      
      wa_layout_1-cwidth_opt   = 'X'.
      wa_layout_2-cwidth_opt  = 'X'.
      wa_layout_1-sel_mode     = 'D'.
      wa_layout_2-sel_mode    = 'D'.
      wa_layout_2-info_fname  = 'ROWCOLOR'.
      wa_layout_1-info_fname   = 'ROWCOLOR'.
      
      CALL METHOD lo_alv_up->set_table_for_first_display
        EXPORTING
          is_layout                     = wa_layout_1
          is_variant                    = lv_repname
          i_save                        = 'A'
        CHANGING
          it_fieldcatalog               = it_fcat_1[]
          it_outtab                     = out_grid_1
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          OTHERS                        = 4.
      IF sy-subrc <> 0.
      ENDIF.

     CALL METHOD lo_alv_dw->set_table_for_first_display
        EXPORTING
          is_layout                     = wa_layout_2
          is_variant                    = lv_repnam2
          i_save                        = 'A'
        CHANGING
          it_fieldcatalog               = it_fcat_2
          it_outtab                     = out_grid_2
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          OTHERS                        = 4.
      IF sy-subrc <> 0.
      ENDIF.
```

Per creare un Header sopra una tabella utilizzare lo splitter per creare due container. Inserire poi il testo:

```abap
DATA:  lo_doc_header         TYPE REF TO cl_dd_document.
  lo_doc_header->initialize_document( ).

  lo_doc_header->add_text( text =  'Legenda' ).
  lo_doc_header->new_line( ).
  lo_doc_header->add_text( text =  'Riga 1' ).
  lo_doc_header->add_gap( ). " Tab
  lo_doc_header->add_text( text =  'Riga 1' ).
  

  lo_doc_header->merge_document( ).
  lo_doc_header->display_document( parent = lo_cont_up ).
```

### Moduli 
Quando si chiama una ALV da un report è necessario utilizzare i moduli della dynpro che si chiama. I moduli sono INPUT e OUTPUT.
Il primo gestisce i pulsanti, il secondo la visualizzazione dei campi.

``` abap
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STAT100'.
  SET TITLEBAR 'Test'.
  
  ... r_alv->set_table_for_first_display ...
ENDMODULE.

MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
```


<h1>Formato celle</h1>    
### Dropdown values    

``` abap    
" Fieldcatalog Setting
fieldname = 'zzfieldname'
drdn_hndl = 'X'
drdn_alias = 'X'

" Value Setting
ls_dropdown-handle = '1'.
ls_dropdown-value = 'G'.
APPEND ls_dropdown TO lt_dropdown.

ls_dropdown-handle = '1'.
ls_dropdown-value = 'H'.
APPEND ls_dropdown TO lt_dropdown.

ls_dropdown-handle = '1'.
ls_dropdown-value = 'I'.
APPEND ls_dropdown TO lt_dropdown.

ls_dropdown-handle = '1'.
ls_dropdown-value = 'M'.
APPEND ls_dropdown TO lt_dropdown.

CALL METHOD go_grid_ctrl->set_drop_down_table
  EXPORTING
    it_drop_down = lt_dropdown.
``` 
