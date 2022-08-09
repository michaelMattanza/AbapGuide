# abap.zcl_alv_manager
Classe per sviluppi ALV-based

<b>Creare oggetto ALV Manager</b>    
Per creare l'oggetto alv manager è necessario passare il nome report e la tabella di output.

```abap
DATA(lo_alv_manager) = NEW zcl_alv_manager(
    iv_program_name = CONV #( sy-repid )
    it_outtab       = <fs_first_table>
  ).
```

La classe creerà in automatico il fieldcatalog e asssegnerà gli handler principali in automatico.
Nel modulo di output chiamare il metodo <i>display_data</i> per mostrare l'alv.
```abap
lo_alv_manager->display_data( is_layout_ft = VALUE lvc_s_layo( sel_mode = 'A' ) ).
```
Se le tabelle sono due è possibile passare un flag al parametro IV_VERTICAL per impostare le due tabelle in visualizzazione verticale.

<b>Impostare la seconda tabella</b>    
Per impostare la seconda tabella chiamare il metodo <i>set_second_table</i>
```abap
lo_alv_manager->set_second_table(
    EXPORTING
      it_outtab    = <fs_second_table>
     )
```

<b>Handler</b>     
Per utilizzare un handler basta creare un form nel report con il nome dell'handler.
```abap
FORM handle_toolbar USING e_object TYPE REF TO cl_alv_event_toolbar_set e_interactive.

  APPEND: VALUE #( butn_type = 3 ) TO e_object->mt_toolbar.
  APPEND: VALUE #( butn_type = 0 text = 'Invia documento' function = 'PRINT') TO e_object->mt_toolbar.

ENDFORM.

FORM handle_user_command USING e_ucomm lt_rows TYPE lvc_t_row.
  MESSAGE |Alv 1 FUNCT { e_ucomm }| TYPE 'I'.
ENDFORM.
```

Non tutti gli handler sono stati implementati: in caso serva l'implementazione di un handler particolare, inserire la chiamata nei metodi <i>set_handler_first_alv</i> e <i>set_handler_second_alv</i>

Aggiungere in firma il collegamento all'evento std
```abap
METHODS nome_handler
      FOR EVENT std_evt OF cl_gui_alv_grid
      IMPORTING
        ...std_parameters... .
```

e nel metodo implementato inserire la chiamata al form
```abap
PERFORM nome_handler IN PROGRAM (gv_program_name) IF FOUND USING ...std_parameters....
```

<i>La dicitura *_st si riferisce alla seconda tabella</i>    

<b>FieldCatalog Custom</b>    
è possibile dare dei valori custom agli elementi del fieldcatalog valorizzando la tabella IT_CUSTOM_FC del costruttore ( o nel metodo <i>set_second_table</i> ) passando il valore nel seguente modo:

Tabella: tty_fc_custom    
Campi: fieldname -> nome campo tabella output
       fc_component -> componente fieldcatalog da modificare
       value -> valore da impostare
       
<i>NB</i>    
Se nel fieldname viene passato il valore "*" la modifca viene considerata per tutti i campi.

