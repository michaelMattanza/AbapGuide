<h1>Sintassi base</h1>

**Dichiarazione variabili input** </br>

```abap
" Chiede in input un valore obbligatorio che alloca nella memoria con id wrk
PARAMETERS werks TYPE werks_d MEMORY ID wrk OBLIGATORY,
           in_data TYPE sy-datum. 

" Chiede in input dei range 
SELECT-OPTIONS: so_kunnr FOR zstock_b-kunnr,
                so_matnr FOR zstock_b-matnr.
                
INITIALIZATION.
in_data = '20190709'. "Imposto come valore consigliato la data 09-07-2019
```

**Search Help**<br>
Un search help viene utilizzato per la ricerca di valori possibili per un determinato campo in input. 

```abap
************************************************************************
* CUSTOM TYPES
************************************************************************
BEGIN OF ty_variant,
         variant TYPE variant,
       END OF ty_variant.
       
************************************************************************
* GLOBAL DATA
************************************************************************
gt_variant TYPE TABLE OF ty_variant.

************************************************************************
* SELECTION SCREEN
************************************************************************
PARAMETERS: p_varnt TYPE variant.

************************************************************************
* INITIALIZATION
************************************************************************
INITIALIZATION.

SELECT variant
  FROM varid
  INTO TABLE gt_variant
  WHERE report EQ sy-repid.

************************************************************************
* AT SELECTION SCREEN
************************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_varnt.
  DATA: ls_variant TYPE ty_variant,
        lt_ret TYPE TABLE OF ddshretval,
        ls_ret TYPE ddshretval.

 CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'VARIANT'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      value_org   = 'S'
    TABLES
      value_tab   = gt_variant
      return_tab  = lt_ret.

 READ TABLE lt_ret INTO ls_ret INDEX 1.
 p_varnt = ls_ret-fieldval.
```


**Dichiarazione variabili interne** </br>

```abap
" Dichiaro diversi tipi di variabili seguendo la naming convention: 
" - l = local,  g = global, i = import, e = export
" - v = variable, s = structure, t = table, f = field, r = range, ref = reference, o = object
DATA: lv_werks TYPE werks_d,
      ls_ekko  TYPE ekko,
      lt_ekpo  TYPE TABLE OF ekpo. 

" Dichiaro un tipo struttra / tabella seguendo la naming convention:
" - ty = type, tt = type table
DATA: BEGIN OF ty_example,
      field1 type type1,
      field2 type type2,
      END OF ty_example,
      tt_example TYPE TABLE OF ty_example. " tt_example è un tipo tabella
 DATA: lt_ex TYPE tt_example. " lt_ex è quindi una tabella
      
```
**Raggruppare il codice**

iL *DEFINE* Viene utilizzato per poter utilizzare più volte le stesse righe di codice. I define non li debuggare e non possono essere utilizzati fuori dal programma. 

```abap
DATA: RESULT TYPE I,
      N1 TYPE I VALUE 5,
      N2 TYPE I VALUE 6.

DEFINE OPERATION.
RESULT = &1 &2 &3.
OUTPUT &1 &2 &3 RESULT.
END-OF-DEFINITION.

DEFINE OUTPUT.
WRITE: / 'The result of &1 &2 &3 is', &4.
END-OF-DEFINITION.

OPERATION 4 + 3.
OPERATION 2 ** 7.
OPERATION N2 - N1.
```
E' possibile utilizzare i *FORM / PERFORM* per poter utilizzare lo stesso codice in più punti. Possono essere chiamati anche da programmi esterni ma non possono essere creati ovunque (es. metodi delle classi).
```abap
PERFORM OPERATION CHANGING n1 n2 tot.

FORM OPERATION CHANGING  lv_n1 TYPE i
                         lv_n2 TYPE i
                         lv_tot TYPE i.
                         
 lv_tot = lv_n1+ lv_n2.
                         
ENDFORM.
```

**Stringhe**<br>
Quando si cerca un valore specifico in una stringa contenente un separatore definito, è possibile ottenere velocemente la sottostringa grazie ad una di queste funzioni.
```abap
data(result) = segment( val = 'VAL1-VAL2-VAL3' index = 3 sep = '-' ).
data(result) = match( val  = 'VAL1-VAL2-VAL3'  regex = '[^-]*$' ).
data(result) = SubString( Val = 'VAL1-VAL2-VAL3' Off = Find( Val = 'VAL1-VAL2-VAL3' Regex = '-[^-]+$' ) + 1 ).
```
Per concatenare delle variabili in una stringa utilizzare la seguente sintassi:
```abap
CONCATENATE lv_var1 lv_var2 INTO lv_string.

"740
lv_string = |{ lv_var1 }{ lv_var2 }|.
```
Utilizzando la sinstassi 740 è possibile sfruttare anche delle funzioni di conversione.
```abap
" Allineamento   
WRITE / |{ 'Left'     WIDTH = 20 ALIGN = LEFT     PAD = '0' }|.   
WRITE / |{ 'Center'   WIDTH = 20 ALIGN = CENTER   PAD = '0' }|.   
WRITE / |{ 'Right'    WIDTH = 20 ALIGN = RIGHT    PAD = '0' }|.   

" Stile carattere   
WRITE / |{ ‘Text’ CASE = (cl_abap_format=>c_raw) }|.   
WRITE / |{ ‘Text’ CASE = (cl_abap_format=>c_upper) }|.   
WRITE / |{ ‘Text’ CASE = (cl_abap_format=>c_lower) }|.   

" Conversione numerica   
DATA(lv_vbeln) = ‘0000012345’.   
WRITE / |{ lv_vbeln  ALPHA = OUT }|.     “or use ALPHA = IN to go in   

" Conversione data   
WRITE / |{ pa_date DATE = ISO }|.           “Date Format YYYY-MM-DD   
WRITE / |{ pa_date DATE = User }|.          “As per user settings   
WRITE / |{ pa_date DATE = Environment }|.   “Formatting setting of   

" Conversione quantità    
DATA(lv_qty) = 13.000.

WRITE lv_menge TO lv_menge_char LEFT-JUSTIFIED UNIT lv_meinh. " se lv_meinh == udm senza decimali (es. pezzi ) -> lv_menge_char = 13
```
<h1>Query</h1>

**Select single:** estraggo solo un valore
 ```abap
SELECT SINGLE campo1, campo2 
  FROM table INTO variable
  WHERE condizione.
  
SELECT SINGLE campo1, campo2 
  FROM table INTO @DATA(variable)
  WHERE condizione.
```

**NB**    
*SELECT SINGLE* estrae da database il primo record che corrisponde alle condizioni definite nella query mentre *SELECT UP TO 1 ROWS* estrae tutti i record che corrispondono alla condizione di where ma verrà mostrato solo il primo nel risultato finale. Il secondo caso però, prima di restituire il risultato, svolge operazioni di aggregazione e raggruppamento, per restituire il risultato che più si addice alle condizioni di ricerca; viene consigliata infatti per le estrazioni senza una chiave primaria completa.

**Select count:** contare le righe di una select
```abap
SELECT COUNT(*) 
  FROM table
  WHERE condizione.
 " sy-dbcnt contiene il counter
  
SELECT COUNT( DISTINT nomecampo )
  FROM table INTO variable
  WHERE   condizione.
```

**Select into table:** estraggo più valori
```abap
SELECT campo1 campo2 
  FROM table INTO TABLE tab
  WHERE condizione.
  
SELECT campo1 campo2
  FROM table INTO CORRESPONDING FIELDS OF TABLE tab
  WHERE condizione.
  
SELECT campo1 campo2
  FROM table INTO TABLE tab
  FOR ALL ENTRIES IN tab2
  WHERE condizione.
  
 SELECT campo1, campo2
  FROM table INTO TABLE @DATA(tab)
  WHERE condizione.
  
*  Nella condizione del WHERE è utilizzabile anche  '<> @( VALUE #( ) )' per indicare un valore vuoto 
```
**For All Entries**   
Il *for all entries* viene utilizzato per fare un'estrazione per ogni record di una tabella interna.   
NON METTERE SELECT NEI LOOP!   
Il limite delle select con il for all entries è che molte funzioni di raggruppamento e alcune keyword della sintassi nuova non sono utilizzabili. Se si sta utilizzando un sistema aggiornato (SAP BASIS 752) è possibile evitare il for all entries ( per approfondimenti vedere link esterni ):   
```abap
SELECT campo1 campo2
  FROM table INTO CORRESPONDING FIELDS OF TABLE tab
  WHERE condizione.
  
" For all entries
SELECT campo1 campo2
  FROM table INTO TABLE tab2
  FOR ALL ENTRIES IN tab
  WHERE campo1 = tab-campo1.
  
 " Senza for all entries
 SELECT campo1 campo2
  FROM table INTO TABLE tab2
  WHERE campo1 IN ( select campo1 from @tab ).
  
```   
**Da select a range:** estraggo più valori
```abap
  SELECT sign opt AS option low high FROM /reg/param_selop
         INTO CORRESPONDING FIELDS OF TABLE itab_tipo_range
         WHERE zfunz EQ nomezfunz.
  
*  Nella condizione del WHERE è utilizzabile anche  '<> @( VALUE #( ) )' per indicare un valore vuoto. Preferibile usare @space.
```
**ATTENZIONE:**
> Per le query esistono due sintassi: la sintassi vecchia che non richiede la virgola tra i campi da selezionare, la nuova sintassi si. 
  Nella nuova sintassi deve essere inserita la chiocciola davanti alle variabili del report (quindi no campi o nomi tabella di dictionary) 
  ed è ammessa la dichiarazione inline della variabile o tabella (@DATA(nome)).  
  
> Differenza tra **INTO TABLE** e **INTO CORRESPONDING FIELD OF TABLE**. Il primo estrae dei dati e li mette nell'ordine di estrazione 
nella tabella di destinazione (quindi la tabella deve avere la struttura ordinata e con i nomi corretti), mentre nel secondo caso la 
query inserisce i valori estratti dal database nella colonna che corrisponde al dato estratto, se la colonna viene trovata.


<h1>Tabelle interne - Lettura valori</h1>   

**Dichiarare un tipo tabella interno al programma**

*ATTENZIONE*
- *sy-index*: indice contenuto nei DO e nei WHILE
- *sy-tabix*: indice nel loop di una tabella

```abap
" Dichiaro un tipo di struttura
TYPES: BEGIN OF ty_example,
    matnr TYPE matnr,
    mtart TYPE mtart,
    END OF ty_example.

DATA : it_example TYPE TABLE OF ty_example, " Istanzio una tabella di tipo ty_example
       wa_example TYPE ty_example.          " Istanzio una struttura di tipo ty_example
 
```
E' possibile usare 3 metodi diversi per estrarre i dati dalle tabelle:

- Utilizzando la struttura come riga della tabella
```abap
loop at it_example into wa_example.
    write:/ wa_example-matnr .
    write:/ wa_example-mtart .
endloop.
```

- Utilizzando i field-symbols come puntatori. 
```abap
loop at it_example assigning field-symbol(<fs_example>).
    write:/ <fs_example>-matnr .
    write:/ <fs_example>-mtart .
endloop.
```

- Utilizzando i field-symbols come puntatori quando una tabella è dinamica (non esiste quindi una struttura fissa ma viene generata da funzioni).
```abap
FIELD-SYMBOLS: <fs_matnr> TYPE any,
               <fs_mtart> TYPE any. " o data

loop at it_example assigning field-symbol(<fs_example>).
     ASSIGN COMPONENT 'MATNR' OF STRUCTURE <fs_example> TO <fs_matnr>.
     ASSIGN COMPONENT 'MTART' OF STRUCTURE <fs_example> TO <fs_mtart>.
    write:/ <fs_matnr> .
    write:/ <fs_mtart> .
endloop.
```

Esiste un'alternativa: le reference (preferibile però per la read table).
```abap
loop at it_example reference into data(lr_example).
    write:/ lr_example->matnr .
    write:/ lr_example->mtart .
endloop.

read table it_example reference into data(lr_example) with key matnr = '22000000'.
IF lr_example IS BOUND.
...
ENDIF.
```

Se si vuole leggere una riga o il valore di una riga senza doverne modificare il valore è consigliato utilizzare controlli inline:
```abap
DATA(ls_example) = VALUE #( it_example[ matnr = '22000000' ] OPTIONAL ).
" Se la riga non esiste viene creata una struttura vuota.

DATA(lv_mtart) = VALUE #( it_example[ matnr = '22000000' ]-mtart OPTIONAL ).
DATA(lv_mtart) = VALUE #( it_example[ matnr = '22000000' ]-mtart DEFAULT def ).
" Se la riga non esiste viene creata la variabile vuota. Nel secondo caso assegno un valore di default se il valore cercato è vuoto
```
<h1>Tabelle interne - Scrittura valori</h1>   

**Move-corresponding** <br>   
Il move corresponding è utile per spostare le righe di una tabella in una tabella di destinazione, con la conversione del tipo riga in base ai dati in movimento. Quando si deve aggiungere delle righe in una tabella con dentro dei record, il move-corresponding sovrascriverebbe i record gia presenti. 
E' possibile bypassare questo problema con la sintassi:   
```abap
gt_outtab = CORRESPONDING #( BASE ( gt_outtab ) lt_tmp_out ).
```

**Inserire nuovi valori**   
Esistono vari modi per inserire una riga di valori in una tabella interna e variano in base all'esigenza.      
```abap
DATA: lt_mara TYPE TABLE OF mara.

" Inserire una riga in coda   
APPEND VALUE #( matnr = '123' ) TO lt_mara.   

" Inserire una riga in una posizione specifica   
INSERT VALUE #( matnr = '123') INTO TABLE lt_mara INDEX n.

" Inserire la prima riga   
lt_mara = ( ( matnr = '123' ) ).
```

<h1>Tabelle interne - Ordinamento e gestione valori</h1>   

**Raggruppamento di una tabella**   
Se in una tabella ho bisogno di fare il *GROUP BY* basandomi su varie colonne, posso sfruttare il ciclo *FOR* per generare una tabella contenente il raggruppamento.   

```abap
  TYPES: BEGIN OF ty_imp,
           sel4     TYPE p_pernr,
           codconto TYPE saknr,
           mese     TYPE string,
           gjahr    TYPE gjahr,
         END OF ty_imp,
         tty_imp TYPE TABLE OF ty_imp WITH EMPTY KEY.
         
DATA(lt_count_imp) = VALUE tty_imp(
    FOR GROUPS ls_group OF ls_file IN gt_int_file GROUP BY ( sel4 = ls_file-sel4 codconto = ls_file-codconto mese = ls_file-mese gjahr = ls_file-gjahr )
    (
       ls_group
     )
  ).
  ```
  
  **Contare righe secondo condizioni definite**
 ```abap
  DATA(lv_lines) = REDUCE i(
      INIT x = 0
      FOR wa IN lt_table WHERE ( matnr IS NOT INITIAL )
      NEXT x = x + 1
    ).
```

 **Ottenere numero riga secondo condizioni definite**
  ```abap
  DATA(lv_index) = line_index( lt_ihpavb[ parvw = 'AG' ] ).
  ```
  
  **Ottenere numero righe tabella**
  ```abap
  DATA(lv_index) = lines( lt_ihpavb ).
  ```
Per poter ottenere la struttura di una tabella interna (nome e tipo delle colonne), è possibile utilizzare delle classi che aiutano l'estrazione di queste informazioni.

```abap

 FORM get_dynamic_field USING istructure TYPE data.
  "mapping partner
  DATA : lo_ref_descr TYPE REF TO cl_abap_structdescr,
         lt_detail    TYPE abap_compdescr_tab,
         ls_detail    LIKE LINE OF lt_detail,
         ls_c_compcd  TYPE idwtcompcd.

  lo_ref_descr ?= cl_abap_typedescr=>describe_by_data( istructure ). "Chiamare metodo statico su una struttura
  lt_detail[] = lo_ref_descr->components.

  LOOP AT lt_detail INTO ls_detail.
    ASSIGN COMPONENT ls_detail-name OF STRUCTURE istructure TO FIELD-SYMBOL(<ls_comp>).
    "Qui ho le colonne della tabella
  ENDLOOP.
  
ENDFORM.
  
```
<h1>Eventi</h1>
Capita si debba sollevare un evento in modo statico da codice (da una badi o da un'altro metodo). Si deve ricorrere quindi ad una classe
standard per fare il RAISE.

```abap
 DATA : lv_objtype          TYPE sibftypeid VALUE 'ZCL_UD_UPDATE_IDOC',
         lv_event            TYPE sibfevent VALUE 'CREATE_IDOC',
         lv_param_name       TYPE swfdname,
         wf_objkey           TYPE sweinstcou-objkey,
         lr_event_parameters TYPE REF TO if_swf_ifs_parameter_container.

  " Chiamo il metodo della classe per ottenere un container dell'evento
  CALL METHOD cl_swf_evt_event=>get_event_container
    EXPORTING
      im_objcateg  = cl_swf_evt_event=>mc_objcateg_cl
      im_objtype   = lv_objtype
      im_event     = lv_event
    RECEIVING
      re_reference = lr_event_parameters. " Parametri dell'evento che si vuole chiamare

  " Setto i parametri dell'evento che voglio chiamare
  TRY.
      lr_event_parameters->set(
        EXPORTING
          name                          = 'I_LOTTO'
          value                         = new_insplot
      ).

    CATCH cx_swf_cnt_cont_access_denied.    "
    CATCH cx_swf_cnt_elem_access_denied.    "
    CATCH cx_swf_cnt_elem_not_found.    "
    CATCH cx_swf_cnt_elem_type_conflict.    "
    CATCH cx_swf_cnt_unit_type_conflict.    "
    CATCH cx_swf_cnt_elem_def_invalid.    "
    CATCH cx_swf_cnt_container.    "
  ENDTRY.

  " Sollevo l'evento
  try.
        call method cl_swf_evt_event=>raise_in_update_task
          exporting
            im_objcateg        = cl_swf_evt_event=>mc_objcateg_cl
            im_objtype         = lv_objtype
            im_event           = lv_event
            im_objkey          = wf_objkey
            im_event_container = lr_event_parameters.
      catch cx_swf_evt_invalid_objtype .
      catch cx_swf_evt_invalid_event .
    endtry.
```
<br>
<br>
<b>Attenzione:</b><br>
Il RAISE non è obbligatorio farlo scattare con il metodo <i>raise_in_update_task</i> ma dipende dal fattore di sincronizzazione del raise


<h1>JSON</h1>

E' possibile utilizzare la classe */UI2/CL_JSON* per trasformare oggetti abap in json e viceversa.

```abap
DATA: lt_flight TYPE STANDARD TABLE OF sflight,
      lrf_descr TYPE REF TO cl_abap_typedescr,
      lv_json   TYPE string.
 
  
SELECT * FROM sflight INTO TABLE lt_flight.
  
" serialize table lt_flight into JSON, skipping initial fields and converting ABAP field names into camelCase<br><br>
lv_json = /ui2/cl_json=>serialize( data = lt_flight compress = abap_true pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).
WRITE / lv_json.
 
CLEAR lt_flight.
  
" deserialize JSON string json into internal table lt_flight doing camelCase to ABAP like field name mapping<br><br>
/ui2/cl_json=>deserialize( EXPORTING json = lv_json pretty_name = /ui2/cl_json=>pretty_mode-camel_case CHANGING data = lt_flight ).
 
" serialize ABAP object into JSON string <br><br>
lrf_descr = cl_abap_typedescr=>describe_by_data( lt_flight ).
lv_json = /ui2/cl_json=>serialize( lrf_descr ).
WRITE / lv_json.
```

