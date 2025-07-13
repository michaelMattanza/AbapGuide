<h1>Basic Syntax</h1>

**Declaring Input Variables** </br>

```abap
" Prompts for an obligatory input value that it allocates in memory with ID wrk
PARAMETERS werks TYPE werks_d MEMORY ID wrk OBLIGATORY,
           in_data TYPE sy-datum. 

" Prompts for input ranges 
SELECT-OPTIONS: so_kunnr FOR zstock_b-kunnr,
                so_matnr FOR zstock_b-matnr.
                
INITIALIZATION.
in_data = '20190709'. "Sets 09-07-2019 as the suggested value
```

**Search Help**<br>
A search help is used to find possible values for a specific input field

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


**Declaring Internal Variables** </br>

```abap
" Declare different types of variables following the naming convention:
" - l = local,  g = global, i = import, e = export
" - v = variable, s = structure, t = table, f = field, r = range, ref = reference, o = object
DATA: lv_werks TYPE werks_d,
      ls_ekko  TYPE ekko,
      lt_ekpo  TYPE TABLE OF ekpo. 

" Declare a structure / table type following the naming convention:
" - ty = type, tt = type table
DATA: BEGIN OF ty_example,
      field1 type type1,
      field2 type type2,
      END OF ty_example,
      tt_example TYPE TABLE OF ty_example. " tt_example è un tipo tabella
 DATA: lt_ex TYPE tt_example. " lt_ex è quindi una tabella
      
```
**Grouping Code**

iL *DEFINE* is used to reuse the same lines of code multiple times. DEFINEs cannot be debugged and cannot be used outside the program.

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
You can use *FORM / PERFORM* to use the same code in multiple places. They can also be called from external programs but cannot be created anywhere (e.g., class methods).

```abap
PERFORM OPERATION CHANGING n1 n2 tot.

FORM OPERATION CHANGING  lv_n1 TYPE i
                         lv_n2 TYPE i
                         lv_tot TYPE i.
                         
 lv_tot = lv_n1+ lv_n2.
                         
ENDFORM.
```

**Strings**<br>
When searching for a specific value in a string containing a defined separator, you can quickly get the substring using one of these functions.

```abap
data(result) = segment( val = 'VAL1-VAL2-VAL3' index = 3 sep = '-' ).
data(result) = match( val  = 'VAL1-VAL2-VAL3'  regex = '[^-]*$' ).
data(result) = SubString( Val = 'VAL1-VAL2-VAL3' Off = Find( Val = 'VAL1-VAL2-VAL3' Regex = '-[^-]+$' ) + 1 ).
```
To concatenate variables into a string, use the following syntax:
```abap
CONCATENATE lv_var1 lv_var2 INTO lv_string.

"740
lv_string = |{ lv_var1 }{ lv_var2 }|.
```
Using the 740 syntax, you can also leverage conversion functions.

```abap
" Alignment   
WRITE / |{ 'Left'     WIDTH = 20 ALIGN = LEFT     PAD = '0' }|.   
WRITE / |{ 'Center'   WIDTH = 20 ALIGN = CENTER   PAD = '0' }|.   
WRITE / |{ 'Right'    WIDTH = 20 ALIGN = RIGHT    PAD = '0' }|.   

" Character style   
WRITE / |{ ‘Text’ CASE = (cl_abap_format=>c_raw) }|.   
WRITE / |{ ‘Text’ CASE = (cl_abap_format=>c_upper) }|.   
WRITE / |{ ‘Text’ CASE = (cl_abap_format=>c_lower) }|.   

" Numeric conversion   
DATA(lv_vbeln) = ‘0000012345’.   
WRITE / |{ lv_vbeln  ALPHA = OUT }|.     “or use ALPHA = IN to go in   

" Date conversion   
WRITE / |{ pa_date DATE = ISO }|.           “Date Format YYYY-MM-DD   
WRITE / |{ pa_date DATE = User }|.          “As per user settings   
WRITE / |{ pa_date DATE = Environment }|.   “Formatting setting of   

" Quantity conversion    
DATA(lv_qty) = 13.000.

WRITE lv_menge TO lv_menge_char LEFT-JUSTIFIED UNIT lv_meinh. " if lv_meinh == udm without decimals (e.g., pieces) -> lv_menge_char = 13
```
<h1>Query</h1>

**Select single:** extracts only one value
 ```abap
SELECT SINGLE field1, field2 
  FROM table INTO variable
  WHERE condition.
  
SELECT SINGLE field1, field2 
  FROM table INTO @DATA(variable)
  WHERE condition.
```

**Info**    
*SELECT SINGLE* extracts the first record from the database that matches the conditions defined in the query, while *SELECT UP TO 1 ROWS* extracts all records that match the WHERE condition, but only the first will be displayed in the final result. However, the second case, before returning the result, performs aggregation and grouping operations to return the result that best suits the search conditions; it is therefore recommended for extractions without a complete primary key.

**Select count:** counts the rows of a select
```abap
SELECT COUNT(*) 
  FROM table
  WHERE condition.
 " sy-dbcnt contains the counter
  
SELECT COUNT( DISTINT field1 )
  FROM table INTO variable
  WHERE   condition.
```

**Select into table:** extracts multiple values
```abap
SELECT field1 field2 
  FROM table INTO TABLE tab
  WHERE condition.
  
SELECT field1 field2
  FROM table INTO CORRESPONDING FIELDS OF TABLE tab
  WHERE condition.
  
SELECT field1 field2
  FROM table INTO TABLE tab
  FOR ALL ENTRIES IN tab2
  WHERE condition.
  
 SELECT field1, field2
  FROM table INTO TABLE @DATA(tab)
  WHERE condition.
  
*  In the WHERE condition, '<> @( VALUE #( ) )' can also be used to indicate an empty value. 
```
**For All Entries**   
*For all entries*  used to extract data for each record of an internal table.   
DO NOT PUT SELECTS IN LOOPS!  
The limitation of SELECT with FOR ALL ENTRIES is that many grouping functions and some keywords of the new syntax are not usable. If you are using an updated system (SAP BASIS 752), it is possible to avoid FOR ALL ENTRIES (for more details, see external links):
```abap
SELECT field1 field2
  FROM table INTO CORRESPONDING FIELDS OF TABLE tab
  WHERE condition.
  
" For all entries
SELECT field1 field2
  FROM table INTO TABLE tab2
  FOR ALL ENTRIES IN tab
  WHERE field1 = tab-field1.
  
 " No for all entries
 SELECT field1 field2
  FROM table INTO TABLE tab2
  WHERE field1 IN ( select field1 from @tab ).
  
```   
**From SELECT to RANGE:** extract multiple values
```abap
  SELECT sign opt AS option low high FROM tvarvc
         INTO CORRESPONDING FIELDS OF TABLE itab_tipo_range
         WHERE name EQ nomezfunz.
  
*  In the WHERE condition, '<> @( VALUE #( ) )' can also be used to indicate an empty value. @space is preferable.
```
**ATTENTION:**
> For queries, there are two syntaxes: the old syntax which does not require a comma between the fields to be selected, and the new syntax which does. In the new syntax, an "@" symbol must be placed before report variables (i.e., not dictionary fields or table names), and inline variable or table declaration (@DATA(name)) is allowed.
  
> Difference between **INTO TABLE** and **INTO CORRESPONDING FIELD OF TABLE**. The first extracts data and places it in the destination table in the order of extraction (so the table must have the correct structure and names), while in the second case, the query inserts the extracted values from the database into the column that corresponds to the extracted data, if the column is found.

<h1>Tabelle interne - Lettura valori</h1>   

**Internal Tables - Reading Values**

*ATTENTION*
- *sy-index*: index contained in DO and WHILE loops
- *sy-tabix*: index in a table loop

```abap
" Declare a structure type
TYPES: BEGIN OF ty_example,
    matnr TYPE matnr,
    mtart TYPE mtart,
    END OF ty_example.

DATA : it_example TYPE TABLE OF ty_example, " Instantiate a table of type ty_example
       wa_example TYPE ty_example.          " Instantiate a structure of type ty_example
 
```
You can use 3 different methods to extract data from tables:

- Using the structure as a table row
```abap
loop at it_example into wa_example.
    write:/ wa_example-matnr .
    write:/ wa_example-mtart .
endloop.
```

- Using field-symbols as pointers. 
```abap
loop at it_example assigning field-symbol(<fs_example>).
    write:/ <fs_example>-matnr .
    write:/ <fs_example>-mtart .
endloop.
```

- Using field-symbols as pointers when a table is dynamic (i.e., there is no fixed structure but it is generated by functions).

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

There is an alternative: references (preferable for read table).
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

If you want to read a row or the value of a row without modifying its value, it's recommended to use inline controls:
```abap
DATA(ls_example) = VALUE #( it_example[ matnr = '22000000' ] OPTIONAL ).
" If the row does not exist, an empty structure is created.

DATA(lv_mtart) = VALUE #( it_example[ matnr = '22000000' ]-mtart OPTIONAL ).
DATA(lv_mtart) = VALUE #( it_example[ matnr = '22000000' ]-mtart DEFAULT def ).
" If the row does not exist, the variable is created empty. In the second case, a default value is assigned if the sought value is empty.
```
<h1>Internal Tables - Writing Values</h1>   

**Move-corresponding** <br>   
MOVE-CORRESPONDING is useful for moving rows from one table to a destination table, with row type conversion based on the data being moved. When adding rows to a table that already contains records, MOVE-CORRESPONDING would overwrite the existing records.
You can bypass this problem with the syntax: 
```abap
gt_outtab = CORRESPONDING #( BASE ( gt_outtab ) lt_tmp_out ).
```

**Inserting New Values**   
There are various ways to insert a row of values into an internal table, and they vary depending on the requirement.      
```abap
DATA: lt_mara TYPE TABLE OF mara.

" Insert a row at the end   
APPEND VALUE #( matnr = '123' ) TO lt_mara.   

" Insert a row at a specific position   
INSERT VALUE #( matnr = '123') INTO TABLE lt_mara INDEX n.

" Insert the first row   
lt_mara = ( ( matnr = '123' ) ).
```

<h1>Internal Tables - Sorting and Value Management</h1>   

**Table Grouping**   
If I need to perform a GROUP BY on a table based on various columns, I can use the FOR loop to generate a table containing the grouping.   

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

  **Loop with GROUP BY**    
Questo ciclo permette di raggruppare in loop un determinato gruppo di valori secondo una chiave      

This loop allows you to group a specific set of values in a loop according to a key.

- GROUP SIZE - Number of rows in the internal table for the particular group key.

- GROUP INDEX - Index of the group from Main loop iteration.

- REFERENCE INTO DATA - Assigning the group data into a separate internal table to access inside the LOOP GROUP.

- ASCENDING / DESCENDING - Sort the groups by the group key in ascending or descending order before the group loop is executed.

- WITHOUT MEMBERS - Constructs groups but there is no access to the rows of the groups in the group loop. If the addition WITHOUT MEMBERS is specified.
The addition WITHOUT MEMBERS is used to improve performance in all cases where the content of the groups is not required.

- LOOP AT GROUP. Group loop to access each row of the group.
Here is my sample code to understand LOOP AT with GROUP BY and some group features.

```abap
TYPES ty_t_mard TYPE SORTED TABLE OF mard WITH UNIQUE KEY matnr werks lgort.
DATA lt_mard TYPE ty_t_mard.


SELECT * FROM mard INTO TABLE lt_mard.
DATA : lv_stock TYPE mard-labst.

LOOP AT lt_mard INTO DATA(wa_mard) GROUP BY ( matnr = wa_mard-matnr
                                              size = GROUP SIZE )
                                              REFERENCE INTO DATA(group1).
  CLEAR : lv_stock.
  WRITE : / group1->matnr , group1->size.

  LOOP AT GROUP group1 ASSIGNING FIELD-SYMBOL(<mard>).
    ADD <mard>-labst TO lv_stock.
  ENDLOOP.
  WRITE :/ 'Total:' ,  lv_stock.
ENDLOOP.
```

  **Counting rows according to defined conditions**
 ```abap
  DATA(lv_lines) = REDUCE i(
      INIT x = 0
      FOR wa IN lt_table WHERE ( matnr IS NOT INITIAL )
      NEXT x = x + 1
    ).
```

 **Getting row number according to defined conditions**
  ```abap
  DATA(lv_index) = line_index( lt_ihpavb[ parvw = 'AG' ] ).
  ```
  
  **Getting number of table rows**
  ```abap
  DATA(lv_index) = lines( lt_ihpavb ).
  ```
To obtain the structure of an internal table (column name and type), you can use classes that help extract this information.

```abap

 FORM get_dynamic_field USING istructure TYPE data.
  "mapping partner
  DATA : lo_ref_descr TYPE REF TO cl_abap_structdescr,
         lt_detail    TYPE abap_compdescr_tab,
         ls_detail    LIKE LINE OF lt_detail,
         ls_c_compcd  TYPE idwtcompcd.

  lo_ref_descr ?= cl_abap_typedescr=>describe_by_data( istructure ). "Call static method on a structure
  lt_detail[] = lo_ref_descr->components.

  LOOP AT lt_detail INTO ls_detail.
    ASSIGN COMPONENT ls_detail-name OF STRUCTURE istructure TO FIELD-SYMBOL(<ls_comp>).
    "Here I have the columns of the table
  ENDLOOP.
  
ENDFORM.
  
```
<h1>Events</h1>
It happens that an event needs to be raised statically from code (from a BADI or another method). One must therefore resort to a standard class to RAISE it.

```abap
 DATA : lv_objtype          TYPE sibftypeid VALUE 'ZCL_UD_UPDATE_IDOC',
         lv_event            TYPE sibfevent VALUE 'CREATE_IDOC',
         lv_param_name       TYPE swfdname,
         wf_objkey           TYPE sweinstcou-objkey,
         lr_event_parameters TYPE REF TO if_swf_ifs_parameter_container.

  " Call the class method to get an event container
  CALL METHOD cl_swf_evt_event=>get_event_container
    EXPORTING
      im_objcateg  = cl_swf_evt_event=>mc_objcateg_cl
      im_objtype   = lv_objtype
      im_event     = lv_event
    RECEIVING
      re_reference = lr_event_parameters. " Parameters of the event to be called

  " Set the parameters of the event I want to call
  TRY.
      lr_event_parameters->set(
        EXPORTING
          name                          = 'I_INSPL'
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

  " Raise the event
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
<b>Attention:</b><br>
The RAISE is not strictly required to be triggered with the raise_in_update_task method, but it depends on the synchronization factor of the raise.

<h1>JSON</h1>

It's possible to use the /UI2/CL_JSON class to transform ABAP objects into JSON and vice versa.

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

