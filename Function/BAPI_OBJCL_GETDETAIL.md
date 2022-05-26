<h1>BAPI_OBJCL_GETDETAIL</h1>
Estrarre il valore di una caratteristica di una classificazione definita    

``` abap
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(OBJECTKEY) LIKE  BAPI1003_KEY-OBJECT OPTIONAL
*"     VALUE(OBJECTTABLE) LIKE  BAPI1003_KEY-OBJECTTABLE
*"     VALUE(CLASSNUM) LIKE  BAPI1003_KEY-CLASSNUM
*"     VALUE(CLASSTYPE) LIKE  BAPI1003_KEY-CLASSTYPE
*"     VALUE(KEYDATE) LIKE  BAPI1003_KEY-KEYDATE DEFAULT SY-DATUM
*"     VALUE(UNVALUATED_CHARS) TYPE  FLAG DEFAULT SPACE
*"     VALUE(LANGUAGE) LIKE  BAPIFIELDSCACL-BAPILANGUA DEFAULT SY-LANGU
*"     VALUE(OBJECTKEY_LONG) LIKE  BAPI1003_KEY-OBJECT_LONG OPTIONAL
*"  EXPORTING
*"     VALUE(STATUS) LIKE  BAPI1003_KEY-STATUS
*"     VALUE(STANDARDCLASS) LIKE  BAPI1003_KEY-STDCLASS
*"  TABLES
*"      ALLOCVALUESNUM STRUCTURE  BAPI1003_ALLOC_VALUES_NUM
*"      ALLOCVALUESCHAR STRUCTURE  BAPI1003_ALLOC_VALUES_CHAR
*"      ALLOCVALUESCURR STRUCTURE  BAPI1003_ALLOC_VALUES_CURR
*"      RETURN STRUCTURE  BAPIRET2

    DATA: lt_valnum  TYPE TABLE OF bapi1003_alloc_values_num,
          lt_valchar TYPE TABLE OF bapi1003_alloc_values_char,
          lt_valcurr TYPE TABLE OF bapi1003_alloc_values_curr,
          lt_return  TYPE TABLE OF bapiret2,
          ls_bapi1003_key TYPE bapi1003_key.

    ls_bapi1003_key-object = ls_data-matnr.
    ls_bapi1003_key-objecttable = 'MARA'.
    ls_bapi1003_key-classnum = 'MATERIALICLIENTE'.
    ls_bapi1003_key-classtype = '001'.

    CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
      EXPORTING
        objectkey       = ls_bapi1003_key-object
        objecttable     = ls_bapi1003_key-objecttable
        classnum        = ls_bapi1003_key-classnum
        classtype       = ls_bapi1003_key-classtype
        keydate         = sy-datum
      TABLES
        allocvaluesnum  = lt_valnum
        allocvalueschar = lt_valchar
        allocvaluescurr = lt_valcurr
        return          = lt_return.
``` 
