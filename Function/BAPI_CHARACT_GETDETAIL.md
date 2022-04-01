<h1>BAPI_CHARACT_GETDETAIL</h1>    
Estrarre i dati di una caratteristica (descrizione, valori possibili, ecc...)

```abap

*"  IMPORTING
*"     VALUE(CHARACTNAME) LIKE  BAPICHARACTKEY-CHARACTNAME
*"     VALUE(KEYDATE) LIKE  BAPICHARACTKEY-KEYDATE DEFAULT SY-DATUM
*"     VALUE(LANGUAGE) LIKE  BAPIFIELDSCACL-BAPILANGUA OPTIONAL
*"  EXPORTING
*"     VALUE(CHARACTDETAIL) LIKE  BAPICHARACTDETAIL STRUCTURE
*"        BAPICHARACTDETAIL
*"  TABLES
*"      CHARACTDESCR STRUCTURE  BAPICHARACTDESCR OPTIONAL
*"      CHARACTVALUESNUM STRUCTURE  BAPICHARACTVALUESNUM OPTIONAL
*"      CHARACTVALUESCHAR STRUCTURE  BAPICHARACTVALUESCHAR OPTIONAL
*"      CHARACTVALUESCURR STRUCTURE  BAPICHARACTVALUESCURR OPTIONAL
*"      CHARACTVALUESDESCR STRUCTURE  BAPICHARACTVALUESDESCR OPTIONAL
*"      CHARACTREFERENCES STRUCTURE  BAPICHARACTREFERENCES OPTIONAL
*"      CHARACTRESTRICTIONS STRUCTURE  BAPICHARACTRESTRICTIONS OPTIONAL
*"      RETURN STRUCTURE  BAPIRET2


CALL FUNCTION 'BAPI_CHARACT_GETDETAIL'
    EXPORTING
      charactname         =  <fs_descr_char>-name_char                " Characteristic Name
*      keydate             = SY-DATUM         " Date
*      language            =                  " Language: Empty = All Languages
*    IMPORTING
*      charactdetail       =                  " Characteristic Attributes
    TABLES
*      charactdescr        =                  " Characteristic Descriptions
*      charactvaluesnum    =                  " Allowed Values for NUM Characteristics
*      charactvalueschar   =                  " Allowed Values for CHAR Characteristics
*      charactvaluescurr   =                  " Allowed Values for CURR Characteristics
*      charactvaluesdescr  =                  " Value Descriptions
*      charactreferences   =                  " Entries for Reference Characteristics
*      charactrestrictions =                  " Restrictions to Class Types
      return              =     lt_return               " Error Messages
    .
```
