<h1>CONVERSION_EXIT_ATINN_INPUT</h1>    
Estrarre il numero interno di una caratteristica
``` abap
*"*"Lokale Schnittstelle:
*"       IMPORTING
*"             VALUE(INPUT)
*"       EXPORTING
*"             VALUE(OUTPUT)

  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT' "Convert char. name to internal char. number
  EXPORTING
    input =  'Nome_car'                   "               Characteristic Name
  IMPORTING
    output = lv_atinn                     "               Internal Characteristic Number
    .
```