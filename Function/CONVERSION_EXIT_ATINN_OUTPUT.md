<h1>CONVERSION_EXIT_ATINN_OUTPUT</h1>   
Estrarre il nome di una caratteristica   
``` abap
*"*"Lokale Schnittstelle:
*"       IMPORTING
*"             VALUE(INPUT)
*"       EXPORTING
*"             VALUE(OUTPUT)

  CALL FUNCTION 'CONVERSION_EXIT_ATINN_OUTPUT' "Convert internal char. number to char. name  
  EXPORTING
    input =  lv_atinn                     " Internal Characteristic Number               
  IMPORTING
    output = 'Nome car'                   " Characteristic Name              
    .
```