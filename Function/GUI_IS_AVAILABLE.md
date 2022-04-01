<h1>GUI_IS_AVAILABLE</h1>
Per controllare che un programma sia eseguito in background o in foreground è possibile utilizzare questa function.   
 ```abap
*"*"Local Interface:
*"  EXPORTING
*"     REFERENCE(RETURN) TYPE  C

 CALL FUNCTION 'GUI_IS_AVAILABLE'
    IMPORTING
      return = lv_gui_is_available.   
```