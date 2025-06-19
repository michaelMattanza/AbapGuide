<h1>RC1_IDOC_SET_STATUS</h1>   

```abap 
CALL FUNCTION 'RC1_IDOC_SET_STATUS'
  EXPORTING
  idoc_number = '1234567890' " Provide your IDoc number
  new_status = '68' " New status to set
  EXCEPTIONS
  others = 1
```
