<h1>UNIT_CONVERSION_SIMPLE</h1>
```abap
*"*"Lokale Schnittstelle:
*"       IMPORTING
*"              INPUT
*"              NO_TYPE_CHECK DEFAULT 'X'
*"              ROUND_SIGN DEFAULT SPACE
*"              UNIT_IN LIKE  T006-MSEHI DEFAULT SPACE
*"              UNIT_OUT LIKE  T006-MSEHI DEFAULT SPACE
*"       EXPORTING
*"              ADD_CONST
*"              DECIMALS
*"              DENOMINATOR
*"              NUMERATOR
*"              OUTPUT
*"       EXCEPTIONS
*"              CONVERSION_NOT_FOUND
*"              DIVISION_BY_ZERO
*"              INPUT_INVALID
*"              OUTPUT_INVALID
*"              OVERFLOW
*"              TYPE_INVALID
*"              UNITS_MISSING
*"              UNIT_IN_NOT_FOUND
*"              UNIT_OUT_NOT_FOUND

    CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
      EXPORTING
        input    = slk-btgew  " Weight in LB
        unit_in  = slk-gewei  " LB
        unit_out = vttkvb-dtmeg " KG
      IMPORTING
        output   = slk-btgew. " Output : Weight in KG
```