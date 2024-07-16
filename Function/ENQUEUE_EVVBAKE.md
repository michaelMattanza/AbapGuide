# ENQUEUE_EVVBAKE
Function per bloccare Ordine di vendita

```abap
CALL FUNCTION 'ENQUEUE_EVVBAKE'
    EXPORTING
      vbeln          = iv_vbeln
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2.
```
