# Smartforms

**Smartforms** are print forms structured similarly to PDF forms, with the difference that everything is contained within a single screen. Smartforms can also receive data directly from a report.

```abap
data: l_fm type rs38l_fnam.
call function 'SSF_FUNCTION_MODULE_NAME'
exporting
formname = 'ZSMARTFROM'
importing
fm_name = l_fm.

call function l_fm
exporting
data = ls_data.
```

The graphical composition is different from that of PDF forms, but the underlying concept is very similar. The variable for printing the current page number is <i>SFSY-PAGE</i>.
 
In Smartforms, templates are used (not to be confused graphically with tables) which allow you to "design" a structure to organize data printing.

If a Smartform is executed and goes into error, you can debug the error by going to:

SE37 -> FM SSFRT_READ_ERROR -> SET DEBUG AT LINE 16 -> READ TABLE errortab. After finding the message, compare it with the one in SE91 using the class in msgid and the message number msgno.

<i>See external links for more details.</i>
