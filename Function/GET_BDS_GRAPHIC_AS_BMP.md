<h1>GET_BDS_GRAPHIC_AS_BMP</h1>
Function per ottenere immagini caricate con SE78
```abap
CALL METHOD cl_ssf_xsf_utilities=>get_bds_graphic_as_bmp
      EXPORTING
        p_object       = 'GRAPHICS'
        p_name         = lv_name " string
        p_id           = 'BMAP'
        p_btype        = 'BCOL'
      RECEIVING
        p_bmp          = lv_bmp " xstring
      EXCEPTIONS
        not_found      = 1
        internal_error = 2
        OTHERS         = 3.
``` 