# Sales area auth

```abap
AUTHORITY-CHECK OBJECT 'V_VBAK_VKO'
        FOR USER lv_uname
        ID 'ACTVT' FIELD '03'
        ID 'VKORG' FIELD ls_tvta-vkorg
        ID 'VTWEG' FIELD ls_tvta-vtweg
        ID 'SPART' FIELD ls_tvta-spart.
```
