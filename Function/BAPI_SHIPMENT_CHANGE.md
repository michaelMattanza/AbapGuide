Structure HEADERDEADLINE must be filled in accordance with the following rule:
TIME_TYPE = logical time --> to be filled with fixed values:
TIME_TYPE = HDRSTPLDT --> Date/time of end of planning
TIME_TYPE = HDRSTCIPDT --> PLANNED date/time of check-in
TIME_TYPE = HDRSTCIADT --> ACTUAL date/time of check-in
TIME_TYPE = HDRSTLSPDT --> PLANNED date/time of loading start
TIME_TYPE = HDRSTLSADT --> ACTUAL date/time of loading start
TIME_TYPE = HDRSTLEPDT --> PLANNED date/time of loading end
TIME_TYPE = HDRSTLEADT --> ACTUAL date/time of loading end
TIME_TYPE = HDRSTCPDT --> PLANNED date/time of shipment completion
TIME_TYPE = HDRSTCADT --> ACTUAL date/time of shipment completion
TIME_TYPE = HDRSTSSPDT --> PLANNED date/time of shipment start
TIME_TYPE = HDRSTSSADT --> ACTUAL date/time of shipment start
TIME_TYPE = HDRSTSEPDT --> PLANNED date/time of shipment end
TIME_TYPE = HDRSTSEADT --> ACTUAL date/time of shipment end

```abap
ls_headerdata = VALUE #( shipment_num = get_cont ).

        APPEND VALUE #( delivery = created_delv ) TO lt_items.

        APPEND VALUE #( delivery  = 'A'
                        itenerary = 'A'  ) TO lt_itemdataaction.

        CALL FUNCTION 'BAPI_SHIPMENT_CHANGE'
          EXPORTING
            headerdata       = ls_headerdata
            headerdataaction = ls_headerdataaction
          TABLES
            itemdata         = lt_items
            itemdataaction   = lt_itemdataaction
            return           = lt_return1.
```
