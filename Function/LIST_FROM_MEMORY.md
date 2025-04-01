Read submit output after 'SUBMIT report EXPORTING LIST TO MEMORY AND RETURN'.

CALL FUNCTION 'LIST_FROM_MEMORY'
    TABLES
      listobject = lt_list_tab
    EXCEPTIONS
      not_found = 1
    OTHERS = 2.
