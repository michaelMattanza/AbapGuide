After the function LIST_FROM_MEMORY


call function 'LIST_TO_ASCI'
       tables
            listobject         = lt_list_tab
            listasci           = lt_ascii_data
       exceptions
            empty_list         = 1
            list_index_invalid = 2
            others             = 3.
