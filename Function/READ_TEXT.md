<h1>READ_TEXT</h1>     

```abap      
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(CLIENT) LIKE  SY-MANDT DEFAULT SY-MANDT
*"     VALUE(ID) LIKE  THEAD-TDID
*"     VALUE(LANGUAGE) LIKE  THEAD-TDSPRAS
*"     VALUE(NAME) LIKE  THEAD-TDNAME
*"     VALUE(OBJECT) LIKE  THEAD-TDOBJECT
*"     VALUE(ARCHIVE_HANDLE) LIKE  SY-TABIX DEFAULT 0
*"     VALUE(LOCAL_CAT) DEFAULT SPACE
*"  EXPORTING
*"     VALUE(HEADER) LIKE  THEAD STRUCTURE  THEAD
*"     VALUE(OLD_LINE_COUNTER) TYPE  THEAD-TDTXTLINES
*"  TABLES
*"      LINES STRUCTURE  TLINE
*"  EXCEPTIONS
*"      ID
*"      LANGUAGE
*"      NAME
*"      NOT_FOUND
*"      OBJECT
*"      REFERENCE_CHECK
*"      WRONG_ACCESS_TO_ARCHIVE

  CALL FUNCTION 'READ_TEXT'
  EXPORTING
    client                  = SY-MANDT         " Mandante
    id                      = <fs_txtname>-lv_id                 " ID del testo da leggere
    language                = sy-langu                  " Lingua del testo da leggere
    name                    = <fs_txtname>-lv_name                 " Nome del testo da leggere
    object                  = 'TEXT'           " Oggetto del testo da leggere
  TABLES
    lines                   = lt_text          " Righe del testo letto
  EXCEPTIONS
    id                      = 1                " ID testo non valida
    language                = 2                " Lingua non valida
    name                    = 3                " Nome testo non valido
    not_found               = 4                " Testo non trovato
    object                  = 5                " Oggetto testo non valido
    reference_check         = 6                " Catena riferimenti interrotta
    wrong_access_to_archive = 7                " Archive-Handle non consentito per l'accesso
    others                  = 8
  .

DATA(lv_text) =  REDUCE string( INIT init_text TYPE string
                               FOR ls_text IN lt_text
                               NEXT init_text = |{ init_text } { ls_text-tdline }|  ) .
```
