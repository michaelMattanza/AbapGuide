# GENERAL DEFINITION

A **CDS view** is an infrastructure that defines a data model (similar to a simple view). This data model is created in the database and not on the server. This advantage stems from the fact that a normal view resides in the dictionary and must create a DB connection via the server for data extraction. A CDS view is a simple 'object' that resides directly in the DB and therefore does not require a connection to be managed with it. There are CDS VIEWS with and without parameters, ABAP CDS, and HANA CDS. When selecting fields to extract, keys can be defined by prefixing the field with the keyword `key` (keys must be contiguous and start from the first field).

# ODATA SERVICE

CDS Views can also be used to expose an **OData service** (otherwise creatable via transaction SEGW) through the annotation `@OData.publish: true`. Once the view is created, use transaction `/IWFND/MAINT_SERVICE` to register the service. Test the service using transaction `/IWFND/GW_CLIENT`.
There might be errors: https://sapyard.com/odata-service-from-cds-annotation-not-working-in-browser-mode/

# S4 CLOUD

CDS Views are necessary for extracting data from tables in **S4 CLOUD**. Standard CDS views are provided, and custom CDS views can be created from these (if you possess the `BR_ADMINISTRATOR` role).

# AUTHORIZATIONS

When a CDS view is created, it has the annotation `@AccessControl.authorizationCheck: #NOT_REQUIRED` to indicate that no particular privileges are needed to use it. An **Access Control** must therefore be created. This access control can be used in 3 different ways:

* **Full access:** No restrictions are applied. Only the view name is entered in the Access Control.
* **Literal Conditions:** Extraction conditions are imposed to limit the data that is returned.
* **PFCG (further details in external links):** A role is created that allows users to use only certain functions.

Other methods exist, such as the combination of literal and PFCG, inherited authorizations, and current user authorization.

# TRANSLATIONS AND TEXTS

In CDS views, texts can be created: `label` (with a maximum of 60 characters) and `quickinfo` (strings).
They are declared with the "@" symbol:
```abap
@EndUserText.label: 'Test'
@EndUserText.quickInfo: 'Second test'
```
These texts can then be translated with transaction SE63, and these translations can be included in a transport with transaction SLXT.
It is possible to access texts and translations dynamically using the class CL_DD_DDL_ANNOTATION_SERVICE method GET_LABEL_4_ELEMENT. With this method, you can specify the language in which you want the texts or allow it to pick the access language (without specifying it).
</br>
</br>
<b>BASIC SYNTAX</b></br>
CDS views are parameterized, meaning they also accept input variables (CDS views are case-sensitive). It is important to remember that they also work through aliases, especially when performing join associations. System variables also exist here:

| Variabile          | Descrizione                                                                 |
|--------------------|-----------------------------------------------------------------------------|
| user               | Corresponds to the sy-uname variable in ABAP. Contains the user name        |
| client             | Corresponds to the sy-mandt variable in ABAP. Contains the client    |               
| system_language    | Corresponds to the sy-langu variable in ABAP. Language used by the system  |
| system_date        | Corresponds to the sy-datum variable in ABAP. Contains the current date      |

- <i>Case distinction</i>
    - CASE a IS NULL THEN 'err'

- <i>Condition</i>
    - a = b
    - a <> b
    - a > b
    - a < b
    - a >= b
    - a <= b
    - a <= n AND b > n2 (per intervalli)
    - a LIKE b
    - a IS [NOT] NULL
    
- <i>Arithmetic expressions</i>

- <i>Aggregate expressions</i>
    - MAX
    - MIN
    - AVG
    - SUM
    - COUNT
    - GROUP BY (mandatory)
    - HAVING
    
- <i>Casting expressions</i>

| Variable               | Description                                                                 |
|-------------------------|-----------------------------------------------------------------------------|
| abap.char( len )        | CHAR with length len                                                        |
| abap.clnt[(3)]          | CLNT                                                                        |               
| abap.cuky( len )        | CHAR with length len                                                        |
| abap.curr(len,decimals) | CURR with length len and decimals decimal places                            |
| abap.dats[(8)]          | DATS                                                                        |
| abap.dec(len,decimals)  | DEC with length len and decimals decimal places                             |
| abap.fltp[(16,16)]	  | FLTP                                                                        |
| abap.int1[(3)]	      | INT1                                                                        |
| abap.int2[(5)]	      | INT2                                                                        |
| abap.int4[(10)]         | INT4                                                                        |
| abap.int8[(19)]         | INT8                                                                        |
| abap.lang[(1)]          | LANG                                                                        |
| abap.numc( len )        | NUMC with length len                                                        |
| abap.quan(len,decimals) | QUAN with length len with decimals decimal places                           |
| abap.raw(len)           | RAW                                                                         |
| abap.sstring(len)       | SSTRING                                                                     |
| abap.tims[(6)]          | TIMS                                                                        |
| abap.unit( len )        | CHAR with length len                                                        |

</br>
</br>
 - Note
    - <b>System access:</b> (privilege) for using certain functions
    - <b>Authorization Objects:</b> These are certain transactions that we can access or not.
    - <b>DCL:</b> Data Control Language. Used to control privileges for database operations
    - <b>GRANT:</b> Used to indicate every privilege for a user
    - <b>REVOKE:</b> Used to remove privileges
    - <b>ROLE:</b> Special permissions for specific functions. Maintained by transaction <i>PFCG</i>.
