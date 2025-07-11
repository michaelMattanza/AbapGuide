# Exits

Exits are provided by SAP to allow modification of standard behavior without changing the source code. Various types of exits exist (such as function, menu, screen, etc.), which can be activated via **CMOD** (tcode for exit groups) and **SMOD** (tcode for individual exits).

### User Exit

A **user exit** is a standard include where a user can insert code to modify specific data within a standard flow.

---

### Customer Exit

A **customer exit** is a function created by SAP to call custom code within a standard flow. The code must be inserted into the include it contains (which is the user exit).

**SXX:** Standard SAP exits.
**UXX:** User exits defined by users.

List of Exits:
- EXIT_SAPLV56K_002 (Idoc DESADV - DELVRY07 Exit)