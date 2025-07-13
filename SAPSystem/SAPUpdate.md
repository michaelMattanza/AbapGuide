**SPAU and SPDD**

These are transactions used for a major SAP package upgrade. They serve to update both dictionary objects and programs through either a "reset to standard" function or manual modification, which adapts the modified object to ensure it is consistent and doesn't cause issues.

---

### SPDD

**SPDD** is a transaction used after a system upgrade. It checks the consistency of dictionary objects and allows you to decide whether to keep them or reset them. In the case of notes, they are usually reset. ALL objects must be processed.

---

### SPAU

**SPAU** is a transaction used to check the consistency of custom code. Like SPDD, you can decide whether to reset it or keep it.
In SPAU, there are 5/6 sections:

* **Notes:** Shows all notes and asks what you want to do with them -> select all -> "Prepare Notes" (updates all notes) -> if "Prepare Notes" doesn't work, select all and click on "Compute adjustment mode."

* **With Assistant:** Things modified with the assistant -> if the modification is unclear, you must ASK. Once restored or modifications applied, the changes must be activated. Reset if changes are useless (like note texts). Documentation is usually reset.

* **Without Assistant**

* **Deletion:** Delete modification log for almost everything (check custom objects).

* **Translations:** Allow all of them to pass.

---

**NB**

* There is **SPAU_ENH** for maintaining enhancements.
* Always create only one CR (Change Request) per transaction.
* Keep track of what's been done in a file where you log the modifications.
* Perform updates in English, using client 000.

*See external links for more details.*