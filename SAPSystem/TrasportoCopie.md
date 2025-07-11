**Transport of Copies**

To transport an object to another system, go to *SE10* and create a CR (Change Request) for a "transport of copies." Once created, select the "Include Objects" box (Ctrl + F11) and enter what you want to transport. After including the objects, set the virtual destination to **VIR** and release it.

---

**Download CR**

Launch transaction *CG3Y*.
Download the files from the following paths:
* \\sapdev\sap\trans\data\R(cr_number).(system_id)
* \\sapdev\sap\trans\cofiles\K(cr_number).(system_id)

These files must be saved with the extension (system_id).

---

**Upload CR**

Launch transaction *CG3Z*.
Upload the R(cr_number).(system_id) and K(cr_number).(system_id) files into the `data` and `cofiles` folders, respectively, of the target system.
Once the files are imported, launch transaction *STMS*, select the development system, and choose *Options > Other requests > Add*. Search for your CR, which will be named (source_system_id)(K)(source_cr_number). Select it and import it.

> **NB**
> The paths are not the same for every system. Find the *TRANS* folder using transaction *AL11* to locate the two relevant folders (`data` and `cofiles`).