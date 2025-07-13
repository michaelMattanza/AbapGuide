Tables are used for the permanent storage of data. There can be **standard tables** (e.g., EKKO, EKPO) and **Z tables**, which are used to save custom data.

---

### Creating a Z Table

Launch transaction *SE11* and enter the desired table name in the *Database table* field, then click *Create*. Enter the column names and data types, along with a description.

> **NB**
> Currency and quantity fields must refer to another field.

Once the table is created, go to *Utilities -> Table maintenance generator* to create the maintenance view, which allows you to interact with the data through *SM30*.

---

### Standard View Settings

To generate the view, a function group must be created (it doesn't matter if you create a new one or use an existing one).
On the view generation screen, enter:

**Authorization Group: &NC&**
**Maintenance Type: One-step**
**Overview Screen: 1**

---

### View Layout Settings

To modify the view layout, so it doesn't appear compressed, go to *Environment -> Modification -> Maintenance Screens*.

**Rows and Columns: 200 and 250**

During the various processes, you will be prompted multiple times for a CR (Change Request), to save, and to activate the various elements.

*See external links for more details.*