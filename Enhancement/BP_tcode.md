# BP Enhancement

This document will show how to implement a custom screen in the standard **BP** transaction by following a practical example.

---

### Creating a Function Group

First, go to **SE80** and create a screen containing the fields to be displayed.
![dynpro setting](https://github.com/user-attachments/assets/7ce21b65-3246-449f-8141-ad785bd35c6c)
![2](https://github.com/user-attachments/assets/46c7a13c-f297-45e6-9df7-9f066d56119b)

Then, create two function modules that will constitute the **PBO** (Process Before Output) and **PAI** (Process After Input).

---

### BP Transaction Information

Once the screen is created, we need to get information from the standard transaction. Go to **BP** and find the section where you want to insert the new tab:
![bp](https://github.com/user-attachments/assets/082799f5-3091-4b89-8ccb-7391b0171844)

Once in the standard transaction, launch **BDT_ANALYZER** to understand which view we need to hook into.
![bp ana](https://github.com/user-attachments/assets/47e86949-2871-4773-a630-ce1fea2f44fe)

---

### CMDS Structure

Go to **SE11** and search for the structure **CMDS_EI_CMD_CENTRAL**:
Then, add an append (including the custom field) to the two structures it contains (**data** and **datax**).
![cmd](https://github.com/user-attachments/assets/60e4a60f-e9ba-424d-a799-4ced8a27eead)
