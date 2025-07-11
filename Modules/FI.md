# SAP FI Module

The **FI (Financial Accounting)** module is used to manage accounting related to payments, incoming and outgoing accounting records, assets, and bank accounts. It's often linked to the **CO (Controlling)** module. The purpose of these modules is to record every financial transaction within a specific period.

---

## Chart of Accounts

The **chart of accounts** reports management and financial statements. The records in this chart of accounts can be either credit or debit.

---

## Company

A **company** is the organizational unit for which financial statements can be prepared according to relevant commercial law. A company can include multiple company codes.

---

## BTE (Business Transaction Events)

**BTEs (Business Transaction Events)** are enhancement techniques developed for the FI module. BTEs utilize interfaces created by developers, linked to an event code in the configuration tables. They are used for business requirements where standard functionalities don't exist and can be accessed via transaction **FIBF**.

### Implementing a BTE

* Go to **FIBF -> Environment -> Infosystem (Processes)** to find a list of implementable interfaces, each with its own process.
* Select the process and press the "Sample Function Module" button to create a custom function module based on a standard function module suitable for the process.
* Once the code is inserted into the function module, return to **FIBF** and, in **Settings**, link the process with our function module in the correct section (Process, etc.).

*See external links*

---

## Accounts Receivable

**Accounts Receivable** in SAP FICO is a sub-module that captures all transactions with customers and manages customer accounts. Transactions in accounts receivable include recording invoices, recording credit memos, down payments, invoice payments, dunning (reminders for overdue payments), and running customer reports.

---

## Accounts Payable

**Accounts Payable** is a sub-module that captures all transactions with vendors and manages vendor accounts. Separate vendor accounts are maintained, and when transactions are recorded in customer accounts, reconciliation accounts in the general ledger are updated with real-time figures. Transactions in accounts payable include recording invoices, recording credit memos, down payments, invoice payments, the automatic payment program, and running vendor reports.

---

## Asset Accounting

**Asset Accounting** in SAP FICO manages all asset-related transactions for an entity. When transactions are recorded in asset accounts, reconciliation accounts in the general ledger are updated in real-time. Operations in asset accounting include asset acquisition, retirement, sale, transfer, revaluation, and depreciation.

---

## Bank Accounting

**Bank Accounting** captures all transactions with banks. Bank reconciliation is performed to reconcile all transactions recorded on bank statements by comparing them with transactions in the system.

All SAP FI sub-modules are integrated, and transactions are updated in real-time, meaning you can extract accurate financial statements from the system at any time.

---

## SAP Funds Management

This SAP accounting module supports all related activities in creating and managing budgets. The calculation of revenues, expenses, and funds is included in the activity lists of SAP Funds Management.

---

## SAP Travel Management

This SAP module accounts for all transactions related to business trips organized within and by the organization. Approvals, bookings, settlements, and various travel expenses are recorded and managed using the SAP Travel Management module.
