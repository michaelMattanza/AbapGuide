# SD Module

This module manages every sale from product registration, customer info, details, pricing, delivery, and billing.

---

## Sales Structure

The organizational structure is composed of:
* **Sales Organization:** defines the company at a logistical level.
* **Distribution Channel:** defines how products are distributed (retail, wholesale, etc.).
* **Division:** Defines the main product categories.

These three parameters define the **sales area**. Depending on the sales area, certain documents may or may not be created.
A customer must have a sales area associated with them to be able to access a product that has been assigned the same sales area in its material master record.

---

## Business Partner

Master data for customers/vendors (they were separate in R/3).
Each master record is divided into three parts: general data, company code data, and sales data.
Every customer has a unique **sold-to party** (themselves) and then three other assigned roles: **payer**, **ship-to party**, and **bill-to party** (1-to-N relationship, e.g., customers with multiple locations).

---

## Incoterms

Mandatory for customers. Defines the limit of supplier/customer responsibility for a product.