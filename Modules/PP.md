# PP Module

The **PP (Production Planning)** module manages the production process of a finished product. It is entirely based on the **Plant** (specific location).
There are 5 main Master Data elements for the PP module.

### Material Master

The **Material Master** contains information on all materials managed and processed by the company. Materials with common characteristics are grouped together (finished, primary, etc.).
It is mainly used for:
* Purchasing material
* Material movements (receipt, issues, etc.)
* Invoicing
* Sales and distribution orders
* Production planning, scheduling, and confirmed process productions

### Bill of Material (BOM)

A **BOM** is a structured and complete list of materials with respective quantities used to produce a product. It is used for material requests and product cost calculation.

### Work Center

A **Work Center** is a machine or a group of machines where operations are performed. Work centers are used in tasks for:
* Scheduling
* Capacity
* Costing

---

## Bill of Material (Distinta Base)

The **Bill of Material** is the set of components necessary for the realization of a material code. It is defined at the plant and usage level. Alternative BOMs can also exist.

---

## Routing (Ciclo di lavoro)

The **routing** defines the processing steps needed to obtain a specific item. It is coded at the material and plant level. During creation, it can be limited to a specific phase (usage), while its status indicates whether it has only been approved or released (general or for a specific scope). Each phase of the routing is identified by a unique number and assigned to a work center from which it inherits processing times (machine/labor). Each phase requires a control key ('YBP1' is a standard value) that drives a series of values.
In each phase, it's possible to define the base quantity required for a component with any unit of measure conversion values.
There are also quality phases where the valued characteristics of a phase are indicated.
A minimum quantity can be associated with each phase; if a production order does not reach this quantity, the phase will not be usable.

---

## Production Version

There can be 'n' variants of bills of material and associated routings. To distinguish which combination is involved in the system, the **production version** is used (mandatory since S/4). It is created based on plant/material code. Each material/plant/production version combination has an associated BOM and routing.

### Routing

A **routing** is a sequence of actions performed in a work center (scheduling). Processing times are also specified.

### Production Version

A **production version** is a link between a BOM and a Routing. There can be multiple process versions for the production of a product.

---

## Production Planning and Control

### Planning

**Planning** is based on the sales plans of a product. With the demand for a product, an input for material request (MRP) is generated, which checks the availability of the various primary materials used during the various production phases of the product through Master Data (e.g., BOM).

### Execution

These planned orders are converted into production orders scheduled for timing and routing. Once the production order has been completed, an order confirmation is created with goods movements and materials used.

---

## Demand Management

**Demand management** is based on the estimation of quantities and delivery dates for the finished product. Demand management utilizes **PIR** (Planned Independent Requirement) and customer requirements.
Planning strategies must be created for each product. This represents the production method. There are two methods for doing this:
* **Made to stock**: Stock is produced independently of sales orders.
* **Made to order**: Material is produced based on generated sales orders.

---

## MRP (Material Requirement Planning)

**MRP** determines shortages and creates procedures to obtain what is needed. It calculates and generates planned orders for in-house production of materials and purchase requisitions for primary material.

It manages scheduling times and calculates production dates in planned orders.

It generates procurement proposals based on the BOM.

---

## Capacity Planning & Leveling

**Capacity planning** is used to analyze the capacity of work centers to handle overload and shift orders to avoid capacity issues (bottlenecks).

Capacity requirements are generated by MRP on work centers, and until work centers operate with infinite capacity and plan everything on the work centers, capacity leveling for work centers is required.

Capacity is set to force production schedules through a planning table.

---

## Production Order

The output of MRP is a planned order that must be converted into a **production order** for the process to continue.
The production order is a fixed receipt element not affected by MRP processing, unlike the planned order.

The production order is a document that contains the materials and quantities that must be produced. It also contains the components and operations to be performed in the work center.

### Production Order Confirmation

When goods are produced, the production order must be confirmed.
During confirmation, materials can be consumed in **Backflush** (BOM withdrawn from stock upon delivery of the finished product), and goods receipt can be done automatically via the Control Key operation in the Routing.

---

## Production Order Close

Once an order is confirmed, it becomes **TECO** (Technically Completed) and is removed from material requirements.