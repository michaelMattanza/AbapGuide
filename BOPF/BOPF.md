# Business Object Processing Framework

**BOPF** is a framework based on ABAP Object-Oriented principles. It provides a set of services and functionalities to accelerate, standardize, and modularize developments. It is used to manage the entire development lifecycle in all aspects, such as managing and adapting various components (e.g., Dynpro UI) to the infrastructure on which they are implemented.

**BOPF Elements**
Each business object in BOPF represents a node in a hierarchical tree. The collection of these nodes forms the BOPF. Each node includes an object with its own implementation logic and a dictionary table, where each instance of the node corresponds to a record in the table. This record links the implemented object to the logic associated with the node type.
Every BOPF object is recognized by its GUID, but it is possible to define other semantically relevant attributes for unique identification.

Each object has its attributes with the possibility of making them visible, modifiable, and mandatory.

**Composition of a Business Object**
*Nodes*
Nodes are used to model the BO. They are hierarchically organized under a main node (XML-like).
There are different types of nodes, but persistent ones are mainly used. There can also be transient nodes, loaded at runtime.
Each node contains one or more attributes that define the data contained within the node itself and vary according to the node type. At runtime, a node is a container with n rows (0 < n < x).

*Actions*
They define the BO's actions and are assigned to the individual node within the BO. The functionality provided by an action is usually defined in a class that implements the interface `/BOBF/IF_FRW_ACTION`.
An action takes place in 3 main steps:
- reading the entity (retrieve method)
- executing the action -> (creation of orders, data to DB, etc.)
- updating the entity based on the action's result (update method)

*Determinations*
Determinations enable the automatic population of certain attributes. They are triggered by specific events and are implemented in the `/bobf/if_frw_determination-execute` method defined in the determination class.

*Validations*
Validations allow for checking the correctness of values assigned to certain attributes and can be triggered by specific events `/bobf/if_frw_determination-execute` defined in the validation class.

*Associations*
Although BOs are created as standalone entities, they can be directly or indirectly related to each other.
This allows them to interact to create complex assemblies (e.g., order header and items of the same order).

*VDM*
The Virtual Data Model is an abstraction of the data layer structured into specific views necessary for Fiori application development. The VDM is composed of Views, Interfaces, and Data.
