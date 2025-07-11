# IDoc / EDI

IDocs are the means of data exchange within ALE with a structure similar to an XML file. An IDoc is identified by: a logical message (identification name), a basic type (which defines its internal segments and, in turn, its fields), and an optional extension. They can be inbound or outbound. In SAP, they are saved to the database in various tables. Each generated segment creates a dictionary structure. Inbound IDocs are processed by function modules (linked to the process code) and workflow tasks.

IDocs are divided into three main sections:
* **Control record**: Header and routing data
* **Payload**: Object data
* **Statuses**: Records indicating status changes

The transmission of IDocs can be managed during partner agreement creation and can be:
* Immediate IDoc transmission
* IDoc transmission via job

## Basic Concepts

### ALE

ALE (**A**pplication **L**ink **E**nabling) is an SAP technology for exchanging data between SAP and another system (external or another SAP system):
* **Application Layer**: Tools made available via an interface.
* **Distribution Layer**: Manages filters and conversions based on custom rules. These can also be conversions for adapting to different system versions.
* **Communication Layer**: Manages communication synchronously/asynchronously with other systems.

### EDI

A structure used for the exchange and management of electronic documents between supplier and customer according to standard formats. Documents exchanged via EDI have the same legal validity as paper documents.

### Message Type

Represents the message exchanged between systems and is linked to the IDoc type.

### Message Variant

Represents alternative steps to the original message under certain conditions.

### Message Function

An alternative to a main message. The same object is processed but with something different (e.g., a different language).

### Logical System

The system that sends/receives the IDocs.

### Distribution Model

The distribution model defines the list of logical messages that the system exchanges with a specific logical system. Each logical message has certain filters that describe it.

### Change Pointer

Change pointers are used to propagate a change made in the system to business objects. They intercept data modifications that we can define as "interesting to us" so that only the modification of key data is propagated.

### Partner Agreements

A mandatory customizing object. It is defined for each point-to-point communication. It's like a container where all the message types that can be exchanged inbound/outbound with a specific recipient are specified. It serves to define the routing of a specific inbound/outbound IDoc.

### RFC Destination

Technical information of the receiving system (IP + port + various info).

### Port

Representation of a communication channel (for IDocs, use RFC).

### Process Code

Code that indicates the function module or API to call for IDoc processing.

## Configure and Extend an IDoc - Step by Step

* **SALE**
    * Go to *Basic Settings > Set up Logical Systems > Name Logical System* and enter the logical system name.

* **WE81**
    * Add the message type you want to use for the IDoc.

* **BD56**
    * Add the segments you want to filter, considering sender, receiver, and their respective categories.

* **BD61**
    * Activate change pointers.

* **BD50**
    * Activate change pointers for the message type.

* **BD64**
    * Create a dummy distribution model and add the filter for the created message.

* **WE31**
    * Create the custom segment you want to add to the IDoc. Once created, set the segment as released.

* **WE30**
    * Create an extension by choosing the basic IDoc type to start from. Then add the custom segment created via WE31, setting its mandatory status, min/max number, and level.

* **WE82**
    * Insert a record in this view to link Message Type, IDoc basic type, extension, and release (enter `*`).

* **BD60**
    * Link the custom IDoc message to the reference message and the function module.

* **WE21**
    * Create a port for IDoc processing (usually RFC).

* **WE20**
    * Create a partner agreement for a specific type. Enter the IDoc-related data in the outbound/inbound parameters.

* **Code**
    * Implement a function or an exit to insert data and the custom segment into the IDoc.

## Create IDoc from Change Pointer

Run report **RBDMIDOC** from *SE38*. By entering the message type, the standard report will generate 'n' IDocs based on the changes captured by the Change Pointers according to the records. This report groups IDocs based on the key: if an object has been modified multiple times, only one IDoc will be created.

## Key Tables

* **Edidc**: Header information
* **Edid4**: Payload (may vary depending on the system version)
* **Edids**: IDoc status records

## Transactions

* **WE19**: Reprocess IDoc in error
* **WE30**: Basic type
* **WE05**: Display exchanged IDocs
* **BD87**: ALE message status

## Standard IDoc

* **aleaud01**
    Confirmation of inbound IDoc to sender system. You can set which messages to intercept and for which partners this IDoc can be triggered.