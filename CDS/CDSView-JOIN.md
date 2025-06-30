### Definition

A **join** combines two tables for a combined data extraction. An **association** is always a join, but it operates at the data model level, not on extracted data. You can also use a join if it's included within the data (see external links - associations part 1).

### Differences

* In an association, keys are taken directly from the main table (without needing to define the table). If no fields are extracted from the associated table, it won't be involved in the join, which improves performance.
* When using the word "AS" in an association, you're not creating an alias like in joins; you're creating a name for the association between tables (the name should start with "_").

### Cardinality

**Cardinality** defines the relationship between two data sources (tables or CDS views). Cardinality is only defined for the target.

By default, it's "0...1", meaning the default minimum value is 0. Adding `[ 1 ]` means "0...1", `[ 3 ]` means "0...3", and `[ * ]` means "0...*". The minimum value cannot be `*`, and the maximum value cannot be `0`.

It's introduced in the select statement using the word `association`.
