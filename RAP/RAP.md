# Understanding RAP in SAP

## What is RAP?

RAP stands for **RESTful ABAP Programming Model**. It is a programming model in SAP that helps developers build modern, cloud-ready, and efficient applications using ABAP. RAP is designed to support the development of SAP Fiori apps and services that follow REST principles.

## Key Features

- Based on ABAP and supports modern development practices.
- Enables creation of business services with transactional capabilities.
- Supports both managed and unmanaged scenarios.
- Integrates with SAP Fiori for user interfaces.

## Simple Example: Book Management App

Let's say we want to create a simple application to manage a list of books.

### Step 1: Define the Data Model

Create a CDS (Core Data Services) entity for books:


@EndUserText.label: 'Book'
@AccessControl.authorizationCheck: #NOT_REQUIRED
define root entity ZI_Book as select from ZBook {
  key ID,
  Title,
  Author,
  PublishedYear
}

### Step 2: Define the Behavior

Create a behavior definition to allow basic operations:

define behavior for ZI_Book
persistent table ZBook
{
  create;
  update;
  delete;
}

### Step 3: Implement the Service

Expose the entity via a service definition:

define service ZUI_BOOK {
  expose ZI_Book;
}
