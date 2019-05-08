.. Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Scala bindings
##############

.. toctree::
  :hidden:

The Scala bindings is a client implementation of the *Ledger API*
based on `Akka Streams API <https://doc.akka.io/docs/akka/current/general/stream/stream-design.html>`_.

Introduction
************

The Scala bindings library lets you write applications that connect to the Digital Asset distributed ledger using Scala programming language.

There are two main components:

- The Akka Streams based API
    The API to send commands to the ledger and receive transactions back.

- Scala codegen
    DAML to Scala code generator. Generates Scala classes from DAML models. The generated Scala code provides a type safe way of creating contracts (create command) and exercising contract choices (exercise command).

The reader must be familiar with:

- DAML language (TODO insert link)
- Ledger API (TODO insert link)
- Akka Streams API
- Scala programming language

Getting started
***************

