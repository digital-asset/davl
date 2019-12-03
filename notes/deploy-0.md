# Designing DAML-Driven Systems, part 1: Where does DAML fit?

This is the first in a series of blog posts about the design and deployment of
DAML-Driven systems, i.e. focusing on how DAML interacts with the rest of your
infrastructure and where it fits architecturally. This series will not cover
the DAML language itself nor the specifics of the DAML Ledger API; see the
reference documentation for that ([DAML Language](), [Ledger gRPC API](),
[Ledger JSON API]()).

This series will assume at least passing familiarity with the DAML core
concepts such as party, template, and contract.

This first article presents a very high-level view of what DAML is,
architecturally speaking, and discusses broad considerations you should keep in
mind when designing a system of which DAML will be a component. Future posts
will delve deeper into more specific aspects and options.

## What is DAML?

To understand where DAML fits, it is useful to start by taking a step back and
taking a look at what DAML is, as a high-level abstract view. There are two
conceptual components:

1. *The DAML Language*: A domain-specific language tailored to describe
   multi-party workflows, i.e. permissioned state machines (templates).
2. *The DAML Runtime*: A system that can take such a set of DAML template
   definitions and turn that into an abstraction layer that exposes a
   corresponding authenticated, permissioned API to manipulate the resulting
   contracts on top of some storage engine.

In the context of this series, we will not look further at the DAML language
itself, and instead focus on the runtime system. We will call "frontend" any
application or system that talks to the generated API (they can range from
automated systems to graphical interfaces for direct user input) and "backend"
any compatible storage system.

Note that the exact shape of the runtime system may depend on the underlying
storage, which is why we are keeping the language fairly vague at this point.

## Authentication

For a given set of templates, the DAML Language has powerful, first-class
support for dynamically defining user roles on a per-contract basis, i.e. the
same party can play different roles in different instances of the same
template.

However, the DAML Language itself has no concept of authentication. It simply
assumes that authentication is handled outside of its world and any connection
to the API that gets generated from its template definitions will be already
authenticated.

Consequently, the DAML Runtime handles authentication. This is done using a
flexible mechanism, documented [here](), designed to let the DAML Runtime plug
into your existing authentication infrastructure.

From an architectural perspective, this means that you will need to ensure the
runtime system has the right type of network connectivity to reach your
authentication system, and the resulting aggregate is properly secured.

## Backend options

The runtime system can use a variety of existing systems as its data store; from
an architectural perspective, it is useful to consider three separate classes
of backends.

### Ephemeral storage

As the name implies, this is a backend that will not be persisted. At the time
of writing, the only backend in this category is the [in-memory sandbox]()
implementation which is part of the open-source [DAML SDK](). In that case, the
runtime system is embodied by a JAR file you can run as a standalone Java
application with no network dependency.

For obvious reasons, the in-memory implementation is not recommended for
production use, especially if you are dealing with data you want to keep.
However, it can still be an attractive deployment option for prototype
demonstrations.

DAML code can be loaded in the in-memory sandbox in two ways:
- By specifying a path to a DAR file as a (repeatable) command-line argument to
  the sandbox directly, or
- After startup, by uploading a DAR file through the gRPC API.

There is no plan for another ephemeral storage at this point.

### Centralized storage

While the DAML Language has been designed to make it easy to express
multi-party, distributed workflows, it also makes it really easy to describe
single-party workflows. We believe the DAML way of modeling data can be
beneficial for most applications and therefore there is a place for DAML models
even in single-stakeholder scenarios.

Note that in this context "centralized storage" means centralized control,
i.e. a single entity controls the entire storage, not necessarily that the
storage engine cannot be distributed, for example for scaling.

To cater to those use-cases, we currently offer a [PostgreSQL-backed persistent
version of the sandbox](), also as part of the open-source [DAML SDK](). Like
the ephemeral backend, this is a single JAR file (in fact, the same one), which
runs as a standalone Java application, though this time with a network
dependency on a PostgreSQL server.

DAML code can be loaded in the PostgreSQL-backed sandbox in two ways:
- By specifying a path to a DAR file as a (repeatable) command-line option to
  the sandbox directly, but only if this instance of the sandbox is going to
  initialize a fresh DAML ledger, or
- After startup, by uploading a DAR file through the gRPC API.

While we do not have active plans to develop another centralized storage option
at the moment, more could come in the future.

### Distributed storage

The original target use-case for the DAML Language is the definition of
multi-party workflows where separate entities want to share parts of a database
such that they can easily verify that their data is properly synchronized
(because they are, in fact, looking at the same data) without relinquishing
control over their own data.

The best technology to deliver such a shared database is known as a distributed
ledger. There are many mature, stable implementations of distributed ledgers
with different tradeoffs and design goals (e.g. throughput vs. latency or
different choices of supported programming languages); DAML provides a
higher-level, portable way of describing workflows to run on many of them.

If you are considering a distributed use-case, please refer to the
[DAML-on-X]() documentation for an up-to-date list of currently-supported DAML
backends, as we and our partners are constantly working hard on adding more.

Different ledgers have different deployment models of their own, as well as
different capabilities that may limit the extent to which they can support the
DAML model; as an example, if the underlying ledger has no notion of privacy,
it is likely that direct read access to the ledger will leak DAML transaction
details, though the DAML privacy model would be preserved for users accessing
the ledger through the DAML Runtime.

Deploying new code to a DAML Ledger backed by a distributed ledger operated by
separate entities may require extra (out-of-band) coordination, such as each
entity pushing the same DAR file to their own instance of the runtime system.

The "runtime system" in this case could itself be a distributed system, such as
a plugin that needs to be installed on each participant node of a distributed
ledger.

## Frontend options

We currently offer two options to talk to the runtime system.

### protobuf over gRPC

The runtime system itself exposes [its main API]() using [gRPC]() as a
transport mechanism and [protobuf]() as a data encoding. From a deployment
perspective, this API requires authentication and lets you do two types of
things:

- Upload new DAR files, i.e. deploy new DAML code, and
- Interact with already-installed DAML code as an authenticated party.

The gRPC API has been designed as an integral part of the DAML ecosystem and
provides the complete set of all DAML services, including the administration of
the DAML engine itself as well as enabling high-performance interactions with
it.

However, this API has two main drawbacks:

- The gRPC transport is not natively supported by many languages and platforms;
  in many cases this means adding extra native dependencies to user code, which
  can add complexity to build and deployment pipelines, especially for managed
  code (e.g. Python, JS, Ruby, Java, etc.), or adding extra layers of proxying
  for network calls when you do not have full control of the clients (e.g.
  browsers do not natively support gRPC).
- The protobuf encoding, while highly efficient, requires the distribution of
  protocol definition files, which is similarly not always practical.

### JSON over HTTP

To alleviate the above issues, the DAML SDK also includes a [JSON API
server](). This is a separate process that connects to the DAML Runtime through
the main gRPC API and exposes an HTTP API on another port; it is currently
distributed in the form of a standalone JAR file.

While this API uses the widely-supported HTTP transport and JSON format,
potentially making build and deployment pipelines simpler as well as allowing
direct browser access, it does have some drawbacks. Mainly:

- It implements only a subset of the API; specifically, it only lets users
  interact with the ledger model as authenticated parties, i.e. creating
  contracts and exercizing choices. In particular, it does not allow the
  uploading of new DAML code nor access to historical data (only "active"
  contracts can be seen).
- HTTP and JSON are a fairly inefficient protocol and format compared to gRPC
  and protobuf.

## Coming up next...

We hope this overview helped clarify where DAML fits and what you should keep
in mind when designing a DAML-Driven system. In the next installment, we will
walk through a deployment scenario for a centralized storage backend and a
web-based frontend using the JSON/HTTP API, as well a discuss some tradeoffs
and considerations specific to that setup.
