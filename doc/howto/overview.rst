Overview
==========================================

The eon distributed data framework combines all of the important pieces of a
distributed framework. Some of those pieces we will build, and some of them
we will integrate from other sources. Note that an important part of the
framework is the ability to allow users to customize certain parts. The most
important customization point involves the one users interact with the most:
the front end.

The Front End
--------------

Exactly what that looks like is nebulous for now, but we know what feaures it
should have:

  * Simple, transparent horizontal scaling. The user should not need to know or
    care where it's running, or how many instances are up.
  * The user should be able to write this piece in any language they prefer.

We will support both of these options by offering an API into the data storage
and delivery system that is easily accessible to all clients. We will also
build a system that allows a user to specify what running their front end means.
In other words, they will provide a simple recipe that will give us a way to
start and stop instances, and a way to talk to them.

The Load Balancer
------------------

This component determines how many instances of other components are needed,
and where they should run. It utilizes statistics from each component to
determine actual load. This piece will start, stop, and migrate instances as
needed to maintain throughput. It should also be able to put nodes into lower
power states when they are not needed.

The Data Store
---------------

Arguably the most complex part of the system, the data store provides a way to
read and write data. It will support at least two different methods for
interacting with user apps:

 #. A SQL dialect, most likely something very close to PostgreSQL. We may also
    support their binary protocol.
 #. A RESTful interface that accepts commands and provides responses using JSON.


Why Should We Write Our Own Data Store?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There are already a number of very good databases, including open source
databases. Why should we write our own? Remember that we are building a
*distributed* system. It is true that we could build a query manager that would
use a number of instances of a standard database and distribute the queries
across them.

In fact, as part of the research undertaken before building this system, such
a configuration was implemented. There are commercial systems which have
implemented such a thing successfully. However, there are significant drawbacks
to such a system.

  #. Joins are very expensive because the data nodes cannot talk to each other
     directly. Instead, join conditions have to be shipped around. Clever things
     can be done, but this is still a severe performance problem.
  #. Transforming the data into different formats to stream around the system
     causes additional performance degradation.
  #. The built-in database operations have to be used with care because they are
     not aware of the distributed nature of the system. For example, avg()
     has to be implemented on a higher level.

What about using a low-level database engine, like leveldb?

  #. Some of these stores provide transactional support, where others require
     that you implement your own. This is not trivial, and can be inefficient.
  #. Most of them are key/value stores which can be optimal for row stores,
     but are not necessarily a good fit for column stores.
  #. Tuning these stores is not trivial, often requiring a deep understanding of
     the storage engine and the workload.

There are a number of small inefficiencies that such an implementation suffers.
Of course, the upside is that you start with a very complete, high-performance,
data store. After looking at all of the pros and cons, it was decided that
implementing a new data store would be the best option.

  #. The system will more naturally support distributed operations.
  #. The query engine can be more flexible.
  #. The open source community does not currently have a distributed database
     system, so building one could be useful in a broader context.


Composition
~~~~~~~~~~~~

The data store itself is composed of a few pieces. There are query engines and
data engines. A query engine is strategic. It is responsible for combining
results from other nodes. It makes simple requests to the data engines, and
then processes results. The query engine is not responsible for managing
the distribution of joins, only for applying high level operations and
delivering data to the front-end.

The data engine is responsible for answering simple requests for data. These
including range queries and filtering, simple summaries, and joins. Data engines
are smart enough to know who to ask when performing a join, making this as
efficient as possible.

Communication
--------------

Each of the components will be built in isolation. There are no tight couplings
between them so that we can augment or substitute pieces as needed. Consequently
we will define an interface that each of the major components must implement.
