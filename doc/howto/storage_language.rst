Language
============

A variety of decisions need to be made about the storage implementation. Chief
among them is language. Various prototypes of various parts were built in
several languages. The characterizations below are subjective, and are the
view of the author. A lack of experience may have led to various frustrations,
however the conclusions are valid as a reflection of honest feelings.

C++
-------------

This is the obvious choice. The amount of control the language provides is
legend. Good compiler iplementations are available for any platform you might
care to work with, and there are many libraries that either support C++
directly, or work seamlessly via C support.

Having prototyped three or four different storage implementations for this
projct in C++, one problem becomes obvious: for every type supported in the
database, efficient implementation requires a large amount of boiler plate. The
rigidity of the type system becomes particularly problematic here. You would
think that much of the work could be done with templates, but alas they are also
cumbersome in this regard. The type of std::array has to be known at compile
time. Using type erasure helps to some degree in keeping handles to arrays or
vectors of different underlying types (as you must do for a table), but you
still end up with a lot of functions with a big switch statement trying to
figure out the "right" thing to do for some input.

In addition, while C++11 and C++14 have dramatically improved the multicore
programming situation, it is still a far cry from Go, Rust, Python, and Haskell.
This isn't a show stopper by any means, but it is part of the equation.

Go
-------------

This relatively "new" language from Google shows a great deal of promise. Its
fast compile times, lightweight syntax, and outstanding concurrency support make
it a very compelling choice. Many libraries now have Go bindings, and the
standard Go library is feature rich.

On the downside, after prototyping some storage implementations in Go,  the lack
of any sort of generics became frustrating. The feeling was that a great deal of
time was spent performing type assertions to try to figure out what the
underlying value really was. Concerns about the performance of this kind of
code dimmed enthusiasm. On the other hand, inexperience with the language may
have led to a poor implementation strategy.

This language may be a better choice for other parts of the system.

Rust
--------------

This very new language seems like the perfect combination of C++ and Go. It has
generics, a sophisticated pointer tracking mechanism designed for safety, and
a pay-for-what-you-use philiosophy much like C++.

A few attempts were made to prototype a storage subsystem in this language.
However, the language itself is still undergoing changes, and there is very
little support in the standard library compared to other choices. While
enthusiasm for this language is high, it is still too soon to use it for a
project like this.

Python
--------------

Supremely flexible, many prototypes were developed in this language. Some of
them were later reimplemented in C++ or other languages. Clever use of the
standard library can reduce memory overhead and improve the speed of the code
dramatically. In addition, PyPy and Pyston, numba, numpy, and various other
facilties provide additional means to improve performance.

Still, after benchmarking, the performance disparity was obvious. It was hard
work to get the performance up to something reasonable. There are enough
challenges in this project. Getting reasonable compute performance shouldn't
be one of them.

Haskell
---------------

This functional language has been growing in popularity, and for good reason.
It is fast, flexible, and has clean syntax. Being closer to the problem space,
some of the algorithms that we'll be implementing fall out very naturally. The
language also has an amazingly good concurrency story, and a high performance
optimizing compiler. Library support is very good, and interoperability with
C is also extremely good.

On the other hand, the language has a formidable reputation for being hard to
learn. While this is in some ways deserved, in other ways it is simply that
many of the tutorials focus on the wrong thing. Recently, a few good books
have been printed, and a lot more documentation has been improved.

Languages Not Considered
-------------------------

For a variety of reasons, some popular or "obvious" choices were not prototyped.
This explains why.

Java
~~~~~~

First, it is important to distinguish between Java the language, and the JVM.
There is nothing wrong with the JVM. However, there is very little to recommend
Java as an implementation language. It is verbose and cumbersome. While some of
these things have been fixed in Java 8, a number of them haven't been and may
never be. In fact, the only thing that might be desirable about the language at
all is its popularity and wide tool support. While Java will certainly be
supported as a front-end language, it does not improve over C++ in any
significant way, and is either slower than or uses more memory than equivalent
C++ programs.

For these reasons, Java was not seriously considered as an implementation
language for the storage engine.

Scala
~~~~~~

Scala is a beautiful language. For single-core implementations it is very nearly
as fast as Java. Concerns about data flow performance were the primary reason
that no prototypes were built in this language. That decision might be revisted
in the future. However, for the data storage layer there was a strong preference
to use a compiled language. It is true that a well-tuned JVM can run as fast
as native code for some workoads, but that tuning can be a lot of work.

C#
~~~~~~~

C# is also a great language. Maybe not quite as elegant as Scala, but still
a very good language. Supporting C# on the front end is planned, but it suffers
from the same problems as Scala. Some benchmarks show that Mono is much slower
than some JVMs on compute tasks. On the other hand, it seems to be much better
on memory.

Experience with the async primitives in C# have proven the concept to be useful,
but the implementation is deepy lacking. This causes a real problem when
trying to optimize code.

Either way, there was no particular reason to spend time implementing a
prototype for data storage in this language.

Ada
~~~~~~~~~

This language is incredibly robust and has excellent perfomance. It has also
experienced a renaissance recently. After some time evaluating it, available
tooling, and the ecosystem, a decision was made not to pursue it any further.

Primarily, the language can be very cumbersome. It is more rigid than C++, with
only a few improvements to recommend it. The smaller community and limited
tooling was not overwhelmed by the increased safety and modularity.

Decisions
--------------

Taking into consideration performance, tooling, community, and experience with
the prototypes, the initial decision is to go with Haskell. Why?

  #. Haskell is lazy and referentially transparent. This makes it profoundly
     suitable for stream processing. Since the data storage engine is
     primarily a stream processor, Haskell is a natural fit.
  #. The referential transparence of the language makes concurrency and
     parallelism simple to reason about. We desire to make full use of the
     machine's resources, so these are important considerations.
  #. The most popular compiler, GHC, is far ahead of other compiled languages
     with support for parallel garbage collection, software transaction memory,
     and other features important for high performance on multicore machines.
  #. Haskell's powerful type system removes a lot of the cumbersome boiler plate.
     While not perfect, it is still superior to C++ and Go in many ways.
