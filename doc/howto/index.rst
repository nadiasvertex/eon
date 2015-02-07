.. eon-howto documentation master file, created by
   sphinx-quickstart on Sat Feb  7 11:40:40 2015.

How to Write a Distributed Data Framework
==========================================

Distributed systems have certain complexities. However, those are more than
compensated for by the complexities they remove from large, scalable systems.
It is in everyone's interest to understand them, and feel comfortable with
them.

This book explains in detail the construction of one such system, named `eon`.
The system has several components which work together. The book begins with
a basic overview of the initial design, and then proceeds with implementation.
The narrative is an honest account, showing implementation changes due to
design failure as it progresses. It does not attempt to imagine that the whole
thing simply came together from the beginning with a complete understanding of
all the issues. This is an important part of the learning process, and so
hopefully it will yield a more informed reader at the end.

.. toctree::
   :maxdepth: 2

   overview
   storage

Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`
