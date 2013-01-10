Simple graph data structure and algorithms
==========================================

**Note**: This is still in early development and is not yet usable.

Data Structure
--------------

Currently Graphs are exposed as two CLOS classes `graph` and
`digraph`, for normal and directed graphs respectively.  Both graphs
and digraphs accommodate hyper-graphs (graphs in which edges are
allowed to contain more than two nodes) as a natural generalization of
standard graphs.  Arbitrary values may be used as nodes, and arbitrary
values may be associated with edges.

Under the CLOS covers graphs are composed of two hashes, one for nodes
and one for edges.

    node hash
     key -- node value
     val -- edge list

    edge hash
     key -- node list
     val -- edge value

Example Usage and API (pending)
------------------------------

For now see the tests in `graph-test.lisp`.

License
-------

Licensed under the Gnu Public License GPL Version 3.  See the
`COPYING` file in this directory for more information.
