# CRDT

Comprehensive collection of data types for eventually consistent systems.

## Installation

In your `build.sbt` add

```scala
resolvers += "Machinomy" at "http://artifactory.machinomy.com/artifactory/snapshot"

libraryDependencies += "com.machinomy" %% "crdt" % "0.0.3"
```

## Usage

Eventually consistent system comprises of machines, that work together. They have to maintain a shared global state.
CAP theorem [[CAP](#CAP)] limits properties of the state could be supported. Some applications permit to loosen Consistency
in favour of Availability and Partitioning. This leads to eventually consistent systems.

Conflict-free Replicated Data Type is a data structure designed to support eventual consistency [[SHAPIRO](#SHAPIRO)]. A machine that belongs to the system maintains a local replica of the global state.
Properties of CRDT guarantee the replicas converge to a common state. That makes the data structure
support simultaneous operations sustainable to network disturbancy.

CRDTs could be of two types:

- operation-based, or op-based for short,
- state-based.

The types can be emulated on top of each other. The difference is in payload the replicas send to each other.
As the name implies, it is an operation, like `add` or `remove`, or full state.

### State-based CRDT

State-based CRDT is a data structure that supports operation `merge`, or `join` for replicas so that:

* `a merge (b merge c) == (a merge b) merge c`,
* `a merge b == b merge a`,
* `a merge a == a`.

Data structure like this is a join-semilattice. We could derive a partial order on the replicas. We could say if `a ≤ b`. This effectively means state-based CRDTs converge to some value, the least upper bound. It gives another name then: Con<b>v</b>ergent Replicated Data Type, or C<b>v</b>RDT.

`merge` operation resolves any conflicts that happen between the replicas by following a formal rule. The rule differs among the types. A developer is responsible for choosing the right data structure for her need.

#### GCounter

Short for grow-only counter. It could be incremented only. The merge takes the maximum count for each replica. Value is the sum of all replicas.

Say, replica id is `Int`, and GCounter manages `Int` replica counters as well:

```scala
import com.machinomy.crdt.state.GCounter
import cats.syntax.all._

val counter = GCounter[Int, Int]()
val firstReplica = counter + (1 -> 1)
val secondReplica = counter + (2 -> 2)
val firstReplicaMerged = firstReplica |+| secondReplica
val secondReplicaMerged = secondReplica |+| firstReplica
firstReplicaMerged == secondReplicaMerged
```

### Operation-based CRDT

## To Do

## License

This code is open source software licensed under the [Mozilla Public License v2.0](http://mozilla.org/MPL/2.0).

## References

* <a name="CAP">[CAP]</a> [Brewer's conjecture and the feasibility of consistent, available, partition-tolerant web services](http://dl.acm.org/citation.cfm?id=564601)
* <a name="SHAPIRO">[SHAPIRO]</a> [A comprehensive study of Convergent and Commutative Replicated Data Types](https://hal.inria.fr/inria-00555588/en/)
