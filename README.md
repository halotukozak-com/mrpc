# mrpc

**AVSystem/commons-style RPC framework for Scala 3**, built on [Made](https://github.com/halotukozak/made) and
[mcodec](https://github.com/halotukozak/mcodec).

> **Experimental / v1 in progress.** Pinned to Scala **3.9.0-RC4**. JVM only. The wire model is fixed to a
> `fire`/`call`/`get` `RawRpc` — generic methods, varargs, `@composite`, and interceptors are out of scope for now.
> See [DIVERGENCES.md](DIVERGENCES.md) for the full, honest list of where mrpc deliberately departs from commons.

## Overview

mrpc reproduces [AVSystem commons](https://github.com/AVSystem/scala-commons) RPC semantics on a Scala 3 stack: an
`@rpcName`/`@methodTag`-annotated trait is derived, at compile time, into a transport-agnostic `RawRpc[Raw]` — a
server-side adapter that turns typed calls into raw invocations, and a client-side proxy that turns raw invocations
back into typed calls.

- **Three fixed arities** — `fire` (procedures, `Unit` result), `call` (`Future[Raw]`), `get` (sub-RPC getters)
- **Abstract `Raw`** — the raw carrier is a type parameter; a leaf codec bridge (`AsRaw`/`AsReal`) plugs in any
  serialization format. `Raw = String` (JSON) is used at the test/transport seam via mcodec.
- **Compile-time derivation** — `RawRpcCompanion[Raw].materializeAsRaw` / `materializeAsReal` build the
  adapter/proxy pair via inline macros over [Made](https://github.com/halotukozak/made) mirrors, no reflection.
- **Self- and mutually-recursive sub-RPCs** — a `get` getter returning the same (or a peer) trait is handled through
  a lazy-given seam instead of infinite macro expansion.
- **Overload-safe dispatch** — overloaded methods get a reorder-stable, signature-hash disambiguation suffix.
- **Call metadata** — `RawInvocation` carries an additive `metadata: Map[String, String]` alongside `rpcName` and
  the per-param-list `args`.

## Installation

> mrpc has not had its first release yet. The coordinates below are how it will be published to Maven Central under
> `com.halotukozak` once tagged. Until then, build from source (see [Build](#build)).

### scala-cli

```scala
//> using scala 3.9.0-RC4
//> using dep com.halotukozak::mrpc::<version>
```

### sbt

```scala
scalaVersion := "3.9.0-RC4"
libraryDependencies += "com.halotukozak" %% "mrpc" % "<version>"
```

### mill

```scala
def scalaVersion = "3.9.0-RC4"
def mvDeps = Seq(mvn"com.halotukozak::mrpc::<version>")
```

## Quickstart

Declare an RPC trait, wire a `Raw = String` companion over mcodec's JSON codecs, and derive both directions.

```scala
import scala.concurrent.{ExecutionContext, Future}
import halotukozak.mcodec.MCodec
import halotukozak.mrpc.codec.JsonRawValue.given
import halotukozak.mrpc.raw.{RawRpc, RawRpcCompanion}

case class User(id: Int, name: String) derives MCodec

trait UserApi:
  def ping(): Unit // fire: routes through `fire`, no result
  def find(id: Int): Future[User] // call: routes through `call`
  def users: UsersApi // get: routes through `get`

trait UsersApi:
  def count(): Future[Int]

object UserApiCodec extends RawRpcCompanion[String]:
  given ExecutionContext = ExecutionContext.parasitic
  given usersRaw: halotukozak.mrpc.conv.AsRaw[RawRpc[String], UsersApi] = materializeAsRaw[UsersApi]
  given usersReal: halotukozak.mrpc.conv.AsReal[RawRpc[String], UsersApi] = materializeAsReal[UsersApi]
  given apiRaw: halotukozak.mrpc.conv.AsRaw[RawRpc[String], UserApi] = materializeAsRaw[UserApi]
  given apiReal: halotukozak.mrpc.conv.AsReal[RawRpc[String], UserApi] = materializeAsReal[UserApi]
```

Wrap a real implementation into a `RawRpc[String]` server adapter, then derive a client proxy back from it — the
proxy makes typed calls that round-trip through the raw layer:

```scala
import UserApiCodec.given

val impl: UserApi = new UserApi:
  def ping(): Unit = println("pinged")
  def find(id: Int): Future[User] = Future.successful(User(id, s"user-$id"))
  def users: UsersApi = new UsersApi:
    def count(): Future[Int] = Future.successful(7)

val rawRpc: RawRpc[String] = UserApiCodec.asRawRpc[UserApi].asRaw(impl)
val proxy: UserApi = UserApiCodec.asRealRpc[UserApi].asReal(rawRpc)

proxy.find(7) // Future.successful(User(7, "user-7")), round-tripped through JSON
```

Rename an RPC on the wire with `@rpcName`, independent of the Scala method name:

```scala
import halotukozak.mrpc.annotation.rpcName

trait StatusApi:
  @rpcName("findOne") def findRenamed(id: Int): Future[User]
```

A `RawRpc[Raw]` can be sandwiched behind an actual transport (see
[`InMemoryTransport`](src/mrpc/transport/InMemoryTransport.scala) for the loopback shape a real transport
implements) — the client proxy and server adapter are unaware whether the raw invocation crosses a process boundary.

## Build

```sh
scala-cli --power compile .
scala-cli --power test .
scala-cli --power fmt .
```

## Acknowledgements

mrpc is inspired by the [**AVSystem commons**](https://github.com/AVSystem/scala-commons) RPC framework by
[**ghik**](https://github.com/ghik). [DIVERGENCES.md](DIVERGENCES.md) documents every point where mrpc's Scala 3
design deliberately departs from that model.
