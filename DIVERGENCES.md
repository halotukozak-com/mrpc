# Divergences from commons

mrpc reproduces the AVSystem/commons RPC semantics on a Scala 3 stack. The story
is "exactly the same as commons — except where the Scala 3 design deliberately
diverges." This document is the honest catalogue of those deliberate deviations:
what mrpc does, what commons does, why, and how the parity tests treat each one
(assert byte-for-byte, normalize, or exclude).

Nothing here is an accident or a bug. Each entry is a design decision; the parity
suites either assert the coincidence directly or explicitly normalize/exclude the
divergence rather than pretend the two models are identical.

## D1 — Nested per-param-list arguments

- **mrpc:** `RawInvocation.args` is nested — `List[List[Raw]]`. Each inner list is
  one parameter list of the invoked method, holding that list's raw argument
  values in declaration order.
- **commons:** a single flat `List[RawValue]` of all argument values.
- **Why:** the nested shape preserves the parameter-list structure of a method, so
  `m(a)(b)` stays distinct from `m(a, b)`. The argument-matching logic needs that
  structure as a single source of truth instead of re-deriving it.
- **Parity treatment:** the golden fixtures were re-grouped flat → nested by the
  documented wrap-once rule (see `fixtures/README.md`). The suite asserts against
  the committed nested form **byte-for-byte**.

## D2 — Fully abstract `Raw`

- **mrpc:** `Raw` is a fully abstract type parameter. It IS the opaque
  serialized-value carrier; no transport or format API leaks into the raw layer.
  Every value argument is encoded through the leaf-codec bridge
  (`AsRaw`/`AsReal`), with a concrete `Raw = String` (JSON) chosen only at the
  companion/test seam.
- **commons:** a concrete per-framework raw value type (e.g. a JSON tree or a
  framework-specific `RawValue`).
- **Why:** keeping `Raw` abstract decouples the engine from any one serialization
  format and lets the same derivation serve any raw representation.
- **Parity treatment:** every value parameter is encoded via the leaf bridge; there
  is no "verbatim by reference equality" path. The fixtures assert the concrete
  `Raw = String` (JSON) rendering.

## D3 — `@verbatim` honored only when the type IS `Raw`

- **mrpc:** `@verbatim` is honored only when the parameter or result type is exactly
  `Raw`. Every other value is encoded through the leaf bridge.
- **commons:** `@single`/`@optional` raw values are verbatim by default across a
  richer raw-method model.
- **Why:** with a fully abstract `Raw` (see D2) there is no meaningful "pass the raw
  value through unchanged" unless the static type already is `Raw`; anything else
  must be encoded.
- **Parity treatment:** documented divergence. The v1 fixtures encode all argument
  values; none rely on verbatim pass-through.

## D4 — Overload suffix is a signature hash

- **mrpc:** overloaded methods are disambiguated with a signature-hash suffix
  (`_<hex>`) that is stable under parameter reordering across the overload set.
- **commons:** a positional suffix (`_1`, `_2`, …) assigned by declaration order,
  which shifts if overloads are reordered.
- **Why:** a content-addressed suffix is reorder-stable — adding or moving an
  overload does not silently rename a different one.
- **Parity treatment:** the ported overload tests assert the disambiguation
  **behaviour** (each overload resolves to a distinct, collision-free name), NOT
  the exact suffix strings, which deliberately differ from commons'.

## D5 — `RawInvocation` carries metadata

- **mrpc:** `RawInvocation` carries an additive `metadata: Map[String, String]`
  field (defaulting to empty) alongside `rpcName` and `args`.
- **commons:** a bare `(rpcName, args)` pair.
- **Why:** the metadata map is an additive, optional channel for call-scoped
  information; it never changes the name/args wire shape.
- **Parity treatment:** the field is ignored in the byte-for-byte argument
  comparison — the golden fixtures assert only `rpcName` + `args`.

## D6 — mcodec JSON at the leaf (not commons GenCodec)

- **mrpc:** leaf values are serialized with mcodec (`Json.write`/`Json.read`).
- **commons:** leaf values are serialized with GenCodec.
- **Why:** mrpc is built on the mcodec stack; GenCodec is a commons-only dependency.
- **Parity treatment:** the leaf JSON coincidence was checked empirically against
  the committed fixtures. Verdict: **assert byte-for-byte everywhere** — primitives
  AND objects. mcodec's `Json.write` matches the golden data for the current fixture
  shapes (primitives such as `2`, `40`, `404`, `"warn"` are byte-for-byte equal; a
  two-string-field object DTO serializes to `{"k1":"v1","k2":"v2"}` exactly, with
  declaration field order preserved and no whitespace). No parse-normalize step is
  required for the committed fixtures. This entry stays in the catalogue as a
  documented **risk** only: a future object whose field types force differences
  (scientific-notation numbers, escaping, field-order sensitivity) could still
  diverge between mcodec and GenCodec and would then need normalization.

## D7 — `@whenAbsent` and multi-slot `@multi` extraction

- **mrpc:** a single param/result whose real type is already `Option[T]`/`List[T]`
  round-trips through the ordinary leaf codec — no special arity handling needed,
  since `Option`/`List` already have codecs. `optional_configure_*` and
  `multi_broadcast` in `GoldenFixtureSuite` cover this. Not implemented:
  `@whenAbsent` (a raw-side default value substituted when decoding fails/is
  absent, distinct from an `Option` simply being `None`), and commons' true
  `@multi` semantics of collecting **several separate raw slots** into one
  collection (or a `@multi` raw method carrying a `@methodName` param to route
  across several real methods) — mrpc's fixed `RawRpc` has no such raw-slot
  model to collect from.
- **commons:** a full arity/default chain (optional params, absent-value defaults,
  multi-value collection extraction across raw slots).
- **Why:** the deeper mechanism needs the generic raw-method framework (D9);
  what a single already-collection-shaped param needs is just its own codec.
- **Parity treatment:** `@whenAbsent` and multi-slot extraction not ported.

## D8 — Generic methods, varargs, `@composite`, interceptors

- **mrpc:** generic methods, varargs / by-name parameters, `@composite` parameter
  bundling, and call interceptors are out of scope for v1's fixed three-method
  `RawRpc` model.
- **commons:** these are supported by the generic raw-method framework.
- **Why:** mrpc's `RawRpc` is intentionally fixed to `fire`/`call`/`get`; the
  generic raw-method machinery is a much larger surface deferred past v1.
- **Parity treatment:** not ported. Documented here as an explicit scope boundary.

## D9 — `RawRpc` is a fixed trait, not the generic raw-method framework

- **mrpc:** `RawRpc[Raw]` (`src/mrpc/raw/RawRpc.scala`) is a single, closed trait
  with exactly three methods (`fire`/`call`/`get`), chosen structurally from each
  real method's result type (`Unit` / `Future[_]` / anything else).
- **commons:** there is no fixed raw trait at all. Any user-declared trait can
  serve as a raw RPC trait, with arbitrary method names/shapes (see
  `core/src/test/scala/com/avsystem/commons/rpc/NewRawRpc.scala`, which declares
  raw methods named `doSomething`, `post`, `prefix`, etc.). `fire`/`call`/`get`
  only exist as one convenience layer (`StandardRPCFramework`,
  `core/src/main/scala/com/avsystem/commons/rpc/StandardRPCFramework.scala`)
  built on top of that generic mechanism — commons users can bypass it entirely.
- **Why:** mrpc deliberately targets the `StandardRPCFramework`-equivalent
  surface, not the fully generic layer underneath it; a generic
  arbitrary-raw-trait framework is a substantially larger macro-engine redesign.
- **Parity treatment:** not ported; this is the root scope decision that D8's
  more specific omissions (generic methods, `@composite` on raw params,
  interceptors, tagging-driven multi-raw-method routing) all fall out of. Not a
  bug — a named architecture boundary.

## D10 — `@methodTag`/`@paramTag`/`@tagged`/`RpcTag` have no dispatch effect (now a compile error)

- **mrpc:** `methodTag`, `paramTag`, `tagged`, and the `RpcTag` base trait exist
  as annotations (`src/mrpc/annotation/tags.scala`) and are captured as
  metadata (`AnnotationCaptureSuite`), but **no derivation or dispatch code
  reads them for routing purposes** — they do not select between raw method
  variants, do not filter matching, and have no effect on `fire`/`call`/`get`
  routing. **Fixed (2026-08-08):** attaching any of them is now a compile
  error instead of silently compiling to a no-op — a plain `hasAnnotation[X[?]]`
  guard (the wildcard matches every instantiation despite `tagged`/`methodTag`/
  `paramTag` being invariant in their type parameter) in `Plans.materialize`
  (trait-level `@methodTag`/`@paramTag`, mirroring where `AnnotationCaptureSuite`
  places them) and in `OpPlan.materialize`/`ParamPlan.encodingOf` (method-/
  param-level `@tagged`) — see `test/mrpc/parity/TagAnnotationsRejectedSuite.scala`.
- **commons:** `@methodTag`/`@paramTag` (declared on the raw trait) plus
  `@tagged[Tag](whenUntagged)` (declared on a raw method/param) actively steer
  which real methods/params a given raw method/param may match — this is how
  commons expresses e.g. GET-vs-POST-style routing over one real API
  (`core/src/main/scala/com/avsystem/commons/rpc/rpcAnnotations.scala:273-333`,
  exercised end-to-end in `NewRawRpc.scala` + `NewRpcMetadataTest.scala`).
- **Why:** tag-driven routing only makes sense against multiple raw method
  variants per real method, which requires the generic raw-method framework
  (D9). With a fixed three-method `RawRpc`, there is nothing for a tag to
  select between — but a silent no-op was a landmine, so it now fails loudly
  instead.
- **Parity treatment:** still not implemented (real tag-driven routing needs
  D9 first); the gap is the same, only the failure mode changed from silent to
  loud. `test/mrpc/parity/TagAnnotationsRejectedSuite.scala` locks in the
  compile-error behavior for `@methodTag`/`@paramTag`/`@tagged` (method and
  param position) so this cannot regress back to a silent no-op unnoticed.

## D11 — No encoding/decoding interceptors

- **mrpc:** no equivalent of `EncodingInterceptor`/`DecodingInterceptor` exists.
- **commons:** `EncodingInterceptor[NewRaw, Raw]`/`DecodingInterceptor[NewRaw, Raw]`
  (`rpc/rpcAnnotations.scala:178-202`) let a real param/method redirect its
  `AsRaw`/`AsReal` lookup to a different type and then run a final
  `NewRaw <-> Raw` conversion function — a per-parameter value transform layered
  on top of normal (de)serialization (e.g. `@prepend("bul:")` on a `Boolean`
  param in `TestRPC.scala`).
- **Why:** deferred past v1 along with the rest of D8/D9's generic-framework
  surface.
- **Parity treatment:** not ported.

## D12 — No `@tried` auto-`Try`-wrapping

- **mrpc:** a `call`-arity method's result is always `Future[Raw]`; there is no
  annotation to make a raw method's result auto-wrap in `Try[_]`.
- **commons:** `@tried` (`rpc/rpcAnnotations.scala`) wraps a raw method's result
  in `Try[_]` automatically. Note mrpc's leaf `AsRaw.forTry`/`AsReal.forTry`
  (`src/mrpc/conv/AsRaw.scala:16`, `AsReal.scala:16`) already provide the
  underlying `Try` typeclass instance "for commons parity" — only the
  engine-level annotation that triggers automatic wrapping is missing.
- **Why:** no arity variant currently produces a raw method returning
  `Future[Try[Raw]]`/`Try[Raw]`.
- **Parity treatment:** not ported.

## D13 — No `@unmatched`/`@unmatchedParam` custom compile errors

- **mrpc:** an unmatched raw/real element always reports the derivation macro's
  generic diagnostic.
- **commons:** `@unmatched(error)`/`@unmatchedParam[Tag](error)`
  (`rpc/rpcAnnotations.scala:213-225`) let a raw method/param author supply a
  custom compile-error message shown when no real counterpart matches.
- **Why:** a DX-only feature with no runtime effect; deferred.
- **Parity treatment:** not ported.

## D14 — No whole-public-API reflection (`ApiMetadataCompanion`/`materializeForApi`/`@ignore`)

- **mrpc:** `RpcMetadataCompanion`/`AsRaw.materialize`/`AsReal.materialize`
  (`src/mrpc/meta/RpcMetadataCompanion.scala`) only ever reflect an RPC trait's
  **abstract** methods, via `made.Done.Of`.
- **commons:** `ApiMetadataCompanion[M[_]]`
  (`meta/RpcMetadataCompanion.scala:23-25` per the AVSystem source) and
  `AsRaw.materializeForApi`/`AsReal.materializeForApi`
  (`rpc/AsRawReal.scala:32,64`) reflect a type's **entire public API**
  (including concrete methods), with `@ignore` (`meta/metaAnnotations.scala:298-302`)
  excluding specific methods from that reflection.
- **Why:** out of scope — mrpc's metadata/dispatch model is RPC-trait-shaped
  (abstract methods only) throughout.
- **Parity treatment:** not ported.

## D15 — No ADT/case-class metadata derivation

- **mrpc:** `TypedMetadata`/`RpcMetadataCompanion` only describe RPC traits
  (methods and their params). There is no metadata mechanism targeting case
  classes or sealed hierarchies.
- **commons:** a parallel, separately-named metadata family exists for ADTs:
  `AdtMetadataCompanion`/`BoundedAdtMetadataCompanion`
  (`meta/AdtMetadataCompanion.scala:16-37`), with steering annotations
  `@adtParamMetadata` (case-class fields), `@adtCaseMetadata` (sealed-hierarchy
  case types), `@adtCaseSealedParentMetadata` (intermediate sealed supertypes),
  `@allowOptional`, and `@allowUnorderedSubtypes`
  (`meta/metaAnnotations.scala:199-221,290-307`).
- **Why:** out of scope — mrpc has never targeted ADT/serialization-shape
  metadata, only RPC-trait metadata. This is a different feature axis than the
  RPC arity work in D7, not a smaller version of it.
- **Parity treatment:** not ported; not currently planned (no RPC use case
  requires it yet).

## D16 — Metadata reflection is missing flags/position/default-value/strictness controls

- **mrpc:** `MetadataDerivation` supports `@composite`, `@reifyName`,
  `@reifyAnnot` (single/optional/multi), `@infer`, `@rpcMethodMetadata`,
  `@rpcParamMetadata` (`src/mrpc/derive/MetadataDerivation.scala`). There is no
  way to reify a param's position, its by-name/repeated/has-default-value
  flags, its Scala-level default value, the number of parameter lists, or a
  plain "is this annotated with `T`" boolean; and there is no relaxation
  control for the completeness/arity checks `MetadataDerivation` already
  enforces (it always aborts on an arity mismatch — see
  `test/mrpc/meta/MetadataCompileErrorSuite.scala`).
- **commons:** `@reifyPosition`, `@reifyFlags`, `@reifyDefaultValue`,
  `@reifyParamListCount`, `@isAnnotated[T]` reify exactly this information
  (`meta/metaAnnotations.scala:244-280`); `@checked` makes an `@infer` implicit
  search failure affect real/raw matching rather than surface as a plain
  compile error, and `@allowIncomplete` relaxes the "every method/param must be
  captured by some metadata param" requirement (`meta/metaAnnotations.scala:282-296`).
- **Why:** v1's metadata steering vocabulary covers what the current test
  fixtures (`MultiCollectionSuite`, `CompositeMetadataSuite`, `MetadataSuite`)
  need; the richer reflective surface was deferred.
- **Parity treatment:** not ported.

## D17 — No `@auxiliary`/`@annotated`/`@notAnnotated` filters, no `Fallback[T]`/`MacroInstances`

- **mrpc:** has no equivalent of:
  - `@auxiliary` (`meta/metaAnnotations.scala:154-162`) — a raw param matching a
    real param without consuming it, so the same real value can be duplicated
    across multiple raw params with different encodings.
  - `@annotated[A]`/`@notAnnotated[A]` (`meta/metaAnnotations.scala:139-152`) —
    a lighter filter-only alternative to the tag hierarchy of D10.
  - `Fallback[T]` (`meta/Fallback.scala:18`) — a wrapper that lowers an
    `AsRaw`/`AsReal`/metadata implicit's priority below normal imports.
  - `MacroInstances[Implicits, Instances]` (`meta/MacroInstances.scala:36-83`) —
    bundles several macro-materialized typeclasses (e.g. `AsRaw`+`AsReal`+
    metadata) behind one implicit constructor param.
- **commons:** all four exist as described above.
- **Why:** each is a standalone convenience/architecture feature with no
  current mrpc use case; none block the v1 fire/call/get model.
- **Parity treatment:** not ported.
