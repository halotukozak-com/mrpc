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
