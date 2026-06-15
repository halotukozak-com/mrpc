# mrpc

## What This Is

A Scala 3 RPC engine that reproduces the behavior and public API of the AVSystem/commons
RPC framework, but built natively on Scala 3 metaprogramming instead of the Scala 2 macro
engine. Derivation is powered by the author's own [`made`](https://github.com/halotukozak/made)
library (the operation-centric `Done` mirror) and serialization by
[`mcodec`](https://github.com/halotukozak/mcodec). For users familiar with commons RPC it
should feel near drop-in: same `AsRaw`/`AsReal`/`RawRpc`/`RpcMetadata` concepts and the same
annotations (`@rpcName`, `@multi`, `@verbatim`, prefixes, etc.).

## Core Value

A real RPC trait can be converted to/from a raw representation and have its metadata
materialized — with the *same* semantics as commons RPC — using `made` + `mcodec` under the
hood. If that round-trip works correctly, everything else is secondary.

## Requirements

### Validated

(None yet — ship to validate)

### Active

- [ ] Raw/Real engine: convert a real RPC trait <-> raw RPC (`AsRaw`/`AsReal`), with
      `RawInvocation`-style dispatch, matching commons semantics
- [ ] Metadata: materialize `RpcMetadata` for an RPC trait (operations, params, annotations)
- [ ] Annotation support: replicate commons RPC annotations (`@rpcName`, `@multi`,
      `@verbatim`, name prefixes, etc.) via `made` metadata
- [ ] Serialization: encode/decode RPC arguments and results through `mcodec`
- [ ] Abstract transport: keep the raw layer transport-agnostic (generic raw type), exactly
      as commons does — concrete REST/HTTP is an instantiation, not baked into the engine
- [ ] Parity validation: prove "exactly the same" three ways — direct behavior-parity tests
      against commons RPC, a port of commons' RPC test suite, and mrpc-native tests

### Out of Scope

- Concrete WebSocket / two-way push transport — commons-style abstract raw layer first; a
  specific transport is a later instantiation
- Scala 2 support — Scala 3 native is the whole point
- Reusing commons GenCodec or third-party JSON (circe) for serialization — `mcodec` instead,
  also to dogfood it
- Porting the commons Scala 2 macro engine — clean Scala 3 implementation, not a port

## Context

- Author controls `made`, `mcodec`, and `mrpc` — all three can evolve together. Gaps found
  in `made`/`mcodec` while building mrpc can be fixed upstream rather than worked around.
- `made` provides the `Done` mirror: operation-centric, models a type by its methods (each
  operation carries input params, output type, annotation metadata, and an `invoke`). This is
  purpose-built for RPC/service interfaces — the intended foundation for real-trait
  introspection in mrpc. `Made` (data-shape mirror) complements it where needed.
- `mcodec` is GenCodec-style: format-agnostic streaming `Input`/`Output` core, JSON backend,
  derivation built on `made`.
- commons RPC is the reference behavior: typesafe `AsRaw`/`AsReal` conversion, `RawRpc`,
  `RpcMetadata`, rich annotation set, transport-agnostic raw layer (used by Udash + commons
  REST).
- Both `made` (0.1.2) and `mcodec` are pinned to Scala **3.8.4** (Scala Next), so mrpc
  follows. Both libraries are experimental/early.

## Constraints

- **Tech stack**: Scala 3.8.4 (Scala Next), built with `scala-cli` — pinned by `made`/`mcodec`
- **Dependencies**: `made` (`made_3:0.1.2`) and `mcodec` for all derivation + serialization;
  no commons, no circe, no Scala 2 macros
- **Compatibility**: public API and behavior must track AVSystem/commons RPC closely enough
  to be near drop-in
- **Maturity**: depends on two experimental libraries the author owns — expect to patch them
  upstream during development

## Key Decisions

| Decision | Rationale | Outcome |
|----------|-----------|---------|
| Build on `made` `Done` mirror | Operation-centric mirror is purpose-built for RPC trait introspection | — Pending |
| Serialize via `mcodec` | Consistent made-based stack; dogfoods mcodec; avoids GenCodec/circe dep | — Pending |
| Mirror commons RPC API (same names/annotations) | Near drop-in for commons users; "exactly the same" goal | — Pending |
| Scala 3 native, not a Scala 2 macro port | Whole motivation; cleaner engine on Scala 3 metaprog | — Pending |
| Keep raw layer transport-agnostic | Matches commons; transport is a later instantiation | — Pending |
| scala-cli + Scala 3.8.4 | Pinned by made/mcodec | — Pending |
| Validate via parity tests + ported commons suite + native tests | Strongest evidence of behavioral equivalence | — Pending |

---
*Last updated: 2026-06-15 after initialization*
