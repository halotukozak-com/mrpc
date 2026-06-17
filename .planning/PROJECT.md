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

## Current Milestone: v2.0 Commons RPC Parity

**Goal:** Reach feature/behavior **1:1 with scala-commons RPC** — implement the full commons RPC
surface and prove it by porting the complete commons RPC test suite. Deliberate representational
divergences D1–D8 (`DIVERGENCES.md`) stay as design choices; "1:1" = every commons feature works and
every ported commons test passes (byte-for-byte where models coincide, normalized/excluded for D1–D8).

**Engine direction taken (supersedes the original "Full Done" plan):** instead of contributing
`Done.materialize` upstream to made, mrpc went **self-contained** — the engine was rewritten as
commons-style Scala 3 macros with **no `made` dependency** (direct symbol introspection, direct
`api.method(args)` dispatch, `Symbol.newClass` proxy, `derives RpcCodec` entry point). That rewrite is
done and green; the parity features below build on it.

**Target features (commons surface):**
- Arity & defaults: `@optional`/`@whenAbsent`/`@tried`; user-definable generic raw methods.
- Tag-based matching: `@methodTag`/`@paramTag`/`@tagged` raw↔real selection.
- Full metadata DSL: `TypedMetadata`, `@reifyName`/`@reifyAnnot`/`@infer`, `@composite`, API metadata.
- API reflection: `materializeApiAsRaw` / multi-RPC API container.
- `derives` recursion (in-macro cycle-breaker for self/mutual traits).
- Legacy `RPCFramework` (Procedure/Function/Getter + Standard/OneWay) + its metadata.
- Concrete HTTP transport (Jetty analog).
- The 1:1 proof: full commons RPC suite ported and passing.

## Requirements

### Validated

- ✓ Raw/Real engine: real RPC trait ⇄ raw RPC (`AsRaw`/`AsReal`/`AsRawReal`) with
  `RawInvocation` nested-arg dispatch and three-arity (`fire`/`call`/`get`) routing — v1.0
- ✓ Metadata: `RpcMetadata` + `RpcMetadataCompanion` materialized from the same `Done`
  structure, strategy markers honored — v1.0
- ✓ Annotation support: full commons-style set (`@rpcName`, `@rpcNamePrefix`, `@single`/`@multi`,
  `@verbatim`/`@encoded`, `@methodName`, tag-based) as `made.MetaAnnotation`s visible in `Done` — v1.0
- ✓ Serialization: leaf encode/decode through `mcodec.MCodec[A]` at the arg/result boundary — v1.0
- ✓ Abstract transport: raw layer stays transport-agnostic (generic `Raw`); one in-memory
  `RawRpc[String]` instantiation proves the full round-trip — v1.0
- ✓ Recursion: lazy (`makeLazy`) sub-RPC getter recursion for self/mutually-referential traits — v1.0
- ✓ Parity validation: proven three ways — byte-for-byte golden fixtures (VAL-01), ported
  commons subset (VAL-02), native munit+scalacheck round-trip laws (VAL-03) — v1.0

### Active

<!-- v2.0 Full Done — internal Done-first rewrite (behavior-preserving). See REQUIREMENTS.md. -->

- [ ] `made.Done` gains a #25245-safe instance synthesizer (`Done.materialize`), PR-ready
- [ ] Server adapter (`AsRaw`) derives via `DoneOperation.apply` — no raw `quotes.reflect`
- [ ] Client proxy (`AsReal`) derives via `Done.materialize` — no hand-built `Symbol.newClass`
- [ ] `OpReflect` introspection moves onto `Done`'s type-level members
- [ ] v1.0 behavior parity preserved (all 25 suites + `DIVERGENCES.md` green)

### Future (post-v2.0)

- `@optional`/`@whenAbsent`/`@tried`/`@mangleOverloads` advanced arity & default chains (OPT-01, EXT-01/02)
- `materializeApiAsRaw` / `ApiMetadataCompanion` (EXT-04)
- Concrete transport (REST/HTTP, WebSocket) as raw-layer instantiations
- Publish to Maven Central — gated on `mcodec` reaching Central first (PUB-01)
- Real-runner CI on the now-existing mrpc GitHub repo (deferred from Phase 1)

### Out of Scope

- Scala 2 support — Scala 3 native is the whole point
- Reusing commons GenCodec or third-party JSON (circe) for serialization — `mcodec` instead,
  also to dogfood it
- Porting the commons Scala 2 macro engine — clean Scala 3 implementation, not a port
- Legacy `RPCFramework`/`StandardRPCFramework` — superseded by the generic `RawRpc` model

## Current State

**Shipped v1.0 "Core Parity" (2026-06-16)** — ~2,942 LOC Scala 3 across `src/` + `test/`,
25 test suites green (`-Ycheck:macros` clean). The full real→raw→(in-memory transport)→raw→real
round-trip plus metadata materialization works with proven parity against commons. 6 phases,
24 plans, 46 commits.

- **Tech stack:** Scala 3.8.4, scala-cli, munit + munit-scalacheck.
- **Deps:** `made` (commit `16bfbdbd`) + `mcodec` (commit `4f4ece4`) consumed via local
  `publishLocal` SNAPSHOTs (neither on Maven Central yet — blocks PUB-01).
- **Honest parity:** deliberate Scala-3 divergences from commons catalogued in `DIVERGENCES.md`
  (D1–D8) — documented, not papered over.
- **Known tech debt:** real-runner CI deferred (repo now exists); Nyquist VALIDATION.md drafts
  for phases 1–5 (process artifact, all phases verified green regardless).

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
| Build on `made` `Done` mirror | Operation-centric mirror is purpose-built for RPC trait introspection | ✓ Good — whole engine (derive + metadata) is a thin fold over `Done` |
| Serialize via `mcodec` | Consistent made-based stack; dogfoods mcodec; avoids GenCodec/circe dep | ✓ Good — leaf bridge round-trips; `makeLazy` also templated mrpc recursion |
| Mirror commons RPC API (same names/annotations) | Near drop-in for commons users; "exactly the same" goal | ✓ Good — full annotation set + parity proven; divergences in DIVERGENCES.md |
| Scala 3 native, not a Scala 2 macro port | Whole motivation; cleaner engine on Scala 3 metaprog | ✓ Good — `Symbol.newClass` proxy, no abstract type members (scala3#25245-safe) |
| Keep raw layer transport-agnostic | Matches commons; transport is a later instantiation | ✓ Good — `Raw` fully abstract; in-memory `RawRpc[String]` is the first instantiation |
| scala-cli + Scala 3.8.4 | Pinned by made/mcodec | ✓ Good — canonical 3.8.4 string; local 3-repo dev loop |
| Validate via parity tests + ported commons suite + native tests | Strongest evidence of behavioral equivalence | ✓ Good — three-pronged proof all green (VAL-01/02/03) |
| Lazy sub-RPC recursion via `makeLazy` by-name wrapper (not `Deferred`) | Cuts infinite inline expansion at given-resolution layer | ✓ Good — self + mutual recursion clean under `-Ycheck:macros` |

---
*Last updated: 2026-06-16 after starting v2.0 Full Done milestone*
