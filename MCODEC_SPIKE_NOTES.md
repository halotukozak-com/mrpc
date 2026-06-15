# mcodec capability spike

Goal: confirm mcodec's leaf codecs cover the value-serialization needs of the RPC engine, and
document anything that is *not* a codec concern so later (engine-layer) work inherits the conclusion.

All claims below are proven by green round-trips in
[`test/mrpc/McodecSpikeTest.scala`](test/mrpc/McodecSpikeTest.scala). Run:

```sh
scala-cli --power test . --test-only 'mrpc.McodecSpikeTest'
```

## Covered (proven)

| Capability area                     | What it proves                                                        | Proving test                                                     |
| ----------------------------------- | --------------------------------------------------------------------- | --------------------------------------------------------------- |
| Primitives                          | `Int` / `String` / `Boolean` round-trip via `Json.read`/`Json.write`  | `primitives round-trip through JSON`                            |
| DTO / case class                    | `case class User(id, name) derives MCodec` round-trips                | `DTO / case class round-trips through JSON`                     |
| Value-class / `@transparent`        | `@transparent case class Email(value: String)` serializes as the **bare value** (`Json.write(Email("a@b")) == "\"a@b\""`) — no wrapper object — and round-trips | `@transparent wrapper serializes as the bare value and round-trips` |
| `@multi` collections                | `List[Int]`, `Map[String, Int]`, `Option[Int]` (both `Some` and `None`) round-trip | `@multi collections round-trip: List / Map / Option`            |
| Generated DTO (property-based)       | `forAll u: User. Json.read(Json.write(u)) == u`                       | `generated DTO round-trips real -> raw -> real`                 |

The codecs come from mcodec's `StdCodecs` (primitives, `Option`, `Map`, `List`, …), the derived
`MCodec` for case classes, and the `@transparent` (value-class) codec. No custom codecs were needed.

## Future / Try: NOT a leaf-codec gap (engine-layer concern)

mcodec's `StdCodecs` provides codecs for primitives, `BigInt`/`BigDecimal`, `UUID`, `Option`,
`Either`, `Tuple`, `Seq`/`Set`/`List`/`Vector`/`IndexedSeq`/`HashSet`/`Array`, and `Map` — but
**no `MCodec[Future[_]]` and no `MCodec[Try[_]]`**, by design.

This is correct and expected. Effect wrappers are handled at the **RPC engine layer**, not the
codec layer: AVSystem/commons wraps results via `AsRaw[Future[RawValue], Future[T]]` (e.g. `mapNow`
over the underlying value codec), so the effect is threaded around a leaf-value codec rather than
being a codec itself.

Consequences for later work:

- Do **not** add a `Future`/`Try` `MCodec` to mcodec — it would be the wrong layer.
- The effect-wrapper handling belongs to the engine's `call`/`get` arity work (the result-wrapping
  layer), which composes an effect over the leaf-value codec. That is where `Future`/`Try` support
  is introduced.
- The spike therefore does **not** block on Future/Try, and exercises only leaf-value codecs.

## Gaps filed

None. Every capability area the spike needed was already covered by mcodec, so no mcodec issue was
opened and no per-gap fix (branch + publishLocal) was required. The only "absence" found — Future /
Try — is intentional and engine-layer, as documented above.
