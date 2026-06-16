# Golden Fixtures

These JSON files are the golden reference for raw RPC invocations. Each fixture
is a single-line JSON object with two fields:

- `rpcName` — the resolved RPC method name (a `String`).
- `args` — the raw arguments, **nested** as a list of parameter lists.

## Args shape: nested `List[List[Raw]]`

`args` is a list of **parameter lists**. Each inner list is one parameter list
of the invoked method, and its elements are the raw-encoded argument values of
that parameter list, in declaration order.

```
args: List[List[Raw]]
       │     └── one raw argument value
       └──────── one parameter list
```

This mirrors the `RawInvocation` value type, whose `args` field is
`List[List[Raw]]`. Keeping the fixtures in the same shape means the byte-for-byte
parity assertion downstream is unambiguous — no implicit reshaping happens
between the golden data and the in-memory model.

## Flat → nested mapping rule

The original fixtures used a **flat** shape (`args: List[Raw]`), matching the
older flat invocation model where a single invocation carried one flat list of
argument values. The nested model is adopted here because it preserves the
parameter-list structure of a method (`(a)(b)` is distinct from `(a, b)`), which
later argument-matching logic needs as a single source of truth.

The mapping is purely structural — values are never changed, only re-grouped by
parameter list:

| Method shape         | Flat (old)   | Nested (current) |
| -------------------- | ------------ | ---------------- |
| `m(a, b)`            | `[a, b]`     | `[[a, b]]`       |
| `m(a)(b)`            | `[a, b]`     | `[[a], [b]]`     |
| `m(a)`               | `[a]`        | `[[a]]`          |
| `m()` (no params)    | `[]`         | `[[]]` or `[]`   |

All current fixtures describe methods with exactly **one** parameter list, so
each one wraps its old flat array in exactly one outer list: `[…] → [[…]]`.

## Caveat: parameter-list nesting vs. a collection-valued argument

It is important not to confuse the **outer** nesting (parameter lists) with a
single argument that happens to *be* a collection.

`multi_broadcast.json` invokes `broadcast` with **one** parameter list
containing **one** argument, where that single argument is itself a collection
(`["a", "b", "c"]` — e.g. a varargs / repeated-parameter value collected into a
list):

```
{"rpcName":"broadcast","args":[[["a","b","c"]]]}
                               │ │ └────────── the collection-valued argument
                               │ └──────────── the single argument slot in the param list
                               └────────────── the one parameter list
```

So three brackets here are: param-list list → one param list → one argument
whose value is the `["a","b","c"]` array. This is **not** three parameter lists,
and **not** three separate arguments. The flat predecessor of this fixture was
`[["a","b","c"]]` (one flat arg list holding one collection-valued arg); applying
the wrap-once rule yields `[[["a","b","c"]]]`.

## Fixture index

| File                             | Method      | Args                                |
| -------------------------------- | ----------- | ----------------------------------- |
| `call_add.json`                  | `add`       | `[[2,40]]`                          |
| `fire_ping.json`                 | `ping`      | `[[7]]`                             |
| `multi_broadcast.json`           | `broadcast` | `[[["a","b","c"]]]`                 |
| `optional_configure_none.json`   | `configure` | `[[null]]`                          |
| `optional_configure_some.json`   | `configure` | `[[30]]`                            |
| `rpcname_renamed_status.json`    | `v2_status` | `[[404]]`                           |
| `tagged_emit.json`               | `emit`      | `[["warn",{"k1":"v1","k2":"v2"}]]`  |

## Parity divergences

These fixtures are asserted against the raw invocation the client proxy emits.
Where mrpc deliberately deviates from commons (nested args, abstract `Raw`,
signature-hash overload suffixes, `@optional` extraction, mcodec-vs-GenCodec JSON
at the leaf), the deviation is catalogued — not papered over. Deliberate
deviations from commons are documented in [DIVERGENCES.md](../DIVERGENCES.md).

In particular: the `optional_configure_*` fixtures are out of scope for v1
(`@optional`, see divergence D7) and are excluded from the parity assertions
rather than silently passing.
