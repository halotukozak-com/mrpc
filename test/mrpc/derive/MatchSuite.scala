package mrpc.derive

/**
 * `matchFrom` compiles a runtime string dispatch directly off a `NamedTuple`'s field names: one
 * `case` per name, each returning the correspondingly-indexed value, with `reject` as the fallback
 * for a scrutinee that names none of them. This is the same by-name dispatch [[Matcher]] will need
 * over an op's resolved rpcNames, exercised here directly against `matchFrom` without any
 * `DoneOperation`/`OpPlan` machinery.
 */
class MatchSuite extends munit.FunSuite:

  test("matchFrom dispatches to the value whose name matches the scrutinee"):
    val args = (foo = 1, bar = "two", baz = true)
    assertEquals(matchFrom("foo", args, reject = -1), 1)
    assertEquals(matchFrom("bar", args, reject = "reject"), "two")
    assertEquals(matchFrom("baz", args, reject = false), true)

  test("matchFrom falls back to reject when the scrutinee names none of the fields"):
    val args = (foo = 1, bar = "two")
    assertEquals(matchFrom("nope", args, reject = -1), -1)

  test("matchFrom dispatches correctly regardless of field order"):
    val args = (bar = "two", foo = 1)
    assertEquals(matchFrom("foo", args, reject = -1), 1)
    assertEquals(matchFrom("bar", args, reject = "reject"), "two")

  test("matchFrom works with a single-field named tuple"):
    val args = (only = 42)
    assertEquals(matchFrom("only", args, reject = 0), 42)
    assertEquals(matchFrom("other", args, reject = 0), 0)

  test("matchFrom re-evaluates a non-literal scrutinee at runtime, not just literals"):
    val args = (foo = 1, bar = 2)
    def dispatch(name: String) = matchFrom(name, args, reject = -1)
    assertEquals(dispatch("foo"), 1)
    assertEquals(dispatch("bar"), 2)
    assertEquals(dispatch("baz"), -1)

  test("matchFrom's result type is the union of the named tuple's value types"):
    val args = (foo = 1, bar = "two")
    val result: Int | String = matchFrom("foo", args, reject = "fallback")
    assertEquals(result, 1)
