package mrpc.derive

import mrpc.derive.SampleApi.*

/**
 * Wave 0 scaffold for decode-then-invoke dispatch safety: primitive and wrapper args must round-trip
 * through decode -> invoke without unsafe-cast crashes. Bodies are pending until the server adapter
 * and client proxy land.
 */
class DispatchSafetySuite extends munit.FunSuite:

  // The concrete Raw=String companion the dispatch-safety checks will materialize against.
  private val codec: SampleApiCodec.type = SampleApiCodec

  test("an Int arg round-trips through decode->invoke".ignore):
    // TODO: filled by the server adapter and client proxy — assert increment decodes to Int safely.
    val _ = codec

  test("a Boolean arg round-trips through decode->invoke".ignore):
    // TODO: filled by the server adapter and client proxy — assert echoBool decodes to Boolean.
    ()

  test("multi-param-list args rebuild via the per-param-list arities".ignore):
    // TODO: filled by the server adapter and client proxy — assert combine rebuilds nested args.
    ()
