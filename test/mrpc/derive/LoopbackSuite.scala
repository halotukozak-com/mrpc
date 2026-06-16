package mrpc.derive

import mrpc.derive.SampleApi.*

/**
 * Wave 0 scaffold for the real->raw->real loopback over the sample trait. Bodies are pending until
 * the server adapter and client proxy land.
 */
class LoopbackSuite extends munit.FunSuite:

  // The concrete Raw=String companion the loopback will materialize against.
  private val codec: SampleApiCodec.type = SampleApiCodec

  test("a call op round-trips real->raw->real and returns the right value".ignore):
    // TODO: filled by the server adapter and client proxy — materialize both, call find, assert.
    val _ = codec

  test("a fire op routes to fire".ignore):
    // TODO: filled by the server adapter and client proxy — assert ping reaches the fire path.
    ()

  test("a sub-RPC getter routes to get".ignore):
    // TODO: filled by the server adapter and client proxy — assert users reaches the get path.
    ()
