//> using scala 3.8.4

// made + mcodec are resolved from a local publishLocal SNAPSHOT.
//> using dep io.github.halotukozak::mcodec:0.0.0-done-SNAPSHOT

//> using test.dep org.scalameta::munit:1.3.3
//> using test.dep org.scalameta::munit-scalacheck:1.3.0
// Toolchain-pinned compiler on the test classpath so compile-time negative tests
// (compileErrors / typeCheckErrors) run against the same compiler version.
//> using test.dep org.scala-lang:scala3-compiler_3:3.8.4

//> using options -deprecation -feature -new-syntax -unchecked
//> using options -language:noAutoTupling
//> using options -Yexplicit-nulls
//> using options -Wsafe-init -Werror -Wunused:all
// Verify generated proxy traits at compile time rather than at first invocation.
//> using options -Ycheck:macros

//> using publish.organization io.github.halotukozak
//> using publish.name mrpc
//> using publish.computeVersion git:tag
//> using publish.license MIT
//> using publish.vcs github:halotukozak/mrpc
//> using publish.repository central
//> using publish.developer "halotukozak|Bartłomiej Kozak|https://github.com/halotukozak"
