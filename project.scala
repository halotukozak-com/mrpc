//> using scala 3.8.4

// mcodec is resolved from a local publishLocal SNAPSHOT.
//> using dep io.github.halotukozak::made:0.2.1-SNAPSHOT
//> using dep io.github.halotukozak::mcodec:0.0.0-done-SNAPSHOT

//> using test.dep org.scalameta::munit:1.3.3
//> using test.dep org.scalameta::munit-scalacheck:1.3.0
// Toolchain-pinned compiler on the test classpath so compile-time negative tests
// (compileErrors / typeCheckErrors) run against the same compiler version.
//> using test.dep org.scala-lang:scala3-compiler_3:3.8.4

//> using options -deprecation -feature -new-syntax -unchecked
//> using options -language:noAutoTupling
//> using options -Vprofile -Xprint-inline
//> using options -Ycheck:macros -Ydebug-flags -Ydebug-missing-refs
//> using options -Ycheck:all
//> using options -Yexplain-lowlevel -Yexplicit-nulls
//> using options -Yshow-suppressed-errors -Yshow-var-bounds
//> using options -Wsafe-init -Werror -Wunused:all

//> using options -Xmax-inlines 100
////> using options -Xprint-suspension


//> using options -Yprofile-enabled -Yprofile-trace:debug.json
//> using publish.organization io.github.halotukozak
//> using publish.name mrpc
//> using publish.computeVersion git:tag
//> using publish.description "mrpc - AVSystem/commons-style RPC framework for Scala 3, built on Made and mcodec"
//> using publish.url https://github.com/halotukozak/mrpc
//> using publish.license MIT
//> using publish.vcs github:halotukozak/mrpc
//> using publish.repository central
//> using publish.developer "halotukozak|Bartłomiej Kozak|https://github.com/halotukozak"
