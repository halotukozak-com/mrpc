//> using scala 3.9.0-RC4

//> using dep com.halotukozak::mcodec::0.1.2

//> using test.dep org.scalameta::munit::1.3.5
//> using test.dep org.scalameta::munit-scalacheck::1.3.0
// Toolchain-pinned compiler on the test classpath so compile-time negative tests
// (compileErrors / typeCheckErrors) run against the same compiler version.
//> using test.dep org.scala-lang:scala3-compiler_3:3.9.0-RC6

//> using options -deprecation -feature -new-syntax -unchecked
//> using options -language:noAutoTupling
//> using options -Vprofile -Xprint-inline
//> using options -Ycheck:macros -Ydebug-flags -Ydebug-missing-refs
//> using options -Ycheck:all
//> using options -Yexplain-lowlevel -Yexplicit-nulls
//> using options -Yshow-suppressed-errors -Yshow-var-bounds
// -Wsafe-init is disabled: the checker never terminates on this codebase's
// inline-derivation-heavy code (confirmed via thread dump stuck for 10+ min
// recursing through dotty.tools.dotc.transform.init.Semantic$$anon$1.traverse
// on TypeAccumulator.foldOver). Re-enable once upstream fixes this, or once
// derivation is scoped down enough for the checker to terminate.
//> using options -Werror//  -Wunused:all

//> using options -Xmax-inlines 100
////> using options -Xprint-suspension

//> using options -Yprofile-enabled -Yprofile-trace:debug.json
//> using publish.organization com.halotukozak
//> using publish.name mrpc
//> using publish.computeVersion git:tag
//> using publish.description "mrpc - AVSystem/commons-style RPC framework for Scala 3, built on Made and mcodec"
//> using publish.url https://github.com/halotukozak/mrpc
//> using publish.license MIT
//> using publish.vcs github:halotukozak/mrpc
//> using publish.repository central
//> using publish.developer "halotukozak|Bartłomiej Kozak|https://github.com/halotukozak"
