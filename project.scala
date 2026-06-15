//> using scala 3.8.4

// made + mcodec are resolved from a local publishLocal SNAPSHOT.
// Left commented until the dev loop publishes them so this scaffold compiles standalone.
////> using dep io.github.halotukozak::made:PLACEHOLDER_LOCAL_SNAPSHOT
////> using dep io.github.halotukozak::mcodec:PLACEHOLDER_LOCAL_SNAPSHOT

//> using test.dep org.scalameta::munit:1.3.3
//> using test.dep org.scalameta::munit-scalacheck:1.3.0

//> using options -deprecation -feature -new-syntax -unchecked
//> using options -language:noAutoTupling
//> using options -Yexplicit-nulls
//> using options -Wsafe-init -Werror -Wunused:all

//> using publish.organization io.github.halotukozak
//> using publish.name mrpc
//> using publish.computeVersion git:tag
//> using publish.license MIT
//> using publish.vcs github:halotukozak/mrpc
//> using publish.repository central
//> using publish.developer "halotukozak|Bartłomiej Kozak|https://github.com/halotukozak"
