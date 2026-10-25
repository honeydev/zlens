// For more information on writing tests, see
import io.parser.*
import io.parser.ParseError.UnexpectedCharacter
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite {

  assertEquals(charParser('x').parse("x"), Right(("x", "")))
  assertEquals(charParser('y').parse("y"), Right(("y", "")))
  assertEquals(
    charParser('z').parse("y"),
    Left(UnexpectedCharacter("Unexpected token: y"))
  )
}
