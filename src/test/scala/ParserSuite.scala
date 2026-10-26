// For more information on writing tests, see
import io.parser.*
import io.parser.ParseError.UnexpectedCharacter
import cats.syntax.all.*
import cats._

class ParserSuite extends munit.FunSuite {

  test("Char parser") {
    assertEquals(charParser('x').parse("x"), Right(("x", "")))
    assertEquals(charParser('y').parse("y"), Right(("y", "")))
    assertEquals(charParser('y').parse("yxz"), Right(("y", "xz")))
    assertEquals(
      charParser('z').parse("y"),
      Left(UnexpectedCharacter("Unexpected token: y"))
    )
  }

  test("Or parser") {
    val p = charParser('z') <|> charParser('y')

    assertEquals(p.parse("z"), Right("z", ""))
    assertEquals(p.parse("y"), Right("y", ""))
    assertEquals(
      p.parse("fff"),
      Left(UnexpectedCharacter("Unexpected token: f"))
    )

  }
}
