// For more information on writing tests, see
import io.parser.Parser
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite {

  val symbolParser = Parser { char => Right(Symbol(char)) }
  test("parse schema valid") {}
}
