import com.melvic.scame.{Parse, SExpr, Show}
import fastparse.Parsed
import org.scalatest.{FlatSpec, Matchers}

class SpecialFormSpec extends FlatSpec with Matchers {
  "A definition" should "return an empty list or unit" in {
    val definitions = Map("x" -> 10, "foo" -> 34.56)
    definitions.foreach { case (key, value) =>
      val Parsed.Success(result, _) = Parse(s"(define $key $value)")
      Show[SExpr](result) should be ("()")
    }
  }
}
