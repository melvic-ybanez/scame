import com.melvic.scame.{Parse, SExpr, Show}
import fastparse.Parsed
import fastparse.Parsed.Success
import org.scalatest.{FlatSpec, Matchers}

class ListSpec extends FlatSpec with Matchers {
  "An empty list" should "evaluate to itself" in {
    val emptyListString = "()"
    val Parsed.Success(value, _) = Parse(emptyListString)
    Show[SExpr](value) should be (emptyListString)
  }
}
