import com.melvic.scame.{Parse, SExpr, Show}
import fastparse.Parsed
import fastparse.Parsed.Success
import org.scalatest.{FlatSpec, Matchers}

class AtomSpec extends FlatSpec with Matchers {
  "An Atom" should "evaluate to itself" in {
    val atoms = List("#t",
      "#f", "123", "#\\c", "#\\newline", "34.5", "3/5")
    atoms.foreach { atom =>
      val Parsed.Success(value, _ ) = Parse(atom)
      Show[SExpr](value) should be (atom)
    }
  }
}
