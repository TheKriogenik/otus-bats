import org.scalacheck.Prop.forAll
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

class ShowSpec extends Specification with ScalaCheck {

  import me.chuwy.otusbats.Show
  import me.chuwy.otusbats.Show._

  "Проверка реализации тайпкласса Show" >> {
    "Должен быть реализован для следующих простых типов:" >> {
      "String" >> forAll { (x: String) =>
        x.show mustEqual x
      }
      "Int" >> forAll { x: Int =>
        x.show mustEqual x.toString
      }
      "Boolean" >> forAll { x: Boolean =>
        x.show mustEqual x.toString
      }
    }
    "Должен быть реализован для следующих контейнеров:" >> {
      "List" >> forAll { l: List[Int] =>
        l.show mustEqual mkString_(l, "[", "]",",")
      }
      "Set" >> forAll { l: Set[Int] =>
        l.show mustEqual mkString_(l.toList, "[", "]", ",")
      }
    }

    "Конструктор fromJvm должен создавать инстанс с использованием Object.toString():" >> forAll { x: Double =>
      implicit val instance: Show[Double] = Show.fromJvm[Double]
      x.show mustEqual x.toString
    }

    "Конструктор fromFunction должен создавать инстанс с использованием переданной функции:" >> forAll { x: Double =>
      val f = (a: Double) => s"[$a]"
      implicit val instance: Show[Double] = Show.fromFunction(f)
      x.show mustEqual f(x)
    }
  }
}
