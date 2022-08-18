import me.chuwy.otusbats.Monad
import me.chuwy.otusbats.Monad.MonadOps
import me.chuwy.otusbats.Monad.MonadPureOps
import org.specs2.mutable.Specification

class MonadSpec extends Specification {

  "Моноид должен подчиняться законам" >> {
    "1. Left identity" >> {
      "List" >> {
        MonadLaws[List].leftIdentity(5)(x => (x + 1).point[List])
      }
      "Option" >> {
        MonadLaws[Option].leftIdentity(5)(x => (x + 1).point[Option])
      }
      "Either" >> {
        MonadLaws[Either[Any, *]].leftIdentity(5)(x => Right(x + 1))
      }
    }

    "2. Right identity" >> {
      "List" >> {
        MonadLaws[List].rightIdentity(5.point[List])
      }
      "Option" >> {
        MonadLaws[Option].rightIdentity(5.point[Option])
      }
      "Either" >> {
        MonadLaws[Either[Any, *]].rightIdentity(Right(5))
      }
    }

    "3. Associativity" >> {
      "List" >> {
        MonadLaws[List].associativity(1)(x => List(x + 1))(x => List(x - 1))
      }
      "Option" >> {
        MonadLaws[Option].associativity(1)(x => Option(x + 1))(x => Option(x - 1))
      }
      "Either" >> {
        MonadLaws[Either[Any, *]].associativity(1)(x => Right(x + 1))(x => Right(x - 1))
      }
    }
  }

  class MonadLaws[F[_] : Monad] {

    def leftIdentity[A, B](a: A)(f: A => F[B]): Boolean = a.point[F].flatMap(f) == f(a)

    def rightIdentity[A](fa: F[A]): Boolean = fa.flatMap(_.point[F]) == fa

    def associativity[A, B, C](a: A)(f: A => F[B])(g: B => F[C]): Boolean =
      a.point[F].flatMap(f).flatMap(g) == f(a).flatMap(g)

  }

  object MonadLaws {
    def apply[F[_]: Monad]: MonadLaws[F] = new MonadLaws[F]
  }

}
