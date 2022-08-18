package me.chuwy.otusbats

trait Monad[F[_]] extends Functor[F] { self =>
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]// = flatten(self.map(fa)(f))

  def point[A](a: A): F[A]

  def flatten[A](fa: F[F[A]]): F[A] = flatMap(fa)(identity)
}

object Monad {

  implicit object optionMonad extends Monad[Option] {
    override def point[A](a: A): Option[A] = Option(a)

    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)

    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }

  implicit object listMonad extends Monad[List] {
    override def point[A](a: A): List[A] = List(a)

    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)

    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }

  implicit def eitherMonad[T]: Monad[Either[T, *]] = new Monad[Either[T, *]] {
    override def flatMap[A, B](fa: Either[T, A])(f: A => Either[T, B]): Either[T, B] = fa.flatMap(f)

    override def point[A](a: A): Either[T, A] = Right(a)

    override def map[A, B](fa: Either[T, A])(f: A => B): Either[T, B] = fa.map(f)
  }

  def apply[M[_]: Monad]: Monad[M] = implicitly[Monad[M]]

  implicit class MonadOps[A, M[_]: Monad](ma: M[A]) {
    def flatMap[B](f: A => M[B]): M[B] = Monad[M].flatMap(ma)(f)
  }

  implicit class MonadPureOps[T](x: T) {
    def point[M[_]: Monad]: M[T] = Monad[M].point(x)
  }

  implicit class MonadFlattenOps[M[_]: Monad, A](mma: M[M[A]]) {
    def flatten: M[A] = Monad[M].flatten(mma)
  }

}
