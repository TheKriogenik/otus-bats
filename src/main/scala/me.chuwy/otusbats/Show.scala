package me.chuwy.otusbats

import scala.annotation.tailrec

trait Show[A] {
  def show(a: A): String
}

object Show {

  // 1.1 Instances (`Int`, `String`, `Boolean`)

  implicit val intSow: Show[Int] = (a: Int) => a.toString

  implicit val stringShow: Show[String] = identity

  implicit val booleanShow: Show[Boolean] = (a: Boolean) => a.toString


  // 1.2 Instances with conditional implicit

  implicit def listShow[A](implicit ev: Show[A]): Show[List[A]] = mkString_(_, "[", "]", ",")

  implicit def setShow[A](implicit ev1: Show[A]): Show[Set[A]] = x => x.toList.show


  // 2. Summoner (apply)

  def apply[A](implicit ev: Show[A]): Show[A] = ev

  // 3. Syntax extensions

  implicit class ShowOps[A](a: A) {
    def show(implicit ev: Show[A]): String = ev.show(a)

    def mkString_[B](begin: String, end: String, separator: String)(implicit S: Show[B], ev: A <:< List[B]): String = {
      // with `<:<` evidence `isInstanceOf` is safe!
      val casted: List[B] = a.asInstanceOf[List[B]]
      Show.mkString_(casted, separator, begin, end)
    }

  }

  /** Transform list of `A` into `String` with custom separator, beginning and ending.
   *  For example: "[a, b, c]" from `List("a", "b", "c")`
   *
   *  @param separator. ',' in above example
   *  @param begin. '[' in above example
   *  @param end. ']' in above example
   */
  def mkString_[A: Show](list: List[A], begin: String, end: String, separator: String): String = {
    @tailrec
    def f[T: Show](xs: List[T], acc: String = ""): String = xs match {
      case ::(head, Nil) => head.show ++ end
      case ::(head, next) => f(next, head.show ++ separator ++ acc)
      case Nil => end
    }
    begin ++ f(list)
  }


  // 4. Helper constructors

  /** Just use JVM `toString` implementation, available on every object */
  def fromJvm[A]: Show[A] = (a: A) => a.toString
  
  /** Provide a custom function to avoid `new Show { ... }` machinery */
  def fromFunction[A](f: A => String): Show[A] = f(_)

}
