trait Par[A]

object Par {
  def unit[A](a: => A): Par[A] = ???
  def get[A](a: Par[A]): A = ???
  def map2[A,B, C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[B] = ???
}