package icfpc2022

package object syntax {
  implicit class IslOps[A](val x: A) extends AnyVal {
    def isl(implicit gen: ISLGenerator[A]): String = gen.isl(x)
  }
}
