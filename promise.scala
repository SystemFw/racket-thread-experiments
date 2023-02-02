//> using lib "org.typelevel::cats-effect::3.4.5"

import cats.effect._
import cats.syntax.all._

trait Promise[A] {
  def read: IO[A]
  def write(a: A): IO[Boolean]
  def tryRead: IO[Option[A]]
}
object Promise {
  def create[A]: IO[Promise[A]] =
    (IO.ref(Option.empty[A]), IO.deferred[Unit]).mapN { case (state, waiters) =>
      new Promise[A] {
        def read: IO[A] =
          state.get.flatMap {
            case Some(a) => IO.pure(a)
            case None =>
              waiters.get >> read
          }

        def write(a: A): IO[Boolean] =
          state.access.flatMap { case (state, set) =>
            state match {
              case Some(_) => IO.pure(false)
              case None =>
                set(Some(a))
                  .flatTap { noConflict => waiters.complete(()).whenA(noConflict) }
                  .uncancelable
                  .flatMap { noConflict =>
                    if (noConflict) IO.pure(true)
                    else write(a)
                  }
            }
          }
        def tryRead: IO[Option[A]] = state.get
      }
    }
}
