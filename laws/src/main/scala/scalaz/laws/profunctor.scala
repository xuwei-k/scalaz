package scalaz
package laws

object profunctor {

  def laws[F[_, _]](implicit F: Profunctor[F], A1: Gen[F[Int, Int]], A2: Equal[F[Int, Int]]): Properties[ScalazLaw] = {
    implicit val a = F.covariantInstance[Int]
    implicit val b = F.contravariantInstance[Int]

    Properties.fromProps(
      ScalazLaw.profunctor,
      functor.all[({type l[a] = F[Int, a]})#l],
      contravariant.all[({type l[a] = F[a, Int]})#l]
    )
  }

  def all[F[_, _]](implicit F: Profunctor[F], A1: Gen[F[Int, Int]], A2: Equal[F[Int, Int]]): Properties[ScalazLaw] =
    laws[F]
}
