package scalaz
package std.java.util

trait MapInstances {

  import java.util.Map.Entry
  import java.util.AbstractMap.SimpleImmutableEntry

  implicit val mapEntryBitraverse: Bitraverse1[Entry] = new Bitraverse1[Entry] {
    override def bimap[A, B, C, D](fab: Entry[A, B])(f: A => C, g: B => D) =
      new SimpleImmutableEntry(f(fab.getKey), g(fab.getValue))

    def bitraverse1Impl[G[_]: Apply, A, B, C, D](fab: Entry[A, B])
                                                 (f: A => G[C], g: B => G[D]) =
      Apply[G].apply2(f(fab.getKey), g(fab.getValue))(new SimpleImmutableEntry(_, _))

    def bifoldMap1[A, B, M](fab: Entry[A,B])(f: A => M)(g: B => M)(implicit M: Semigroup[M]): M =
      M.append(f(fab.getKey), g(fab.getValue))

    def bifoldMapRight1[A, B, C](fab: Entry[A, B])(l: A => C, r: B => C)(f: (A, => C) => C)(g: (B, => C) => C) =
      f(fab.getKey, r(fab.getValue))
  }
}

object map extends MapInstances
