package scalaz
package syntax
package std

final class MapOps[Map[_, _], BKC[_], K, A](self: Map[K, A])
                  (dict: scalaz.std.MapSubFunctions{
                     type XMap[A, B] = Map[A, B]
                     type BuildKeyConstraint[A] = BKC[A]
                   }) {
  final def alter(k: K)(f: (Option[A] => Option [A]))(implicit bk: BKC[K]): Map[K, A] = dict.alter(self, k)(f)
  final def intersectWithKey[B, C](m: Map[K, B])(f: (K, A, B) => C)(implicit bk: BKC[K]): Map[K, C] = dict.intersectWithKey(self, m)(f)
  final def intersectWith[B, C](m: Map[K, B])(f: (A, B) => C)(implicit bk: BKC[K]): Map[K, C] = dict.intersectWith(self, m)(f)
  final def mapKeys[K2: BKC](f: K => K2): Map[K2, A] = dict.mapKeys(self)(f)
  final def unionWithKey(m: Map[K, A])(f: (K, A, A) => A)(implicit bk: BKC[K]): Map[K, A] = dict.unionWithKey(self, m)(f)
  final def unionWith(m: Map[K, A])(f: (A, A) => A)(implicit bk: BKC[K]): Map[K, A] = dict.unionWith(self, m)(f)
  final def insertWith(k: K, v: A)(f: (A, A) => A)(implicit bk: BKC[K]): Map[K, A] = dict.insertWith(self, k, v)(f)
}

trait ToMapOps {
  import scalaz.std.{map => dict}

  implicit def ToMapOpsFromMap[K, V](m: Map[K, V]): MapOps[Map, dict.BuildKeyConstraint, K, V] = new MapOps[Map, dict.BuildKeyConstraint, K, V](m)(dict)

  implicit def ToInvariantFunctorOpsMap[A, B](v: Map[A, B])(implicit F0: InvariantFunctor[({type λ[α] = Map[A, α]})#λ]) =
    new InvariantFunctorOps[({type λ[α] = Map[A, α]})#λ, B](v)

  implicit def ToFunctorOpsMap[A, B](v: Map[A, B])(implicit F0: Functor[({type λ[α] = Map[A, α]})#λ]) =
    new FunctorOps[({type λ[α] = Map[A, α]})#λ, B](v)

  implicit def ToAlignOpsMap[A, B](v: Map[A, B])(implicit F0: Align[({type λ[α] = Map[A, α]})#λ]) =
    new AlignOps[({type λ[α] = Map[A, α]})#λ, B](v)

  implicit def ToTraverseOpsMap[A, B](v: Map[A, B])(implicit F0: Traverse[({type λ[α] = Map[A, α]})#λ]) =
    new TraverseOps[({type λ[α] = Map[A, α]})#λ, B](v)

  implicit def ToFoldableOpsMap[A, B](v: Map[A, B])(implicit F0: Foldable[({type λ[α] = Map[A, α]})#λ]) =
    new FoldableOps[({type λ[α] = Map[A, α]})#λ, B](v)

  implicit def ToApplyOpsMap[A, B](v: Map[A, B])(implicit F0: Apply[({type λ[α] = Map[A, α]})#λ]) =
    new ApplyOps[({type λ[α] = Map[A, α]})#λ, B](v)

  implicit def ToBindOpsMap[A, B](v: Map[A, B])(implicit F0: Bind[({type λ[α] = Map[A, α]})#λ]) =
    new BindOps[({type λ[α] = Map[A, α]})#λ, B](v)

  implicit def ToPlusOpsMap[A, B](v: Map[A, B])(implicit F0: Plus[({type λ[α] = Map[A, α]})#λ]) =
    new PlusOps[({type λ[α] = Map[A, α]})#λ, B](v)

  implicit def ToPlusEmptyOpsMap[A, B](v: Map[A, B])(implicit F0: PlusEmpty[({type λ[α] = Map[A, α]})#λ]) =
    new PlusEmptyOps[({type λ[α] = Map[A, α]})#λ, B](v)

  implicit def ToIsEmptyOpsMap[A, B](v: Map[A, B])(implicit F0: IsEmpty[({type λ[α] = Map[A, α]})#λ]) =
    new IsEmptyOps[({type λ[α] = Map[A, α]})#λ, B](v)

}

