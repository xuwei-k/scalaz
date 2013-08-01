/**
 * @see [[_root_]]
 */
package object scalaz {
  import Id._

  implicit val idInstance: Traverse1[Id] with Each[Id] with Monad[Id] with Comonad[Id] with Distributive[Id] with Zip[Id] with Unzip[Id] with Cozip[Id] = Id.id

  type Tagged[T] = {type Tag = T}

  /**
   * Tag a type `T` with `Tag`. The resulting type is a subtype of `T`.
   *
   * The resulting type is used to discriminate between type class instances.
   *
   * @see [[scalaz.Tag]] and [[scalaz.Tags]]
   *
   * Credit to Miles Sabin for the idea.
   */
  type @@[+T, Tag] = T with Tagged[Tag]

  /** A [[scalaz.NaturalTransformation]][F, G]. */
  type ~>[-F[_], +G[_]] = NaturalTransformation[F, G]
  /** A [[scalaz.NaturalTransformation]][G, F]. */
  type <~[+F[_], -G[_]] = NaturalTransformation[G, F]
  type ~~>[-F[_,_], +G[_,_]] = BiNaturalTransformation[F, G]

  type ⊥ = Nothing
  type ⊤ = Any

  type |>=|[G[_], F[_]] = MonadPartialOrder[G, F]

  type ReaderT[F[_], E, A] = Kleisli[F, E, A]
  type =?>[E, A] = Kleisli[Option, E, A]
  type Reader[E, A] = ReaderT[Id, E, A]

  type Writer[W, A] = WriterT[Id, W, A]
  type Unwriter[W, A] = UnwriterT[Id, W, A]

  object Reader {
    def apply[E, A](f: E => A): Reader[E, A] = Kleisli[Id, E, A](f)
  }

  object Writer {
    def apply[W, A](w: W, a: A): WriterT[Id, W, A] = WriterT[Id, W, A]((w, a))
  }

  object Unwriter {
    def apply[U, A](u: U, a: A): UnwriterT[Id, U, A] = UnwriterT[Id, U, A]((u, a))
  }

  type StateT[F[_], S, A] = IndexedStateT[F, S, S, A]
  type IndexedState[-S1, S2, A] = IndexedStateT[Id, S1, S2, A]
  /** A state transition, representing a function `S => (A, S)`. */
  type State[S, A] = StateT[Id, S, A]

  // important to define here, rather than at the top-level, to avoid Scala 2.9.2 bug
  object StateT extends StateTInstances with StateTFunctions {
    def apply[F[_], S, A](f: S => F[(S, A)]): StateT[F, S, A] = new StateT[F, S, A] {
      def apply(s: S) = f(s)
    }
  }
  object IndexedState extends StateFunctions {
    def apply[S1, S2, A](f: S1 => (S2, A)): IndexedState[S1, S2, A] = new IndexedState[S1, S2, A] {
      def apply(s: S1) = f(s)
    }
  }
  object State extends StateFunctions {
    def apply[S, A](f: S => (S, A)): State[S, A] = new StateT[Id, S, A] {
      def apply(s: S) = f(s)
    }
  }

  type StoreT[F[_], A, B] = IndexedStoreT[F, A, A, B]
  type IndexedStore[I, A, B] = IndexedStoreT[Id, I, A, B]
  type Store[A, B] = StoreT[Id, A, B]
  // flipped
  type |-->[A, B] = Store[B, A]
  object StoreT extends StoreTInstances with StoreTFunctions {
    def apply[F[_], A, B](r: (F[A => B], A)): StoreT[F, A, B] =
      storeT(r)
  }
  object IndexedStore {
    def apply[I, A, B](f: A => B, i: I): IndexedStore[I, A, B] = IndexedStoreT.indexedStore(i)(f)
  }
  object Store {
    def apply[A, B](f: A => B, a: A): Store[A, B] = StoreT.store(a)(f)
  }


  type ReaderWriterStateT[F[_], -R, W, S, A] = IndexedReaderWriterStateT[F, R, W, S, S, A]
  object ReaderWriterStateT extends ReaderWriterStateTInstances with ReaderWriterStateTFunctions {
    def apply[F[_], R, W, S, A](f: (R, S) => F[(W, A, S)]): ReaderWriterStateT[F, R, W, S, A] = IndexedReaderWriterStateT[F, R, W, S, S, A] { (r: R, s: S) => f(r, s) }
  }
  type IndexedReaderWriterState[-R, W, -S1, S2, A] = IndexedReaderWriterStateT[Id, R, W, S1, S2, A]
  object IndexedReaderWriterState extends ReaderWriterStateTInstances with ReaderWriterStateTFunctions {
    def apply[R, W, S1, S2, A](f: (R, S1) => (W, A, S2)): IndexedReaderWriterState[R, W, S1, S2, A] = IndexedReaderWriterStateT[Id, R, W, S1, S2, A] { (r: R, s: S1) => f(r, s) }
  }
  type ReaderWriterState[-R, W, S, A] = ReaderWriterStateT[Id, R, W, S, A]
  object ReaderWriterState extends ReaderWriterStateTInstances with ReaderWriterStateTFunctions {
    def apply[R, W, S, A](f: (R, S) => (W, A, S)): ReaderWriterState[R, W, S, A] = IndexedReaderWriterStateT[Id, R, W, S, S, A] { (r: R, s: S) => f(r, s) }
  }
  type IRWST[F[_], -R, W, -S1, S2, A] = IndexedReaderWriterStateT[F, R, W, S1, S2, A]
  val IRWST: IndexedReaderWriterStateT.type = IndexedReaderWriterStateT
  type IRWS[-R, W, -S1, S2, A] = IndexedReaderWriterState[R, W, S1, S2, A]
  val IRWS: IndexedReaderWriterState.type = IndexedReaderWriterState
  type RWST[F[_], -R, W, S, A] = ReaderWriterStateT[F, R, W, S, A]
  val RWST: ReaderWriterStateT.type = ReaderWriterStateT
  type RWS[-R, W, S, A] = ReaderWriterState[R, W, S, A]
  val RWS: ReaderWriterState.type = ReaderWriterState

  type Alternative[F[_]] = ApplicativePlus[F]

  /**
   * An [[scalaz.Validation]] with a [[scalaz.NonEmptyList]] as the failure type.
   *
   * Useful for accumulating errors through the corresponding [[scalaz.Applicative]] instance.
   */
  type ValidationNel[+E, +X] = Validation[NonEmptyList[E], X]

  type FirstOf[A] = A @@ Tags.FirstVal
  type LastOf[A] = A @@ Tags.LastVal
  type MinOf[A] = A @@ Tags.MinVal
  type MaxOf[A] = A @@ Tags.MaxVal

  type FirstOption[A] = Option[A] @@ Tags.First
  type LastOption[A] = Option[A] @@ Tags.Last
  type MinOption[A] = Option[A] @@ Tags.Min
  type MaxOption[A] = Option[A] @@ Tags.Max

  //
  // Lens type aliases
  //
  /** A lens that doesn't transform the type of the record. */
  type Lens[A, B] = LensFamily[A, A, B, B]

  // important to define here, rather than at the top-level, to avoid Scala 2.9.2 bug
  object Lens extends LensInstances with LensFunctions {
    def apply[A, B](r: A => Store[B, A]): Lens[A, B] =
      lens(r)
  }

  type @>[A, B] = Lens[A, B]

  //
  // Partial Lens type aliases
  //
  /** A partial lens that doesn't transform the type of the record. */
  type PLens[A, B] = PLensFamily[A, A, B, B]

  // important to define here, rather than at the top-level, to avoid Scala 2.9.2 bug
  object PLens extends PLensInstances with PLensFunctions {
    def apply[A, B](r: A => Option[Store[B, A]]): PLens[A, B] =
      plens(r)
  }

  type @?>[A, B] = PLens[A, B]

  type PIndexedStateT[F[_], -S1, S2, A] = IndexedStateT[F, S1, S2, Option[A]]
  type PStateT[F[_], S, A] = PIndexedStateT[F, S, S, A]

  type PIndexedState[-S1, S2, A] = PIndexedStateT[Id, S1, S2, A]
  type PState[S, A] = PStateT[Id, S, A]

  type IndexedConts[W[_], R, O, A] = IndexedContsT[W, Id, R, O, A]
  object IndexedConts extends IndexedContsTInstances with IndexedContsTFunctions {
    def apply[W[_], R, O, A](f: W[A => O] => R): IndexedConts[W, R, O, A] = IndexedContsT[W, Id, R, O, A](f)
  }
  type IndexedContT[M[_], R, O, A] = IndexedContsT[Id, M, R, O, A]
  object IndexedContT extends IndexedContsTInstances with IndexedContsTFunctions {
    def apply[M[_], R, O, A](f: (A => M[O]) => M[R]): IndexedContT[M, R, O, A] = IndexedContsT[Id, M, R, O, A](f)
  }
  type IndexedCont[R, O, A] = IndexedContT[Id, R, O, A]
  object IndexedCont extends IndexedContsTInstances with IndexedContsTFunctions {
    def apply[W[_], R, O, A](f: (A => O) => R): IndexedCont[R, O, A] = IndexedContsT[Id, Id, R, O, A](f)
  }
  type ContsT[W[_], M[_], R, A] = IndexedContsT[W, M, R, R, A]
  object ContsT extends IndexedContsTInstances with IndexedContsTFunctions {
    def apply[W[_], M[_], R, A](f: W[A => M[R]] => M[R]): ContsT[W, M, R, A] = IndexedContsT[W, M, R, R, A](f)
  }
  type Conts[W[_], R, A] = ContsT[W, Id, R, A]
  object Conts extends IndexedContsTInstances with IndexedContsTFunctions {
    def apply[W[_], R, A](f: W[A => R] => R): Conts[W, R, A] = IndexedContsT[W, Id, R, R, A](f)
  }
  type ContT[M[_], R, A] = ContsT[Id, M, R, A]
  object ContT extends IndexedContsTInstances with IndexedContsTFunctions {
    def apply[M[_], R, A](f: (A => M[R]) => M[R]): ContT[M, R, A] = IndexedContsT[Id, M, R, R, A](f)
  }
  type Cont[R, A] = ContT[Id, R, A]
  object Cont extends IndexedContsTInstances with IndexedContsTFunctions {
    def apply[R, A](f: (A => R) => R): Cont[R, A] = IndexedContsT[Id, Id, R, R, A](f)
  }

  @deprecated("Cojoin has been merged into Cobind", "7.1")
  type Cojoin[F[_]] = Cobind[F]

  @deprecated("Cojoin has been merged into Cobind", "7.1")
  val Cojoin = Cobind
}
