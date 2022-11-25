package scalaz

object Scalaz
  extends StateFunctions        // Functions related to the state monad
  with syntax.ToTypeClassOps    // syntax associated with type classes
  with syntax.ToDataOps         // syntax associated with Scalaz data structures
  with std.AllInstances         // Type class instances for the standard library types
  with std.AllFunctions         // Functions related to standard library types
  with syntax.std.ToAllStdOps   // syntax associated with standard library types
  with IdInstances {            // Identity type and instances

  implicit def scalazIdInstance: Traverse1[Id] with Monad[Id] with BindRec[Id] with Comonad[Id] with Distributive[Id] with Zip[Id] with Unzip[Id] with Align[Id] with Cozip[Id] with Optional[Id] = id
}
