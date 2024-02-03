// Continuation passing style enumerators

// The idea is that we can represent a logic program as a function that takes a continuation as an argument.
// The continuation gets called multiple times, and each time the state of the logic variables is different.
// We always reset the logic variables to the initial state after we are done with calling the continuation.

class E(f: (() => Unit) => Unit):
  def apply(k: () => Unit) = f(k)
  def &(e: => E) = E(k => f(() => e(k)))
  def |(e: => E) = E(k => { f(k); e(k) })
  def run = f(() => ())

val no = E(k => ()) // unit of (|)
val yes = E(k => k()) // unit of (&)

// Terms and Logic Variables

var n = 0 // unique identifier for logic variables; only for pretty printing

enum Tm:
  case V(var v: Option[Tm] = None, id: Int = { n += 1; n })
  case T(a: String, xs: List[Tm] = Nil)

  def ≡(that: Tm): E =
    (this, that) match
      case (T(a, xs), T(b, ys)) =>
        if a == b && xs.length == ys.length then xs.zip(ys).map(_ ≡ _).fold(yes)(_ & _) else no
      case (V(Some(x), _), _) => x ≡ that
      case (x @ V(None, _), _) => E(k => { x.v = Some(that); k(); x.v = None })
      case _ => that ≡ this

  override def toString: String =
    this match
      case V(Some(x), _) => x.toString
      case V(None, id) => s"?$id"
      case T(a, List()) => a
      case T(a, xs) => s"$a(${xs.mkString(", ")})"

import Tm._

// Existential Quantification

def ∃(f: Tm => E) = f(V())
def ∃∃(f: (Tm, Tm) => E) = ∃(x => ∃(y => f(x, y)))
def ∃∃∃(f: (Tm, Tm, Tm) => E) = ∃(x => ∃(y => ∃(z => f(x, y, z))))
def show(f: Tm => E): E = { val x = V(); E(k => f(x)(() => { println(x); k() })) }

// Example Logic Programs

// We can construct a Prolog tree with the T constructor:
val tree = T("node", List(T("leaf"), T("node", List(T("leaf"), T("leaf")))))

// Define convenient syntax for lists
val nil = T("nil")
def cons(x: Tm, xs: Tm) = T("cons", List(x, xs))

// Define the member and append predicates on lists
def member(x: Tm, list: Tm): E =
  ∃∃((y, ys) => list ≡ cons(y, ys) & (x ≡ y | member(x, ys)))
def append(xs: Tm, ys: Tm, zs: Tm): E =
  (xs ≡ nil & ys ≡ zs) | ∃∃∃((h, t, r) => xs ≡ cons(h, t) & zs ≡ cons(h, r) & append(t, ys, r))

// Example Queries
val xs = cons(T("A"), cons(T("B"), nil))
val ys = cons(T("C"), cons(T("D"), nil))
val zs = cons(T("A"), cons(T("B"), cons(T("C"), cons(T("D"), nil))))

// This calls the continuation with all elements of the list xs
show(x => member(x, xs)).run

// This appends xs to ys, similar to how append works in a functional language
show(zs2 => append(xs, ys, zs2)).run

// This runs append in reverse: we ask what we need to append to xs in order to get zs
show(ys2 => append(xs, ys2, zs)).run

// This also runs append in reverse: we ask what we need to prepend to ys in order to get zs
show(xs2 => append(xs2, ys, zs)).run

// This shows all possible ways to append two lists to get zs
show(xs2 => ∃(ys2 => append(xs2, ys2, zs))).run

// Can you figure out what this does?
show(xs2 => ∃(as => ∃(bs => ∃(cs => append(as, cs, zs) & append(xs2, bs, cs))))).run

// Simple cases
show(x => yes).run
show(x => no).run
show(x => yes | yes).run
