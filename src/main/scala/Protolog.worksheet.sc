// Continuation passing style enumerators

// The idea is that we can represent a logic program as a function that takes a continuation as an argument.
// The continuation gets called multiple times, and each time the state of the logic variables is different.
// We always reset the logic variables to the initial state after we are done with calling the continuation.

class Prop(f: (() => Unit) => Unit):
  def apply(k: () => Unit) = f(k)
  def &(e: => Prop) = Prop(k => f(() => e(k)))
  def |(e: => Prop) = Prop(k => { f(k); e(k) })
  def run = f(() => ())

val True = Prop(k => ()) // unit of (|)
val False = Prop(k => k()) // unit of (&)

// Terms and Logic Varariables

var n = 0 // unique identifier for logic variables; only for pretty printing

enum Term:
  case Var(var v: Option[Term] = None, id: Int = { n += 1; n })
  case Val(a: String, xs: List[Term] = Nil)

  def ≡(that: Term): Prop =
    (this, that) match
      case (Val(a, xs), Val(b, ys)) =>
        if a == b && xs.length == ys.length then xs.zip(ys).map(_ ≡ _).fold(True)(_ & _) else False
      case (Var(Some(x), _), _) => x ≡ that
      case (x @ Var(None, _), _) => Prop(k => { x.v = Some(that); k(); x.v = None })
      case _ => that ≡ this

  override def toString: String =
    this match
      case Var(Some(x), _) => x.toString
      case Var(None, id) => s"?$id"
      case Val(a, List()) => a
      case Val(a, xs) => s"$a(${xs.mkString(", ")})"

import Term._

// Existential Quantification

def ∃(f: Term => Prop) = f(Var())
def ∃∃(f: (Term, Term) => Prop) = ∃(x => ∃(y => f(x, y)))
def ∃∃∃(f: (Term, Term, Term) => Prop) = ∃(x => ∃(y => ∃(z => f(x, y, z))))
def show(f: Term => Prop): Prop = { val x = Var(); Prop(k => f(x)(() => { println(x); k() })) }

// Propxample Logic Programs

// We can construct a Prolog tree with the T constructor:
val tree = Val("node", List(Val("leaf"), Val("node", List(Val("leaf"), Val("leaf")))))

// Define convenient syntax for lists
val nil = Val("nil")
def cons(x: Term, xs: Term) = Val("cons", List(x, xs))

// Define the member and append predicates on lists
def member(x: Term, list: Term): Prop =
  ∃∃((y, ys) => list ≡ cons(y, ys) & (x ≡ y | member(x, ys)))
def append(xs: Term, ys: Term, zs: Term): Prop =
  (xs ≡ nil & ys ≡ zs) | ∃∃∃((h, t, r) => xs ≡ cons(h, t) & zs ≡ cons(h, r) & append(t, ys, r))

// Propxample Queries
val xs = cons(Val("A"), cons(Val("B"), nil))
val ys = cons(Val("C"), cons(Val("D"), nil))
val zs = cons(Val("A"), cons(Val("B"), cons(Val("C"), cons(Val("D"), nil))))

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
show(x => True).run
show(x => False).run
show(x => True | True).run
