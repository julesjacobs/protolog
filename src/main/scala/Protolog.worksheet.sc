// Continuation passing style enumerators

// The idea is that we can represent a logic program as a function that takes a continuation as an argument.
// The continuation gets called multiple times, and each time the state of the logic variables is different.
// We always reset the logic variables to the initial state after we are done with calling the continuation.

class E(f: (() => Unit) => Unit):
  def apply(k: () => Unit) = f(k)
  def &(e2: => E) = E(k => f(() => e2(k)))
  def |(e2: => E) = E(k => { f(k); e2(k) })
  def run = f(() => ())

val no = E(k => ()) // unit of (|)
val yes = E(k => k()) // unit of (&)

// Terms and Logic Variables

var counter = 0 // unique identifier for logic variables; only for pretty printing

enum Term:
  case V(var value: Option[Term] = None, id: Int = { counter += 1; counter })
  case T(name: String, args: List[Term] = Nil)

  def ≡(that: Term): E =
    (this, that) match
      case (T(name1, args1), T(name2, args2)) =>
        if name1 == name2 && args1.length == args2.length then args1.zip(args2).map(_ ≡ _).fold(yes)(_ & _) else no
      case (V(Some(x), _), _) => x ≡ that
      case (x @ V(None, _), _) => E(k => { x.value = Some(that); k(); x.value = None })
      case _ => that ≡ this

  override def toString: String =
    this match
      case V(Some(value), _) => value.toString
      case V(None, id) => s"?$id"
      case T(name, List()) => name
      case T(name, args) => s"$name(${args.mkString(", ")})"

import Term._

// Existential Quantification

def ∃(f: Term => E) = f(V())
def ∃∃(f: (Term, Term) => E) = ∃(x => ∃(y => f(x, y)))
def ∃∃∃(f: (Term, Term, Term) => E) = ∃(x => ∃(y => ∃(z => f(x, y, z))))
def show(f: Term => E): E =
  val x = V(); E(k => f(x)(() => { println(x); k() }))

// Example Logic Programs

// We can construct a Prolog tree with the T constructor:
val tree = T("node", List(T("leaf"), T("node", List(T("leaf"), T("leaf")))))

// Define convenient syntax for lists
val nil = T("nil")
def cons(head: Term, tail: Term) = T("cons", List(head, tail))

// Define the member and append predicates on lists
def member(x: Term, list: Term): E =
  ∃∃((y, ys) => list ≡ cons(y, ys) & (x ≡ y | member(x, ys)))
def append(xs: Term, ys: Term, zs: Term): E =
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
