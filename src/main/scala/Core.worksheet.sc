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

enum Term:
  case Var(var v: Option[Term] = None)
  case Val(a: String, xs: List[Term] = Nil)

  def ≡(that: Term): Prop =
    (this, that) match
      case (Val(a, xs), Val(b, ys)) =>
        if a == b && xs.length == ys.length then xs.zip(ys).map(_ ≡ _).fold(True)(_ & _) else False
      case (Var(Some(x)), _) => x ≡ that
      case (x @ Var(None), _) => Prop(k => { x.v = Some(that); k(); x.v = None })
      case _ => that ≡ this

import Term._

// Existential Quantification

def ∃(f: Term => Prop) = f(Var())
def show(f: Term => Prop): Prop = { val x = Var(); Prop(k => f(x)(() => { println(x); k() })) }
