class Prop(val run: (() => Unit) => Unit):
  def &(e: => Prop) = Prop(k => run(() => e.run(k)))
  def |(e: => Prop) = Prop(k => { run(k); e.run(k) })

val True = Prop(k => ())
val False = Prop(k => k())

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

def ∃(f: Term => Prop) = f(Term.Var())
