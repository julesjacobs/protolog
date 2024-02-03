class Prop(val r: (=> Unit) => Unit):
  def &(p: => Prop) = Prop(k => r(p.r(k)))
  def |(p: => Prop) = Prop(k => { r(k); p.r(k) })
  def run = r {}

val True = Prop(k => ())
val False = Prop(k => k)
def ∃(f: Term => Prop) = f(Term.Var())
def trace(f: => Unit) = Prop(k => { f; k })

enum Term:
  case Var(var v: Option[Term] = None)
  case Val(a: String, xs: List[Term] = Nil)

  def ≡(that: Term): Prop =
    (this, that) match
      case (Val(a, xs), Val(b, ys)) =>
        if a == b && xs.length == ys.length then xs.zip(ys).map(_ ≡ _).fold(True)(_ & _) else False
      case (Var(Some(x)), _) => x ≡ that
      case (x @ Var(None), _) => Prop(k => { x.v = Some(that); k; x.v = None })
      case _ => that ≡ this

∃(x => (x ≡ Term.Val("A") | x ≡ Term.Val("B")) & trace { print(x) }).run

var n = 0
∃(x => (x ≡ Term.Val("A") | x ≡ Term.Val("B")) & trace { n += 1 }).run
n
