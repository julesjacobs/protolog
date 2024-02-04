enum Proof:
  case True
  case And(pf1: Proof, pf2: Proof)
  case OrL(pf: Proof)
  case OrR(pf: Proof)
  case Exists(x: Term, pf: Proof)
  case Refl(x: Term)
  case Cong(a: String, xs: List[Proof])

class Prop(val r: (Proof => Unit) => Unit):
  def &(p: => Prop) = Prop(k => r(pf => p.r(pf2 => k(Proof.And(pf, pf2)))))
  def |(p: => Prop) = Prop(k => { r(pf => k(Proof.OrL(pf))); p.r(pf => k(Proof.OrR(pf))) })
  def run = r(pf => ())

val False = Prop(k => ())
val True = Prop(k => k(Proof.True))
def ∃(f: Term => Prop) = f(Term.Var())
def trace(f: => Unit) = Prop(k => { f; k(Proof.True) })

enum Term:
  case Var(var v: Option[Term] = None)
  case Val(a: String, xs: List[Term] = Nil)

  def ≡(that: Term): Prop =
    (this, that) match
      case (Val(a, xs), Val(b, ys)) =>
        if a == b && xs.length == ys.length then xs.zip(ys).map(_ ≡ _).fold(True)(_ & _) else False // FIXME: generate better proof here (use Proof.Cong).
      case (Var(Some(x)), _) => x ≡ that
      case (x @ Var(None), _) => Prop(k => { x.v = Some(that); k(Proof.Refl(that)); x.v = None }) // FIXME: occurs check
      case _ => that ≡ this

∃(x => (x ≡ Term.Val("A") | x ≡ Term.Val("B")) & trace { print(x) }).run

var n = 0
∃(x => (x ≡ Term.Val("A") | x ≡ Term.Val("B")) & trace { n += 1 }).r { pf => println(pf) }
n
