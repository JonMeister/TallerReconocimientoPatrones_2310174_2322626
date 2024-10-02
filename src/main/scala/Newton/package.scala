import math.{pow,log}
package object Newton {

  trait Expr
  case class Numero(d : Double) extends  Expr
  case class Atomo(x : Char) extends  Expr
  case class Suma( e1 : Expr, e2 : Expr) extends  Expr
  case class Prod(e1 : Expr, e2 : Expr) extends  Expr
  case class Resta( e1 : Expr , e2 : Expr) extends  Expr
  case class Div( e1 : Expr , e2 : Expr) extends  Expr
  case class Expo( e1 : Expr , e2 : Expr) extends  Expr
  case class Logaritmo( e1 : Expr ) extends  Expr

  def mostrar(e : Expr) : String = e match {
    case Numero(n) => s" $n "
    case Atomo(n) => s" ${n} "
    case Suma(e1,e2) => s" (${mostrar(e1)} + ${mostrar(e2)}) "
    case Prod(e1,e2) => s" (${mostrar(e1)} * ${mostrar(e2)}) "
    case Resta(e1,e2) => s" (${mostrar(e1)} - ${mostrar(e2)}) "
    case Div(e1,e2) => s" (${mostrar(e1)} / ${mostrar(e2)}) "
    case Expo(e1,e2) => s" (${mostrar(e1)} ^ ${mostrar(e2)}) "
    case Logaritmo(e1) => s" (lg(${mostrar(e1)})) "

  }

  def getAtomValue(a : Atomo): Char = a match {
    case Atomo(x) => x
  }

  def derivar(f: Expr, a: Atomo): Expr = f match {

    case Numero(n) => Numero(0)
    case Atomo(n) if n != getAtomValue(a) => Numero(0)
    case Atomo(n) if n == getAtomValue(a) => Numero(1)
    case Suma(e1, e2) => Suma(derivar(e1, a), derivar(e2, a))
    case Prod(e1, e2) => Suma(Prod(derivar(e1, a), e2), Prod(e1, derivar(e2, a)))
    case Resta(e1, e2) => Resta(derivar(e1, a), derivar(e2, a))
    case Div(e1, e2) => Div(Resta(Prod(derivar(e1, a), e2), Prod(e1, derivar(e2, a))), Expo(e2, Numero(2)))
    case Expo(e1, e2) => Prod(Expo(e1, e2), Suma(Div(Prod(derivar(e1, a), e2), e1), Prod(derivar(e2, a), Logaritmo(e1))))
    case Logaritmo(e1) => Div(derivar(e1, a), e1)

  }

  def evaluar(f : Expr,a : Atomo, v : Double) : Double = f match {

    case Numero(n) => n
    case Atomo(x) if (x == getAtomValue(a)) => v
    case Suma(e1,e2) => evaluar(e1,a,v) + evaluar(e2,a,v)
    case Prod(e1,e2) => evaluar(e1,a,v) * evaluar(e2,a,v)
    case Resta(e1,e2) => evaluar(e1,a,v) - evaluar(e2,a,v)
    case Div(e1,e2) => {
      if (e2 == Numero(0.0)) throw new Error ("División por cero, se indetermina")
      else evaluar(e1,a,v) / evaluar(e2,a,v)
    }
    case Expo(e1,e2) => pow(evaluar(e1,a,v),evaluar(e2,a,v))
    case Logaritmo(e1) => log(evaluar(e1,a,v))
    case Atomo(x)if x != getAtomValue(a)  => throw new Error ("Variable desconocida")


  }

  def limpiar(f : Expr) : Expr = f match {

    case Numero(n) => Numero(n)
    case Atomo(n) => Atomo(n)

    case Suma(e1,e2) => {

      if (limpiar(e1) == Numero(0.0)) limpiar(e2)
      else if (limpiar(e2) == Numero(0.0)) limpiar(e1)
      else Suma(limpiar(e1),limpiar(e2))

    }

    case Prod(e1,e2) => {

      if (limpiar(e1) == Numero(1.0)) limpiar(e2)
      else if (limpiar(e2) == Numero(1.0)) limpiar(e1)
      else if (limpiar(e1) == Numero(0.0)) Numero(0.0)
      else if (limpiar(e2) == Numero(0.0)) Numero(0.0)
      else Prod(limpiar(e1),limpiar(e2))

    }

    case Resta(e1,e2) => {

      if (limpiar(e2) == Numero(0.0)) limpiar(e1)
      else if (limpiar(e1) == Numero(0.0)) Prod(Numero(-1),limpiar(e2))
      else Resta(limpiar(e1),limpiar(e2))
    }

    case Div(e1,e2) => {

      if (limpiar(e1) == Numero(0.0)) Numero(0.0)
      else if (limpiar(e1) == limpiar(e2)) Numero(1.0)
      else if (limpiar(e2) == Numero(1.0)) limpiar(e1)
      else Div(limpiar(e1),limpiar(e2))

    }

    case Expo(e1,e2) => {

      if (limpiar(e1) == Numero(0.0)) Numero(0.0)
      else if (limpiar(e1) == Numero(1.0)) Numero(1.0)
      else if (limpiar(e2) == Numero(0.0)) Numero(1.0)
      else if (limpiar(e2) == Numero(1.0)) limpiar(e1)
      else Expo(limpiar(e1),limpiar(e2))

    }

    case Logaritmo(e1) => {


      if (limpiar(e1) == Numero(1.0)) Numero(0.0)
      else Logaritmo(limpiar(e1))

    }
  }



  def raizNewton(f: Expr, a : Atomo ,x0: Double,
                 ba: (Expr, Atomo, Double) => Boolean): Double = {

    def aux(f : Expr, df : Expr, x0 : Double) : Double = {

      if (ba(f, a, x0)) {
        x0
      }else {
        val new_aprox: Double  = x0 - ((evaluar(f, a, x0)) / (evaluar(df, a, x0)))
        aux(f, df,new_aprox)

      }

    }

    aux(f,derivar(f,a),x0)

  }


}
