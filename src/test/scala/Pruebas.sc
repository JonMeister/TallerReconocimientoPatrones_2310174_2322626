import Newton._

val expr1 = Suma(Atomo('x'), Numero(2))
val expr2 = Prod(Atomo('x'), Atomo('x'))
val expr3 = Suma(expr1, Expo(expr2, Numero(5)))
val expr4 = Logaritmo(Atomo('x'))
val expr5 = Prod(Div(expr1, expr2), Resta(expr3, expr4))
val expr6 = Expo(Atomo('x'), Numero(3))
mostrar(expr1)
mostrar(expr2)
mostrar(expr3)
mostrar(expr4)
mostrar(expr5)
mostrar(expr6)
mostrar(derivar(expr6, Atomo('x')))
mostrar(derivar(expr2, Atomo('x')))
mostrar(derivar(expr2, Atomo('y')))
mostrar(derivar(Suma(Atomo('k'), Prod(Numero(3.0), Atomo('x'))), Atomo('x')))
mostrar(Numero(5.0))
evaluar(Numero(5.0), Atomo('x'), 1.0)
mostrar(Atomo('x'))
evaluar(Atomo('x'), Atomo('x'), 5.0)
mostrar(Suma(expr1, expr2))
evaluar(Suma(expr1, expr2), Atomo('x'), 5.0)
mostrar(Prod(expr1, expr2))
evaluar(Prod(expr1, expr2), Atomo('x'), 5.0)
mostrar(Resta(expr1, expr2))
evaluar(Resta(expr1, expr2), Atomo('x'), 5.0)
mostrar(Div(expr1, expr2))
evaluar(Div(expr1, expr2), Atomo('x'), 5.0)
mostrar(Expo(expr1, expr2))
evaluar(Expo(expr1, expr2), Atomo('x'), 5.0)
mostrar(Logaritmo(expr1))
evaluar(Logaritmo(expr1), Atomo('x'), 5.0)
limpiar(derivar(Suma(Atomo('k'), Prod(Numero(3.0), Atomo('x'))), Atomo('x')))
mostrar(limpiar(derivar(Suma(Atomo('k'), Prod(Numero(3.0), Atomo('x'))), Atomo('x'))))

def buenaAprox(f: Expr, a: Atomo, d: Double): Boolean = {
  evaluar(f, a, d) <= 0.00001
}
val e1 = Resta(Prod(Atomo('x'), Atomo('x')), Numero(2.0))
val e2 = Resta(Prod(Atomo('x'), Atomo('x')), Numero(4.0))
val e3 = Suma(Resta(Prod(Atomo('x'), Atomo('x')), Numero(4.0)), Prod(Numero(3.0), Atomo('x')))
val e4 = Resta(Div(Numero(10.0), Atomo('x')), Numero(1.0))  // 10 / x - 1
val e5 = Suma(Expo(Atomo('x'), Numero(5.0)), Logaritmo(Atomo('x'))) // x^2 + log(x)
raizNewton(e1, Atomo('x'), 2.0, buenaAprox)
raizNewton(e2, Atomo('x'), 2.0, buenaAprox)
raizNewton(e3, Atomo('x'), 2.0, buenaAprox)
raizNewton(e4, Atomo('x'), 2.0, buenaAprox)
raizNewton(e5, Atomo('x'), 2.0, buenaAprox)

/**
object Pruebas {
  def main(args: Array[String]): Unit = {
    val expr1 = Suma(Atomo('x'), Numero(2.0))
    val expr2 = Prod(Atomo('x'), Atomo('x'))
    val expr3 = Suma(expr1, Expo(expr2,Numero(5)))
    val expr4 = Logaritmo(Atomo('x'))
    val expr5 = Prod(Div(expr1, expr2), Resta(expr3,expr4))
    val expr6 = Expo(Atomo('x'),Numero(3))
    println(mostrar(expr1))
    println(mostrar(expr2))
    println(mostrar(expr3))
    println(mostrar(expr4))
    println(mostrar(expr5))
    println(mostrar(expr6))

    val expr7 = Resta(Expo(Atomo('x'),Numero(9)),Numero(2))
    val expr8 = Expo(Atomo('x'),Numero(3))
    println(mostrar(expr8))
    println(RaizNewton(expr8,Atomo('x'),2.0,buenaAprox))

  }
}

 val e1 = Suma(Atomo('x'),Numero(2))

 val e2 = Prod(Atomo('x'),Atomo('x'))

 val e3 = Suma(e1, Expo(e2,Numero(5)))

 val e4 = Logaritmo(Atomo('x'))

 val e5 = Prod(Div(e1,e2), Resta(e3,e4))

 val e6 = Expo(Atomo('x'),Numero(3))

 mostrar(e1)
 mostrar(e2)
 mostrar(e3)
 mostrar(e4)
 mostrar(e5)
 mostrar(e6)

 val  d1 = derivar(e6,Atomo('x'))

 val l1_d1 = limpiar(d1)



 mostrar(limpiar(derivar(Suma(Atomo('k'), Prod(Numero(3.0),Atomo('x'))),Atomo('x')))



 **/