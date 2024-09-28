import Newton._

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
/**
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