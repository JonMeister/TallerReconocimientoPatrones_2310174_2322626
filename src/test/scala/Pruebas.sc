import Newton._

val expr1 = Suma(Atomo('x'), Numero(2))
val expr2 = Prod(Atomo('x'), Atomo('x'))
val expr3 = Suma(expr1, Expo(expr2, Numero(5)))
val expr4 = Logaritmo(Atomo('x'))
val expr5 = Prod(Div(expr1, expr2), Resta(expr3, expr4))
val expr6 = Expo(Atomo('x'), Numero(3))
//Casos de prueba MOSTRAR
mostrar(expr1)
mostrar(expr2)
mostrar(expr3)
mostrar(expr4)
mostrar(expr5)
mostrar(expr6)

//Casos de prueba DERIVAR
mostrar(derivar(expr6, Atomo('x')))
mostrar(derivar(expr2, Atomo('x')))
mostrar(derivar(Div(Atomo('x'), Suma(Atomo('x'), Numero(1.0))), Atomo('x')))
mostrar(derivar(Suma(Atomo('k'), Prod(Numero(3.0), Atomo('x'))), Atomo('x')))
mostrar(derivar(Prod(Logaritmo(Atomo('x')),Suma(Atomo('x'),Numero(2.0))), Atomo('x')))
/*mostrar(Numero(5.0))
evaluar(Numero(5.0), Atomo('x'), 1.0)
mostrar(Atomo('x'))
evaluar(Atomo('x'), Atomo('x'), 5.0)
mostrar(Resta(expr1, expr2))
evaluar(Resta(expr1, expr2), Atomo('x'), 5.0)*/

//Casos de prueba EVALUAR
mostrar(Suma(expr1, expr2))
evaluar(Suma(expr1, expr2), Atomo('x'), 5.0)
mostrar(Prod(expr1, expr2))
evaluar(Prod(expr1, expr2), Atomo('x'), 5.0)
mostrar(Div(expr1, expr2))
evaluar(Div(expr1, expr2), Atomo('x'), 5.0)
mostrar(Expo(expr1, expr2))
evaluar(Expo(expr1, expr2), Atomo('x'), 5.0)
mostrar(Logaritmo(expr1))
evaluar(Logaritmo(expr1), Atomo('x'), 5.0)

//Casos de prueba LIMPIAR
mostrar(limpiar(derivar(expr6, Atomo('x'))))
mostrar(limpiar(derivar(expr2, Atomo('x'))))
mostrar(limpiar(derivar(Div(Atomo('x'), Suma(Atomo('x'), Numero(1.0))), Atomo('x'))))
mostrar(limpiar(derivar(Suma(Atomo('k'), Prod(Numero(3.0), Atomo('x'))), Atomo('x'))))
mostrar(limpiar(derivar(Prod(Logaritmo(Atomo('x')), Suma(Atomo('x'), Numero(2.0))), Atomo('x'))))
def buenaAprox(f: Expr, a: Atomo, d: Double): Boolean = {
  evaluar(f, a, d) <= 0.00001
}
val e1 = Resta(Prod(Atomo('x'), Atomo('x')), Numero(2.0))
val e2 = Resta(Prod(Atomo('x'), Atomo('x')), Numero(4.0))
val e3 = Suma(Resta(Prod(Atomo('x'), Atomo('x')), Numero(4.0)), Prod(Numero(3.0), Atomo('x')))
val e4 = Resta(Div(Numero(10.0), Atomo('x')), Numero(1.0))  // 10 / x - 1
val e5 = Suma(Expo(Atomo('x'), Numero(5.0)), Logaritmo(Atomo('x'))) // x^2 + log(x)

//Casos de prueba raizNewton
raizNewton(e1, Atomo('x'), 2.0, buenaAprox)
raizNewton(e2, Atomo('x'), 2.0, buenaAprox)
raizNewton(e3, Atomo('x'), 2.0, buenaAprox)
raizNewton(e4, Atomo('x'), 2.0, buenaAprox)
raizNewton(e5, Atomo('x'), 2.0, buenaAprox)
