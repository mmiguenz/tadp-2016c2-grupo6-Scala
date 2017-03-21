package ar.edu.utn.tadp.dragonBallFuncional

trait Resultado {
}

case class ResultadoParcial(g1:Guerrero,g2:Guerrero) extends Resultado 
 
case class ResultadoFinal(vencedor:Guerrero) extends Resultado
  
