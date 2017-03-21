package ar.edu.utn.tadp.dragonBallFuncional

package object Criterios {
  
    
  type Criterio = (Guerrero, Guerrero) => Int

  def tacanio:Criterio =  (g1:Guerrero, g2:Guerrero) => 100*(g1.inventario.length)

  def defensivo:Criterio = (g1:Guerrero, g2:Guerrero) => 10*(g1.ki)
  
  def atacante:Criterio = (g1:Guerrero, g2:Guerrero) => 10*(g1.ki - g2.ki)

 
}