package ar.edu.utn.tadp.dragonball

import ar.edu.utn.tadp.dragonball.movimientos.Movimiento

class PlanDeLucha (var movimientos:List[Movimiento]){

  def cantidadRounds : Int = movimientos.size  
  
}