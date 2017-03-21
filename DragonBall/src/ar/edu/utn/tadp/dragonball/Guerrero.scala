package ar.edu.utn.tadp.dragonball

import ar.edu.utn.tadp.dragonball.movimientos.Movimiento


trait  Guerrero{
  var ki : Int = 0
  var kiMaximo : Int = 0
  var nombre : String = ""
  var inconsciente : Boolean = false
  var inventario : List[Item] = List()
  var movimientos : List[Movimiento] = List()

  
  
  def aprender(m : Movimiento) = (movimientos = movimientos.::(m))
  def movimientosMasEfectivoContra(g:Guerrero) (c:Criterio) = movimientos.toList.maxBy { m => c.analizar(this, m, g) } 
  def pelearContra(g : Guerrero, p: PlanDeLucha) = ??? 
  def pelearRound(m : Movimiento, g : Guerrero) = ???
 }