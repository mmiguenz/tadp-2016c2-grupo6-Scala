package ar.edu.utn.tadp.dragonball

import ar.edu.utn.tadp.dragonball.movimientos.Movimiento

trait Criterio {
   def  analizar (guerrero : Guerrero, movimiento: Movimiento, oponente : Guerrero) : Int = ???
  
}

