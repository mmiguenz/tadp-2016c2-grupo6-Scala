package ar.edu.utn.tadp.dragonball

import ar.edu.utn.tadp.dragonball.movimientos.Movimiento

class Saiyajin (var Fusionado: Guerrero, var cola:Boolean) extends Guerrero{
  
   var estado: EstadoSaiyajin = null

   override def pelearRound(m : Movimiento, g : Guerrero) = ??? 
   
   
   //def transformarse(nivelExaltacion: Int):Saiyajin = {
  //   var guerrero = new Saiyajin(this.Fusionado,this.cola)
   
  //   guerrero.estado = new SuperSaiyanjin(nivelExaltacion)
    // return guerrero     
 }
