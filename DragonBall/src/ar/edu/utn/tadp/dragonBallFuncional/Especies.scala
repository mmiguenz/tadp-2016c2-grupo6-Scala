package ar.edu.utn.tadp.dragonBallFuncional

trait Especie {
    //type i = (Guerrero,Guerrero)
  
}

trait Fusionable {
  
}

//case class Saiyajin(cola:Boolean) extends Especie
//case class SuperSaiyajin(nivel:Int) extends Especie
case class Saiyajin(cola:Boolean,estado:EstadoSaiyajin) extends Especie with Fusionable
case class Androide(bateria:Int) extends Especie
case class Monstruo(tipomonstruo:TipoMonstruo) extends Especie
case object Humano extends Especie with Fusionable
case object Namekusei extends Especie with Fusionable
case class Fusionado(guerreroOriginal:Guerrero) extends Especie

trait EstadoSaiyajin
case class SuperSaiyajin (nivel: Int = 1) extends EstadoSaiyajin
case object MonoGigante extends EstadoSaiyajin
case object Normal extends EstadoSaiyajin


trait TipoMonstruo{
  def apply(guerrero:Guerrero,oponente:Guerrero)= (guerrero,oponente) 
}
  
case object Cell extends TipoMonstruo {
	 override def apply(guerrero:Guerrero,oponente:Guerrero)= {
	    oponente.especie match {
	        case Androide(_) => (guerrero.copy(listaMov = guerrero.listaMov:::oponente.listaMov), oponente.morir)
	       
	        case _ => (guerrero,oponente)
	    }
	 }
		
}

case object MayinBoo extends TipoMonstruo {
	 override def apply(guerrero:Guerrero,oponente:Guerrero)= {
	   (guerrero.copy(listaMov = oponente.listaMov), oponente.morir)
	 }
		
}




