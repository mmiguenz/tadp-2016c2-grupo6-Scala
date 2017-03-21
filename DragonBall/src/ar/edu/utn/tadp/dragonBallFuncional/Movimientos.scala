package ar.edu.utn.tadp.dragonBallFuncional
import scala.util.Try
import scala.math._

trait TipoAtaque{
  
}

case object Fisico extends TipoAtaque
case object DeEnergia extends TipoAtaque

trait Movimientos {
    
	  def apply(guerrero:Guerrero,oponente:Guerrero): (Guerrero,Guerrero)
	  
	  def modificarKi(unGuerrero:Guerrero,ki:Int,tipoAtaque:TipoAtaque):Guerrero = {
			  (unGuerrero.especie, tipoAtaque) match {
			      case (Androide(_),DeEnergia) =>  unGuerrero.incrementarKi(ki)
			      case _ => unGuerrero.disminuirKi(ki)
			  }
	      
	  }
	}

case object dejarseFajar extends Movimientos{
		def apply(guerrero:Guerrero,oponente:Guerrero)= {
		  (guerrero.copy(vecesFajado = guerrero.vecesFajado + 1 ), oponente)
		}

	}
case object cargarKi extends Movimientos {
  	def apply(guerrero:Guerrero,oponente:Guerrero)= {
  	  
      	guerrero.especie match {
      	case Saiyajin(_,SuperSaiyajin(nivel)) => (guerrero.incrementarKi(150 * nivel), oponente)
      	case Androide(_) => (guerrero.copy(), oponente)
      	case _ => (guerrero.incrementarKi(100), oponente)
      	}
  	}
}

case class usarItem(unItem:Item) extends Movimientos {
		def apply(guerrero:Guerrero,oponente:Guerrero)= {
          guerrero.inventario.find(x => x == unItem) match {
		        case Some(_)  => unItem.accionar(guerrero, oponente)
		        case _  => (guerrero,oponente)
		        }
		  }
	}

	case object  comerserAlOponente extends Movimientos{
	def apply(guerrero:Guerrero,oponente:Guerrero)= {      
    (guerrero.especie, oponente.especie) match {
				case (Monstruo(digerir), Androide(bateria)) if (guerrero.ki > bateria) => digerir(guerrero,oponente)
				case (Monstruo(digerir), _) if (guerrero.ki > oponente.ki) => digerir(guerrero,oponente)
				case _ => (guerrero, oponente)

		}
	}

}

	case object ConvertirseEnMono extends Movimientos{
  def apply(guerrero:Guerrero,oponente:Guerrero)= {
    
     guerrero.especie match {
       
      case Saiyajin(true,Normal) | Saiyajin(true,SuperSaiyajin(_)) if guerrero.inventario.exists{x =>  x==FotoLuna}  => (guerrero.incrementarKi(guerrero.kiMaximo - guerrero.ki).copy(especie = Saiyajin(true,MonoGigante), kiMaximo = 3*guerrero.kiMaximo), oponente)
      case  _ => (guerrero,oponente)
       
     }
  }
  
}
	
	case object ConvertirseEnSuperSaiyajin extends Movimientos{
  def apply(guerrero:Guerrero,oponente:Guerrero)= {
    
     guerrero.especie match {
       
      case Saiyajin(cola,Normal) if (guerrero.ki*2 >= guerrero.kiMaximo) => (guerrero.copy(especie = Saiyajin(cola,SuperSaiyajin(1))).copy(kiMaximo = 5*guerrero.kiMaximo), oponente)
      case Saiyajin(cola,SuperSaiyajin(nivel)) if (guerrero.ki*2 >= guerrero.kiMaximo) => (guerrero.copy(especie = Saiyajin(cola,SuperSaiyajin(nivel + 1))).copy(kiMaximo = 5*guerrero.kiMaximo*(nivel + 1)), oponente)
      case  _ => (guerrero,oponente)
       
     }
  }
  
}
	
	case object Fusion  extends Movimientos{
  
  def apply(guerrero:Guerrero,oponente:Guerrero)= {          
    (guerrero.especie, oponente.especie)  match {
        case (_:Fusionable,_:Fusionable) => (guerrero.construirFusionado(oponente))
        case _ => (guerrero,oponente)
    }
  } 
}

	case object Magia extends Movimientos{
  
	  
  def apply(guerrero:Guerrero,oponente:Guerrero)= {
    val magicos = List(Namekusei,Monstruo)
    
    magicos.find { x => x == guerrero.especie } match{

      case Some(_) => (guerrero.copy(ki=guerrero.kiMaximo,kiMaximo = guerrero.kiMaximo * 2), oponente.copy(inventario = oponente.inventario.diff(oponente.inventario) ))
      case _ if (guerrero.inventario.count(_ == EsferaDragon) >= 7) => (guerrero.usarEsferas.copy(ki=guerrero.kiMaximo,kiMaximo = guerrero.kiMaximo * 2), oponente.copy(inventario = oponente.inventario.diff(oponente.inventario) ))
      case _ => (guerrero,oponente)
  }
  
  
}
  
}

trait Atacar {

	val multiploExplosion = {especie:Especie =>
	especie match {
	case Androide(_) => 3
	case _ => 2
	}
	}
}

case object MuchosGolpesNinja extends Movimientos with Atacar
{
	  def apply(guerrero:Guerrero,oponente:Guerrero)= {
        (guerrero.especie,oponente.especie) match {
           case (Humano,Androide(_)) => (modificarKi(guerrero,10,Fisico),oponente)
           case _ if (guerrero.obtenerKi >= oponente.obtenerKi()) => (guerrero,modificarKi(oponente, 20, Fisico))
           case _ => (modificarKi(guerrero, 20, Fisico),oponente)
          }
    }
}

case object Explotar extends Movimientos with Atacar
{
			def apply(guerrero:Guerrero,oponente:Guerrero)= {
        (guerrero.especie,oponente.especie) match {
           case (Monstruo(_) | Androide(_), Namekusei) => (guerrero.morir,oponente.copy(ki = (modificarKi(oponente,multiploExplosion(oponente.especie) * guerrero.obtenerKi(),Fisico).ki.max(1))))
           case (Monstruo(_) | Androide(_), Androide(_)) => (guerrero.morir, oponente.copy(especie = (modificarKi(oponente,multiploExplosion(oponente.especie) * guerrero.obtenerKi(),Fisico).especie)))
           case (Monstruo(_) | Androide(_), _) => (guerrero.morir, oponente.copy(ki = (modificarKi(oponente,multiploExplosion(oponente.especie) * guerrero.obtenerKi(),Fisico).obtenerKi())))
           case _ => (guerrero,oponente)
           }
		}
}

case class Onda(kiLanzado: Int) extends Movimientos with Atacar
{
	def apply(guerrero:Guerrero,oponente:Guerrero)= {
      oponente.especie match {
          case Monstruo(_) if (guerrero.obtenerKi() >= kiLanzado) => (guerrero.disminuirKi(kiLanzado),modificarKi(oponente,(kiLanzado / 2),DeEnergia))
          case _ if (guerrero.obtenerKi() >= kiLanzado) => (guerrero.disminuirKi(kiLanzado),modificarKi(oponente, (kiLanzado * 2),DeEnergia))
          case _ => (guerrero,oponente)
  		}
   }
} 

case object Genkidama extends Movimientos with Atacar
{
		def apply(guerrero:Guerrero,oponente:Guerrero)= { 
			  (guerrero.copy(vecesFajado = 0), modificarKi(oponente,Math.pow(10, guerrero.vecesFajado).toInt,DeEnergia))
		}
}

