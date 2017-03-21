package ar.edu.utn.tadp.dragonBallFuncional

trait Item {
  
  def accionar(guerrero:Guerrero, oponente:Guerrero):(Guerrero, Guerrero)
  
}

case object ArmaRoma extends Item{
  
   def accionar(guerrero:Guerrero, oponente:Guerrero):(Guerrero, Guerrero) = {
      if (oponente.ki < 300)
          (guerrero, guerrero.dejarInconsciente(oponente.copy())._2)
      else
          (guerrero,oponente.copy())
  }
  
}
case object ArmaFilosa extends Item{
  
  def accionar(guerrero:Guerrero, oponente:Guerrero):(Guerrero, Guerrero) = {   
      var nuevoGuerrero = oponente.disminuirKi(guerrero.ki/100)
      nuevoGuerrero.especie match {
        case Saiyajin(true,MonoGigante) => (guerrero,nuevoGuerrero.copy(especie = new Saiyajin(false,Normal), ki = 1, inconsciente = true))
        case Saiyajin(true,miEstado) => (guerrero, nuevoGuerrero.copy(especie = new Saiyajin(false,miEstado), ki = 1))
        case _ => (guerrero,nuevoGuerrero) 
      }
  }  
}

case class ArmaFuego(municion:Int) extends Item {
  
  
  def accionar(guerrero:Guerrero, oponente:Guerrero):(Guerrero, Guerrero) =
  {
    oponente.especie match {
      case Humano if (municion > 0) => (guerrero.usarMunicion(this,municion), oponente.copy(ki = oponente.disminuirKi(20).ki))
      case Namekusei if (municion > 0) && (oponente.inconsciente == true)  => (guerrero.usarMunicion(this,municion), oponente.copy(ki = (oponente.disminuirKi(10).ki)))
      case _ => (guerrero,oponente)
    }
  }

  
}

case object SemillaErmitaño extends Item{
  
  def accionar(guerrero:Guerrero, oponente:Guerrero):(Guerrero, Guerrero) = (guerrero, oponente.incrementarKi(oponente.kiMaximo - oponente.ki))
  
}
  
case object EsferaDragon extends Item{
  
  def accionar(guerrero:Guerrero, oponente:Guerrero):(Guerrero, Guerrero) = ???
  
}


case object FotoLuna extends Item{
  
  def accionar(guerrero:Guerrero, oponente:Guerrero):(Guerrero, Guerrero) = ???
  
}

