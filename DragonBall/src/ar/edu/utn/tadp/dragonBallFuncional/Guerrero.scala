package ar.edu.utn.tadp.dragonBallFuncional
import scala.util.Try
import com.sun.org.apache.xalan.internal.xsltc.compiler.Copy
import ar.edu.utn.tadp.dragonball.Esfera

//import ar.edu.utn.tadp.dragonBallFuncional.Criterios



case class Guerrero( ki:Int
                    ,kiMaximo:Int
                    ,nombre:String
                    ,vecesFajado:Int
                    ,inconsciente:Boolean
                    ,inventario:List[Item]
                    ,especie:Especie
                    ,listaMov:List[Movimientos])
{
  type ItemGuerrero = (Item,Guerrero)
  type Criterio = (Guerrero, Guerrero) => Int

  type Movimiento = (Guerrero,Guerrero) => (Guerrero,Guerrero) 

  def incrementarKi(incremento:Int):Guerrero ={
    this.especie match {
        case Androide(bateria) => copy( vecesFajado = 0,especie = Androide((bateria + incremento).min(kiMaximo)))
        case _ => copy( ki = (ki + incremento).min(kiMaximo) , vecesFajado = 0)
    }
  }
  
  def estaVivo = this.especie match {
    case Androide(bateria) => bateria>0
    case _ => this.ki>0
  }
  
  def disminuirKi(reduccion:Int):Guerrero ={
    this.especie match {
      case Androide(bateria) => copy(vecesFajado = 0,especie = Androide((bateria - reduccion).max(0)))
      case Fusionado(guerreroOriginal) if (this.ki - reduccion <= 0) => this.morir
      case _ => copy( vecesFajado = 0, ki = (ki - reduccion).max(0) )
    }
  }
  
  def pelearRound(mov:Movimiento)(oponente:Guerrero)= {
    
    val estadoDespuesDeQueAtaqueYo = hacerMovimiento(mov, oponente)
    val movimientoDeOponente = estadoDespuesDeQueAtaqueYo._2.movimientoMasEfectivoContra(estadoDespuesDeQueAtaqueYo._1)(Criterios.atacante) 
    val resultadoDeRoundInvertido = estadoDespuesDeQueAtaqueYo._2.hacerMovimiento(movimientoDeOponente, estadoDespuesDeQueAtaqueYo._1)
    //Invierto el orden de los guerreros (al estado original) en la tupla para que se pueda repetir el proceso sin confundirse
    (resultadoDeRoundInvertido._2, resultadoDeRoundInvertido._1)
  }
  
  
  def planDeAtaque(oponente:Guerrero, cantidadDeRounds:Int)(criterio:Criterio) = {
    planificar(this, oponente, criterio, cantidadDeRounds)
  }
  
 
  
  def planificar(yo:Guerrero, oponente:Guerrero, criterio:Criterio, cantidadDeRoundsRestantes:Int):List[Movimiento] = 
  {
    if(cantidadDeRoundsRestantes==1)
    {
      List(movimientoMasEfectivoContra(oponente)(criterio))
    }
    else
    {
      val mov = yo.movimientoMasEfectivoContra(oponente)(criterio)
      val resultadoDeRound = yo.pelearRound(mov)(oponente)
      return List(mov).:::(planificar(resultadoDeRound._1, resultadoDeRound._2, criterio, cantidadDeRoundsRestantes-1))
    }
  }
   
  def pelearContra(oponente:Guerrero)(plan:List[Movimiento]):Resultado = {
    
    def estanVivos (par:(Guerrero,Guerrero)) = par._1.estaVivo && par._2.estaVivo
    def peleen (par:(Guerrero,Guerrero), m:Movimiento) = if (estanVivos(par)) par._1.pelearRound(m)(par._2) else par
    def elDeMayorKi(par:(Guerrero,Guerrero)) = if(par._1.ki > par._2.ki) par._1 else par._2 
    
    val parFinal = plan.foldLeft((this,oponente)){(par,m)=> peleen(par,m)}
    
    if(estanVivos(parFinal))
    {
      return new ResultadoParcial(parFinal._1,parFinal._2)
    }
    else
    {
      return new ResultadoFinal(elDeMayorKi(parFinal))
    }
      
    
  }
  
 
  
  def obtenerKi():Int ={
    this.especie match {
      case Androide(bateria) => bateria
      case _ => ki
    }
  }
  
  
  def movimientoMasEfectivoContra(oponente:Guerrero)(criterio:Criterio):Movimiento= {
    
  val listMovValor =   
    for {      
      mov <- listaMov      
      tupla <- Option(hacerMovimiento(mov.apply _,oponente))      
      valor = criterio(tupla._1,tupla._2)
      
    } yield (mov.apply _,valor) 
   
     listMovValor.maxBy(_._2)._1    
  }

  def morir:Guerrero = {
    
    this.especie match {
      
      case Androide(bateria) => copy(especie= Androide(0))
      case Fusionado(guerreroOriginal) => guerreroOriginal.copy(ki = 0)
      case _ => copy( ki = 0 ) 
    }
    
  }   
   


  def dejarInconsciente = {otroGuerrero:Guerrero => 
    otroGuerrero.especie match {
      case Androide(_) => (copy(), otroGuerrero.copy())
      case Saiyajin(cola,SuperSaiyajin(_)) => (copy(), otroGuerrero.copy(especie = Saiyajin(cola,Normal)))
      case Fusionado(guerreroOriginal) => if (otroGuerrero.ki < 300) (copy(),(guerreroOriginal.copy(inconsciente = true))) else (copy(),otroGuerrero.copy())
      case _ => if (otroGuerrero.ki < 300) (copy(),otroGuerrero.copy(inconsciente = true)) else (copy(),otroGuerrero.copy())
    }
  }
  
  def usarMunicion(unItem:Item, muni:Int) = {
      val nuevoItem = unItem.asInstanceOf[ArmaFuego].copy(municion = muni -1).asInstanceOf[Item]
      val nuevoInventario = this.inventario.diff(List(unItem)).::(nuevoItem)
      this.copy(inventario = nuevoInventario)
  }
  
  def construirFusionado = {otroGuerrero:Guerrero =>
   
    (copy(ki = ki + otroGuerrero.ki, kiMaximo = kiMaximo + otroGuerrero.kiMaximo, especie = Fusionado(this), listaMov = this.listaMov:::otroGuerrero.listaMov) ,otroGuerrero)
  }
  
  def hacerPaseMagia = {otroGuerrero:Guerrero =>
      (copy(ki=kiMaximo,kiMaximo = kiMaximo * 2), Option(otroGuerrero.copy(inventario = otroGuerrero.inventario.diff(otroGuerrero.inventario) )))
  }
  
  def usarEsferas:Guerrero =this.copy(inventario = this.inventario.filterNot {x => x == EsferaDragon})
    
  
  def hacerMovimiento(movimiento:(Guerrero,Guerrero) => (Guerrero,Guerrero), oponente:Guerrero):(Guerrero,Guerrero) = movimiento(this, oponente )
  
}