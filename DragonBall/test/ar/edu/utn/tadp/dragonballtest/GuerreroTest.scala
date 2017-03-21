package ar.edu.utn.tadp.dragonballtest


import scala.util.{ Success, Failure, Try }
import org.junit.Assert._
import org.junit.Test
import org.junit.Before
import org.junit.runner.RunWith
import ar.edu.utn.tadp.dragonBallFuncional._
import scala.util.Try

class GuerreroTest extends {
  
  @Test
  def `test dejarse fajar 1 ` ={  
   var unGuerrero = new Guerrero(100,200,"guerrero1",0, false, null ,new Saiyajin(false,Normal),null)
   var otroGuerrero = new Guerrero(100,200,"guerrero1",0, false, null ,new Saiyajin(false,Normal),null)
   var mov = dejarseFajar
   
   var guerrerotupla = mov(unGuerrero,otroGuerrero)

   assertEquals(guerrerotupla._1.ki ,unGuerrero.ki)
   
  }
  
  @Test
  def `test dejarse fajar 2 ` ={
     var unGuerrero = new Guerrero(100,200,"guerrero1",0, false, null ,new Saiyajin(false,Normal),null)
     var otroGuerrero = new Guerrero(100,200,"guerrero1",0, false, null ,new Saiyajin(false,Normal),null)
     var mov = dejarseFajar
     
     var guerrerotupla = mov(unGuerrero,otroGuerrero)
     
     
     assertEquals(guerrerotupla._1.vecesFajado ,1)
   
  }

  @Test
  def `test cargar ki 1 ` ={
   var unGuerrero = new Guerrero(100,200,"guerrero1",0, false, null ,new Saiyajin(false,Normal),null)
   var otroGuerrero = new Guerrero(100,200,"guerrero1",0, false, null ,new Saiyajin(false,Normal),null)
   var mov = cargarKi
   
   var guerrerotupla = mov(unGuerrero,otroGuerrero)
   
   
   assertEquals(guerrerotupla._1.ki ,200)
   
  }

  @Test
  def `test usar item arma roma ` ={
    var unGuerrero = new Guerrero(150,200,"guerrero1",0,false, List[Item](ArmaFilosa,ArmaRoma,FotoLuna) ,new Saiyajin(false,Normal),null)
    var otroGuerrero = new Guerrero(150,500,"guerrero1",0,false, null ,new Saiyajin(true,MonoGigante),null)

    var mov = usarItem(ArmaRoma)
    var guerrerotupla = mov(unGuerrero,otroGuerrero)
    
    assertEquals(guerrerotupla._2.inconsciente, true)  
    
  }
  
  
  @Test
  def `test usar item arma filosa ` ={

    var unGuerrero = new Guerrero(150,200,"guerrero1",0,false, List[Item](ArmaFilosa,ArmaRoma,FotoLuna) ,new Saiyajin(false,Normal), null)
    var otroGuerrero = new Guerrero(150,500,"guerrero1",0,false, null ,new Saiyajin(true,MonoGigante),null)

    var mov = usarItem(ArmaFilosa)
    var guerrerotupla = mov(unGuerrero,otroGuerrero)
    
    assertEquals(guerrerotupla._2.ki, 1)  
  }

  @Test
  def `test usar item arma fuego ` ={
    var unGuerrero = new Guerrero(150,200,"guerrero1",0,false, List[Item](ArmaFilosa,ArmaFuego(10),FotoLuna) ,new Saiyajin(false,Normal),null)
    var otroGuerrero = new Guerrero(150,500,"guerrero1",0,true, null,Namekusei,null)
    
    var item = ArmaFuego(10)
    
    var mov = usarItem(item)
    var guerrerotupla = mov(unGuerrero,otroGuerrero)

    assertEquals(guerrerotupla._1.inventario, List[Item](ArmaFuego(9),ArmaFilosa,FotoLuna))
    assertEquals(guerrerotupla._2.ki, 140)  
  }

  @Test def `test usar item semilla ermitaÃ±o ` ={
     var unGuerrero = new Guerrero(0,200,"guerrero1",0,true,List[Item](ArmaFilosa,ArmaFuego(10),SemillaErmitaño,FotoLuna), Androide(50),null)
     var primerGuerrero = new Guerrero(0,200,"guerrero1",0,true,List[Item](ArmaFilosa,ArmaFuego(10),SemillaErmitaño,FotoLuna), Androide(50),null)
     var segundoGuerrero = new Guerrero(70,250,"guerrero2",0,false,List[Item](ArmaFilosa,ArmaFuego(10),SemillaErmitaño,FotoLuna), Namekusei,null)
     var tercerGuerrero = new Guerrero(0,121,"guerrero3",0,false,List[Item](ArmaFilosa,ArmaFuego(10),SemillaErmitaño,FotoLuna), Namekusei,null)
 
     var mov = usarItem(SemillaErmitaño)
     
     var guerrerotupla = mov(unGuerrero,primerGuerrero)
     var guerrerotupla2 = mov(unGuerrero,segundoGuerrero)
     var guerrerotupla3 = mov(unGuerrero,tercerGuerrero)
     
     assertEquals(guerrerotupla._2.ki,0)
     assertEquals(guerrerotupla._2.especie,Androide(guerrerotupla._2.kiMaximo))
     assertEquals(guerrerotupla2._2.ki,250)
     assertEquals(guerrerotupla3._2.ki,121)
  }
  
  @Test def `test convertirse en mono gigante ` ={
      var unGuerrero = new Guerrero(70,250,"guerrero1",0,false,List[Item](ArmaFilosa,ArmaFuego(10),SemillaErmitaño,FotoLuna), Saiyajin(true,SuperSaiyajin(1)),null)
      var otroGuerrero = new Guerrero(150,500,"guerrero1",0,true, null,Namekusei,null)
      
      var mov = ConvertirseEnMono
      
      var guerrerotupla = mov(unGuerrero,otroGuerrero)
      
      assertEquals(guerrerotupla._1.especie,Saiyajin(true,MonoGigante))
      assertEquals(guerrerotupla._1.ki,250)
      assertEquals(guerrerotupla._1.kiMaximo,750)
  }
  
  @Test def `test convertirse en Super Saiyajin ` ={
      var unGuerrero = new Guerrero(231,250,"guerrero1",0,false,List[Item](ArmaFilosa,ArmaFuego(10),SemillaErmitaño,FotoLuna), Saiyajin(false,SuperSaiyajin(7)),null)
      var otroGuerrero = new Guerrero(150,500,"guerrero1",0,true, null,Namekusei,null)
      
      var mov = ConvertirseEnSuperSaiyajin
      
      var guerrerotupla = mov(unGuerrero,otroGuerrero)
      
      assertEquals(guerrerotupla._1.especie,Saiyajin(false,SuperSaiyajin(8)))
      assertEquals(guerrerotupla._1.ki,231)
      assertEquals(guerrerotupla._1.kiMaximo,10000)
  }
  
  @Test def `test fusionar ` ={      
      val movimientos : List[Movimientos] = List(dejarseFajar,cargarKi)
      val movimientos2 : List[Movimientos] = List(Genkidama,Explotar)

      var unGuerrero = new Guerrero(70,250,"guerrero1",0,false,List[Item](ArmaFilosa,ArmaFuego(10),SemillaErmitaño,FotoLuna), Saiyajin(false,SuperSaiyajin(1)),movimientos)
      var otroGuerrero = new Guerrero(150,200,"guerrero2",0,false, List[Item](ArmaFilosa,ArmaFuego(10),FotoLuna) ,Saiyajin(true,MonoGigante),movimientos2)

      var mov = Fusion
      
      var tuplaGuerrero = mov(unGuerrero,otroGuerrero)
      
      assertEquals(tuplaGuerrero._1.ki, 220)
      assertEquals(tuplaGuerrero._1.kiMaximo, 450)
      assertEquals(tuplaGuerrero._1.listaMov, List(dejarseFajar,cargarKi,Genkidama,Explotar))
      assertEquals(tuplaGuerrero._1.especie, Fusionado(unGuerrero))
  }
  
  @Test def `test hacer magia ` ={
      var primerAtacante = new Guerrero(70,250,"guerrero1",0,false,List[Item](ArmaFilosa,EsferaDragon,FotoLuna), Namekusei,null)
      var primerAtacado = new Guerrero(150,200,"guerrero2",0,false, List[Item](ArmaFilosa,ArmaRoma,FotoLuna) ,new Saiyajin(false,Normal), null)
      var segundoAtacante = new Guerrero(223,270,"guerrero3",0,false,List[Item](EsferaDragon,ArmaFilosa,EsferaDragon,EsferaDragon,EsferaDragon,EsferaDragon,EsferaDragon,EsferaDragon,EsferaDragon,EsferaDragon,EsferaDragon,EsferaDragon,FotoLuna,EsferaDragon), Humano,null)
      var segundoAtacado = new Guerrero(108,300,"guerrero4",0,false,List[Item](ArmaFilosa,EsferaDragon,ArmaRoma,FotoLuna),Namekusei,null)
    
      var mov = Magia
      
      var guerrerotupla1 = mov(primerAtacante,primerAtacado)
      var guerrerotupla2 = mov(segundoAtacante,segundoAtacado)
      
      //Primer ataque
      assertEquals(guerrerotupla1._1.ki,250)
      assertEquals(guerrerotupla1._1.kiMaximo, 500)
      assertEquals(guerrerotupla1._1.inventario,List[Item](ArmaFilosa,EsferaDragon,FotoLuna))
      assertEquals(guerrerotupla1._2.inventario,List())
    
      //Segundo ataque
      assertEquals(guerrerotupla2._1.ki,270)
      assertEquals(guerrerotupla2._1.kiMaximo,540)
  }

  @Test def `test muchos golpes ninja ` ={
      var primerAtacante = new Guerrero(70,250,"guerrero1",0,false,List[Item](ArmaFilosa,EsferaDragon,FotoLuna), Namekusei,null)
      var primerAtacado = new Guerrero(150,200,"guerrero2",0,false, List[Item](ArmaFilosa,ArmaRoma,FotoLuna) ,new Saiyajin(false,Normal), null)
      
      var segundoAtacante = new Guerrero(200,250,"guerrero3",0,false,List[Item](ArmaFilosa,EsferaDragon,FotoLuna), Humano,null)
      var segundoAtacado = new Guerrero(0,100,"guerrero4",0,false, List[Item](ArmaFilosa,FotoLuna) ,new Androide(30), null)

      var mov = MuchosGolpesNinja
      
      var guerrerotupla1 = mov(primerAtacante,primerAtacado)
      var guerrerotupla2 = mov(segundoAtacante,segundoAtacado)
      
      assertEquals(guerrerotupla1._1.ki,50)
      assertEquals(guerrerotupla1._2.ki,150)
      assertEquals(guerrerotupla2._1.ki,190)
      assertEquals(guerrerotupla2._2.especie,Androide(30))
  }
  
  @Test def `test explotar ` ={
      var primerAtacante = new Guerrero(150,200,"guerrero1",0,false, List[Item](ArmaFilosa,ArmaRoma,FotoLuna) ,Monstruo(Cell), null)
      var primerAtacado = new Guerrero(70,250,"guerrero2",0,false,List[Item](ArmaFilosa,EsferaDragon,FotoLuna), Namekusei,null)
      var segundoAtacante = new Guerrero(0,400,"guerrero3",0,false, List[Item](ArmaFilosa,ArmaRoma,EsferaDragon,EsferaDragon,FotoLuna) ,Androide(200), null)
      var segundoAtacado = new Guerrero(0,800,"guerrero4",0,false, List[Item](ArmaFilosa,ArmaRoma,EsferaDragon,EsferaDragon,FotoLuna) ,Androide(700), null)
 
      var mov = Explotar
      
      var guerrerotupla1 = mov(primerAtacante,primerAtacado)
      var guerrerotupla2 = mov(segundoAtacante,segundoAtacado)
      
      assertEquals(guerrerotupla1._1.ki,0)
      assertEquals(guerrerotupla1._2.ki,1)
      assertEquals(guerrerotupla2._1.especie,Androide(0))
      assertEquals(guerrerotupla2._2.especie,Androide(100))
      
  }
  
  @Test def `test onda de energia ` ={
      var primerAtacante = new Guerrero(0,200,"guerrero1",0,false, List[Item](ArmaFilosa,ArmaRoma,FotoLuna) ,Androide(120), null)
      var primerAtacado = new Guerrero(400,450,"guerrero2",0,false,List[Item](ArmaFilosa,EsferaDragon,FotoLuna), Namekusei,null)
      var segundoAtacante = new Guerrero(480,500,"guerrero3",0,false, List[Item](ArmaFilosa,ArmaRoma,EsferaDragon,EsferaDragon,FotoLuna) ,Humano, null)
      var segundoAtacado = new Guerrero(300,800,"guerrero4",0,false, List[Item](ArmaFilosa,ArmaRoma,EsferaDragon,EsferaDragon,FotoLuna) ,Monstruo(Cell), null)

      var mov = Onda(100)
      
      var guerrerotupla1 = mov(primerAtacante,primerAtacado)
      var guerrerotupla2 = mov(segundoAtacante,segundoAtacado)
      
      assertEquals(guerrerotupla1._1.especie,Androide(20))
      assertEquals(guerrerotupla1._2.ki,200)
      assertEquals(guerrerotupla2._1.ki,380)
      assertEquals(guerrerotupla2._2.ki,250)
  }
  
  @Test def `test genkidama ` ={
      var atacante = new Guerrero(0,200,"guerrero1",4,false, List[Item](ArmaFilosa,ArmaRoma,FotoLuna) ,Androide(120), null)
      var primerAtacado = new Guerrero(400,450,"guerrero2",4,false,List[Item](ArmaFilosa,EsferaDragon,FotoLuna), Namekusei,null)
      var segundoAtacado = new Guerrero(20435,30000,"guerrero3",0,false, List[Item](ArmaFilosa,ArmaRoma,EsferaDragon,EsferaDragon,FotoLuna) ,Monstruo(Cell), null)

      var mov = Genkidama
      
      var guerrerotupla1 = mov(atacante,primerAtacado)
      var guerrerotupla2 = mov(atacante,segundoAtacado)
      
      assertEquals(guerrerotupla1._1.vecesFajado,0)
      assertEquals(guerrerotupla1._2.ki,0)
      assertEquals(guerrerotupla2._1.vecesFajado,0)
      assertEquals(guerrerotupla2._2.ki,10435)
  }
  
  @Test def `comerse al oponente `  = {
      var movimientos1 = List(dejarseFajar, Explotar, comerserAlOponente)
      var movimientos2 = List(Genkidama)

      var atacante = new Guerrero(150,200,"Majin Boo",4,false, List[Item](ArmaFilosa,ArmaRoma,FotoLuna) ,Monstruo(Cell), movimientos1)
      var atacado = new Guerrero(400,450,"guerrero2",4,false,List[Item](ArmaFilosa,EsferaDragon,FotoLuna), Androide(100),movimientos2)

      var mov = comerserAlOponente
      
      var par = mov(atacante, atacado)
      
      //println(par.get._1.listaMov.toString)
      assertEquals(par._1.listaMov.exists{x =>  x==Genkidama}, true)
      assertEquals(par._2.especie, Androide(0))
      
  }
  
  @Test def `elige movimiento más efectivo a la Onda `  = {
      var movimientos1 = List(dejarseFajar, Onda(20))
      var movimientos2 = List(dejarseFajar)

      var atacante = new Guerrero(150,200,"Majin Boo",4,false, List[Item](ArmaFilosa,ArmaRoma,FotoLuna) ,Monstruo(Cell), movimientos1)
      var atacado = new Guerrero(400,450,"guerrero2",4,false,List[Item](ArmaFilosa,EsferaDragon,FotoLuna), Humano,movimientos2)
 
      var movElegido = atacante.movimientoMasEfectivoContra(atacado)(Criterios.atacante)
      
      var resultado = atacante.hacerMovimiento(movElegido, atacado)
      
      // Eligió la Onda(20)
      assertEquals(resultado._2.ki, 360)
  }
  
  @Test def `pelea un round con dos ondas`  = {
      var movimientos1 = List(dejarseFajar, Onda(50))
      var movimientos2 = List(dejarseFajar, Onda(10))
    
      var krillin = new Guerrero(150,200,"Krillin",4,false, List[Item](ArmaFilosa,ArmaRoma,FotoLuna) ,Humano, movimientos1)
      var yamcha = new Guerrero(400,450,"Yamcha",4,false,List[Item](ArmaFilosa,EsferaDragon,FotoLuna), Humano,movimientos2)
  
      var parResultado = krillin.pelearRound(krillin.movimientoMasEfectivoContra(yamcha)(Criterios.atacante))(yamcha)
      
      assertEquals(parResultado._1.ki,80)
      assertEquals(parResultado._2.ki,290)
      
  }
  
   @Test def `planifica un ataque con tres ondas`  = {
      var movimientos1 = List(dejarseFajar, Onda(50))
      var movimientos2 = List(dejarseFajar, Onda(10))
    
      var krillin = new Guerrero(150,200,"Krillin",4,false, List[Item](ArmaFilosa,ArmaRoma,FotoLuna) ,Humano, movimientos1)
      var yamcha = new Guerrero(400,450,"Yamcha",4,false,List[Item](ArmaFilosa,EsferaDragon,FotoLuna), Humano,movimientos2)
  
      var plan = krillin.planDeAtaque(yamcha, 3)(Criterios.atacante)
      
      assertEquals(plan.length, 3)
      
  }
   
  @Test def `pelea usando plan de tres ondas y quedan los dos peleando` = {
     var movimientos1 = List(dejarseFajar, Onda(50))
      var movimientos2 = List(dejarseFajar, Onda(10))
    
      var krillin = new Guerrero(150,200,"Krillin",4,false, List[Item](ArmaFilosa,ArmaRoma,FotoLuna) ,Humano, movimientos1)
      var yamcha = new Guerrero(400,450,"Yamcha",4,false,List[Item](ArmaFilosa,EsferaDragon,FotoLuna), Humano,movimientos2)
  
      var plan = krillin.planDeAtaque(yamcha, 3)(Criterios.atacante)
      
      var res = krillin.pelearContra(yamcha)(plan)
      
      assertEquals(res match {
        case ResultadoParcial(a,b) => a.nombre == "Krillin" && b.nombre == "Yamcha"
        case _ => false
      }, true)
      
      
  }
  
  @Test def `pelea usando plan de tres ondas y gana krillin` = {
     var movimientos1 = List(dejarseFajar, Onda(50))
      var movimientos2 = List(dejarseFajar, Onda(10))
    
      var krillin = new Guerrero(150,200,"Krillin",4,false, List[Item](ArmaFilosa,ArmaRoma,FotoLuna) ,Humano, movimientos1)
      var yamcha = new Guerrero(10,450,"Yamcha",4,false,List[Item](ArmaFilosa,EsferaDragon,FotoLuna), Humano,movimientos2)
  
      var plan = krillin.planDeAtaque(yamcha, 3)(Criterios.atacante)
      
      var res = krillin.pelearContra(yamcha)(plan)
      
      assertEquals(res match {
        case ResultadoFinal(vencedor) => vencedor.nombre == "Krillin"
        case _ => false
      }, true)
      
  }
  
  
}