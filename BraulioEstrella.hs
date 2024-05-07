import Text.Show.Functions()

type PoderBasico = Personaje -> Personaje
type SuperPoder = Personaje -> Personaje

data Personaje = UnPersonaje {
    nombre           :: String,
    poderBasico      :: PoderBasico,
    superPoder       :: SuperPoder,
    superPoderActivo :: Bool,
    vida             :: Int
} deriving Show

espina :: Personaje
espina = UnPersonaje {
    nombre              = "Espina",
    poderBasico         = bolaEspinosa,
    superPoder          = granadaDeEspinas 5,
    superPoderActivo    = True,
    vida                = 4800
}

pamela :: Personaje
pamela = UnPersonaje {
    nombre              = "Pamela",
    poderBasico         = lluviaDeTuercas "Sanadoras",
    superPoder          = torretaCurativa,
    superPoderActivo    = False,
    vida                = 9600
}


{--Funciones Auxiliares--}
cambiarVida :: (Int -> Int) -> Personaje -> Personaje
cambiarVida fn personaje = personaje {vida = fn . vida $ personaje}

cambiarNombre :: (String -> String) -> Personaje -> Personaje
cambiarNombre fn personaje = personaje {nombre = fn . nombre $ personaje}
{--Funciones Auxiliares--}

{--Poderes--}
bolaEspinosa :: PoderBasico
bolaEspinosa personaje = cambiarVida (max 0.subtract 1000) personaje

lluviaDeTuercas :: String -> PoderBasico
lluviaDeTuercas tipoTuerca personaje = cambiarVida (tipoDeTuercas tipoTuerca) $ personaje

granadaDeEspinas :: Int -> SuperPoder
granadaDeEspinas radioExplosion personaje 
    | radioExplosion > 3 && vida personaje < 800    = desactivarSuper.cambiarVida (min 0) $ personaje
    | radioExplosion > 3                            = cambiarNombre ("Espina Estuvo Aqui" ++) personaje
    | otherwise                                     = bolaEspinosa personaje

torretaCurativa :: SuperPoder
torretaCurativa personaje = activarSuper.cambiarVida (*2) $ personaje
{--Poderes--}


{--Lógica Extra--}
divisionEntera :: (Int -> Int)
divisionEntera entero = div entero 2

tipoDeTuercas :: String -> (Int -> Int)
tipoDeTuercas "Sanadoras" = (+800)
tipoDeTuercas "Dañinas"   = divisionEntera
tipoDeTuercas  _          = (+0)

desactivarSuper :: Personaje -> Personaje
desactivarSuper personaje = personaje {superPoderActivo = False}

activarSuper :: Personaje -> Personaje
activarSuper personaje = personaje {superPoderActivo = True}

estaEnLasUltimas :: Personaje -> Bool
estaEnLasUltimas personaje 
    | vida personaje <=800 = True
    | otherwise            = False

nombresDeLosPersonajes :: [Personaje] -> [String]
nombresDeLosPersonajes personajes = map (nombre) personajes

estanEnLasUltimas :: [Personaje] -> [Personaje]
estanEnLasUltimas personajes = filter estaEnLasUltimas personajes

{--Lógica Extra--}

{--Reportes--}
atacarConPoderEspecial :: Personaje -> Personaje
atacarConPoderEspecial personaje 
    | superPoderActivo personaje == True = (superPoder personaje).(poderBasico personaje) $ personaje
    | otherwise                          = personaje

nombresEstanEnLasUltimas :: [Personaje] -> [String]
nombresEstanEnLasUltimas personajes = nombresDeLosPersonajes.estanEnLasUltimas $ personajes
{--Reportes--}