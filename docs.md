# Laboratorio Lenguajes y Compiladores

## Constructores sintácticos para `<intexp>` y `<boolexp>`

Los constructores sintácticos correspondientes al lenguaje de expresiones
enteras y booleanas pueden verse en la definición del tipo `Expr a` junto a su
función semántica en `DomSem Int` y `DomSem Bool`. Algo a considerar es la
implementación de la semántica de `Div`, donde para el caso de la división por 0
se eligió un valor arbitrario como resultado de esta operación similar a la
convención que se trabajó durante la matería para el lenguaje imperativo simple.

## Constructores sintácticos para `<comm>`

De manera similar que las expresiones enteras y booleanas, la implementación de
los comandos puede encontrarse en `Expr a` y `DomSem Ω`.

## Casos de Tests

Para verificar que la implementación se realizaron casos de tests utilizando
`HUnit` y pueden verse en `Tests.hs`. La definición de los ejemplos empleados se encuentran en `Examples.hs` donde se pone énfasis en propiedades que la
implementación debe cumplir, y ejercicios de las guías que requerían calculo de
semánticas. A su vez, los ejemplos fueron agregados en su forma de evaluación en
`Eval.hs` para visualizar aquellos que producían output, solicitaban input, o no
realizaban ninguna de estas acciones. Para la ejecución de los tests basta con
ejecutar lo siguiente por terminal de comandos:

```haskell
ghci Tests.hs 
*Main> main
Cases: 11  Tried: 11  Errors: 0  Failures: 0
Counts {cases = 11, tried = 11, errors = 0, failures = 0}
*Main>
```

## Definición de `bot: Ω`

En la implementación de la función semántica, la representación de `⊥` está dada
por la no terminación de la sentencia `While`, a diferencia de la definición
teórica donde tal comportamiento se distinguía con un elemento del domínio. Si
bien, se podría agregar a la definición de `Ω` un constructor `Bottom`, y
expandir los operadores de transferencia de control `(*.)`, `(†.)`, `(+.)` como
muestra el siguiente ejemplo, no se podrían expresar comandos que al obtener su
semántica obtengamos dicho constructor ya que para ello deberíamos determinar
cuando un comando terminaría.


```haskell
data Ω = Normal Σ | Abort Σ | Out (Int, Ω) | In (Int -> Ω) | Bottom

(*.) :: (Σ -> Ω) -> Ω -> Ω
(*.) _ Bottom      = Bottom
(*.) f (Normal σ)  = f σ
(*.) _ (Abort σ)   = Abort σ
(*.) f (Out (n,ω)) = Out (n, f *. ω)
(*.) f (In g)      = In ((f *.) . g)

(†.) :: (Σ -> Σ) -> Ω -> Ω
(†.) _ Bottom      = Bottom
(†.) f (Normal σ)  = Normal $ f σ
(†.) f (Abort σ)   = Abort $ f σ
(†.) f (Out (n,ω)) = Out (n, f †. ω)
(†.) f (In g)      = In ((f †.) . g)

(+.) :: (Σ -> Ω) -> Ω -> Ω
(+.) _ Bottom      = Bottom
(+.) _ (Normal σ)  = Normal σ
(+.) f (Abort σ)   = f σ
(+.) f (Out (n,ω)) = Out (n, f +. ω)
(+.) f (In g)      = In ((f +.) . g)
```

No obstante, si se puede lograr una definición de la no terminación en función
de `fix`. Si se inspeccióna la semántica del `While`, se obtienen los siguientes
tipos de las expresiones que ocurren en ella.

```haskell
sem (While b c) σ = fix f σ
    where (f w) σ = if sem b σ then w *. (sem c σ) else Normal σ

-- f     ::  (Σ -> Ω) -> (Σ -> Ω)
-- w     ::  (Σ -> Ω)
-- σ     ::   Σ
-- (f w) ::  (Σ -> Ω)
-- fix   :: ((Σ -> Ω) -> (Σ -> Ω)) -> (Σ -> Ω)
-- fix f ::  (Σ -> Ω)
```

Luego aplicando la definición de `fix` y de `f`.

```haskell
sem (While b c) σ = fix f σ
                  = f (fix f) σ
                  = if sem b σ then (fix f) *. (sem c σ) else Normal σ
```

Si suponemos que de `sem c σ` se obtiene un valor de la forma `Normal σ'`, es
decir, que al hacer una iteración del `While` el comando termina y no obtiene
fallas, por definición de `(*.)` se obtiene lo siguiente.

```haskell
sem (While b c) σ = fix f σ
                  = f (fix f) σ
                  = if sem b σ then (fix f) σ' else Normal σ
```

Notar que si `sem b σ` fuese verdadero, se podría decir que la iteración
realizada no afectó lo suficiente la función de estados para terminar la
ejecución y por lo tanto `f (fix f) σ = fix f σ'`. Siguiendo está idea, una
posibilidad de definir `⊥` es la siguiente.

```haskell
bot :: Σ -> Ω
bot σ = fix f σ
    where f w σ = w σ

-- o equivalentemente

bot :: Ω
bot = fix f
    where f w = w
```

Esto nos dice que la función constantenemente `⊥` es aquella que para cada
estado `σ` la función `f` no altera lo suficiente el estado como para que `fix`
se detenga y por lo tanto continúa otra iteración indefinidamente. Finalmente,
removiendo los estados σ en ambos lados de la igualdad se obtiene la definición
de `⊥` que se buscaba.
