Data
  clear
  use "https://drive.google.com/uc?id=11rcxVn8Rh8qe09yh9z8-QiEPmeY57948&export=download"
  keep edad grupo capital_extranjero IntensidadK Patentes DepIDI LIsector programa FPf2 FB1 Div LExportacion LIT1_e IT_e
  %head 5
Previo
  *EJEMPLO I

  * DISTRIBUCION Y DESCRIPTIVOS DEL GASTO EN INNOVACION

  * Describimos la variable gasto en innovaciòn (IT_e)
  sum IT_e
  sum IT_e if IT_e == 0
  sum IT_e if IT_e <200000 & IT_e > 0
  quietly hist IT_e if IT_e < 200000, name(g1)
  quietly kdensity IT_e if IT_e > 0 & IT_e < 200000, name(g2)
  graph combine g1 g2
  twoway (kdensity IT_e if IT_e > 0 & IT_e < 200000) (hist IT_e if IT_e < 200000)

  * DISTRIBUCION DEL LOGARITMO DEL GASTO EN INNOVACION
  hist LIT1_e
  kdensity LIT1_e
  * La distribución de los valores mayores a cero parece seguir una normal o al menos una distribuciòn simètrica en forma de campana si obviamos los valores extremos

  * para confrmarlo graficamos
  kdensity LIT1_e if LIT1_e > 0
  * lo que muestra una curva muy cercana a la normal, pero con una cola muy alargada a la derecha

  * Ahora encontramos los descriptivos
  sum LIT1_e, detail
  * la media es negativa, y tiene una alta des. estándar.  Así mismo el sesgo y la kurtosis no parecen indicar normalidad

  * Excluyendo los valores negativos
  sum LIT1_e if LIT1_e > 0, detail
  * La media se hace psositva y la des. est mucho menor.  Sin embargo el sesgo y la curtosis aun persisten

  * creamos el histograma y vemos lo que al parecer su pocos valores muy elevados
  hist IT_e if IT_e > 0
  * obtenemos los centiles para tener una mejor idea
  centile IT_e, centile(25 50 75 90 99 99.5 99.9)
  * Nos damos cuenta de que solamente 0.1% de los datos son mayores a 1.5 millones que representan solo 6 observaciones, ya que
  sum IT_e if IT_e > 1500000
  * proponemos entonces eliminar los valores mayores 1.5 millones, lo que equivale a un valor de LIT1_e igal a 14.22

  * verificamos obteniendo el historgrama y el kernel sin esos datos
  hist LIT1_e if LIT1_e >0 & LIT1_e < 14.22
  kdensity LIT1_e if LIT1_e > 0 & LIT1_e < 14.22
  * que se aproxima mucho mas a  una normal.  Verificamos con summarize
  sum LIT1_e if LIT1_e >0 & LIT1_e < 14.22, detail
  * que da un coeficiente de sesgo muy bajo y una curtosis casi normal
  hist LIT1_e if LIT1_e < 14
  hist LIT1_e if LIT1_e >0 & LIT1_e < 14
  kdensity LIT1_e if LIT1_e > 0 & LIT1_e < 14
  twoway (kdensity LIT1_e if IT_e > 0 & LIT1_e < 14) (hist IT_e if LIT1_e < 0)
  *******************************************************************************************

  * Generamos nuevamente la variable en logaritmos y asignamos a los valores cnesurados un valor un poco menor al logaritmo del mìnimo valor censurado
  generate y = IT_e
  generate dy = IT_e >0
  generate lny = ln(y)
  quietly sum lny
  scalar gamma = r(min)
  display gamma
  replace lny = gamma - 0.00000001 if lny== .
Estandarización y Gráfica
  generate id = _n
  sum lny
  generate z_IT_e= (IT_e-r(mean))/r(sd)
  line z_IT_e id
  twoway function y = 3, range(1 6080) || line z_IT_e id
  generate ati_p2 = (z_IT_e>3)
  tab ati_p2
Uso de comando Grubbs
  grubbs lny
  tab grubbs_lny
box plot
  graph box LIT1_e
  graph box IT_e
  graph box lny
Had
  hadimvo LIT1_e DepIDI , generate(atipico_h dm_h) p(0.01)
Distancia de cook
  scatter LIT1_e   IntensidadK
  twoway lfitci LIT1_e   IntensidadK ,level(99) || lfitci LIT1_e IntensidadK, level(99) stdf ciplot (rline) || scatter LIT1_e   IntensidadK

  regress LIT1_e IntensidadK
  estimate store MCO
  predict cook, cooksd
  bro LIT1_e   IntensidadK  if  cook>1
  gen descook=cook
  sum cook
  count if cook>1 & cook<.
  gen dcook=(cook>1 & cook<.)
  tab dcook
REGRESIONES MCO Y TOBIT

  * Construimos el global
  global x2I1 edad grupo capital_extranjero IntensidadK Patentes DepIDI LIsector programa FPf2 FB1 Div LExportacion
  * Regresión MCO
  regress LIT1_e $x2I1 if LIT1_e < 14, vce (robust)
  estimate store MCO

  * Regresión Tobit censurada a la izquierda en cero
  tobit lny $x2I1 if lny < 14, ll(0.13621) vce (robust)
  estimate store TOBIT

  tobit LIT1_e $x2I1 if LIT1_e < 14, ll(0) vce (robust)
  estimate store TOBIT1

  * Como referencia, corremos la regresión Tobit censurada a la izquierda en 0 y a la derecha en 14
  tobit LIT1_e $x2I1, ll(0.136) ul(14.2) vce (robust)


  *PREDICCIONES CON EL MODELO TOBIT
  tobit lny $x2I1 if lny < 14, ll(0.13621) vce (robust)
  * prediccion de la variable indice
  predict y_indice
  * prediccion de la variable indice nuevamente
  predict y_indice2, ystar(.,.)
  * prediccion de la variable censurada
  predict y_cens, ystar(0,.)
  * prediccon de la variable truncada
  predict y_trun, e(0,.)
  sum y_*

  *EFECTOS MARGINALES MODELO TOBIT

  * Efecto marginal sobre la variable latente
  margins, dydx(*)
  mfx
  * Efecto marginal sobre la variable censurada
  mfx, predict(ystar(0,.))
  margins, dydx(*) predict(ystar(0,.))
  * Efecto marginal sobre la variable truncada
  mfx, predict(e(0,.))
  margins, dydx(*) predict(e(0,.))

  *PREDICCIONES SOBRE LAs PROBABILIDAD DE QUE LA V.DEPENDIENTE ESTE EN UN RAGO DEFINIDO

  * Predicciòn de que la variable (en logaritmos)sea mayor a cero y càlculo del efecto marginal sobre esa probabilidad
  predict probe, pr(0,.)
  mfx, predict(p(0,.))

  * Ejemplo, pronostico de la probabilidad de que 1000<IT_e<20000

  * Este intervalo corresponde a 6.9077<LIT1_e<9.90348 para la variable en logaritmos
  predict probe1, pr(6.9077,9.9035)
  * Predicciòn de la probabilidad de que la variable sea menor a 1000

  * Indica en promedio una probabilidad de 7.3% de estar entre mil y 20 mil
  predict probe2, pr(0,6.907)
  sum probe*
  * Indica en promedio una probabilidad de 7.3% de estar entre mil y 20 mil

  esttab MCO TOBIT TOBIT1, b(a4) se(4) star(* 0.1 ** 0.05 *** 0.01) s(N  chi2 rho chi2_c p ll)

  * LITERAL g) PRONOSTICO DEL VALOR ESPERADO

  * Encontramos la varianza sigma2
  predict xb3, xb
  matrix btobit = e(b)
  scalar sigma2 = btobit[1,e(df_m)+2]
  display sigma2
  * se podia tambien haber tomado directamente el valor de 32.8 de la tabla

  * Generamos el primer y segundo factores de la ecuacion (16.4) de Cameron
  generate fact1 = exp(xb3 + (sigma2/2))
  generate aux = (0.1361-xb3-sigma2)/(sigma2^0.5)
  generate fact2 = 1-normal(aux)
  generate pronostico = fact1*fact2
  sum pronostico if lny<14
  kdensity pronostico if lny<14
TEST DE HOMOCEDASTICIDAD Y NORMALIDAD
  quietly tobit LIT1_e $x2I1, ll(0) vce (robust)
  generate dy = IT_e > 0
  generate threshold = (0-xb3)/(sigma2^0.5)
  generate lambda = normalden(threshold)/normal(threshold)
  quietly generate uif = (LIT1_e - xb3)/(sigma2^0.5) if dy==1
  quietly generate double gres1 = uif
  quietly replace gres1=-lambda if dy==0
  sum gres1

  quietly generate double gres2 = uif^2 -1
  quietly replace gres2=-threshold*lambda if dy==0
  quietly generate double gres3 = uif^3
  quietly replace gres3=-(2 + threshold^2)*lambda if dy==0
  quietly generate double gres4 = uif^4 -3
  quietly replace gres4=-(3*threshold + threshold^3)*lambda if dy==0

  foreach var in $x2I1 {
  generate score`var' = gres1*`var'
  }
  global scores score* gres1 gres2

  * test de normailidad
  generate ones=1
  quietly regress ones gres3 gres4 $scores, noconstant
  display " N R2 = " e(N)*e(r2) "con p-value = " chi2tail(2,e(N)*e(r2))
  ***********************************************************************************
EJEMPLO II
  global x1 tamaño2012 edad grupo capital_extranjero IntensidadK centro SectorAT Patentes DepIDI Part Productividad_2012 Exportador
  global x2I2 edad capital_extranjero IntensidadK centro Patentes DepIDI LIsector programa Fcostos Fmercado Demanda Tecno FP FB Intrafirma Imercado coopH coopV coopC Div Productividad_2012 LExportacion

  *2. Intensidad de Inversión en Innovación

  // No hubo diferencias significativas entre el modelo de heckman en dos pasos o por máxima verosimilitud.

  * Maxima Verosimilitud
  heckman LIT1_e $x2I2, select(DInnov1=$x1) vce(robust) nolog
  estimate store mod1
  quietly heckman LIT1_e $x2I2, select(DInnov1=$x1) twostep nolog
  estimates store mod2
  esttab mod1 mod2, b(a4) se(4) star(* 0.1 ** 0.05 *** 0.01) s(N N_cens chi2 rho chi2_c p_c sigma lambda ll)
  * Los resultados no difieren demasiado.  Los los signos son los mismos y todos los coeficientes significativos en la ecuación de interés estimados por MV son significativos en la ecuación estimada por dos etapas. En la ecuación de participación, el coeficiente de capital_extranjero es sig. al 10% en la estimación por MV, pero no en la ecuación estimada por dos etapas.  En general, esta última estimación muestra errores estándar algo mayores.  Finalmente, el coeficiente del RIM es altamente significativo y el coeficeinte rho (entre 0.81 y 0.89)indica que la corrección por sesgo es válida

  * Calculamos los efectos marginales en la poblacion, en la muestra y en la prob. de seleccion
  quietly heckman LIT1_e $x2I2, select(DInnov1=$x1) vce(robust) nolog
  mfx
  mfx, predict(ycond)
  mfx, predict(psel)

  * La primera elasticidad es el coeficiente estimado = 0.094, que indica que para las empresas privadas ecuatorianas un aumento del 1% en la inversión de cap. fijo por trabajador, está asociado a un aumento de 0.1% en el gasto en I&D, ceteris paribus

  * La segunda elasticidad, se calcula mediante la formula dada en clase.  Para ello necesitamos delta promedio estimado.  Los demas valores ya son conocidos

  * Para encontrar delta promedio predecimos x'b en la ecuacion de seleccion y calculamos el RIM para cada individuo
  predict xxl, xbsel
  generate lambda = normalden(xxl)/normal(xxl)
  generate SIG = lambda*(lambda + xxl)
  sum SIG
  * Entonces la elasticidad buscada es 0.094 - (0.0432)(1.828)(0.6658) = 0.0414

  * que indica que para las empresas que decidieron invertir en innovacion, un aumento del 1% en la inversión de cap. fijo por trabajador, está asociado a un aumento de 0.04% en el gasto en I&D, ceteris paribus

  * También se puede usar directamente el comando
  mfx, predict(ycond)

  * LITERAL c)

  * Hacemos la corrida de dos etapas paso a paso
  probit DInnov1 $x1
  predict xblin, xb
  generate lambda1 = normalden(xblin)/normal(xblin)
  regress LIT1_e $x2I2 lambda1 if LIT1_e>0
  estimates store mod3
  esttab mod2 mod3, b(a4) se(4) star(* 0.1 ** 0.05 *** 0.01) s(N N_cens chi2 rho chi2_c p_c sigma lambda ll)
  * Los resultados muestran una alta similitud entre las dos salidas, aunque mantienen pequeñas diferencias

  * LITERAL d)

  * Calculamos a mano el coef. de correlacion y la desv. estandar de la ecuacion de interes

  * Utilizando las formulas de la nota de clase (Green, 5ta ed) generamos los errores al cuadrado de la regresion MCO y delta promedio estimado
  predict residuales, residuals
  generate residuales2 = residuales^2
  generate SIG1 = lambda1*(lambda1 + xblin)
  * verificamos sus valores
  sum SIG1 residuales residuales2 if LIT1_e>0
  * vemos que delta promedio estimado es 0.6 y que la suma de los errores al cuadrado sobre n es 3.09

  * entonces calculamos la varianza de la ecuacion de interes como: 3.09 + (0.6)(2.11)^2, que nos da 5.76.  Notese que 2.11 es el coeficiente asociado al termino RIM

  * su raiz es 2.4, que es la des.estandar estimada del shock de la ecuacion de interes, muy similar al valor obtenido directamente de 2.44

  * Finalmente, el coef. de correlacion es rho = 2.11/2.44 = 0.87.  Tambien muy cercano al valor obtenido directamente de 0.89

  *2.1. Variantes del modelo
  heckman LIPriv_e $x2I2, select(DInnov1=$x1) vce(robust) nolog
  estimate store modI2

  esttab mod1 modI2, b(a4) se(4) star(* 0.1 ** 0.05 *** 0.01) s(N N_cens chi2 rho chi2_c p_c sigma lambda ll)

  *ETAPA DOS

  *Ecuación de conocimiento
  global x3CO1 Iprivf tamaño2014 edad capital_extranjero IntensidadK centro Patentes DepIDI programa Demanda Tecno FP FB Intrafirma Imercado coopH coopV coopC Part Div Productividad_2012 LExportacion
  *1. Probabilidad de innovar en producto o proceso
  qui biprobit innov_prod innov_proc $x3CO1, vce(robust)
  estimate store modCO1
  esttab modCO1, b(a4) se(4) star(* 0.1 ** 0.05 *** 0.01) s(N  chi2 rho chi2_c p ll)
