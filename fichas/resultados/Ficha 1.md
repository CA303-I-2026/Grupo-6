# Ficha 1

## Nivel descriptivo

- **Titular:** El promedio académico cae después de seis horas diarias en redes
- **Nombre del hallazgo:** Umbral de inflexión en la relación entre uso de redes y rendimiento académico
- **Resumen en una oración:** El promedio académico permanece estable hasta las seis horas diarias en redes, luego desciende de forma pronunciada.
- **Método o análisis que lo produjo:** Gráfico de dispersión con curva de suavizamiento LOESS y banda de confianza al 95 %, construido sobre las variables horas_redes (eje X) y promedio (eje Y).
- **Evidencia:** Figura \ref{fig:grafico1} (Asociación entre el tiempo diario en redes sociales y el promedio académico en estudiantes universitarios.) La curva LOESS se mantiene aproximadamente horizontal entre 0 y 5 horas y desciende visiblemente a partir de las 6–8 horas, con la banda de confianza ensanchándose en el extremo derecho por escasez de observaciones.

## Nivel analítico

- **Conexión con la pregunta de investigación:** Hasta ahora esta es la mejor respuesta a la pregunta si existe una relación entre el tiempo en redes y el rendimiento académico. La forma no lineal de la curva dice que la relación no es constante ni proporcional, lo que implica que dosis moderadas de redes sociales no se asocian con un deterioro académico observable en estos datos.
- **Contraste con la literatura:** Este resultado coincide en parte con la Ficha 1 \cite{leon2023}, que no encontró relación significativa entre redes y rendimiento en una muestra de 46 estudiantes de Finanzas; una posible explicación de esa discrepancia es que su muestra pequeña no capturara suficientes estudiantes con uso muy intensivo como para detectar el efecto que aquí aparece a partir de las seis horas. Contrasta con la Ficha 3 \cite{aljemely2020}, que sí reporta impacto negativo de las redes sobre el rendimiento, aunque en ese estudio el mecanismo operaba a través de la variable de adicción, no del tiempo bruto de uso.
- **Lo que NO explica este resultado:** Esta curva solo describe una asociación, no una causa, entonces no es posible concluir si son las redes que deterioran el rendimiento de los estudiantes o si los estudiantes con menor rendimiento utilizan más las redes como refugio. Tampoco se contemplan otras variables como las horas de sueño, ansiedad o nivel socioeconómico.
- **Implicación:** El umbral de seis horas como punto de quiebre justifica estadística y conceptualmente los cortes utilizados en la variable cat_horas_redes y orienta al equipo a prestar atención especial al grupo de más de seis horas en los análisis inferenciales de entregas posteriores.
