

# metabench
## Un benchmark selectivo de razonamiento y conocimiento en LLMs
🤗 metabench condensa el [Open LLM Leaderboard 1](https://huggingface.co/spaces/open-llm-leaderboard-old/open_llm_leaderboard) a menos del 3% de su tamaño original\
🧑‍🏫 la selección de ítems se basa en análisis de teoría de respuesta al ítem de más de 5000 LLMs\
🔥 las puntuaciones de los seis benchmarks[^1] pueden reconstruirse con un error absoluto medio promedio de <0.9%\
☄️ la puntuación del Open LLM Leaderboard puede reconstruirse con un error absoluto medio de <0.5%

Este repositorio contiene el código fuente para el [scraping de conjuntos de datos](scraping) en Python y el [análisis estadístico](analysis) en R.\
Para más detalles, por favor lea nuestro [artículo en ICLR](https://openreview.net/forum?id=4T33izzFpK).

## Configuración
Se requiere el [lenguaje de programación R](https://www.r-project.org/) para ejecutar metabench. Una vez instalado, puedes configurar todas las dependencias de R ejecutando esta línea en el directorio actual:
```console
Rscript setup.R
```

## Prueba de tu LLM con metabench
Paso 1 - evalúa tu LLM usando [lm-evaluation-harness](https://github.com/EleutherAI/lm-evaluation-harness)
```console
lm-eval --model hf \
    --model_args {model_id} \
    --tasks metabench{version}{permute} \            
    --output_path path/to/metabench/harness-results \
    --log_samples # this saves the instance level results as a jsonl
```
- `{model_id}` es tu ID de modelo en [Hugging Face](https://huggingface.co/),
- `{version}` es "" para la versión principal o "_secondary" para la versión de evaluación repetida
- `{permute}` es "" para las respuestas sin permutar y "_permute" para las respuestas permutadas.


Paso 2 - reconstruye los puntos completos
```console
Rscript reconstruct.R {model_id} {ver} {per}
```
- `{ver}` es "A" para la versión principal y "B" para la versión de evaluación repetida\
- `{per}` es "False" para las respuestas sin permutar y "True" para las respuestas permutadas.



## Cómo se construyó metabench
<img src="https://github.com/adkipnis/metabench/blob/main/figures/overview/overview.png" width="750" />

1. Recopilar precisiones por ítem de todos los LLMs disponibles para cada benchmark en Open LLM Leaderboard.
2. Eliminar ítems basándose en estadísticas simples como la varianza.
3. Realizar un submuestreo con validación cruzada hasta 350 ítems por benchmark.
4. Ajustar variantes de modelos IRT a los ítems restantes, inferir información del ítem a partir de los parámetros y seleccionar los ítems más informativos para construir metabench.
5. Usar los ajustes del modelo para estimar las habilidades específicas de cada benchmark y reconstruir las puntuaciones originales (normalizadas) del benchmark, así como su media, utilizando un modelo aditivo generalizado con validación cruzada.

## Datos
Si desea reproducir nuestros resultados, puede encontrar los **conjuntos de datos** completos utilizados en este proyecto en [zenodo](https://zenodo.org/records/12819251).\
Simplemente descarga y extrae `data.tar.gz` a `data` dentro de tu directorio `.../metabench/`. 

## Carpetas
- [analysis](analysis): Análisis estadísticos (preprocesamiento, muestreo aleatorio con validación cruzada, teoría de respuesta al ítem, filtrado de información, análisis factorial, simulaciones de pruebas adaptivas por computadora)
- [bash](bash): Plantillas para ejecutar scripts en un clúster de cómputo con slurm
- [figures](figures): Scripts para generar las figuras mostradas en el artículo
- [scraping](scraping): Scripts para descargar y procesar respuestas por ítem de LLMs disponibles públicamente

## Cómo citar el proyecto
Para citar metabench en publicaciones:

```bibtex
@inproceedings{
    kipnis.2025,
    author    = {Alex Kipnis and Konstantinos Voudouris and Luca M. Schulze Buschoff and Eric Schulz},
    title     = {metabench - A Sparse Benchmark of Reasoning and Knowledge in Large Language Models},
    booktitle = {The Thirteenth International Conference on Learning Representations},
    year      = {2025},
    url       = {https://openreview.net/forum?id=4T33izzFpK}
}
```


[^1]: ARC, GSM8K, HellaSwag, MMLU, TruthfulQA y WinoGrande
