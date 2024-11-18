(deftemplate enfermedad
   (slot nombre)
   (slot tipo)
   (multislot sintomas))

(deffacts enfermedades
   (enfermedad (nombre "Gripe") 
               (tipo "Viral") 
               (sintomas "fiebre" "tos" "dolor de cabeza" "fatiga"))
   (enfermedad (nombre "Tuberculosis") 
               (tipo "Bacteriana") 
               (sintomas "tos persistente" "fiebre" "sudores nocturnos" "pérdida de peso"))
   (enfermedad (nombre "COVID-19") 
               (tipo "Viral") 
               (sintomas "fiebre" "tos seca" "dificultad para respirar" "pérdida del olfato"))
   (enfermedad (nombre "Salmonelosis") 
               (tipo "Bacteriana") 
               (sintomas "diarrea" "fiebre" "dolor abdominal" "vómitos"))
   (enfermedad (nombre "Sarampión") 
               (tipo "Viral") 
               (sintomas "fiebre alta" "erupción cutánea" "tos" "conjuntivitis"))
   (enfermedad (nombre "Meningitis") 
               (tipo "Bacteriana") 
               (sintomas "rigidez en el cuello" "fiebre" "dolor de cabeza" "náuseas"))
   (enfermedad (nombre "Varicela") 
               (tipo "Viral") 
               (sintomas "fiebre" "erupciones con picazón" "fatiga"))
   (enfermedad (nombre "Leptospirosis") 
               (tipo "Bacteriana") 
               (sintomas "fiebre alta" "dolor muscular" "ictericia" "vómitos"))
   (enfermedad (nombre "Hepatitis A") 
               (tipo "Viral") 
               (sintomas "fatiga" "ictericia" "dolor abdominal" "náuseas"))
   (enfermedad (nombre "Tétanos") 
               (tipo "Bacteriana") 
               (sintomas "espasmos musculares" "rigidez en el cuello" "dificultad para tragar"))
)


(deffunction agregar-enfermedad (?nombre ?tipo $?sintomas)
   (assert (enfermedad (nombre ?nombre) (tipo ?tipo) (sintomas $?sintomas))))


(deffunction actualizar-enfermedad (?nombre ?nuevo-tipo $?nuevos-sintomas)
   (do-for-all-facts ((?f enfermedad)) 
      (eq ?f:nombre ?nombre) 
      (retract ?f))
   (assert (enfermedad (nombre ?nombre) (tipo ?nuevo-tipo) (sintomas $?nuevos-sintomas))))


(deffunction eliminar-enfermedad (?nombre)
   (do-for-all-facts ((?f enfermedad)) 
      (eq ?f:nombre ?nombre) 
      (retract ?f))
   (printout t "La enfermedad " ?nombre " ha sido eliminada (si existía)." crlf))

