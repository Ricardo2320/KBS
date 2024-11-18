;; Definición de plantillas (templates) para los diferentes tipos de objetos.

;; Plantilla para un smartphone con atributos de marca, modelo, color y precio.
(deftemplate smartphone
  (slot marca)  ;; Marca del smartphone
  (slot modelo) ;; Modelo del smartphone
  (slot color)  ;; Color del smartphone
  (slot precio)) ;; Precio del smartphone

;; Plantilla para una computadora con atributos similares al smartphone.
(deftemplate computadora
  (slot marca)  ;; Marca de la computadora
  (slot modelo) ;; Modelo de la computadora
  (slot color)  ;; Color de la computadora
  (slot precio)) ;; Precio de la computadora

;; Plantilla para una tarjeta de crédito, con banco, grupo y fecha de expiración.
(deftemplate tarjetacred
  (slot banco)   ;; Banco emisor de la tarjeta
  (slot grupo)   ;; Tipo de tarjeta (Ej. Visa, Oro)
  (slot exp-date)) ;; Fecha de expiración de la tarjeta

;; Plantilla para un cliente con su nombre y tipo (menudista o mayorista).
(deftemplate cliente
  (slot nombre) ;; Nombre del cliente
  (slot tipo))  ;; Tipo de cliente (menudista o mayorista)

;; Plantilla para una orden de compra que incluye el producto, modelo, cantidad, cliente y método de pago.
(deftemplate orden
  (slot producto)    ;; Producto comprado (smartphone o computadora)
  (slot modelo)      ;; Modelo del producto
  (slot cantidad)    ;; Cantidad comprada
  (slot cliente)     ;; Cliente que realiza la compra
  (slot metodo-pago)) ;; Método de pago (Ej. Banamex, Liverpool VISA)

;; Plantilla para los vales que se otorgan a un cliente, con la cantidad de vales.
(deftemplate vale
  (slot cliente)   ;; Cliente que recibe los vales
  (slot cantidad)) ;; Cantidad de vales otorgados

;; Plantilla para el inventario de productos, con su cantidad disponible.
(deftemplate inventario
  (slot producto)   ;; Producto en el inventario (smartphone o computadora)
  (slot modelo)     ;; Modelo del producto
  (slot cantidad))  ;; Cantidad disponible en el inventario

;; Hechos iniciales (base de conocimientos), con datos de productos, inventarios, clientes y tarjetas de crédito.
(deffacts base-conocimientos
  ;; Smartphones
  (smartphone (marca "Apple") (modelo "iPhone16") (color "Rojo") (precio 27000))
  (smartphone (marca "Samsung") (modelo "Note21") (color "Negro") (precio 22000))

  ;; Computadoras
  (computadora (marca "Apple") (modelo "MacBookPro") (color "Gris") (precio 47000))

  ;; Inventarios
  (inventario (producto "smartphone") (modelo "iPhone16") (cantidad 50))
  (inventario (producto "smartphone") (modelo "Note21") (cantidad 30))
  (inventario (producto "computadora") (modelo "MacBookPro") (cantidad 20))

  ;; Tarjetas de crédito
  (tarjetacred (banco "Banamex") (grupo "Oro") (exp-date "01-12-23"))
  (tarjetacred (banco "Liverpool") (grupo "VISA") (exp-date "15-11-24"))

  ;; Clientes
  (cliente (nombre "Juan Perez") (tipo "menudista"))
  (cliente (nombre "Ana Lopez") (tipo "mayorista"))
)

;; Reglas para aplicar promociones basadas en diferentes condiciones.

;; Promoción para pagos con "Banamex": 24 meses sin intereses.
(defrule promocion-banamex
  (orden (producto ?producto) (modelo ?modelo) (metodo-pago "Banamex"))
  =>
  (printout t "Promoción: 24 meses sin intereses para el producto " ?producto " modelo " ?modelo crlf))

;; Promoción para compras del modelo "Note21" pagadas con "Liverpool VISA": 12 meses sin intereses.
(defrule promocion-liverpool
  (orden (producto ?producto) (modelo "Note21") (metodo-pago "Liverpool VISA"))
  =>
  (printout t "Promoción: 12 meses sin intereses para el producto " ?producto " modelo Note21." crlf))

;; Promoción para compras de 20 o más smartphones: un regalo de una funda.
(defrule promocion-regalo
  (orden (producto "smartphone") (modelo ?modelo) (cantidad ?cantidad))
  (test (>= ?cantidad 20))
  =>
  (printout t "Promoción: Regalo de una funda para el modelo " ?modelo crlf))

;; Clasificación de clientes según la cantidad comprada.

;; Si la cantidad comprada es menor a 10, el cliente se clasifica como "Menudista".
(defrule clasificar-clientes
  (orden (cliente ?cliente) (cantidad ?cantidad))
  (cliente (nombre ?cliente) (tipo ?tipo))
  (test (< ?cantidad 10))
  =>
  (printout t "Cliente " ?cliente " clasificado como Menudista (Cantidad: " ?cantidad ")." crlf))

;; Si la cantidad comprada es 10 o más, el cliente se clasifica como "Mayorista".
(defrule clasificar-clientes-mayorista
  (orden (cliente ?cliente) (cantidad ?cantidad))
  (cliente (nombre ?cliente) (tipo ?tipo))
  (test (>= ?cantidad 10))
  =>
  (printout t "Cliente " ?cliente " clasificado como Mayorista (Cantidad: " ?cantidad ")." crlf))

;; Reglas para otorgar vales según el monto de la compra.

;; Si la compra supera los 10,000 pesos (1000 por cada unidad), se otorgan vales equivalentes.
(defrule otorgar-vales
  (orden (cliente ?cliente) (cantidad ?cantidad))
  (test (>= (* ?cantidad 1000) 10000)) ;; Cada 1000 pesos
  =>
  (bind ?vales (/ (* ?cantidad 1000) 1000))
  (assert (vale (cliente ?cliente) (cantidad ?vales)))
  (printout t "Cliente " ?cliente " recibe " ?vales " vales por su compra." crlf))

;; Funciones personalizadas para manejar el inventario.

;; Función para actualizar el stock después de una venta.
(deffunction actualizar-stock (?producto ?modelo ?cantidad)
  (do-for-fact ((?f inventario))
     (and (eq ?f:producto ?producto) (eq ?f:modelo ?modelo)) ;; Buscar el producto y modelo
     (progn
        (bind ?nuevo-stock (- ?f:cantidad ?cantidad)) ;; Actualiza el stock restando la cantidad vendida
        (if (>= ?nuevo-stock 0) then
          (retract ?f) ;; Elimina el hecho viejo
          (assert (inventario (producto ?producto) (modelo ?modelo) (cantidad ?nuevo-stock))) ;; Agrega el hecho actualizado
          (printout t "Stock actualizado para " ?producto " modelo " ?modelo ": " ?nuevo-stock crlf))
        else
          (printout t "Error: No hay stock suficiente para " ?producto " modelo " ?modelo crlf)))) ;; Si no hay suficiente stock, muestra un error.

;; Función para mostrar el stock total de todos los productos.
(deffunction mostrar-stock-total ()
   (printout t "Inventario actual:" crlf)
   (do-for-all-facts ((?f inventario)) ;; Itera sobre todos los hechos de inventario
      TRUE
      (printout t "Producto: " (fact-slot-value ?f producto)
                    ", Modelo: " (fact-slot-value ?f modelo)
                    ", Cantidad: " (fact-slot-value ?f cantidad) crlf)))

;; Regla para eliminar una orden procesada.
(defrule eliminar-orden
   ?orden <- (orden (producto ?producto) (modelo ?modelo) (cantidad ?cantidad) (cliente ?cliente) (metodo-pago ?metodo))
   =>
   (retract ?orden)  ;; Elimina la orden procesada
   (printout t "Orden procesada y eliminada: " ?producto " modelo " ?modelo crlf))

;; Regla de validación de la cantidad en una orden. Si la cantidad es cero o menor, muestra un error.
(defrule validar-cantidad
   (orden (cantidad ?cantidad))
   (test (<= ?cantidad 0)) ;; Verifica que la cantidad sea mayor a 0
   =>
   (printout t "Error: La cantidad de la orden debe ser mayor a 0." crlf))
