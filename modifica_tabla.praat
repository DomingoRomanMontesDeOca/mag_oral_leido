# modifica tabla de respuestas


tabla_judith = selected("Table")

ene_filas = Get number of rows

Append column: "tipo"
Append column: "sexoLector"
Append column: "sexoInform"
Append column: "Edad"
Append column: "RespuestaCorrecta"



for i to ene_filas

	estimulo$ = Get value: i, "stimulus"

	tipo$ = left$(estimulo$,1)

	sexoL$ =mid$(estimulo$,3,1)

	respuesta$ = Get value: i, "response"


	if (tipo$ == "o" and respuesta$ == "e") or (tipo$ == "l" and respuesta$ == "l")

		Set string value: i, "RespuestaCorrecta", "1"

	else
		Set string value: i, "RespuestaCorrecta", "0"

	endif

	select tabla_judith 

	Set string value: i, "tipo", tipo$

	Set string value: i, "sexoLector", sexoL$

endfor

