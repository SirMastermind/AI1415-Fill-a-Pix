(load "examples.fas")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

					; Primeira Entrega

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; TIPO RESTRICAO ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (restricao
	     (:constructor faz-restricao)
	     (:constructor faz-res))
  lista_vars predicado res)

(defun cria-restricao (lista-vars func)
  (faz-restricao
   :lista_vars lista-vars
   :predicado func))

(defun restricao-variaveis (res)
  (if (equal res nil)
      nil
      (restricao-lista_vars res)))

(defun restricao-funcao-validacao (res)
  (if (equal res nil)
      nil
      (restricao-predicado res)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; TIPO PSR ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (psr
	     (:constructor faz-psr))
  lista_vars lista_dominios lista_restricoes lista_atribuicoes restricoes_vars)

(defun cria-psr (lista-vars lista-doms lista-res)
  (faz-psr
   :lista_vars lista-vars
   :lista_dominios lista-doms
   :restricoes_vars nil
   :lista_restricoes lista-res
   :lista_atribuicoes (make-list (length lista-vars))))

(defun psr-atribuicoes (psr_a)
  (let (lista-atribuicoes)
    (dotimes (i (length (psr-lista_vars psr_a)) lista-atribuicoes)
      (cond ((lista-nil (psr-lista_atribuicoes psr_a))
	     (return nil))
	    ((nth i (psr-lista_atribuicoes psr_a))
	     (setf lista-atribuicoes (append lista-atribuicoes (list (cons (nth i (psr-lista_vars psr_a)) (nth i (psr-lista_atribuicoes psr_a)))))))
	    (T T)))))

(defun psr-variaveis-todas (psr_a)
  (psr-lista_vars psr_a))

(defun psr-variaveis-nao-atribuidas (psr_a)
  (let (lista-nao-atribuidas)
    (dotimes (i (length (psr-lista_atribuicoes psr_a)) lista-nao-atribuidas)
      (if (not (nth i (psr-lista_atribuicoes psr_a)))
	  (setf lista-nao-atribuidas (append lista-nao-atribuidas (list (nth i (psr-lista_vars psr_a)))))))))

(defun psr-variavel-valor (psr_a variavel)
  (let (var)
    (dotimes (i (length (psr-lista_vars psr_a)) var)
      (if (string-equal variavel (nth i (psr-lista_vars psr_a)))
	  (setf var (nth i (psr-lista_atribuicoes psr_a)))))))

(defun psr-variavel-dominio (psr_a variavel)
  (let (dominio)
    (dotimes (i (length (psr-lista_vars psr_a)) dominio)
      (if (string-equal variavel (nth i (psr-lista_vars psr_a)))
	  (setf dominio (nth i (psr-lista_dominios psr_a)))))))

(defun psr-variavel-restricoes (psr_a variavel)
  (let ((restricoes)
	(lista-vars))
    (dotimes (j (length (psr-lista_restricoes psr_a)) restricoes)
      (setf lista-vars (restricao-variaveis (nth j (psr-lista_restricoes psr_a))))
      (if (meufind lista-vars variavel)
	  (setf restricoes (append restricoes (list (nth j (psr-lista_restricoes psr_a)))))))))

(defun psr-adiciona-atribuicao! (psr_a variavel valor)
  (dotimes (i (length (psr-lista_vars psr_a)))
    (if (string-equal variavel (nth i (psr-lista_vars psr_a)))
	(setf (nth i (psr-lista_atribuicoes psr_a)) valor))))

(defun psr-remove-atribuicao! (psr_a variavel)
  (psr-adiciona-atribuicao! psr_a variavel nil))

(defun psr-altera-dominio! (psr_a variavel dominio)
  (dotimes (i (length (psr-lista_vars psr_a)))
    (if (string-equal variavel (nth i (psr-lista_vars psr_a)))
	(setf (nth i (psr-lista_dominios psr_a)) dominio))))

(defun psr-completo-p (psr_a)
  (if (not (psr-variaveis-nao-atribuidas psr_a))
      T
      NIL))

(defun psr-consistente-p (psr_a)
  (let ((funcao-avaliadora)
	(contador 0)
	(logic t))
    (dotimes (i (length (psr-lista_restricoes psr_a)) (values logic contador))
      (setf funcao-avaliadora (restricao-funcao-validacao (nth i (psr-lista_restricoes psr_a))))
      (cond ((equal funcao-avaliadora nil)
	     (setf logic t))
	    ((funcall funcao-avaliadora psr_a)
	     (setf logic t)
	     (incf contador))
	    (T (setf logic nil)
	       (incf contador)))
      (when (not logic) (return (values logic contador))))))

(defun psr-variavel-consistente-p (psr_a variavel)
  (let ((res)
	(logic t)
	(funcao)
	(contador 0))
    (setf res (psr-variavel-restricoes psr_a variavel))
    (dotimes (i (length res)(values logic contador))
      (setf funcao (restricao-funcao-validacao (nth i res)))
      (cond ((equal funcao nil)
	     (setf logic t))
	    ((funcall funcao psr_a)
	     (setf logic t)
	     (incf contador))
	    (T (setf logic nil)
	       (incf contador)))
      (when (not logic) (return (values logic contador))))))

(defun psr-atribuicao-consistente-p (psr_a variavel valor)
  (let ((valor-original)
	(contador)
	(logic))
    (setf valor-original (psr-variavel-valor psr_a variavel))
    (psr-adiciona-atribuicao! psr_a variavel valor)
    (setf (values logic contador) (psr-variavel-consistente-p psr_a variavel))
    (psr-adiciona-atribuicao! psr_a variavel valor-original)
    (values logic contador)))

(defun psr-atribuicoes-consistentes-arco-p (psr_a variavel valor variavel2 valor2)
  (let ((valor-original)
	(valor-original2)
	(funcao)
	(res)
	(contador 0)
	(logic t))
    (setf res (psr-variavel-restricoes psr_a variavel))
    (setf valor-original (psr-variavel-valor psr_a variavel))
    (setf valor-original2 (psr-variavel-valor psr_a variavel2))
    (psr-adiciona-atribuicao! psr_a variavel valor)
    (psr-adiciona-atribuicao! psr_a variavel2 valor2)
    (dotimes (i (length res)(values logic contador))
      (if (meufind (restricao-variaveis (nth i res)) variavel2)
	  (setf funcao (restricao-funcao-validacao (nth i res)))
	  (setf funcao nil))
      (cond ((equal funcao nil)
	     (setf logic t))
	    ((funcall funcao psr_a)
	     (setf logic t)
	     (incf contador))
	    (T (setf logic nil)
	       (incf contador)))
      (when (not logic) (return (values logic contador))))
    (psr-adiciona-atribuicao! psr_a variavel valor-original)
    (psr-adiciona-atribuicao! psr_a variavel2 valor-original2)
    (values logic contador)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; FUNCOES CONVERSORAS ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun psr->fill-a-pix (psr_a linhas colunas)
  (let ((array)
	(linha)
	(coluna)
	(atribuicoes))
    (setf atribuicoes (psr-atribuicoes psr_a))
    (setf array (make-array (list linhas colunas) :initial-element 0))
    (dotimes (i (length atribuicoes) array)
      (setf (values linha coluna) (posicao-tabela (car (nth i atribuicoes))))
      (if (equal (cdr (nth i atribuicoes)) 1)
	  (setf (aref array linha coluna) 1)))))

(defun fill-a-pix->psr (array)
  (let ((lista_variaveis)
	(dominio (list 0 1))
	(lista_dominios)
	(lista_restricao))
    (dotimes (i (array-dimension array 0) (cria-psr lista_variaveis lista_dominios lista_restricao))
      (dotimes (j (array-dimension array 1))
	(setf lista_variaveis (append lista_variaveis (list (concatenate 'string (write-to-string i) "-" (write-to-string j)))))
	(setf lista_dominios (append lista_dominios (list dominio)))
	(if (not (equal nil (aref array i j)))
	    (setf lista_restricao (append lista_restricao (list (cria-restricao (cria-variaveis (tipo-variavel i j array) i j) (eval (read-from-string
																      (cria-funcao-linear (cria-variaveis (tipo-variavel i j array) i j) (aref array i j)))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; FUNCAO RESOLVE SIMPLES ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun resolve-simples(array)
  (let ((psr)
	(array-final))
    (setf psr (fill-a-pix->psr array))
    (setf psr (procura-retrocesso-simples psr))
    (cond ((null psr)
	   nil)
	  (T (setf array-final (psr->fill-a-pix psr (array-dimension array 0) (array-dimension array 1)))))))

(defun procura-retrocesso-simples (psr)
  (let ((contador 0)
	(logic))
    (setf (values logic contador) (retrocesso-simples psr contador))
    (if logic
	(values psr contador)
	(values nil contador))))

(defun retrocesso-simples (psr contador)
  (cond ((psr-completo-p psr)
	 (return-from retrocesso-simples (values t contador)))
	(t
	 (let ((contador-aux)
	       (var)
	       (logic)
	       (resultado))
	   (setf var (first (psr-variaveis-nao-atribuidas psr)))
	   (dotimes (i (length (psr-variavel-dominio psr var)) (values nil contador))
	     (setf (values logic contador-aux) (psr-atribuicao-consistente-p psr var (nth i (psr-variavel-dominio psr var))))
	     (setf contador (+ contador contador-aux))
	     (cond (logic
		    (psr-adiciona-atribuicao! psr var (nth i (psr-variavel-dominio psr var)))
		    (setf (values resultado contador) (retrocesso-simples psr contador))
		    (cond (resultado
			   (return-from retrocesso-simples (values psr contador)))
			  (t (psr-remove-atribuicao! psr var))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; FUNCOES AUXILIARES ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

					; Funcao que separa uma string por um '-' e devolve as diferentes subpartes da string
(defun split-by-dash (string)
  (loop for i = 0 then (1+ j)
     as j = (position #\- string :start i)
     collect (subseq string i j)
     while j))

					; Funcao que cria o perdicado de uma restricao
(defun cria-funcao-linear (lista_vars resultado)
  (let ((funcao)
	(num))
    (setf num (length lista_vars))
    (setf funcao "#'(lambda (psr_a) (let ((resultado ")
    (setf funcao (concatenate 'string funcao (write-to-string resultado)))
    (setf funcao (concatenate 'string funcao ") (nao-atribuidas 0) (variaveis-a-1 0) (lista-vars (list "))
    (dotimes (i num funcao)
      (setf funcao (concatenate 'string funcao "(psr-variavel-valor psr_a \""))
      (setf funcao (concatenate 'string funcao (nth i lista_vars)))
      (setf funcao (concatenate 'string funcao "\")"))
      (if (not (equal i (1- num)))
	  (setf funcao (concatenate 'string funcao " "))
	  (setf funcao (concatenate 'string funcao "))) (dotimes (i (length lista-vars)) (if (not (null (nth i lista-vars))) (if (equal (nth i lista-vars) 1) (setf variaveis-a-1 (1+ variaveis-a-1))) (setf nao-atribuidas (1+ nao-atribuidas)))) (if (zerop nao-atribuidas) (= resultado (soma-lista lista-vars)) (if (and (>= resultado variaveis-a-1) (<= resultado (+ nao-atribuidas variaveis-a-1))) T NIL) ))))"))))))

					; Funcao que soma o valor dos argumentos
(defun soma-lista (lista)
  (let ((resultado 0))
    (dotimes (i (length lista) resultado)
      (setf resultado (+ resultado (nth i lista))))))

					; Funcao que devolve a posicao em que esta a restricao no fill-a-pix de input
					;+---+---------+---+
					;| 1 |    2    | 3 |
					;+---+---------+---+
					;|   |         |   |
					;| 4 |    9    | 5 |
					;|   |         |   |
					;+---+---------+---+
					;| 6 |    7    | 8 |
					;+---+---------+---+

(defun tipo-variavel (linha coluna array)
  (let ((xmax)
	(ymax))
    (setf xmax (array-dimension array 0))
    (setf ymax (array-dimension array 1))
    (cond ((= 0 linha)
	   (cond ((= 0 coluna)
		  1)
		 ((= (1- xmax) coluna)
		  3)
		 (T 2)))
	  ((= (1- ymax) linha)
	   (cond ((= 0 coluna)
		  6)
		 ((= (1- xmax) coluna)
		  8)
		 (T 7)))
	  (T (cond ((= 0 coluna)
		    4)
		   ((= (1- xmax) coluna)
		    5)
		   (T 9))))))

					; Funcao que devolve a posicao em que determinada variavel esta no fill-a-pix de input
(defun posicao-tabela (variavel)
  (let (pos)
    (setf pos (split-by-dash variavel))
    (values (parse-integer (first pos)) (parse-integer (second pos)))))

					; Funcao que cria as variaveis dependendo da sua posicao no fill-a-pix de input
(defun cria-variaveis (tipo linha coluna)
  (let (lista_vars)
    (cond ((= tipo 1)
	   (setf lista_vars (list (concatenate 'string (write-to-string linha) "-" (write-to-string coluna))
				  (concatenate 'string (write-to-string linha) "-" (write-to-string (1+ coluna)))
				  (concatenate 'string (write-to-string (1+ linha)) "-" (write-to-string coluna))
				  (concatenate 'string (write-to-string (1+ linha)) "-" (write-to-string (1+ coluna))))))
	  ((= tipo 2)
	   (setf lista_vars (list (concatenate 'string (write-to-string linha) "-" (write-to-string (1- coluna)))
				  (concatenate 'string (write-to-string linha) "-" (write-to-string coluna))
				  (concatenate 'string (write-to-string linha) "-" (write-to-string (1+ coluna)))
				  (concatenate 'string (write-to-string (1+ linha)) "-" (write-to-string (1- coluna)))
				  (concatenate 'string (write-to-string (1+ linha)) "-" (write-to-string coluna))
				  (concatenate 'string (write-to-string (1+ linha)) "-" (write-to-string (1+ coluna))))))
	  ((= tipo 3)
	   (setf lista_vars (list (concatenate 'string (write-to-string linha) "-" (write-to-string (1- coluna)))
				  (concatenate 'string (write-to-string linha) "-" (write-to-string coluna))
				  (concatenate 'string (write-to-string (1+ linha)) "-" (write-to-string (1- coluna)))
				  (concatenate 'string (write-to-string (1+ linha)) "-" (write-to-string coluna)))))
	  ((= tipo 4)
	   (setf lista_vars (list (concatenate 'string (write-to-string (1- linha)) "-" (write-to-string coluna))
				  (concatenate 'string (write-to-string (1- linha)) "-" (write-to-string (1+ coluna)))
				  (concatenate 'string (write-to-string linha) "-" (write-to-string coluna))
				  (concatenate 'string (write-to-string linha) "-" (write-to-string (1+ coluna)))
				  (concatenate 'string (write-to-string (1+ linha)) "-" (write-to-string coluna))
				  (concatenate 'string (write-to-string (1+ linha)) "-" (write-to-string (1+ coluna))))))
	  ((= tipo 5)
	   (setf lista_vars (list (concatenate 'string (write-to-string (1- linha)) "-" (write-to-string (1- coluna)))
				  (concatenate 'string (write-to-string (1- linha)) "-" (write-to-string coluna))
				  (concatenate 'string (write-to-string linha) "-" (write-to-string (1- coluna)))
				  (concatenate 'string (write-to-string linha) "-" (write-to-string coluna))
				  (concatenate 'string (write-to-string (1+ linha)) "-" (write-to-string (1- coluna)))
				  (concatenate 'string (write-to-string (1+ linha)) "-" (write-to-string coluna)))))
	  ((= tipo 6)
	   (setf lista_vars (list (concatenate 'string (write-to-string (1- linha)) "-" (write-to-string coluna))
				  (concatenate 'string (write-to-string (1- linha)) "-" (write-to-string (1+ coluna)))
				  (concatenate 'string (write-to-string linha) "-" (write-to-string coluna))
				  (concatenate 'string (write-to-string linha) "-" (write-to-string (1+ coluna))))))
	  ((= tipo 7)
	   (setf lista_vars (list (concatenate 'string (write-to-string (1- linha)) "-" (write-to-string (1- coluna)))
				  (concatenate 'string (write-to-string (1- linha)) "-" (write-to-string coluna))
				  (concatenate 'string (write-to-string (1- linha)) "-" (write-to-string (1+ coluna)))
				  (concatenate 'string (write-to-string linha) "-" (write-to-string (1- coluna)))
				  (concatenate 'string (write-to-string linha) "-" (write-to-string coluna))
				  (concatenate 'string (write-to-string linha) "-" (write-to-string (1+ coluna))))))
	  ((= tipo 8)
	   (setf lista_vars (list (concatenate 'string (write-to-string (1- linha)) "-" (write-to-string (1- coluna)))
				  (concatenate 'string (write-to-string (1- linha)) "-" (write-to-string coluna))
				  (concatenate 'string (write-to-string linha) "-" (write-to-string (1- coluna)))
				  (concatenate 'string (write-to-string linha) "-" (write-to-string coluna)))))
	  ((= tipo 9)
	   (setf lista_vars (list (concatenate 'string (write-to-string (1- linha)) "-" (write-to-string (1- coluna)))
				  (concatenate 'string (write-to-string (1- linha)) "-" (write-to-string coluna))
				  (concatenate 'string (write-to-string (1- linha)) "-" (write-to-string (1+ coluna)))
				  (concatenate 'string (write-to-string linha) "-" (write-to-string (1- coluna)))
				  (concatenate 'string (write-to-string linha) "-" (write-to-string coluna))
				  (concatenate 'string (write-to-string linha) "-" (write-to-string (1+ coluna)))
				  (concatenate 'string (write-to-string (1+ linha)) "-" (write-to-string (1- coluna)))
				  (concatenate 'string (write-to-string (1+ linha)) "-" (write-to-string coluna))
				  (concatenate 'string (write-to-string (1+ linha)) "-" (write-to-string (1+ coluna))))))
	  (T (print "Failed creating stuff")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; FUNCOES UTILITARIAS ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun meufind (lista elemento)
  (cond ((null lista)
	 nil)
	((string-equal (first lista) elemento)
	 T)
	(T (meufind (rest lista) elemento))))

(defun meufind2 (lista elemento)
  (cond ((null lista)
	 nil)
	((string-equal (first (first lista)) elemento)
	 T)
	(T (meufind2 (rest lista) elemento))))

(defun lista-nil (lista)
  (cond ((null lista)
	 T)
	((not (first lista))
	 (lista-nil (rest lista)))
	(T nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

					; Segunda Entrega

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; FUNCAO RESOLVE GRAU ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun procura-retrocesso-grau (psr)
  (let ((contador 0)
	(logic))
    (setf (values logic contador) (procura-retrocesso-grau-aux psr 0))))

(defun procura-retrocesso-grau-aux (psr contador)
  (cond ((psr-completo-p psr)
	 (return-from procura-retrocesso-grau-aux (values t contador)))
	(t
	 (let ((contador-aux)
	       (var)
	       (logic)
	       (resultado))
	   (setf var (var-maior-grau psr))
	   (dotimes (i (length (psr-variavel-dominio psr var)) (values nil contador))
	     (setf (values logic contador-aux) (psr-atribuicao-consistente-p psr var (nth i (psr-variavel-dominio psr var))))
	     (setf contador (+ contador contador-aux))
	     (cond (logic
		    (psr-adiciona-atribuicao! psr var (nth i (psr-variavel-dominio psr var)))
		    (setf (values resultado contador) (procura-retrocesso-grau-aux psr contador))
		    (cond (resultado
			   (return-from procura-retrocesso-grau-aux (values psr contador)))
			  (t (psr-remove-atribuicao! psr var))))))))))

(defun var-maior-grau (psr)
  (let ((nao-atribuidas)
	(restricoes)
	(grau)
	(variaveis)
	(logic t)
	(max 0))
    (setf nao-atribuidas (psr-variaveis-nao-atribuidas psr))
    (setf grau (make-list (length nao-atribuidas) :initial-element 0))
    (dotimes (i (length nao-atribuidas))
      (setf restricoes (psr-variavel-restricoes psr (nth i nao-atribuidas)))
      (dotimes (j (length restricoes))
	(setf variaveis (restricao-variaveis (nth j restricoes)))
	(dotimes (k (length variaveis))
	  (cond ((string-equal (nth k variaveis) (nth i nao-atribuidas)))
		(t
		 (setf logic (and (psr-variavel-valor psr (nth k variaveis)) logic))))
	  (if (not logic)
	      (setf (nth i grau) (1+ (nth i grau))))
	  (when (not logic) (setf k (length variaveis)) (setf logic t)))))
    (dotimes (i (length grau))
      (if (< (nth max grau) (nth i grau))
	  (setf max i)))
    (nth max nao-atribuidas)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; FUNCAO RESOLVE FC MRV ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun procura-retrocesso-fc-mrv (psr)
  (let ((contador 0)
	(logic))
    (setf (values logic contador) (procura-retrocesso-fc-mrv-aux psr 0))))

(defun procura-retrocesso-fc-mrv-aux (psr contador)
  (cond ((psr-completo-p psr)
	 (return-from procura-retrocesso-fc-mrv-aux (values t contador)))
	(t
	 (let ((contador-aux)
	       (var)
	       (logic)
	       (resultado)
	       (inferencias)
	       (antigo-dom))
	   (setf var (mrv psr))
	   (dotimes (i (length (psr-variavel-dominio psr var)) (values nil contador))
	     (setf (values logic contador-aux) (psr-atribuicao-consistente-p psr var (nth i (psr-variavel-dominio psr var))))
	     (setf contador (+ contador contador-aux))
	     (cond ( logic
		    (psr-adiciona-atribuicao! psr var (nth i (psr-variavel-dominio psr var)))
		    (setf (values inferencias contador-aux) (forward-checking psr var))
		    (setf contador (+ contador contador-aux))
		    (cond ((not (equal inferencias 0))
			   (cond ((not (null inferencias))
				  (setf antigo-dom (antigo-dom psr inferencias))
				  (troca-dominio psr inferencias)))
			   (setf (values resultado contador) (procura-retrocesso-fc-mrv-aux psr contador))
			   (cond (resultado
				  (return-from procura-retrocesso-fc-mrv-aux (values psr contador))))
			   (cond ((not (null inferencias))
				  (troca-dominio psr antigo-dom)))))
		    (psr-remove-atribuicao! psr var))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; FUNCAO RESOLVE MAC MRV ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun procura-retrocesso-mac-mrv (psr)
  (let ((contador 0)
	(logic))
    (setf (values logic contador) (procura-retrocesso-mac-mrv-aux psr 0))))

(defun procura-retrocesso-mac-mrv-aux (psr contador)
  (cond ((psr-completo-p psr)
	 (return-from procura-retrocesso-mac-mrv-aux (values t contador)))
	(t
	 (let ((contador-aux)
	       (var)
	       (logic)
	       (resultado)
	       (inferencias)
	       (antigo-dom))
	   (setf var (mrv psr))
	   (dotimes (i (length (psr-variavel-dominio psr var)) (values nil contador))
	     (setf (values logic contador-aux) (psr-atribuicao-consistente-p psr var (nth i (psr-variavel-dominio psr var))))
	     (setf contador (+ contador contador-aux))
	     (cond ( logic
		    (psr-adiciona-atribuicao! psr var (nth i (psr-variavel-dominio psr var)))
		    (setf (values inferencias contador-aux) (mac psr var))
		    (setf contador (+ contador contador-aux))
		    (cond ((not (equal inferencias 0))
			   (cond ((not (null inferencias))
				  (setf antigo-dom (antigo-dom psr inferencias))
				  (troca-dominio psr inferencias)))
			   (setf (values resultado contador) (procura-retrocesso-mac-mrv-aux psr contador))
			   (cond (resultado
				  (return-from procura-retrocesso-mac-mrv-aux (values psr contador))))
			   (cond ((not (null inferencias))
				  (troca-dominio psr antigo-dom)))))
		    (psr-remove-atribuicao! psr var))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; FUNCAO FORWARD CHECKING ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun forward-checking (psr var)
  (let ((inferencias)
	(contador 0)
	(lista-arcos)
	(revise 0)
	(contador-aux))
    (setf inferencias (list ))
    (setf lista-arcos (arcos-vizinhos-nao-atribuidos psr var))
    (dotimes (i (length lista-arcos) (values inferencias contador))
      (setf (values revise contador-aux inferencias) (revise psr (first (nth i lista-arcos)) (second (nth i lista-arcos)) inferencias))
      (setf contador (+ contador contador-aux))
      (cond (revise
	     (cond ((null (dom-inferencias inferencias (first (nth i lista-arcos))))
		    (return-from forward-checking (values 0 contador)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; FUNCAO REVISE ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun revise (psr x y inferencias)
  (let ((contador 0)
	(revised nil)
	(x-dom)
	(y-dom)
	(consistente nil)
	(contador-aux 0)
	(new-x-dom)
	(foundconsistent nil))
    (if (dom-inferencias inferencias x)
	(setf x-dom (dom-inferencias inferencias x))
	(setf x-dom (psr-variavel-dominio psr x)))
    (setf new-x-dom x-dom)
    (if (psr-variavel-valor psr y)
	(setf y-dom (list (psr-variavel-valor psr y)))
	(if (dom-inferencias inferencias y)
	    (setf y-dom (dom-inferencias inferencias y))
	    (setf y-dom (psr-variavel-dominio psr y))))
    (dotimes (i (length x-dom))
      (setf foundconsistent nil)
      (dotimes (j (length y-dom))
	(setf (values consistente contador-aux) (psr-atribuicoes-consistentes-arco-p psr x (nth i x-dom) y (nth j y-dom)))
	(setf contador (+ contador contador-aux))
	(cond (consistente
	       (setf j (length y-dom))
	       (setf foundconsistent t))))
      (cond ((not foundconsistent)
	     (setf revised t)
	     (setf new-x-dom (remove (nth i x-dom) new-x-dom)))))
    (cond (revised
	   (cond ((meufind2 inferencias x)
		  (dotimes (k (length inferencias))
		    (cond ((string-equal x (first (nth k inferencias)))
			   (setf (second (nth k inferencias)) new-x-dom)))))
		 ((null inferencias)
		  (setf inferencias (list (list x new-x-dom))))
		 (t
		  (setf inferencias (append inferencias (list (list x new-x-dom))))))))
    (values revised contador inferencias)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;; FUNCAO MRV ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mrv (psr)
  (let ((nao-atribuidas)
	(var-menor-dom)
	(menor-dom))
    (setf nao-atribuidas (psr-variaveis-nao-atribuidas psr))
    (setf var-menor-dom (first nao-atribuidas))
    (setf menor-dom (length (psr-variavel-dominio psr var-menor-dom)))
    (dotimes (i (length nao-atribuidas) var-menor-dom)
      (cond ((< (length (psr-variavel-dominio psr (nth i nao-atribuidas))) menor-dom)
	     (setf var-menor-dom (nth i nao-atribuidas))
	     (setf menor-dom (length (psr-variavel-dominio psr var-menor-dom))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;; FUNCAO MAC ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mac (psr var)
  (let ((inferencias)
	(contador 0)
	(lista-arcos)
	(revise 0)
	(contador-aux)
	(novos-arcos)
	(i 0))
    (setf inferencias (list ))
    (setf lista-arcos (arcos-vizinhos-nao-atribuidos psr var))
    (loop
       (setf (values revise contador-aux inferencias) (revise psr (first (nth i lista-arcos)) (second (nth i lista-arcos)) inferencias))
       (setf contador (+ contador contador-aux))
       (cond (revise
	      (cond ((null (dom-inferencias inferencias (first (nth i lista-arcos))))
		     (return-from mac (values 0 contador))))
	      (setf novos-arcos (arcos-vizinhos-nao-atribuidos psr (first (nth i lista-arcos))))
	      (dotimes (j (length novos-arcos))
		(if (and (string-equal (second (nth i lista-arcos)) (first (nth j novos-arcos))) (string-equal (first (nth i lista-arcos)) (second (nth j novos-arcos))))
		    (setf novos-arcos (remove (nth j novos-arcos) novos-arcos))))
	      (setf lista-arcos (append lista-arcos novos-arcos))))
       (setf i (+ i 1))
       (when (not (nth i lista-arcos)) (return-from mac (values inferencias contador))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; FUNCOES AUXILIARES ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

					; Funcao que altera o dominio de uma variavel para o seu antigo
(defun antigo-dom (psr inf)
  (let (dom)
    (dotimes (i (length inf) dom)
      (setf dom (append dom (list (list (first (nth i inf)) (psr-variavel-dominio psr (first (nth i inf))))))))))

(defun troca-dominio (psr inf)
  (cond ((not (null inf))
	 (dotimes (i (length inf))
	   (psr-altera-dominio! psr (first (nth i inf)) (second (nth i inf)))))))

(defun arcos-vizinhos-nao-atribuidos (psr var)
  (let ((restricoes)
	(vars)
	(vars-res)
	(nao-atribuidas))
    (setf restricoes (psr-variavel-restricoes psr var))
    (setf nao-atribuidas (psr-variaveis-nao-atribuidas psr))
    (setf vars (list ))
    (dotimes (i (length nao-atribuidas) vars)
      (cond ((not (string-equal var (nth i nao-atribuidas)))
	     (dotimes (j (length restricoes))
	       (setf vars-res (restricao-variaveis (nth j restricoes)))
	       (dotimes (k (length vars-res))
		 (if (and (string-equal (nth i nao-atribuidas) (nth k vars-res)) (not (meufind2 vars (nth k vars-res))))
		     (setf vars (append vars (list (list (nth k vars-res) var))))))))))))

(defun dom-inferencias (inferencias var)
  (dotimes (i (length inferencias))
    (cond ((string-equal (first (nth i inferencias)) var)
	   (return-from dom-inferencias (second (nth i inferencias)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; RESOLVE BEST ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

					; Na elaboracao do algoritmo resolve best, os tipos antigos de PSR e Restricao foram otimizados.;
					; Para tornar o programa mais rapido, grande parte da interface dos tipos foi adequada as suas novas estruturas, agora hash-tables
					; As funcoes de conversao entre fill-a-pix e psr e o contrario foram igualmente otimizadas.

					; Quanto ao algoritmo resolve best, este recebe como input um psr que foi pre processado pelas funcoes que estao na zona de FUNCOES NOVAS, no fim do ficheiro.


(defun resolve-best (array)
  (let ((psr)
	(array-final))
    (setf psr (best-fill-a-pix->psr array))
    (setf psr (best-procura-retrocesso-fc-mrv psr))
    (cond ((null psr)
	   nil)
	  (T
	   (setf array-final (psr->fill-a-pix-best psr (array-dimension array 0) (array-dimension array 1)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; FUNCOES CONVERSORAS ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun best-fill-a-pix->psr (array)
  (let ((lista_variaveis)
	(lista-vars)
	(lista_restricao)
	(lista_alteracoes)
	(var)
	(psr))
    (dotimes (i (array-dimension array 0))
      (dotimes (j (array-dimension array 1))
	(setf var (concatenate 'string (write-to-string i) "-" (write-to-string j)))
	(setf lista_variaveis (cons var lista_variaveis))
	(cond ((not (equal nil (aref array i j)))
	       (setf lista_restricao (append lista_restricao (list (cria-res-best (cria-variaveis (tipo-variavel i j array) i j) (eval (read-from-string (cria-funcao-linear-best (cria-variaveis (tipo-variavel i j array) i j) (aref array i j)))) (aref array i j)))))
	       (cond ((= 9 (aref array i j))
		      (setf lista_alteracoes (append lista_alteracoes (list (cria-variaveis (tipo-variavel i j array) i j) 1))))
		     ((= 6 (aref array i j))
		      (if (or (= 2 (tipo-variavel i j array)) (= 4 (tipo-variavel i j array)) (= 5 (tipo-variavel i j array)) (= 7 (tipo-variavel i j array)))
			  (setf lista_alteracoes (append lista_alteracoes (list (cria-variaveis (tipo-variavel i j array) i j) 1)))))
		     ((= 4 (aref array i j))
		      (if (or (= 1 (tipo-variavel i j array)) (= 3 (tipo-variavel i j array)) (= 6 (tipo-variavel i j array)) (= 8 (tipo-variavel i j array)))
			  (setf lista_alteracoes (append lista_alteracoes (list (cria-variaveis (tipo-variavel i j array) i j) 1)))))
		     ((= 0 (aref array i j))
		      (setf lista_alteracoes (append lista_alteracoes (list (cria-variaveis (tipo-variavel i j array) i j) 0)))))))))
    (setf lista-vars (hash-vars-restricoes (nreverse lista_variaveis) lista_restricao))
    (setf psr (cria-psr-best (nreverse lista_variaveis) lista-vars lista_restricao))
    (best-filler psr lista_alteracoes)
    (best-completa-restricoes psr)
    (values psr)))

(defun psr->fill-a-pix-best (psr_a linhas colunas)
  (let ((array)
	(linha)
	(coluna))
    (setf array (make-array (list linhas colunas) :initial-element 0))
    (loop for key being the hash-keys of (psr-lista_atribuicoes psr_a) using (hash-value value)
       do (setf (values linha coluna) (posicao-tabela key))
	 (setf (aref array linha coluna) (gethash key (psr-lista_atribuicoes psr_a))))
    (values array)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; REDEFINICAO DOS TIPOS ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cria-res-best (lista-vars func res)
  (faz-res
   :lista_vars lista-vars
   :predicado func
   :res res))

(defun cria-psr-best (lista-vars restricoes_vars lista-res)
  (let (psr)
    (setf psr (faz-psr
	       :lista_vars lista-vars
	       :lista_dominios (make-hash-table :test 'equal)
	       :restricoes_vars restricoes_vars
	       :lista_restricoes lista-res
	       :lista_atribuicoes (make-hash-table :test 'equal)))
    (dotimes (i (length (psr-variaveis-todas psr)) psr)
      (psr-altera-dominio!-best psr (nth i (psr-variaveis-todas psr)) (list 0 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; FUNCOES OTIMIZADAS ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun psr-variaveis-nao-atribuidas-best (psr_a)
  (let (lista-nao-atribuidas)
    (dotimes (i (length (psr-lista_vars psr_a)) (nreverse lista-nao-atribuidas))
      (if (not (gethash (nth i (psr-lista_vars psr_a)) (psr-lista_atribuicoes psr_a)))
	  (setf lista-nao-atribuidas (cons (nth i (psr-lista_vars psr_a)) lista-nao-atribuidas ))))))

(defun psr-variavel-valor-best (psr_a variavel)
  (gethash variavel (psr-lista_atribuicoes psr_a)))

(defun psr-variavel-dominio-best (psr_a variavel)
  (gethash variavel (psr-lista_dominios psr_a)))

(defun psr-variavel-restricoes-best (psr_a variavel)
  (gethash variavel (psr-restricoes_vars psr_a)))

(defun psr-adiciona-atribuicao!-best (psr_a variavel valor)
  (setf (gethash variavel (psr-lista_atribuicoes psr_a)) valor))

(defun psr-remove-atribuicao!-best (psr_a variavel)
  (setf (gethash variavel (psr-lista_atribuicoes psr_a)) nil))

(defun psr-altera-dominio!-best (psr_a variavel dominio)
  (setf (gethash variavel (psr-lista_dominios psr_a)) dominio))

(defun psr-completo-p-best (psr_a)
  (if (not (psr-variaveis-nao-atribuidas-best psr_a))
      T
      NIL))

(defun psr-variavel-consistente-p-best (psr_a variavel)
  (let ((res)
	(logic t)
	(funcao)
	(contador 0))
    (setf res (psr-variavel-restricoes-best psr_a variavel))
    (dotimes (i (length res)(values logic contador))
      (setf funcao (restricao-funcao-validacao (nth i res)))
      (cond ((equal funcao nil)
	     (setf logic t))
	    ((funcall funcao psr_a)
	     (setf logic t)
	     (incf contador))
	    (T (setf logic nil)
	       (incf contador)))
      (when (not logic) (return (values logic contador))))))

(defun psr-atribuicao-consistente-p-best (psr_a variavel valor)
  (let ((valor-original)
	(contador)
	(logic))
    (setf valor-original (psr-variavel-valor-best psr_a variavel))
    (psr-adiciona-atribuicao!-best psr_a variavel valor)
    (setf (values logic contador) (psr-variavel-consistente-p-best psr_a variavel))
    (psr-adiciona-atribuicao!-best psr_a variavel valor-original)
    (values logic contador)))

(defun psr-atribuicoes-consistentes-arco-p-best (psr_a variavel valor variavel2 valor2)
  (let ((valor-original)
	(valor-original2)
	(funcao)
	(res)
	(contador 0)
	(logic t))
    (setf res (psr-variavel-restricoes-best psr_a variavel))
    (setf valor-original (psr-variavel-valor-best psr_a variavel))
    (setf valor-original2 (psr-variavel-valor-best psr_a variavel2))
    (psr-adiciona-atribuicao!-best psr_a variavel valor)
    (psr-adiciona-atribuicao!-best psr_a variavel2 valor2)
    (dotimes (i (length res)(values logic contador))
      (if (meufind (restricao-variaveis (nth i res)) variavel2)
	  (setf funcao (restricao-funcao-validacao (nth i res)))
	  (setf funcao nil))
      (cond ((equal funcao nil)
	     (setf logic t))
	    ((funcall funcao psr_a)
	     (setf logic t)
	     (incf contador))
	    (T (setf logic nil)
	       (incf contador)))
      (when (not logic) (return (values logic contador))))
    (psr-adiciona-atribuicao!-best psr_a variavel valor-original)
    (psr-adiciona-atribuicao!-best psr_a variavel2 valor-original2)
    (values logic contador)))

(defun cria-funcao-linear-best (lista_vars resultado)
  (let ((funcao)
	(num))
    (setf num (length lista_vars))
    (setf funcao "#'(lambda (psr_a) (let ((resultado ")
    (setf funcao (concatenate 'string funcao (write-to-string resultado)))
    (setf funcao (concatenate 'string funcao ") (nao-atribuidas 0) (variaveis-a-1 0) (lista-vars (list "))
    (dotimes (i num funcao)
      (setf funcao (concatenate 'string funcao "(psr-variavel-valor-best psr_a \""))
      (setf funcao (concatenate 'string funcao (nth i lista_vars)))
      (setf funcao (concatenate 'string funcao "\")"))
      (if (not (equal i (1- num)))
	  (setf funcao (concatenate 'string funcao " "))
	  (setf funcao (concatenate 'string funcao "))) (dotimes (i (length lista-vars)) (if (not (null (nth i lista-vars))) (if (equal (nth i lista-vars) 1) (setf variaveis-a-1 (1+ variaveis-a-1))) (setf nao-atribuidas (1+ nao-atribuidas)))) (if (zerop nao-atribuidas) (= resultado (soma-lista lista-vars)) (if (and (>= resultado variaveis-a-1) (<= resultado (+ nao-atribuidas variaveis-a-1))) T NIL) ))))"))))))

(defun best-procura-retrocesso-fc-mrv (psr)
  (let ((contador 0)
	(logic))
    (setf (values logic contador) (best-procura-retrocesso-fc-mrv-aux psr 0))
    (if logic
	(values psr contador)
	(values nil contador))))

(defun best-procura-retrocesso-fc-mrv-aux (psr contador)
  (if (psr-completo-p-best psr)
      (return-from best-procura-retrocesso-fc-mrv-aux (values t contador))
      (progn
	(let ((contador-aux)
	      (var)
	      (logic t)
	      (resultado)
	      (inferencias)
	      (antigo-dom)
	      (length))
	  (setf var (best-maior-grau psr))
	  (setf length (length (psr-variavel-dominio-best psr var)))
	  (dotimes (i length (values nil contador))
	    (setf logic (psr-atribuicao-consistente-p-best psr var (nth i (psr-variavel-dominio-best psr var))))
	    (if logic
		(progn
		  (psr-adiciona-atribuicao!-best psr var (nth i (psr-variavel-dominio-best psr var)))
		  (setf (values inferencias contador-aux) (best-forward-checking psr var))
		  (setf contador (+ contador contador-aux))
		  (if (not (equal inferencias 0))
		      (progn
			(if (not (null inferencias))
			    (progn
			      (setf antigo-dom (antigo-dom-best psr inferencias))
			      (troca-dominio-best psr inferencias)))
			(setf (values resultado contador) (best-procura-retrocesso-fc-mrv-aux psr contador))
			(if resultado
			    (return-from best-procura-retrocesso-fc-mrv-aux (values psr contador)))
			(if (not (null inferencias))
			    (troca-dominio-best psr antigo-dom))))
		  (psr-remove-atribuicao!-best psr var))))))))

(defun best-forward-checking (psr var)
  (let ((inferencias)
	(contador 0)
	(lista-arcos)
	(revise 0)
	(contador-aux))
    (setf inferencias (list ))
    (setf lista-arcos (arcos-vizinhos-nao-atribuidos-best psr var))
    (dotimes (i (length lista-arcos) (values inferencias contador))
      (setf (values revise contador-aux inferencias) (best-revise psr (first (nth i lista-arcos)) (second (nth i lista-arcos)) inferencias))
      (setf contador (+ contador contador-aux))
      (if revise
	  (if (null (dom-inferencias inferencias (first (nth i lista-arcos))))
	      (return-from best-forward-checking (values 0 contador)))))))

(defun arcos-vizinhos-nao-atribuidos-best (psr var)
  (let ((restricoes)
	(vars)
	(vars-res)
	(nao-atribuidas))
    (setf restricoes (psr-variavel-restricoes-best psr var))
    (setf nao-atribuidas (psr-variaveis-nao-atribuidas-best psr))
    (setf vars (list ))
    (dotimes (i (length nao-atribuidas) vars)
      (cond ((not (string-equal var (nth i nao-atribuidas)))
	     (dotimes (j (length restricoes))
	       (setf vars-res (restricao-variaveis (nth j restricoes)))
	       (dotimes (k (length vars-res))
		 (if (and (string-equal (nth i nao-atribuidas) (nth k vars-res)) (not (meufind2 vars (nth k vars-res))))
		     (setf vars (append vars (list (list (nth k vars-res) var))))))))))))

(defun best-revise (psr x y inferencias)
  (let ((contador 0)
	(revised nil)
	(x-dom)
	(y-dom)
	(consistente nil)
	(contador-aux 0)
	(new-x-dom)
	(foundconsistent nil))
    (if (dom-inferencias inferencias x)
	(setf x-dom (dom-inferencias inferencias x))
	(setf x-dom (psr-variavel-dominio-best psr x)))
    (setf new-x-dom x-dom)
    (if (psr-variavel-valor-best psr y)
	(setf y-dom (list (psr-variavel-valor-best psr y)))
	(if (dom-inferencias inferencias y)
	    (setf y-dom (dom-inferencias inferencias y))
	    (setf y-dom (psr-variavel-dominio-best psr y))))
    (dotimes (i (length x-dom))
      (setf foundconsistent nil)
      (dotimes (j (length y-dom))
	(setf (values consistente contador-aux) (psr-atribuicoes-consistentes-arco-p-best psr x (nth i x-dom) y (nth j y-dom)))
	(setf contador (+ contador contador-aux))
	(if consistente
	    (progn
	      (setf j (length y-dom))
	      (setf foundconsistent t))))
      (if (not foundconsistent)
	  (progn
	    (setf revised t)
	    (setf new-x-dom (remove (nth i x-dom) new-x-dom)))))
    (if revised
	(progn
	  (if (meufind2 inferencias x)
	      (progn
		(dotimes (k (length inferencias))
		  (if (string-equal x (first (nth k inferencias)))
		      (setf (second (nth k inferencias)) new-x-dom))))
	      (progn
		(if (null inferencias)
		    (setf inferencias (list (list x new-x-dom)))
		    (progn
		      (nreverse inferencias)
		      (setf inferencias (cons (list x new-x-dom) inferencias))
		      (nreverse inferencias)))))))
    (values revised contador inferencias)))

(defun best-maior-grau (psr)
  (let ((nao-atribuidas)
	(restricoes)
	(grau)
	(variaveis)
	(logic t)
	(max 0))
    (setf nao-atribuidas (psr-variaveis-nao-atribuidas-best psr))
    (setf grau (make-list (length nao-atribuidas) :initial-element 0))
    (dotimes (i (length nao-atribuidas))
      (setf restricoes (psr-variavel-restricoes-best psr (nth i nao-atribuidas)))
      (if (equal 1 (length (psr-variavel-dominio-best psr (nth i nao-atribuidas))))
	  (return-from best-maior-grau (nth i nao-atribuidas)))
      (dotimes (j (length restricoes))
	(setf variaveis (restricao-variaveis (nth j restricoes)))
	(dotimes (k (length variaveis))
	  (if (not (string-equal (nth k variaveis) (nth i nao-atribuidas)))
	      (setf logic (and (psr-variavel-valor-best psr (nth k variaveis)) logic)))
	  (if (not logic)
	      (setf (nth i grau) (1+ (nth i grau))))
	  (when (not logic) (setf k (length variaveis)) (setf logic t)))))
    (dotimes (i (length grau))
      (if (< (nth max grau) (nth i grau))
	  (setf max i)))
    (nth max nao-atribuidas)))

(defun troca-dominio-best (psr inf)
  (cond ((not (null inf))
	 (dotimes (i (length inf))
	   (psr-altera-dominio!-best psr (first (nth i inf)) (second (nth i inf)))))))

(defun antigo-dom-best (psr inf)
  (let (dom)
    (dotimes (i (length inf) dom)
      (setf dom (append dom (list (list (first (nth i inf)) (psr-variavel-dominio-best psr (first (nth i inf))))))))))

(defun psr-atribuicoes-best (psr_a)
  (let (lista-atribuicoes)
    (dotimes (i (length (psr-lista_vars psr_a)) (nreverse lista-atribuicoes))
      (setf lista-atribuicoes (cons (gethash (nth i (psr-lista_vars psr_a)) (psr-lista_atribuicoes psr_a)) lista-atribuicoes)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; FUNCOES NOVAS ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

					; Esta funcao trata de preencher o psr com atribuicoes e alteracoes de dominio que resultam da analise de casos triviais por parte do conversor de fill-a-pix->psr

(defun best-filler (psr lista_alteracoes)
  (let ((var_alvo)
	(i 0)
	(dom-novo))
    (loop
       (setf var_alvo (nth i lista_alteracoes))
       (setf dom-novo (nth (+ i 1) lista_alteracoes))
       (dotimes (j (length var_alvo))
	 (psr-adiciona-atribuicao!-best psr (nth j var_alvo) dom-novo)
	 (psr-altera-dominio!-best psr (nth j var_alvo) dom-novo))
       (setf i (+ i 2))
       (when (not (nth i lista_alteracoes)) (return-from best-filler psr)))))

					; Esta funcao trata da parte seguinte do pre-processamento: tentar preencher as restricoes que, apos tratamento de casos triviais, sao trivialmente resolvidas atribuindo valores as restantes variaveis
(defun best-completa-restricoes (psr)
  (let ((restricoes)
	(lista-vars)
	(res)
	(cont)
	(vars-a-1)
	(vars-a-0)
	(vars-a-completar))
    (setf restricoes (psr-lista_restricoes psr))
    (dotimes (i (length restricoes))
      (setf lista-vars (restricao-variaveis (nth i restricoes)))
      (setf (values res vars-a-1 vars-a-0 vars-a-completar) (best-completa-restricoes-aux psr lista-vars))
      (setf cont (restricao-res (nth i restricoes)))
      (if (equal res cont)
	  (progn
	    (dotimes (j (length vars-a-completar))
	      (psr-adiciona-atribuicao!-best psr (nth j vars-a-completar) 0)
	      (psr-altera-dominio!-best psr (nth j vars-a-completar) (list 0))
	      (setf i 0)))
	  (if (equal (- cont res) (length vars-a-completar))
	      (progn
		(dotimes (j (length vars-a-completar))
		  (psr-adiciona-atribuicao!-best psr (nth j vars-a-completar) 1)
		  (psr-altera-dominio!-best psr (nth j vars-a-completar) (list 1))
		  (setf i 0))))))))

(defun best-completa-restricoes-aux (psr lista-vars)
  (let ((res 0)
	(vars)
	(vars-a-0)
	(vars-nao-atribuidas))
    (setf vars (list ))
    (setf vars-a-0 (list ))
    (setf vars-nao-atribuidas (list ))
    (dotimes (i (length lista-vars) (values res (nreverse vars) (nreverse vars-a-0) (nreverse vars-nao-atribuidas)))
      (if (equal (psr-variavel-valor-best psr (nth i lista-vars)) 1)
	  (progn
	    (setf res (+ res 1))
	    (setf vars (cons (nth i lista-vars) vars)))
	  (if (equal (psr-variavel-valor-best psr (nth i lista-vars)) 0)
	      (setf vars-a-0 (cons (nth i lista-vars) vars-a-0))
	      (setf vars-nao-atribuidas (cons (nth i lista-vars) vars-nao-atribuidas)))))))

					; Funcao que recebe a lista de variaveis e associa a cada uma as restricoes nas quais a mesma participa
(defun hash-vars-restricoes (vars restricoes)
  (let ((lista-vars)
	(vars-res))
    (setf lista-vars (make-hash-table :test 'equal))
    (dotimes (i (length vars))
      (setf (gethash (nth i vars) lista-vars) nil))
    (dotimes (i (length restricoes) lista-vars)
      (setf vars-res (restricao-variaveis (nth i restricoes)))
      (dotimes (j (length vars-res))
	(setf (gethash (nth j vars-res) lista-vars) (cons (nth i restricoes) (gethash (nth j vars-res) lista-vars)))))))
