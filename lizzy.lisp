;;;;;;;------------explode & implode------------------------
(defun explode (object)
     (loop for char across (prin1-to-string object)
           collect (intern (string char))))

(defun implode (list)
     (read-from-string (coerce (mapcar #'character list) 'string)))

;;;;;-------------atomcar & atomcdr-------------------------
(defun  atomcar (x)
	(car (explode x) ) )

(defun  atomcdr  (x)
	(implode (cdr (explode x) ) ) )

;;;;;-------------MATCH-------------------------------------
(defun match ( p d )
	(cond
		( (and (null p) (null d) ) t)
		( (and (equal (atomcar(car p)) '*) (null (cdr p)))
						(set (atomcdr (car p)) d) t)
		( (or (null p) (null d)) nil )

		( (and	(equal (atomcar(car p)) '>)	(match (cdr p) (cdr d) ))
			(set (atomcdr(car p)) (car d))  t)

		( (equal (car p) (car d) )			;;if heads are equal
			(match (cdr p) (cdr d) )			;; then continue with tail
		)

		( (equal (atomcar(car p)) '*)					;;if it's a '*' or '* * ..'
			(cond

				( (match (cdr p) d)
						(set (atomcdr (car p)) '()) t)			;;if * for empty symbol
				( (null (cdr p))
						(set (atomcdr (car p)) d) t)
				( (match (cdr p) (cdr d))
						(set (atomcdr (car p)) (list (car d))) t)	;;if after * everything's equal
				( (match p (cdr d))
						(set 	(atomcdr (car p))
								(cons (car d) (eval (atomcdr (car p))))) t)			;;if different after*
			)
		)
	)
)

(defun atomlast (x)
	(car (last (explode x) ) ) )

(defun atombutlast (x)
	(implode (butlast (explode x) ) ) )

(defun isitaquestion (s)
	(equal (atomlast (car (last s))) '?))

(defun isitavozglas (s)
	(equal (atomlast (car (last s))) '!))

(defun deletevoskl (s)
	(append (butlast s) (list (atombutlast (car (last s))))))

(defun addquestion (s)
	(append (butlast s) (list (implode (append (explode (car (last s))) '(?))) ) ))

;;;;;-----------
(defun lizzy()
	(setq l nil work nil govori nil s nil smth1 nil smth2 nil x 1)		
	(print '(Zdravstvuite!))
	(loop while (= x 1) do
		(setq s (read))
		(cond
			(
				(match '(Zdravstvuite!) s) 
				(print '(Chem ya mogu Vam pomoch?))
			)			
			(
				(match '(Chto za *l) s)
				
				(print
					(append '(Chto Vy imeete v vidu govorya )
							(cond
								( (isitavozglas l) (addquestion (deletevoskl l)) )
								(t (addquestion l))
							)
					)
				)
			)
			(
				(match '(Menya volnuet *l) s) 
				(print '(Kak davno Vas eto volnuet?) )
			)
			(
				(or (match '(*smth1 sdelka *smth2) s) (match '(*smth1 sdelku *smth2) s))
				(setq work t) (print '(Rasskjite bolshe o vashih partnerah) )
			)			
			(
				(match '(Konechno *l) s) 
				(print '(Vy kak budto somnevaetes v etom ) )
			)
			(
				(isitaquestion s) 
				(print '(A pochemu Vy sprshivaete?))
			)
			(
				(or (match '(net) s) (match '(da) s) )
				(print '(Pojaluista ne budte stol kratkim) )
			)
			(	work
				(setq work nil) (print '(Vy govorili o Vashem biznese) )
			)
			(
				(or (match '(Do svidaniya) s) (match '(Proshaite) s))
				(return '(Do svidaniya) )
			)
			(	t
				(cond
					(govori
						(setq govori nil)
						(print '(Ya ne uveren chto polnostyu Vas ponimayu))
					)
					(t
						(setq govori t)
						(print '(Pojaluista prodoljaite))
					)
				);end of cond
			);end of default option
		)
	)
)