; Word, terminals, nonterminals - (l e t t e r s)
; F_i(X) - (X (word1 word2 ...))
; Rule - (left_word right_word)
; Grammar G = (nonterminals terminals rules start_word)

(setq rule1 (list '(s) '(b a)))
(setq rule2 (list '(a) '(p b a)))
(setq rule3 (list '(a) '()))
(setq rule4 (list '(b) '(d c)))
(setq rule5 (list '(c) '(m d c)))
(setq rule6 (list '(c) '()))
(setq rule7 (list '(d) '(r s r)))
(setq rule8 (list '(d) '(q)))

(setq valid_symbols '(a b c d s q p m r))

;Grammar G
(setq g (list 
			'(a b c d s) 
			'(q p m r) 
			(list rule1 rule2 rule3 rule4 rule5 rule6 rule7 rule8) 
			'(s)
		)
)

(defun get_terms(g)
	(cadr g)
)

(defun get_rules(g)
	(caddr g)
)

; Cut word to length k
(defun cut_word(word k) 
	(if (or (eql k '0) (null word))
		nil
		(cons 
			(car word) 
			(cut_word (cdr word) (- k 1))
		)
	)
)

; Cut all words to length k
(defun cut_word_list(word_list k) 
	(if (null word_list)
		nil
		(append
			(list (cut_word (car word_list) k))
			(cut_word_list (cdr word_list) k)
		)
	)
)

(defun is_in_list(elem lst)
	(if (null lst)
		nil
		(if (equal (car lst) elem)
			T
			(is_in_list elem (cdr lst))
		)
	)
)

; T if lst1 is subset of lst2
(defun is_subset(lst1 lst2)
	(if (null lst1)
		T
		(if (is_in_list (car lst1) lst2)
			(is_subset (cdr lst1) lst2)
			nil
		)
	)
)

(defun equal_set(set1 set2) 
	(if (and (is_subset set1 set2) (is_subset set2 set1))
		T
		nil
	)
)

(defun remove_duplicates(lst)
	(if (null lst)
		nil
		(if (is_in_list (car lst) (cdr lst))
			(remove_duplicates (cdr lst))
			(append
				(list (car lst))
				(remove_duplicates (cdr lst))
			)
		)
	)
)

; F(Y)
(defun get_fx(x f_list) 
	(if (null f_list) 
		nil
		(if (eql x (caar f_list))
			(cadar f_list)
			(get_fx x (cdr f_list))
		)
	)
)

; Get words which can be get from left_word using rules
(defun get_implying_words(left_word rule_list) 
	(if (null rule_list)
		nil
		(append 
			(if (equal (caar rule_list) left_word) 
				(list (cadar rule_list)) 
				nil
			)
			(get_implying_words left_word (cdr rule_list))
		)
	)
)

; T if sequence consists of terminals only
(defun is_term_seq(seq term_list)
	(if (null seq)
		T
		(and 
			(is_in_list (car seq) term_list)
			(is_term_seq (cdr seq) term_list)
		)
	)
)

; Return list of words which consist of terminal characters only from given list of words
(defun select_term_words(word_list term_list)
	(if (null word_list)
		nil		
		(append 
			(if (is_term_seq (car word_list) term_list) 
				(list (car word_list)) 
				nil
			)
			(select_term_words (cdr word_list) term_list)
		)
	)
)

; Initialize all F with empty set for each character in char_list
(defun init_f(char_list)
	(if (null char_list)
		nil
		(append
			(list (list (car char_list) nil))
			(init_f (cdr char_list))
		)
	)
)

; Concatenate given word with all words from list
(defun concatenate_word_list(word word_list)
	(if (null word_list)
		nil
		(append 
			(list (append word (car word_list)))
			(concatenate_word_list word (cdr word_list))
		)
	)
)

; Concatenate all words from list2 with words from list1
(defun concatenate_two_lists(list1 list2) 
	(if (null list1)
		nil
		(append 
			(concatenate_word_list (car list1) list2)
			(concatenate_two_lists (cdr list1) list2)
		)
	)
)

; Concatenates all possible ordered char sequences for list
(defun concatenate_lists(lists_list)
	(if (null lists_list)
		(list nil) ; possible ERROR
		(concatenate_two_lists (car lists_list) (concatenate_lists (cdr lists_list)))
	)
)

; The same as concatenate_lists with cutting all words to length maxlen
(defun concatenate_lists_maxlen(lists_list maxlen)
	(cut_word_list (concatenate_lists lists_list) maxlen)
)

; Get list of F(X) for every word from x_list
(defun get_fx_list(x_list f)
	(if (null x_list)
		nil
		(append
			(list (get_fx (car x_list) f))
			(get_fx_list (cdr x_list) f)
		)
	)
)

; Calculate new F for given list of implying words
(defun calc_concat_f_list(implying_words_list f_prev k term_list)
	(if (null implying_words_list)
		nil
		(append
			(select_term_words
				(concatenate_lists_maxlen 
					(get_fx_list (car implying_words_list) f_prev) k
				)
				term_list
			)
			(calc_concat_f_list (cdr implying_words_list) f_prev k term_list)
		)
	)
)

; Calculate next F(X) for given x - terminal or nonterminal
(defun calculate_f(g x f_prev k)
	(if (is_term_seq (list x) (get_terms g))
		(list x (list (list x)))
		(list
			x
			(remove_duplicates (append 
				(get_fx x f_prev)
				(calc_concat_f_list 
					(get_implying_words (list x) (get_rules g)) 
					f_prev
					k
					(get_terms g)
				)
			))
		)
	)
)

(defun calculate_f_list(g x_lst f_prev k)
	(if (null x_lst)
		nil
		(append 
			(list (calculate_f g (car x_lst) f_prev k))
			(calculate_f_list g (cdr x_lst) f_prev k)
		)
	)
)

(defun stop_condition(f_cur f_prev x_lst)
	(if (null x_lst) 
		T
		(and 
			(equal_set
				(get_fx (car x_lst) f_cur)
				(get_fx (car x_lst) f_prev)
			)
			(stop_condition f_cur f_prev (cdr x_lst))
		)
	)
)

(defun first_k(g x_lst f_prev k)
	(if (stop_condition 
			(calculate_f_list g x_lst f_prev k) f_prev x_lst 
		)
		f_prev
		(first_k g x_lst (calculate_f_list g x_lst f_prev k) k)
	)
)

(first_k g valid_symbols (init_f valid_symbols) 1)