; Data formats:
; Word - (list s e q u e n c e o f l e t t e r s)
; F_i(X) - (list X (list word1 word2 ...))
; Rule - (list left_side_word right_word)     (may be multiple rules for one left_side_word)
; Terminals and nonterminals are just like words
; Grammar - (list Nonterminals Terminals Rules Start_word)

; *******************************************************************
; DATA

; Rules
(setq rule1 (list '(s) '(b a)))
(setq rule2 (list '(a) '(p b a)))
(setq rule3 (list '(a) '()))
(setq rule4 (list '(b) '(d c)))
; (setq rule3 (list '(a) '(b)))
; (setq rule4 (list '(b) '(a)))
(setq rule5 (list '(c) '(m d c)))
(setq rule6 (list '(c) '()))
(setq rule7 (list '(d) '(r s r)))
(setq rule8 (list '(d) '(x)))

; Gramar

; All characters
(setq all_ch_set '(a b c d s x p m r))

(setq g (list 
			'(a b c d s) 
			'(x p m r) 
			(list rule1 rule2 rule3 rule4 rule5 rule6 rule7 rule8) 
			'(s)))

; *******************************************************************
; Getters for Grammar G
(defun get_nonterms(g) 
	(car g)
)

(defun get_terms(g)
	(cadr g)
)

(defun get_rules(g)
	(caddr g)
)

(defun get_start(g)
	(cadddr g)
)


; *******************************************************************
; Auxiliary functions

; Cut word to length k
(defun cut_word(word len) 
	(if (or (eql len '0) (null word))
		nil
		(cons 
			(car word) 
			(cut_word (cdr word) (- len 1))
		)
	)
)

; Cut all words to length k
(defun cut_word_list(word_list len) 
	(if (null word_list)
		nil
		(append
			(list (cut_word (car word_list) len))
			(cut_word_list (cdr word_list) len)
		)
	)
)

; T if element is in list
; nill - otherwise
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
; nil otherwise
(defun is_subset(lst1 lst2)
	(if (null lst1)
		T
		(if (is_in_list (car lst1) lst2)
			(is_subset (cdr lst1) lst2)
			nil
		)
	)
)
; T iff  each element from set1 is in set2 and vice versa
(defun equal_set(set1 set2) 
	(if (and (is_subset set1 set2) (is_subset set2 set1))
		T
		nil
	)
)

; Remove element elem from list
(defun remove_from_list(elem lst)
	(if (null lst)
		nil
		(if (equal (caar lst) elem)
			(remove_from_list elem (cdr lst))
			(append
				(list (car lst))
				(remove_from_list elem (cdr lst))
			)
		)
	)
)

; Get F(Y)
(defun get_fx_set(y f_list) 
	(if (null f_list) 
		nil
		(if (eql y (caar f_list))
			(cadar f_list)
			(get_fx_set y (cdr f_list))
		)
	)
)

; Get words which can be get form left_word using rule from rule_list
(defun get_implying_words(left_word rule_list) 
	(if (null rule_list)
		nil
		(if (equal (caar rule_list) left_word)
			(append 
				(list (cadar rule_list))
				(get_implying_words left_word (cdr rule_list))
			)
			(get_implying_words left_word (cdr rule_list))
		)
	)
)

; T if sequence consists of terminals only
; nil otherwise
(defun is_term_seq(seq term_list)
	(if (null seq)
		T
		(and 
			(is_in_list (car seq) term_list)
			(is_term_seq (cdr seq) term_list)
		)
	)
)

; Selects list of words which consist of terminal
; characters only from given list of words
(defun select_term_words(word_list term_list)
	(if (null word_list)
		nil
		(if (is_term_seq (car word_list) term_list)
			(append 
				(list (car word_list))
				(select_term_words (cdr word_list) term_list)
			)
			(select_term_words (cdr word_list) term_list)
		)
	)
)

; Calculate initial list of F (F_0s) for given 
; character (either terminal or not), grammar and k 
; DEPRECATED
; Use empty sets for F instead
(defun init_calc_f(char g k)
	(if (is_in_list char (get_terms g))
		(list char (list (list char)))											; If char is terminal in grammar
		(list 
			char
			(select_term_words 													; If char is nonterm - get all terminal sequences such that for
				(cut_word_list 													; each sequence x char->xa and |x| <= k for any a or a = e
					(get_implying_words (list char) (get_rules g)) k
				)
				(get_terms g)
			)
		)
	)
)

; DEPRECATED
(defun init_calc_f_for_list(char_lst g k)
	(if (null char_lst)
		nil
		(append
			(list (init_calc_f (car char_lst) g k))
			(init_calc_f_for_list (cdr char_lst) g k)
		)
	)
)

; Initialize all f sets with empty set for each character in ch_set
(defun initialize_f_for_list(char_list)
	(if (null char_list)
		nil
		(append
			(list (list (car char_list) nil))
			(initialize_f_for_list (cdr char_list))
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

; Concatenate all possible ordered pairs from first and second list
(defun concatenate_list_list(list1 list2) 
	(if (null list1)
		nil
		(append 
			(concatenate_word_list (car list1) list2)
			(concatenate_list_list (cdr list1) list2)
		)
	)
)

; Concatenates all possible ordered char sequences for list
(defun concatenate_lists(lists_list)
	(if (null lists_list)
		(list nil) ; may be ERROR there
		(concatenate_list_list (car lists_list) (concatenate_lists (cdr lists_list)))
	)
)

; The same as concatenate_lists but cuts it to length k
(defun concatenate_lists_maxlen(lists_list maxlen)
	(cut_word_list (concatenate_lists lists_list) maxlen)
)

; Remove duplicates from list and return new list
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

; Get list in which each list is F(X)
(defun get_fx_set_list(x_list f)
	(if (null x_list)
		nil
		(append
			(list (get_fx_set (car x_list) f))
			(get_fx_set_list (cdr x_list) f)
		)
	)
)

; Calculate new F for given list of implying words
(defun concatenation_f_list(implying_words_list f_prev k term_list)
	(if (null implying_words_list)
		nil
		(append
			(select_term_words
				(concatenate_lists_maxlen 
					(get_fx_set_list (car implying_words_list) f_prev) k
				)
				term_list
			)
			(concatenation_f_list (cdr implying_words_list) f_prev k term_list)
		)
	)
)

; Calculate next F(X) for given X - either terminal or not
; Data required : X, List of F_prev(A), g, k
(defun calculate_f(g x f_prev k)
	(if (is_term_seq (list x) (get_terms g))
		(list x (list (list x)))
		(list
			x
			(remove_duplicates (append 
				(get_fx_set x f_prev)
				(concatenation_f_list 
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

(defun stop_condition(f_cur f_prev b)
	(if (null b) 
		T
		(and 
			(equal_set
				(get_fx_set (car b) f_cur)
				(get_fx_set (car b) f_prev)
			)
			(stop_condition f_cur f_prev (cdr b))
		)
	)
)

(setq f1 (initialize_f_for_list all_ch_set))
(setq f2 (calculate_f_list g all_ch_set f1 1))
(setq f3 (calculate_f_list g all_ch_set f2 1))
(setq f4 (calculate_f_list g all_ch_set f3 1))
(setq f5 (calculate_f_list g all_ch_set f4 1))

(defun first_k(g x_lst f_prev k)
	(if (stop_condition 
			(calculate_f_list g x_lst f_prev k) f_prev x_lst ;(get_nonterm g)
		)
		;(remove_duplicates
		;	(concatenation_f_list (list x_lst) f_prev k (get_terms g))
		;)
		f_prev
		(first_k g x_lst (calculate_f_list g x_lst f_prev k) k)
	)
)

(defun first_k_for_char(g ch k)
	(get_fx_set ch (first_k g all_ch_set f1 k))
)

(defun first_k_for_list(g ch_list k)
	(remove_duplicates
		(concatenation_f_list (list ch_list) (first_k g all_ch_set f1 k) k (get_terms g))
	)
)

; Returns subsequence following sequence to_find or nil
; if there is no such sequence
(defun get_following_subseq(x seq)
	(if (null seq)
		nil
		(if (eql x (car seq))
			(if (eql nil (cdr seq))
				(list nil)
				(cdr seq)
			)
			(get_following_subseq x (cdr seq))
		)
	)
)

; Subrules format:
; (list left_char after_char_sequence)

(defun get_rules_containing_x(x rules_list)
	(if (null rules_list)
		nil
		(append
			(if (is_in_list x (cadar rules_list))
				(list (list (caar rules_list) (get_following_subseq x (cadar rules_list))))
				nil
			)
			(get_rules_containing_x x (cdr rules_list))
		)
	)
)

(defun calculate_f_follow(g f_prev subrule_list k)
	(if (null subrule_list)
		nil 
		(if (equal (list nil) (cadar subrule_list))
			(remove_from_list nil (get_fx_set (caaar subrule_list) f_prev))
			(append
				(remove_from_list nil 
					(first_k_for_char
						g 
						(caadar subrule_list) 
						k
					)
				)
				(if (is_in_list nil (first_k_for_char
										g 
										(caadar subrule_list) 
										k
									)
					)
					(get_fx_set (caaar subrule_list) f_prev)
					nil					
				)
			)
		)
	)
)

(defun calculate_f_follow_list(g x_list f_prev k)
	(if (null x_list)
		nil
		(append 
			(list (list 
				(car x_list)
				(calculate_f_follow 
					g
					f_prev 
					(get_rules_containing_x (car x_list) (get_rules g)) 
					k
				)
			))
			(calculate_f_follow_list g (cdr x_list) f_prev k)
		)
	)
)

(defun follow_k(g x_list f_prev k)
	(if (stop_condition (calculate_f_follow_list g x_list f_prev k) f_prev x_list)
		f_prev
		(follow_k g x_list (calculate_f_follow_list g x_list f_prev k) k)
	)
)