	t0 :=int neg_int 1000
	myInt@2,5 :=int t0
	myReal@3,5 :=real 9.876
	myBool@4,5 :=bool false
	myChar@5,5 :=char 'a'

	myStr@6,5 :=addr ptr$str0
	myInit@7,5 :=real 0.0
	t1 :=real neg_real 1.234
	myRealVar@8,5 :=real t1
	myConstInt@11,7 :=int -1000

	myConstReal@12,7 :=real 9.876
	myConstBool@13,7 :=bool false
	myConstChar@14,7 :=char 'a'
	myyConstVoid@15,7 :=addr ptr$str1
	myConstStr@16,7 :=addr ptr$str2

	myConstInit@17,7 :=real 1.5
	myConstRealVar@18,7 :=real -1.234
	t2[0] :=int 1
	t2[4] :=int 2
	t2[8] :=int 3

	t2[12] :=int 4
	t2[16] :=int -5000
	weird@20,7 :=addr t2
	call main@23,6 0
	exit


main@23,6:
	return

endFun0:
	return
	
	# Static Data


	ptr$str0		"Some string..."
	ptr$str1		"No"
	ptr$str2		"Some string..."
