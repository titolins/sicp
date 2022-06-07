; quote is a special form which serves the same purpose as the quotation mark. So, 'abracadabra
; is exactly the same as (quote abracadabra). Input either of those in the interpreter evaluates
; to the symbol "abracadabra". If we double quote it, the interpreter expands the expression into a list:
; (quote abracadabra)
; car'ing this will return its first element, "quote"
