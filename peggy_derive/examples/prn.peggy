S = _:B_WHITESPACE                          # Whitespace
DEC_SEP = _:("." | ",")                     # Decimal separator

int = B_ASCII_DIGIT+                        # Integer
float = int DEC_SEP int                     # Floating-point number
number = int | float                        # Number

operator = "+" | "-" | "*" | "/"            # Operator
operand = number | paren_expr               # Operand
operation = operand S+ operand S* operator  # Complete operation

paren_expr = _:"(" S* expr S* _:")"         # Expression wrapped between parenthesis
expr = number | operation | paren_expr      # Complete expression

main = expr                                 # Grammar's entrypoint