S = °B_WHITESPACE                           # Whitespace
DEC_SEP = °("." | ",")                      # Decimal separator

int = @(B_ASCII_DIGIT+)                     # Integer
float = int DEC_SEP int                     # Floating-point number
number = int | float                        # Number

operator = "+" | "-" | "*" | "/"            # Operator
operand = number | paren_expr               # Operand
operation = operand S+ operand S* operator  # Complete operation

paren_expr = °"(" S* expr S* °")"           # Expression wrapped between parenthesis
expr = number | operation | paren_expr      # Complete expression

main = expr                                 # Grammar's entrypoint