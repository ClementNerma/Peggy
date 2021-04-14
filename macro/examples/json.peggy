s = °(" " | "    " | B_NEWLINE_CR | B_NEWLINE_LF)*

object = "{" s "}" | "{" s pair s ("," s pair s)* "}"
pair = string s ":" s value
array = "[" s "]" | "[" s value s ("," s value s)* "]"

value = object | array | string | number | boolean | null

boolean = "true" | "false"

null = "null"

string = °B_DOUBLE_QUOTE @(car*) °B_DOUBLE_QUOTE
car = @((!(B_DOUBLE_QUOTE | "\") B_ANY) | ("\" (B_DOUBLE_QUOTE | "\" | "/" | "b" | "f" | "n" | "r" | "t")) | ("\" "u" B_ASCII_HEXDIGIT B_ASCII_HEXDIGIT B_ASCII_HEXDIGIT B_ASCII_HEXDIGIT)) #"

number = @("-"? ("0" | (!"0" B_ASCII_DIGIT*)) ("." B_ASCII_DIGIT*)? (!"e" ("+" | "-")? B_ASCII_DIGIT+)?)

json = object | array

main = json
