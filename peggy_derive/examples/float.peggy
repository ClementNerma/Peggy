D = B_ASCII_DIGIT

dec_sep = _:("." | ",")

int = @:(D+)

float = int? (dec_sep int)?

main = float
