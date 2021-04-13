D = B_ASCII_DIGIT

dec_sep = °("." | ",")

int = @(D+)

float = int? (dec_sep int)?

main = float
