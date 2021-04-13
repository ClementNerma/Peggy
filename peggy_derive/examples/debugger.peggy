
main = ~"Peggy" rule1

rule1 = rule2
rule2 = rule3
rule3 = rule4
rule4 = (@(rule5_1 rule5_2 rule5_3) °s?)*
rule5_1 = "Pe"
rule5_2 = "gg"
rule5_3 = "y" !(~"?") !(!"!") "!"

s = B_WHITESPACE+
