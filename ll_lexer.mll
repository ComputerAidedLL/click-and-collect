{
open Ll_parser        (* The type token is defined in ll_parser.mli *)
}
rule token = parse
    [' ' '\t']          { token lexbuf }     (* skip blanks *)
    | ['\n']            { EOL }
    | "|-" | "⊢"        { THESIS }
    | ','               { COMMA }
    | '('               { LPAREN }
    | ')'               { RPAREN }
    | '^'               { DUAL }
    | '*' | "⊗"         { TENSOR }
    | '|' | "⅋"         { PAR }
    | '&'               { WITH }
    | '+' | "⊕"         { PLUS }
    | '1'               { ONE }
    | "bot" | "⊥" | '_' { BOTTOM }
    | "top" | "⊤" | 'T' { TOP }
    | '0'               { ZERO }
    | "-o" | "⊸"        { LOLLIPOP }
    | '!'               { OFCOURSE }
    | '?'               { WHYNOT }
    | ['a'-'z' 'A'-'Z']+['0'- '9']*'\''*"′"*"’"* as l  { LITT l }
    | ("Α"|"α"|"Β"|"β"|"Γ"|"γ"|"Δ"|"δ"|"Ε"|"ε"|"Ζ"|"ζ"|"Η"|"η"|"Θ"|"θ"|"Ι"|"ι"|"Κ"|"κ"|"Λ"|"λ"|"Μ"|"μ"|"Ν"|"ν"|"Ξ"|"ξ"|"Ο"|"ο"|"Π"|"π"|"Ρ"|"ρ"|"Σ"|"σ"|"Τ"|"τ"|"Υ"|"υ"|"Φ"|"φ"|"Χ"|"χ"|"Ψ"|"ψ"|"Ω"|"ω")['0'- '9']*'\''*"′"*"’"* as l  { LITT l }
    | eof               { EOL }