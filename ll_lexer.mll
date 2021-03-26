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
    | '^'               { ORTH }
    | '*' | "⊗"         { TENSOR }
    | '|'               { PAR }
    | '&'               { WITH }
    | '+' | "⊕"         { PLUS }
    | '1'               { ONE }
    | "bot" | "⊥" | '_' { BOTTOM }
    | "top" | "⊤" | 'T' { TOP }
    | '0'               { ZERO }
    | "-o" | "⊸"        { LOLLIPOP }
    | '!'               { OFCOURSE }
    | '?'               { WHYNOT }
    | ['a'-'z' 'A'-'Z']+['0'- '9']*['\'']* as l   { LITT l }
    | "Α" as l | "α" as l | "Β" as l | "β" as l | "Γ" as l | "γ" as l | "Δ" as l | "δ" as l | "Ε" as l
    | "ε" as l | "Ζ" as l | "ζ" as l | "Η" as l | "η" as l | "Θ" as l | "θ" as l | "Ι" as l | "ι" as l
    | "Κ" as l | "κ" as l | "Λ" as l | "λ" as l | "Μ" as l | "μ" as l | "Ν" as l | "ν" as l | "Ξ" as l
    | "ξ" as l | "Ο" as l | "ο" as l | "Π" as l | "π" as l | "Ρ" as l | "ρ" as l | "Σ" as l | "σ" as l
    | "Τ" as l | "τ" as l | "Υ" as l | "υ" as l | "Φ" as l | "φ" as l | "Χ" as l | "χ" as l | "Ψ" as l
    | "ψ" as l | "Ω" as l | "ω" as l              { LITT l }
    | eof               { EOL }