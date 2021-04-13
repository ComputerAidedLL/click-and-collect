open Proof

exception Cannot_export_proof_as_latex_exception of string;;

let proof_to_latex proof =
    let header = "% This LaTeX file has been generated using the Click&coLLect tool.\n"
        ^ "% https://click-and-collect.linear-logic.org/\n"
        ^ "% you can use it to generate pdf or image\n"
        ^ "% pdflatex ccLLproof.tex\n"
        ^ "% convert ccLLproof.pdf -colorspace RGB ccLLproof.png\n\n" in
    let packages = "\\documentclass[preview,border=0.5cm,varwidth]{standalone}\n\n"
        ^ "\\usepackage{cmll}\n"
        ^ "\\usepackage{ebproof}\n\n" in
    let macros = "\\newcommand*{\\orth}{^\\perp}\n"
        ^ "\\newcommand*{\\tensor}{\\otimes}\n"
        ^ "\\newcommand*{\\one}{1}\n"
        ^ "\\newcommand*{\\plus}{\\oplus}\n"
        ^ "\\newcommand*{\\zero}{0}\n\n"
        ^ "\\newcommand*{\\hypv}[1]{\\hypo{\\vdash #1}}\n"
        ^ "\\newcommand*{\\exv}[1]{\\infer{1}[\\ensuremath{\\mathit{ex}}]{\\vdash #1}}\n"
        ^ "\\newcommand*{\\axv}[1]{\\infer{0}[\\ensuremath{\\mathit{ax}}]{\\vdash #1}}\n"
        ^ "\\newcommand*{\\cutv}[1]{\\infer{2}[\\ensuremath{\\mathit{cut}}]{\\vdash #1}}\n"
        ^ "\\newcommand*{\\onev}[1]{\\infer{0}[\\ensuremath{\\one}]{\\vdash #1}}\n"
        ^ "\\newcommand*{\\botv}[1]{\\infer{1}[\\ensuremath{\\bot}]{\\vdash #1}}\n"
        ^ "\\newcommand*{\\topv}[1]{\\infer{0}[\\ensuremath{\\top}]{\\vdash #1}}\n"
        ^ "\\newcommand*{\\tensorv}[1]{\\infer{2}[\\ensuremath{\\tensor}]{\\vdash #1}}\n"
        ^ "\\newcommand*{\\parrv}[1]{\\infer{1}[\\ensuremath{\\parr}]{\\vdash #1}}\n"
        ^ "\\newcommand*{\\withv}[1]{\\infer{2}[\\ensuremath{\\with}]{\\vdash #1}}\n"
        ^ "\\newcommand*{\\pluslv}[1]{\\infer{1}[\\ensuremath{\\plus_1}]{\\vdash #1}}\n"
        ^ "\\newcommand*{\\plusrv}[1]{\\infer{1}[\\ensuremath{\\plus_2}]{\\vdash #1}}\n"
        ^ "\\newcommand*{\\ocv}[1]{\\infer{1}[\\ensuremath{\\oc}]{\\vdash #1}}\n"
        ^ "\\newcommand*{\\wkv}[1]{\\infer{1}[\\ensuremath{?\\mathit{w}}]{\\vdash #1}}\n"
        ^ "\\newcommand*{\\cov}[1]{\\infer{1}[\\ensuremath{?\\mathit{c}}]{\\vdash #1}}\n"
        ^ "\\newcommand*{\\dev}[1]{\\infer{1}[\\ensuremath{?\\mathit{d}}]{\\vdash #1}}\n\n" in
    let start_proof = "\\begin{document}\n\n%\scriptsize\n\\begin{prooftree}\n" in
    let proof_lines = Proof.to_latex proof in
    let end_proof = "\\end{prooftree}\n\n\\end{document}\n\n" in
    Printf.sprintf "%s%s%s%s%s%s" header packages macros start_proof proof_lines end_proof;;

let export_as_latex_with_exceptions request_as_json =
    let proof = Proof.from_json request_as_json in
    proof_to_latex proof;;

let export_as_latex request_as_json =
    try let proof_as_latex = export_as_latex_with_exceptions request_as_json in
        true, proof_as_latex
    with Proof.Json_exception m -> false, "Bad proof json: " ^ m
        | Sequent.Json_exception m -> false, "Bad sequent json: " ^ m
        | Rule_request.Json_exception m -> false, "Bad rule_request json: " ^ m
        | Proof.Technical_exception m -> false, "Invalid proof: " ^ m
        | Cannot_export_proof_as_latex_exception m -> false, "Cannot export proof as LaTeX: " ^ m;;
