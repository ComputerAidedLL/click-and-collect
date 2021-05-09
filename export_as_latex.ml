open Proof

let proof_to_latex implicit_exchange proof =
    let header = "% This LaTeX file has been generated using the Click&coLLect tool.\n"
        ^ "% https://click-and-collect.linear-logic.org/\n\n" in
    let packages = "\\documentclass{article}\n\n"
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
        ^ "\\newcommand*{\\dev}[1]{\\infer{1}[\\ensuremath{?\\mathit{d}}]{\\vdash #1}}\n\n"
        ^ "\\newcommand{\\adaptpage}[1]{\n"
        ^ "  \\setlength{\\hoffset}{-0.7in}\n"
        ^ "  \\setlength{\\voffset}{-0.7in}\n"
        ^ "  \\newsavebox{\\proof}\n"
        ^ "  \\sbox{\\proof}{#1}\n"
        ^ "  \\setlength{\\pdfpageheight}{\\dimexpr\\ht\\proof+\\dp\\proof+0.6in\\relax}\n"
        ^ "  \\setlength{\\pdfpagewidth}{\\dimexpr\\wd\\proof+0.6in\\relax}\n"
        ^ "  \\shipout\\box\\proof\n"
        ^ "}\n\n" in
    let start_proof = "\\begin{document}\n\n\\adaptpage{\n\\begin{prooftree}\n" in
    let proof_lines = Proof.to_latex implicit_exchange proof in
    let end_proof = "\\end{prooftree}\n}\n\n\\end{document}\n\n" in
    Printf.sprintf "%s%s%s%s%s%s" header packages macros start_proof proof_lines end_proof;;

let export_as_latex_with_exceptions implicit_exchange request_as_json =
    let proof = Proof.from_json request_as_json in
    proof_to_latex implicit_exchange proof;;

let temp_directory = "local/var/temp";;

let create_file_prefix =
    let now = Unix.localtime (Unix.time ()) in
    let date_as_string = Printf.sprintf "%04d%02d%02d%d%d%d" (1900 + now.tm_year) (1 + now.tm_mon) now.tm_mday now.tm_hour now.tm_min now.tm_sec in
    let random_suffix = string_of_int (Random.int 1000000) in
    Printf.sprintf "export_as_latex_%s_%s" date_as_string random_suffix;;

let is_failure = function
    | Unix.WEXITED 0 -> false
    | _ -> true;;

let read_and_remove_file file_name =
    let channel = open_in file_name in
    let file_as_string = really_input_string channel (in_channel_length channel) in
    close_in channel;
    Sys.remove file_name;
    file_as_string

let convert_to_pdf proof_as_latex =
    (try let _ = Sys.is_directory temp_directory in () with Sys_error _ -> Unix.mkdir temp_directory 0o777);

    let file_prefix = create_file_prefix in
    let tex_file_name = Printf.sprintf "%s/%s.tex" temp_directory file_prefix in
    let tex_file = Unix.openfile tex_file_name [Unix.O_CREAT; Unix.O_WRONLY] 0o666 in
    let _ = Unix.write_substring tex_file proof_as_latex 0 (String.length proof_as_latex) in
    Unix.close tex_file;

    let log_file = "local/var/log/click_and_collect/latex.log" in
    let process_status = Unix.system (Printf.sprintf "pdflatex -interaction=nonstopmode -halt-on-error -output-directory %s %s >> %s" temp_directory tex_file_name log_file) in
    (if is_failure process_status then raise (Failure (Printf.sprintf "An error occured during pdflatex. Check logs in %s." log_file)));
    Sys.remove tex_file_name;

    let aux_file_name_to_remove = Printf.sprintf "%s/%s.aux" temp_directory file_prefix in
    Sys.remove aux_file_name_to_remove;
    let log_file_name_to_remove = Printf.sprintf "%s/%s.log" temp_directory file_prefix in
    Sys.remove log_file_name_to_remove;

    file_prefix

let get_pdf_file proof_as_latex =
    let file_prefix = convert_to_pdf proof_as_latex in

    let pdf_file_name = Printf.sprintf "%s/%s.pdf" temp_directory file_prefix in
    read_and_remove_file pdf_file_name

let get_png_file proof_as_latex =
    let file_prefix = convert_to_pdf proof_as_latex in

    let pdf_file_name = Printf.sprintf "%s/%s.pdf" temp_directory file_prefix in
    let process_status = Unix.system (Printf.sprintf "pdftoppm %s %s/%s -png" pdf_file_name temp_directory file_prefix) in
    (if is_failure process_status then raise (Failure "An error occured during pdftoppm."));
    Sys.remove pdf_file_name;

    let png_file_name = Printf.sprintf "%s/%s-1.png" temp_directory file_prefix in
    read_and_remove_file png_file_name

let export_as_latex implicit_exchange format request_as_json =
    try let proof_as_latex = export_as_latex_with_exceptions implicit_exchange request_as_json in
        if format = "tex" then true, proof_as_latex, "text/plain"
        else if format = "pdf" then true, get_pdf_file proof_as_latex, "application/pdf"
        else if format = "png" then true, get_png_file proof_as_latex, "image/png"
        else false, "Unknown format: " ^ format, ""
    with Proof.Json_exception m -> false, "Bad proof json: " ^ m, ""
        | Raw_sequent.Json_exception m -> false, "Bad sequent json: " ^ m, ""
        | Rule_request.Json_exception m -> false, "Bad rule_request json: " ^ m, ""
        | Proof.Rule_exception (_, m) -> false, "Invalid proof: " ^ m, "";;
