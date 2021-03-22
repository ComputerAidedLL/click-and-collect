exception Bad_request_exception of string

let get_key d k =
    let value = Yojson.Basic.Util.member k d in
    if value = `Null
    then raise (Bad_request_exception ("required argument " ^ k ^" is missing"))
    else value

let get_string d k =
    let value = get_key d k in
    try Yojson.Basic.Util.to_string value
    with Yojson.Basic.Util.Type_error (_, _) -> raise (Bad_request_exception ("argument " ^ k ^" must be a string"))

let get_int d k =
    let value = get_key d k in
    try Yojson.Basic.Util.to_int value
    with Yojson.Basic.Util.Type_error (_, _) -> raise (Bad_request_exception ("argument " ^ k ^" must be an int"))

let get_list d k =
    let value = get_key d k in
    try Yojson.Basic.Util.to_list value
    with Yojson.Basic.Util.Type_error (_, _) -> raise (Bad_request_exception ("argument " ^ k ^" must be a list"))

let apply_rule_with_exceptions request_as_json =
    let rule = get_string request_as_json "rule" in
    let formula_list_as_json = get_list request_as_json "formulaList" in
    let formula_position = get_int request_as_json "formulaPosition" in
    let formula_list = List.map Linear_logic.json_to_formula formula_list_as_json in
    Linear_logic.apply_rule rule formula_list formula_position

let apply_rule request_as_json =
    try true, `List (List.map Linear_logic.formula_to_json (apply_rule_with_exceptions request_as_json))
    with
        | Bad_request_exception m -> false, `String m
        | Linear_logic.Bad_formula_json_exception m -> false, `String  ("Bad formula json: " ^ m)
        | Linear_logic.Apply_rule_exception m -> false, `String  m
