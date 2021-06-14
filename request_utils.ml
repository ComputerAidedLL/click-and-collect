exception Bad_request_exception of string

let get_key d k =
    let value =
        try Yojson.Basic.Util.member k d
        with Yojson.Basic.Util.Type_error (_, _) -> raise (Bad_request_exception ("request body must be a json object"))
    in
    if value = `Null
    then raise (Bad_request_exception ("required argument '" ^ k ^ "' is missing"))
    else value

let get_string d k =
    let value = get_key d k in
    try Yojson.Basic.Util.to_string value
    with Yojson.Basic.Util.Type_error (_, _) -> raise (Bad_request_exception ("field '" ^ k ^ "' must be a string"));;