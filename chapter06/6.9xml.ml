type ('a, 'b) xml = XLf of 'b option | XBr of 'a * ('a, 'b) xml list
type token = PCDATA of string | Open of string | Close of string

let rec collect tag = function
    Close t :: rest when t = tag -> []
  | token :: rest -> token :: collect tag rest
  | [] -> []

let rec collect_rest tag = function
    Close t :: rest when t = tag -> rest
  | token :: rest -> collect_rest tag rest
  | [] -> []

let rec xml_list_of_tokens = function
    PCDATA str :: rest -> XLf (Some str) :: xml_list_of_tokens rest
  | Open tag :: rest
    -> (match collect tag rest with
        [] -> XBr (tag, [ XLf None ])
      | _ as col -> XBr (tag, xml_list_of_tokens col))
        :: xml_list_of_tokens (collect_rest tag rest)
  | [] -> []
  | _ -> [ XLf None ]

let xml_of_tokens = function
    Open tag :: rest -> XBr (tag, xml_list_of_tokens (collect tag rest))
  | PCDATA str :: [] -> XLf (Some str)
  | _ -> XLf None
