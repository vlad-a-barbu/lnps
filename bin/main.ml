type number =
  | Int of int
  | Float of float

type token =
  | Whitespace of string
  | Number of number
  | String of string
  | Array of token list

module Scanners = struct
  let whitespace state stack _ = function
    | None when state = `Start -> `Rejected, stack
    | None -> `Accepted, stack
    | Some ch ->
      (match ch with
       | ' ' | '\t' | '\r' | '\n' -> `Whitespace, stack
       | _ when state = `Start -> `Rejected, stack
       | _ -> `Accepted, stack)
  ;;

  let number state stack _ = function
    | None when state <> `Start -> `Accepted, stack
    | None -> `Rejected, stack
    | Some ch ->
      (match ch with
       | '0' .. '9' -> `Digit, stack
       | '.' when state <> `Dot -> `Dot, stack
       | _ when state <> `Start -> `Accepted, stack
       | _ -> `Rejected, stack)
  ;;

  let string state stack _ = function
    | None when state = `End -> `Accepted, stack
    | None -> `Rejected, stack
    | Some ch ->
      (match state, ch with
       | `Start, '"' -> `Quote, stack
       | `Quote, '"' -> `End, stack
       | `Quote, '\\' -> `Escape, stack
       | `Quote, _ -> `Quote, stack
       | `Escape, _ -> `Quote, stack
       | `End, _ -> `Accepted, stack
       | _ -> `Rejected, stack)
  ;;

  let array state stack parse = function
    | None when state = `End -> `Accepted, stack
    | None -> `Rejected, stack
    | Some ch ->
      (match state, ch with
       | `Start, '[' -> `ParseChild, stack
       | `ParseChild, ',' -> `Comma, stack
       | `Comma, _ -> `ParseChild, stack
       | `ParseChild, ']' -> `End, stack
       | `End, _ -> `Accepted, stack
       | `ParseChild, _ | `Position _, _ ->
         (match parse () with
          | None -> `Rejected, stack
          | Some (pos, token) -> `Position pos, token :: stack)
       | _ -> `Rejected, stack)
  ;;

  let all = [ `Array, array; `String, string; `Number, number; `Whitespace, whitespace ]
end

let convert str acc = function
  | `Array -> Array acc
  | `String -> String str
  | `Whitespace -> Whitespace str
  | `Number ->
    (match float_of_string_opt str with
     | Some x -> Number (Float x)
     | None ->
       let x = int_of_string str in
       Number (Int x))
;;

let char_at str pos =
  try Some (String.get str pos) with
  | Invalid_argument _ -> None
;;

let rec scan str start_pos target scanner =
  let parse_at pos = parse str pos in
  let rec go str pos scanner state stack =
    let ch = char_at str pos in
    let parse_child () = parse_at pos in
    match scanner state stack parse_child ch with
    | `Accepted, stack -> Some (pos, stack)
    | `Rejected, _ -> None
    | `Position pos, stack -> go str pos scanner state stack
    | state, stack -> go str (pos + 1) scanner state stack
  in
  match go str start_pos scanner `Start [] with
  | None -> None
  | Some (pos, stack) ->
    let len = pos - start_pos in
    let str = String.sub str start_pos len in
    let token = convert str stack target in
    Some (pos, token)

and parse str start_pos =
  let scan_with (target, scanner) = scan str start_pos target scanner in
  let results = List.filter_map scan_with Scanners.all in
  let max_munch acc (target, pos) =
    match acc with
    | None -> Some (target, pos)
    | Some (_, curr_pos) -> if pos > curr_pos then Some (target, pos) else acc
  in
  List.fold_left max_munch None results
;;

let token_of_str str =
  let str = String.trim str in
  let len = String.length str in
  match parse str 0 with
  | Some (pos, token) when pos = len -> Some token
  | _ -> None
;;

let read_file path =
  try
    let content = In_channel.with_open_text path In_channel.input_all in
    Some content
  with
  | _ -> None
;;

let token_of_file path =
  match read_file path with
  | Some str -> token_of_str str
  | None -> None
;;

let () =
  match Array.to_list Sys.argv with
  | [] | [ _ ] -> print_endline "path = ?"
  | _ :: path :: _ ->
    (match token_of_file path with
     | Some _ -> print_endline "accepted"
     | None -> print_endline "rejected")
;;
