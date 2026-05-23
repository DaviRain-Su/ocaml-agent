(* JSONL session persistence: each turn is appended as one JSON line so a
   conversation can be resumed later. *)

type t = { path : string; oc : out_channel }

(* Load previously-saved turns from [path]; returns [] if the file is absent. *)
let load path : Llm.turn list =
  if not (Sys.file_exists path) then []
  else begin
    let ic = open_in path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () ->
        let rec loop acc =
          match input_line ic with
          | line ->
            let line = String.trim line in
            let acc =
              if line = "" then acc
              else
                match Yojson.Safe.from_string line with
                | j -> Llm.turn_of_json j :: acc
                | exception _ -> acc
            in
            loop acc
          | exception End_of_file -> List.rev acc
        in
        loop [])
  end

(* Open [path] for appending. Creates parent directories as needed. *)
let create path : t =
  let dir = Filename.dirname path in
  if dir <> "." && not (Sys.file_exists dir) then
    ignore (Sys.command (Printf.sprintf "mkdir -p %s" (Filename.quote dir)));
  let oc = open_out_gen [ Open_append; Open_creat ] 0o644 path in
  { path; oc }

let append t (turn : Llm.turn) =
  output_string t.oc (Yojson.Safe.to_string (Llm.turn_to_json turn));
  output_char t.oc '\n';
  flush t.oc

let close t = close_out_noerr t.oc
