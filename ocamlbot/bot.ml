open Spotlib.Spot
open Twitter
open Api11
open Orakuda.Regexp.Infix

module Consumer = Twitter.Auth.Consumer

let auth_file = match Exn.catch ~f:Sys.getenv "HOME" with
  | `Ok home -> home ^/ ".ocamltter_auths"
  | `Error _exn -> !!% "Env var HOME is not found@."; exit 1

let () =
  if not & File.Test._e auth_file then begin
    Ocauth.Auth.save_dummy auth_file;
    !!% "No auth table found. Created a dummy file: %s@." auth_file;
    exit 1
  end

let () = prerr_endline "getting oauth..."
let app, user = 
  Ocauth.Auth.find (Ocauth.Auth.load auth_file) ~app:"ocamlbot" ~user:"ocamlbot"
let o = Ocauth.Auth.oauth app user
let () = prerr_endline "oauth done"

let is_ocaml_misspell = 
  let rex = <:m<ocaml/i>> in
  let rec loop text = 
    match text =~ rex with
    | None -> false
    | Some res ->
        match res#_0 with
        | "ocaml" | "OCAML" | "OCaml" -> loop res#_right
        | _ -> 
            (* Ok it is like "Ocaml" *)
            (* But we ignore the text just "Ocaml". *)
            String.length (<:s<\s//g>> text) > 5
  in
  loop 

let shindan tw =
  String.contains ~needle:"shindanmaker.com" tw#text
  ||
  match tw#entities with
  | None -> assert false
  | Some ents -> 
      List.exists (fun x -> 
        String.contains ~needle:"shindanmaker.com" x)
      & List.map (fun x -> x#expanded_url) ents#urls

let do_ocaml_misspell tw =
  let text = tw#text in
  let user_name tw = (from_Some tw#user#details)#name in
  let user_name = user_name tw in
  if is_ocaml_misspell text 
    && user_name <> "planet_ocaml"
    && not (shindan tw)
  then begin
    !!% "%Ld: %s: %s@." tw#id user_name text;
    if user_name <> "planet_ocaml" then begin 
(* Had a bug of int64 id 
      begin match Tweets.show tw#id o with
      | `Ok tw' ->
          assert (tw#id = tw'#id);
          assert (tw#text = tw'#text)
      | `Error e ->
          !!% "ERROR: @[%a@]@." Api11.Error.format_error e
      end;
*)
      let res, time = 
        Unix.timed (fun () ->
          match Favorites.create o tw#id with
          | `Ok _ -> !!% "OK@."
          | `Error e ->
              !!% "ERROR: @[%a@]@." Api11.Error.format_error e) ()
      in
      !!% "TIME=%f@." time;
      res
    end
  end else 
    !!% "XXX: %s@." text

let rec loop since_id = 
  match Search.tweets o ~count:100 ?since_id "ocaml" with
  | `Error (`Http _) -> 
      prerr_endline "HTTP";
      Unix.sleep 600;
      loop since_id
  | `Error (`Json_parse _) -> 
      prerr_endline "JSON PARSE";
      Unix.sleep 600;
      loop since_id
  | `Error (`Json e) -> 
      Format.eprintf "Json_conv: %a@." Json_conv.format_full_error e;
      Unix.sleep 600;
      loop since_id
  | `Ok res -> 
      match res#statuses with
      | [] -> 
          Format.eprintf "no updates. scheduled@.";
          Unix.sleep 600;
          loop since_id
      | ts -> 
          let last_id = 
            ts |> List.fold_left (fun id tw ->
              begin match tw#retweeted_status with
              | Some _ -> ()
              | None -> do_ocaml_misspell tw
              end;
              max id tw#id) (Option.default since_id (fun () -> 0L))
          in
          Format.eprintf "scheduled from %Ld@." last_id;
          Unix.sleep 10;
          loop (Some last_id)

let () = loop None
