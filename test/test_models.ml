open Agent_lib

let failures = ref 0

let check name cond =
  if cond then Printf.printf "ok   - %s\n" name
  else begin
    incr failures;
    Printf.printf "FAIL - %s\n" name
  end

let contains0 hay needle =
  try ignore (Str.search_forward (Str.regexp_string needle) hay 0); true with Not_found -> false

let () =
  Unix.putenv "AGENT_SCOPED_MODELS" "";

  (* --- model catalog --- *)
  check "model context window lookup" (Models.context_window "deepseek-v4-pro" = Some 1000000);
  check "model unknown -> None" (Models.context_window "no-such-model" = None);
  check "model list filters" (List.for_all (fun (e : Models.entry) -> contains0 e.Models.id "glm" || contains0 e.Models.provider "zai") (Models.list ~pat:"zai" ()));
  check "model list nonempty" (Models.list () <> []);
  let parsed = Model_spec.parse (Some "openai/gpt-4o:high") in
  check "model spec parses provider prefix"
    (parsed.Model_spec.provider = Some "openai" && parsed.model = Some "gpt-4o" && parsed.thinking = Some "high");
  let parsed = Model_spec.parse (Some "openai") in
  check "model spec preserves provider-only switch" (parsed.Model_spec.provider = Some "openai" && parsed.model = None);
  let parsed = Model_spec.parse ~provider:"openrouter" (Some "openai/gpt-4o") in
  check "explicit provider keeps slash model id"
    (parsed.Model_spec.provider = Some "openrouter" && parsed.model = Some "openai/gpt-4o");
  Unix.putenv "AGENT_SCOPED_MODELS" "anthropic/*\nglm:high";
  let scoped = Models.scoped_from_env () in
  check "scoped models match globs and strip thinking suffix"
    (List.exists (fun (e : Models.entry) -> e.provider = "anthropic") scoped
     && List.exists (fun (e : Models.entry) -> contains0 e.id "glm") scoped
     && not (List.exists (fun (e : Models.entry) -> e.provider = "openai") scoped));
  Unix.putenv "AGENT_SCOPED_MODELS" "";

  Printf.printf "\n%s\n" (if !failures = 0 then "All model tests passed." else "FAILURES present.");
  exit (if !failures = 0 then 0 else 1)
