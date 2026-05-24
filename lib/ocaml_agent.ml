(* Public SDK facade.

   Aggregates the modules that make up the stable, programmatic surface of
   ocaml-agent so library consumers can build agents and providers without
   reaching into internal modules (Rpc, Tui, Commands, Extensions, Session,
   Packages, Skills, ...). The accompanying .mli narrows the surface. *)

module Transport = Transport
module Llm = Llm
module Tools = Tools
module Agent = Agent
module Extension_sdk = Extension_sdk
