(* This file is (C) 2014 by Gerd Stolpmann. It is distributed under the
   same license conditions as OMake. It was developed with financial support
   from Lexifi.
 *)

open Printf

type probe =
    { probe_name : string;
      mutable probe_count : int;
      mutable probe_acc_time : float;
      mutable probe_self_time : float;
      mutable probe_min_time : float;
      mutable probe_max_time : float;
      mutable probe_invocations : int;
      mutable probe_recursive : bool;
      mutable probe_error : bool;
    }

let registry = ref []

let create probe_name =
  let p =
    { probe_name;
      probe_count = 0;
      probe_acc_time = 0.0;
      probe_self_time = 0.0;
      probe_min_time = infinity;
      probe_max_time = 0.0;
      probe_invocations = 0;
      probe_recursive = false;
      probe_error = false;
    } in
  registry := p :: !registry;
  p

let callstack = Stack.create()


let start probe =
  let ts0 = Unix.gettimeofday() in
  if not (Stack.is_empty callstack) then (
    let (top_probe, _, ts2) = Stack.top callstack in
    let t2 = ts0 -. ts2 in
    top_probe.probe_self_time <- top_probe.probe_self_time +. t2
  );
  probe.probe_invocations <- probe.probe_invocations + 1;
  Stack.push (probe, ts0, ts0) callstack

let fst3 (x,_,_) = x

let stop probe =
  while not (Stack.is_empty callstack || fst3(Stack.top callstack) != probe) do
    let (p,_,_) = Stack.pop callstack in
    p.probe_error <- true;
  done;
  if Stack.is_empty callstack then
    probe.probe_error <- true
  else
    let (p,ts1,ts2) = Stack.pop callstack in
    assert(p == probe);
    let t0 = Unix.gettimeofday() in
    let t1 = t0 -. ts1 in
    let t2 = t0 -. ts2 in
    probe.probe_invocations <- probe.probe_invocations - 1;
    probe.probe_count <- probe.probe_count + 1;
    probe.probe_self_time <- probe.probe_self_time +. t2;
    probe.probe_min_time <- min probe.probe_min_time t2;
    probe.probe_max_time <- max probe.probe_max_time t2;
    if probe.probe_invocations > 0 then
      probe.probe_recursive <- true
    else
      probe.probe_acc_time <- probe.probe_acc_time +. t1

let finish() =
  Stack.iter (fun (p,_,_) -> p.probe_error <- true) callstack;
  Stack.clear callstack

let instrument probe f arg =
  start probe;
  try
    let r = f arg in
    stop probe;
    r
  with error ->
    stop probe;
    raise error


let report() =
  let all = List.rev !registry in
  printf "Probes:\n";
  printf "%-20s %2s %9s %9s %9s %9s %9s\n"
         "NAME" "FL" "COUNT" "ACC" "SELF" "MIN" "MAX";
  List.iter
    (fun p ->
       printf
         "%-20s %2s %9d %9.0f %9.0f %9.0f %9.0f\n"
         p.probe_name
         ( (if p.probe_recursive then "R" else " ") ^
             (if p.probe_error then "E" else " ") )
         p.probe_count
         p.probe_acc_time
         p.probe_self_time
         p.probe_min_time
         p.probe_max_time
    )
    all;
  flush stdout
