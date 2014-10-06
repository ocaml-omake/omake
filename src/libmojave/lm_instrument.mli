(* This file is (C) 2014 by Gerd Stolpmann. It is distributed under the
   same license conditions as OMake. It was developed with financial support
   from Lexifi.
 *)

type probe

val create : string -> probe
  (** Create a new probe with this name *)

val start : probe -> unit
  (** Start the probe timer *)

val stop : probe -> unit
  (** Stop the probe timer *)

val instrument : probe -> ('a -> 'b) -> 'a -> 'b
  (** [instrument p f arg]: run [f arg] and return the result (or exception).
      While running the runtime is measured.
   *)

val finish : unit -> unit
  (** Globally finish all timers *)

val report : unit -> unit
  (** Print a report to stdout *)
