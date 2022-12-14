
module type MT_policy =
  sig
    type t

    class handler : object
      val handle : t
      method finalize : unit
    end

    val lock   : t -> unit
    val unlock : t -> unit
  end

module Foo (Policy : MT_policy) : sig
  class t : object
    method do_something : unit
    method finalize     : unit
  end
end = struct

  class t = object
    inherit Policy.handler

    method do_something =
      Policy.lock handle ;
      print_endline "do something" ;
      Policy.unlock handle
  end

end


(* ---------------------------------------------------------------- *)

    (* ----------------
       external library
       ---------------- *)


module CRITICAL_SECTION : sig
  type t
  val define     : unit -> t
  val initialize : t -> unit
  val delete     : t -> unit
  val enter      : t -> unit
  val leave      : t -> unit
end = struct

  type t = int ref
  let define () = ref 0

  let log action sec = Printf.printf "%s SEC %d\n" action !sec

  let initialize sec = sec := 42 ; log "INIT" sec
  let delete     sec = log "DELETE" sec ; sec := 0
  let enter      sec = log "ENTER" sec
  let leave      sec = log "LEAVE" sec

end


module Mutex : sig
  type t
  val create   : unit -> t
  val wait_for : t -> unit
  val release  : t -> unit
end = struct

  type t = Mx

  let create   () = Mx
  let wait_for Mx = print_endline "Waiting for mutex..."
  let release  Mx = print_endline "Release mutex"

end


(* ---------------------------------------------------------------- *)


module No_sync = struct

  type t = unit

  class handler = object
    val handle = ()
    method finalize = ()
  end

  let lock   () = print_endline "no lock"
  let unlock () = print_endline "no unlock"

end


module CT_sync = struct

  type t = CRITICAL_SECTION.t

  class handler = object
    val handle = CRITICAL_SECTION.define ()
    method finalize = CRITICAL_SECTION.delete handle
  initializer
    CRITICAL_SECTION.initialize handle
  end

  let lock   = CRITICAL_SECTION.enter
  let unlock = CRITICAL_SECTION.leave

end


module MX_sync = struct

  type t = Mutex.t

  class handler = object
    val handle = Mutex.create ()
    method finalize = ()
  end

  let lock   = Mutex.wait_for
  let unlock = Mutex.release

end


(* ---------------------------------------------------------------- *)


let new_foo (policy : (module MT_policy)) =
  let module F = Foo (val policy) in
  new F.t

type 'a finalizable =
  < finalize : unit
  ; .. > as 'a

let using : 'a finalizable -> ('a -> unit) -> unit
  = fun res f ->
    f res ;
    res #finalize

let _ =
  using (new_foo (module CT_sync)) @@ fun private_foo ->
  using (new_foo (module MX_sync)) @@ fun shared_foo ->
  using (new_foo (module No_sync)) @@ fun up_to_f_ck_foo ->

  print_endline "---- doing something..." ;

  private_foo    # do_something ;
  shared_foo     # do_something ;
  up_to_f_ck_foo # do_something ;

  print_endline "---- finalizing..."
