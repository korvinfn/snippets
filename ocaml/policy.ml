
module type MT_policy =
  sig
    class t : object
      method private lock   : unit
      method private unlock : unit

      method finalize : unit
    end
  end


module Foo (Policy : MT_policy) = struct

  class t = object (self)
    inherit Policy.t

    method do_something =
      self #lock ;
      print_endline "do something" ;
      self #unlock
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
  type handler
  val create   : bool -> handler option
  val wait_for : handler -> unit
  val release  : handler -> unit
end = struct

  type handler = HANDLER

  let create   broken  = if broken then None else Some HANDLER
  let wait_for HANDLER = print_endline "Waiting for mutex..."
  let release  HANDLER = print_endline "Release mutex"

end


(* ---------------------------------------------------------------- *)


module No_sync = struct

  class t = object
    method private lock   = print_endline "no lock"
    method private unlock = print_endline "no unlock"

    method finalize = ()
  end

end


module CT_sync = struct

  class t = object
    val ct = CRITICAL_SECTION.define ()

    method private lock   = CRITICAL_SECTION.enter ct
    method private unlock = CRITICAL_SECTION.leave ct

    method finalize = CRITICAL_SECTION.delete ct
  initializer
    CRITICAL_SECTION.initialize ct
  end

end


module MX_sync = struct

  class t = object
    val h_mux = ( match Mutex.create false with
                | Some h -> h
                | None   -> failwith "Can't create mutex" )

    method private lock   = Mutex.wait_for h_mux
    method private unlock = Mutex.release h_mux

    method finalize = ()
  end

end


(* ---------------------------------------------------------------- *)


let new_foo (policy : (module MT_policy)) =
  let module F = Foo (val policy) in
  new F.t


let _ =
  let private_foo    = new_foo (module CT_sync) in
  let shared_foo     = new_foo (module MX_sync) in
  let up_to_f_ck_foo = new_foo (module No_sync) in

  let foos = [
    private_foo ;
    shared_foo ;
    up_to_f_ck_foo ;
  ] in

  foos |> List.iter (fun x -> x # do_something) ;
  foos |> List.iter (fun x -> x # finalize) ;

  print_endline ""
