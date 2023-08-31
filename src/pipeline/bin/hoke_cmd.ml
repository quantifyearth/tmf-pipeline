open Lwt.Infix
module Secret = Capnp_rpc_net.Restorer.Id

let () = Mirage_crypto_rng_lwt.initialize (module Mirage_crypto_rng.Fortuna)

module Store = struct
  let hash_size = 256

  module Fixed_string = Index.Key.String_fixed (struct
    let length = 256
  end)

  module I = Index_unix.Make (Fixed_string) (Fixed_string) (Index.Cache.Noop)
  module Secret = Capnp_rpc_net.Restorer.Id

  type t = {
    store : I.t;
    make_sturdy : Secret.t -> Uri.t;
    load :
      validate:(unit -> bool) ->
      sturdy_ref:[ `Generic ] Capnp_rpc_lwt.Sturdy_ref.t ->
      Capnp_rpc_net.Restorer.resolution Lwt.t;
  }

  let create ~make_sturdy ~load path =
    let store = I.v ~log_size:4096 path in
    { store; make_sturdy; load }

  let pad_name name =
    let diff = hash_size - String.length name in
    if diff >= 0 then String.make diff ' ' ^ name else failwith "Name too long!"

  let add_client t name =
    let name = String.trim name in
    let secret = Secret.generate () in
    let hash = Secret.digest `SHA256 secret in
    let name = pad_name name in
    let store_secret = pad_name hash in
    I.replace t name store_secret;
    I.replace t store_secret name;
    I.merge t;
    secret

  let lookup t name =
    let name = pad_name name in
    try Some (I.find t name) with Not_found -> None

  let lookup_by_hash t digest =
    try Some (I.find t (pad_name digest)) with Not_found -> None

  let remove t name =
    let name = String.trim name in
    let padded_name = pad_name name in
    I.filter t (fun (k, _) -> k = padded_name);
    I.merge t

  let list t =
    let lst = ref [] in
    I.iter (fun k _ -> lst := String.trim k :: !lst) t;
    List.stable_sort String.compare !lst

  module type T = Capnp_rpc_net.Restorer.LOADER

  let hash _ = `SHA256
  let make_sturdy t = t.make_sturdy

  let validate t digest () =
    match lookup t.store digest with None -> false | Some _ -> true

  let load t self digest =
    Logs.info (fun f -> f "Looking up %s" digest);
    match lookup_by_hash t.store digest with
    | None ->
        Logs.info (fun f -> f "Nothing found :(");
        Lwt.return Capnp_rpc_net.Restorer.unknown_service_id
    | Some _ ->
        t.load ~validate:(validate t digest)
          ~sturdy_ref:(Capnp_rpc_lwt.Sturdy_ref.cast self)
end

module Admin = struct
  let add_client t restorer name =
    match Store.lookup t name with
    | Some _ -> Fmt.failwith "Client %s already exists!" name
    | None -> (
        let secret = Store.add_client t name in
        Capnp_rpc_net.Restorer.restore restorer secret >|= function
        | Ok v -> Ok v
        | Error exn -> Error (`Capnp (`Exception exn)))

  let remove_client t name =
    Store.remove t name;
    Lwt.return_ok ()

  let list_clients t = Store.list t |> Lwt.return

  let v sr restorer t =
    let add_client = add_client t restorer in
    let remove_client = remove_client t in
    let list_clients () = list_clients t in
    Hoke.Admin.v ~add_client ~remove_client ~list_clients sr
end

module Server = struct
  open Capnp_rpc_net

  let or_fail = function Ok v -> v | Error (`Msg m) -> failwith m

  let export ~secrets_dir ~vat ~name id =
    let path = Filename.concat secrets_dir (name ^ ".cap") in
    Capnp_rpc_unix.Cap_file.save_service vat id path |> or_fail;
    Logs.app (fun f -> f "Wrote capability reference to %S" path)

  let daemon capnp services builder store secrets_dir =
    let restore = Restorer.of_table services in
    let builder_id = Capnp_rpc_unix.Vat_config.derived_id capnp "builder" in
    let builder =
      let sr = Capnp_rpc_net.Restorer.Table.sturdy_ref services builder_id in
      Hoke.Client.v ~sr builder
    in
    Restorer.Table.add services builder_id builder;
    let admin_id = Capnp_rpc_unix.Vat_config.derived_id capnp "admin" in
    let admin =
      let sr = Capnp_rpc_net.Restorer.Table.sturdy_ref services admin_id in
      Admin.v sr restore store
    in
    Restorer.Table.add services admin_id admin;
    Capnp_rpc_unix.serve capnp ~restore >>= fun vat ->
    export ~secrets_dir ~vat ~name:"admin" admin_id;
    let v = Capnp_rpc_unix.Vat_config.sturdy_uri capnp admin_id in
    Logs.info (fun f -> f "E: %a" Uri.pp v);
    Logs.info (fun f -> f "Hoke running...");
    fst @@ Lwt.wait ()
end
