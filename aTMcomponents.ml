type id = int ;;

type action =
  | Balance
  | Withdraw of int
  | Deposit of int
  | Next
  | Finished
;;

type account_spec = {name : string; id : id; balance : int} ;;

let database = ref [] ;;

let initialize (lst : account_spec list) : unit =
  database := lst ;;

let acquire_id () : id =
  print_string "Enter customer id: ";
  read_int () ;;

let acquire_amount () : int =
  print_string "Enter amount: ";
  read_int () ;;

let acquire_act () : action =
  print_string "Enter action: (B) Balance (-) Withdraw (+) Deposit (=) Done (X) Exit: ";
  match read_line () with
  | "B" -> Balance
  | "-" -> Withdraw (acquire_amount ())
  | "+" -> Deposit (acquire_amount ())
  | "=" -> Next
  | "X" -> Finished
  | _ -> raise (Invalid_argument "invalid action") ;;

let get_balance (id : id) : int =
  (List.find (fun x -> x.id = id) !database).balance ;;

let get_name (id : id) : string =
  (List.find (fun x -> x.id = id) !database).name ;;

let update_balance (id : id) (bal : int) : unit =
  let name = get_name id in
  database := List.filter (fun x -> x.id <> id) !database;
  database := {id = id; name = name; balance = bal} :: !database ;;

let present_message (message : string) : unit =
  print_string (message ^ "\n") ;;

let rec deliver_cash (cash : int) : unit =
  if cash >= 20 then
    begin
      print_string "[20 @ 20]";
      deliver_cash (cash - 20)
    end
  else Printf.printf " and %i more\n" cash ;;
