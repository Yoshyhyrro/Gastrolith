type runtime_state = {
  mutable memory: (string, int) Hashtbl.t;
  mutable instruction_pointer: int;
}

let create_runtime_state () =
  {
    memory = Hashtbl.create 10;
    instruction_pointer = 0;
  }

let set_memory state address value =
  Hashtbl.replace state.memory address value

let get_memory state address =
  try Hashtbl.find state.memory address
  with Not_found -> 0

let increment_instruction_pointer state () =
  state.instruction_pointer <- state.instruction_pointer + 1

let reset_instruction_pointer state () =
  state.instruction_pointer <- 0

let execute_instruction state instruction =
  (* Placeholder for instruction execution logic *)
  increment_instruction_pointer state ()