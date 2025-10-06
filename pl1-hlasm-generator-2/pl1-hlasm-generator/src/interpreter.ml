(* This file implements the interpreter logic for executing generated assembly code from PL/I source code. *)

type instruction =
  | Add of string * string
  | Sub of string * string
  | Mul of string * string
  | Div of string * string
  | Load of string
  | Store of string
  | Jump of string
  | JumpIfZero of string
  | Halt

type state = {
  mutable registers: (string, int) Hashtbl.t;
  mutable program_counter: int;
  instructions: instruction list;
}

let create_state instructions =
  {
    registers = Hashtbl.create 10;
    program_counter = 0;
    instructions;
  }

let execute_instruction state instruction =
  match instruction with
  | Add (reg1, reg2) ->
      let value1 = Hashtbl.find state.registers reg1 in
      let value2 = Hashtbl.find state.registers reg2 in
      Hashtbl.replace state.registers reg1 (value1 + value2);
      state.program_counter <- state.program_counter + 1
  | Sub (reg1, reg2) ->
      let value1 = Hashtbl.find state.registers reg1 in
      let value2 = Hashtbl.find state.registers reg2 in
      Hashtbl.replace state.registers reg1 (value1 - value2);
      state.program_counter <- state.program_counter + 1
  | Mul (reg1, reg2) ->
      let value1 = Hashtbl.find state.registers reg1 in
      let value2 = Hashtbl.find state.registers reg2 in
      Hashtbl.replace state.registers reg1 (value1 * value2);
      state.program_counter <- state.program_counter + 1
  | Div (reg1, reg2) ->
      let value1 = Hashtbl.find state.registers reg1 in
      let value2 = Hashtbl.find state.registers reg2 in
      Hashtbl.replace state.registers reg1 (value1 / value2);
      state.program_counter <- state.program_counter + 1
  | Load reg ->
      state.program_counter <- state.program_counter + 1
  | Store reg ->
      state.program_counter <- state.program_counter + 1
  | Jump label ->
      state.program_counter <- int_of_string label
  | JumpIfZero label ->
      if Hashtbl.find state.registers "zero_flag" = 1 then
        state.program_counter <- int_of_string label
      else
        state.program_counter <- state.program_counter + 1
  | Halt ->
      state.program_counter <- List.length state.instructions

let run state =
  while state.program_counter < List.length state.instructions do
    let instruction = List.nth state.instructions state.program_counter in
    execute_instruction state instruction
  done

let interpret instructions =
  let state = create_state instructions in
  run state

(* Additional functions for setting up the interpreter, loading programs, etc. can be added here. *)