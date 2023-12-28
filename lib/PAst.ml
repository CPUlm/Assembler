open Integers
open Labels
open Isa

type future_instr = {res_space: int; op: future_value}

and future_value =
  | FuturePLabelLoad of reg * ProgramLabel.t * reg
  | FutureJumpOffset of flag option * ProgramLabel.t

type pprog =
  { pprog_instrs: (ProgramAddress.t * (isa, future_instr) Either.t) Monoid.t
  ; pprog_label_mapping: ProgramLabel.t SMap.t
  ; pprog_label_position: ProgramAddress.t ProgramLabel.map
  ; pprog_next_address: ProgramAddress.t }

type fprog =
  { fprog_instrs: (ProgramAddress.t * isa) Monoid.t
  ; fprog_label_mapping: ProgramLabel.t SMap.t
  ; fprog_label_position: ProgramAddress.t ProgramLabel.map
  ; fprog_next_address: ProgramAddress.t }
