open Integers
open Labels
open Isa

type future_instr = { res_space : int; op : future_value }

and future_value =
  | FuturePLabelLoad of reg * ProgramLabel.t * reg
  | FutureJumpOffset of flag option * ProgramLabel.t

type tprog_pos = {
  prog_pinstrs : (ProgramAddress.t * (isa, future_instr) Either.t) Monoid.t;
  prog_label_mapping : ProgramLabel.t SMap.t;
  prog_label_position : ProgramAddress.t ProgramLabel.map;
}
