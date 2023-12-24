open Integers
open Labels

type data_file = {
  data_bytes : bytes Monoid.t;
  data_label_mapping : DataLabel.t SMap.t;
  data_label_position : MemoryAddress.t DataLabel.map;
}

type instr_file = {
  instr_bytes : bytes Monoid.t;
  instr_label_mapping : ProgramLabel.t SMap.t;
  instr_label_position : ProgramAddress.t ProgramLabel.map;
}
