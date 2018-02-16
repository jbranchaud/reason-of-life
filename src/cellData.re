type data = {
  value: int,
  color: string,
};

let make = (~value as v: int=0, ~color as c: string="#000000", ()) => {
  {value: v, color: c};
};

