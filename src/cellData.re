type data = {
  value: int,
  color: string,
};

let generateColor = (status, boardSize, rowIndex, cellIndex): string => {
  let red = (256 / boardSize) * (rowIndex + 1);
  let green = (256 / boardSize) * (cellIndex + 1);
  let blue = 128;

  let alpha =
    switch (status) {
      | 1 => 1.0
      | _ => 0.3
      };
  Printf.sprintf("rgba(%i, %i, %i, %f)", red, green, blue, alpha);
};

let make = (~value as v: int=0, ~boardSize as size: int=1, ~rowIndex: int=1,
            ~cellIndex: int=1, ()) => {
  let c = generateColor(v, size, rowIndex, cellIndex);
  {value: v, color: c};
};
