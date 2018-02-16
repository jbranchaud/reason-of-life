type generationType = Empty | Random;

let generateBoard = (size: int): list(list(CellData.data)) => {
  let board = Array.make(size, Array.to_list(Array.make(size, CellData.make(()))));
  Array.to_list(board);
};

let generateRandomBoard = (size: int): list(list(CellData.data)) => {
  Random.init(int_of_float(Js.Date.now()));
  let board = Array.make(size, []);

  for (rowIndex in 0 to size-1) {
    let currRow = Array.make(size, CellData.make(()));
    for (cellIndex in 0 to size-1) {
      if (Random.int(3) == 0) {
        currRow[cellIndex] = CellData.make(~value=1, ());
      };
    };
    board[rowIndex] = Array.to_list(currRow);
  };

  Array.to_list(board);
};

let generate = (size: int, genType: generationType): list(list(CellData.data)) => {
  switch (genType) {
  | Empty => generateBoard(size)
  | Random => generateRandomBoard(size)
  };
};

let toggleCell = (currBoard: list(list(CellData.data)), x: int, y: int):
  list(list(CellData.data)) => {
    List.mapi((rowIndex, row) => {
      List.mapi((cellIndex, cell: CellData.data) => {
        switch ((cell.value, rowIndex == x && cellIndex == y)) {
        | (1, true) => CellData.make(~value=0, ())
        | (0, true) => CellData.make(~value=1, ())
        | _ => cell
        }
      }, row);
    }, currBoard);
  };

let neighborValue = (board: list(list(CellData.data)), rowIndex: int, cellIndex: int):
  int => {
    switch (List.nth(List.nth(board, rowIndex), cellIndex)) {
    | cellData => cellData.value
    | exception Invalid_argument("List.nth") => 0
    | exception Failure("nth") => 0
    };
  };

let tick = (currBoard: list(list(CellData.data))): list(list(CellData.data)) => {
  let size = List.length(currBoard);
  let board = Array.make(size, []);

  for (rowIndex in 0 to size-1) {
    let currRow = Array.make(size, CellData.make(()));
    for (cellIndex in 0 to size-1) {

      /* Any live cell with fewer than two live neighbours dies, as if caused by underpopulation. */
      /* Any live cell with two or three live neighbours lives on to the next generation. */
      /* Any live cell with more than three live neighbours dies, as if by overpopulation. */
      /* Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction. */
      let liveNeighborCount = {
        /* 0 1 2 */
        /* 3 _ 4 */
        /* 5 6 7 */
        let n0 = neighborValue(currBoard, rowIndex-1, cellIndex-1);
        let n1 = neighborValue(currBoard, rowIndex-1, cellIndex);
        let n2 = neighborValue(currBoard, rowIndex-1, cellIndex+1);
        let n3 = neighborValue(currBoard, rowIndex, cellIndex-1);
        let n4 = neighborValue(currBoard, rowIndex, cellIndex+1);
        let n5 = neighborValue(currBoard, rowIndex+1, cellIndex-1);
        let n6 = neighborValue(currBoard, rowIndex+1, cellIndex);
        let n7 = neighborValue(currBoard, rowIndex+1, cellIndex+1);
        n0 + n1 + n2 + n3 + n4 + n5 + n6 + n7
      };

      /* 256 / size * rowIndex (or cellIndex) placed into RGBA */
      let red = (256 / size) * (rowIndex + 1);
      let green = (256 / size) * (cellIndex + 1);
      let blue = 128;

      let currCellData = List.nth(List.nth(currBoard, rowIndex), cellIndex);
      let status =
        switch (currCellData.value, liveNeighborCount) {
        | (1, 0) => 0
        | (1, 1) => 0
        | (1, 2) => 1
        | (1, 3) => 1
        | (1, _) => 0
        | (0, 3) => 1
        | _ => currCellData.value
        };
      let alpha =
        switch (status) {
        | 1 => 1.0
        | _ => 0.3
        };
      let newColor = Printf.sprintf("rgba(%i, %i, %i, %f)", red, green, blue, alpha);
      currRow[cellIndex] = CellData.make(~value=status, ~color=newColor, ())
    };
    board[rowIndex] = Array.to_list(currRow);
  };

  Array.to_list(board);
};
