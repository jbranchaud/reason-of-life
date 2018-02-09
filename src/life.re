type action =
  | StartTimer
  | StopTimer
  | Tick;

type state = {
  running: bool,
  timerId: ref(option(Js.Global.intervalId)),
  board: list(list(Cell.data))
};

let cell = (cd: Cell.data) => {
  if (cd.value == 1) {
    <span className="cell alive"
      style=(ReactDOMRe.Style.make(~backgroundColor=cd.color, ())) />
  } else {
    <span className="cell dead"
      style=(ReactDOMRe.Style.make(~backgroundColor=cd.color, ())) />
  };
};

let generateBoard = (size: int): list(list(Cell.data)) => {
  let board = Array.make(size, Array.to_list(Array.make(size, Cell.make(()))));
  Array.to_list(board);
};

let generateRandomBoard = (size: int): list(list(Cell.data)) => {
  let board = Array.make(size, []);

  for (rowIndex in 0 to size-1) {
    let currRow = Array.make(size, Cell.make(()));
    for (cellIndex in 0 to size-1) {
      if (Random.int(3) == 0) {
        currRow[cellIndex] = Cell.make(~value=1, ());
      };
    };
    board[rowIndex] = Array.to_list(currRow);
  };

  Array.to_list(board);
};

let neighborValue = (board: list(list(Cell.data)), rowIndex: int, cellIndex: int):
  int => {
    switch (List.nth(List.nth(board, rowIndex), cellIndex)) {
    | cellData => cellData.value
    | exception Invalid_argument("List.nth") => 0
    | exception Failure("nth") => 0
    };
  };

let tickBoard = (currBoard: list(list(Cell.data))): list(list(Cell.data)) => {
  let size = List.length(currBoard);
  let board = Array.make(size, []);

  for (rowIndex in 0 to size-1) {
    let currRow = Array.make(size, Cell.make(()));
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
      let blue = (256 / (size * 2)) * (cellIndex + rowIndex + 2);

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
      currRow[cellIndex] = Cell.make(~value=status, ~color=newColor, ())
    };
    board[rowIndex] = Array.to_list(currRow);
  };

  Array.to_list(board);
};

let life = ReasonReact.reducerComponent("Life");

let make = (_children) => {
  ...life,
  didMount: (self) => {
    self.state.timerId := Some(Js.Global.setInterval(() => {
      self.send(Tick)
    }, 1000));
    ReasonReact.NoUpdate
  },
  willUnmount: (self) => {
    if (self.state.running) {
        switch (self.state.timerId^) {
        | Some(id) => Js.Global.clearInterval(id);
        | None => ()
        }
    }
  },
  willUpdate: ({oldSelf, newSelf}) => {
    if (oldSelf.state.running && !newSelf.state.running) {
      switch (newSelf.state.timerId^) {
      | Some(id) => Js.Global.clearInterval(id);
      | None => ()
      }
    };
    if (!oldSelf.state.running && newSelf.state.running) {
      newSelf.state.timerId := Some(Js.Global.setInterval(() => {
        newSelf.send(Tick) }, 1000));
    };
  },
  initialState: () => {
      running: true,
      timerId: ref(None),
      board: generateRandomBoard(20)
  },
  reducer: (action, state) =>
    switch (action) {
    | StartTimer => ReasonReact.Update({...state, running: true })
    | StopTimer => ReasonReact.Update({...state, running: false })
    | Tick => {
      let newBoard = tickBoard(state.board);
      ReasonReact.Update({...state, board: newBoard})
      }
    },
  render: ({state, send}) => {
    <div>
      <div className="board">
      (ReasonReact.arrayToElement(Array.of_list(
                  List.map((row) => {
                    <span className="row">
                    (ReasonReact.arrayToElement(Array.of_list((List.map((cellData) => {
                      cell(cellData)
                    }, row)))))
                    </span>
                  },
                  state.board)
      )))
      </div>
      <button onClick={_event => send(StartTimer)}>{
        ReasonReact.stringToElement("Start") }</button>
      <button onClick={_event => send(StopTimer)}>{
        ReasonReact.stringToElement("Stop") }</button>
    </div>
  }
};
