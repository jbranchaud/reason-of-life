type action =
  | StartTimer
  | StopTimer
  | Tick;

type state = {
  running: bool,
  timerId: ref(option(Js.Global.intervalId)),
  board: list(list(int))
};

let cell = (value: int) => {
  if (value == 1) {
    <span className="cell alive" />
  } else {
    <span className="cell dead" />
  };
};

let generateBoard = (size: int): list(list(int)) => {
  let board = Array.make(size, Array.to_list(Array.make(size, 0)));
  Array.to_list(board);
};

let generateRandomBoard = (size: int): list(list(int)) => {
  let board = Array.make(size, []);

  for (rowIndex in 0 to size-1) {
    let currRow = Array.make(size, 0);
    for (cellIndex in 0 to size-1) {
      if (Random.int(3) == 0) {
        currRow[cellIndex] = 1;
      };
    };
    board[rowIndex] = Array.to_list(currRow);
  };

  Array.to_list(board);
};

let neighborValue = (board: list(list(int)), rowIndex: int, cellIndex: int):
  int => {
    switch (List.nth(List.nth(board, rowIndex), cellIndex)) {
    | value => value
    | exception Invalid_argument("List.nth") => 0
    | exception Failure("nth") => 0
    };
  };

let tickBoard = (currBoard: list(list(int))): list(list(int)) => {
  let size = List.length(currBoard);
  let board = Array.make(size, []);

  for (rowIndex in 0 to size-1) {
    let currRow = Array.make(size, 0);
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

      let currStatus = List.nth(List.nth(currBoard, rowIndex), cellIndex);
      let status =
        switch (currStatus, liveNeighborCount) {
        | (1, 0) => 0
        | (1, 1) => 0
        | (1, 2) => 1
        | (1, 3) => 1
        | (1, _) => 0
        | (0, 3) => 1
        | _ => currStatus
        };
      currRow[cellIndex] = status;
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
      <hr />
      <div className="board">
      (ReasonReact.arrayToElement(Array.of_list(
                  List.map((row) => {
                    <span className="row">
                    (ReasonReact.arrayToElement(Array.of_list((List.map((cellValue) => {
                      cell(cellValue)
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
