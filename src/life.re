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
      let newBoard = List.map((row) => {
        List.map((cell) => {
          if (cell == 0) {
            1
          } else {
            0
          };
        }, row)
      }, state.board);
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
