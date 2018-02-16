type action =
  | StartTimer
  | StopTimer
  | Tick
  | ResetBoard;

type state = {
  running: bool,
  timerId: ref(option(Js.Global.intervalId)),
  board: list(list(CellData.data)),
  size: int
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
      board: Board.generate(30, Random),
      size: 30
  },
  reducer: (action, state) =>
    switch (action) {
    | StartTimer => ReasonReact.Update({...state, running: true })
    | StopTimer => ReasonReact.Update({...state, running: false })
    | Tick => {
      let newBoard = Board.tick(state.board);
      ReasonReact.Update({...state, board: newBoard})
      }
    | ResetBoard => {
      let newBoard = Board.generate(state.size, Random);
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
                      <Cell cellData={cellData} />
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
      <button onClick={_event => send(ResetBoard)}>{
        ReasonReact.stringToElement("Reset") }</button>
    </div>
  }
};
