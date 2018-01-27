type action =
  | Increment
  | Decrement
  | StartTimer
  | StopTimer;

type state = {
  running: bool,
  timerId: ref(option(Js.Global.intervalId)),
  count: int
};

let counter = ReasonReact.reducerComponent("Counter");

let make = (_children) => {
  ...counter,
  didMount: (self) => {
    self.state.timerId := Some(Js.Global.setInterval(() => {
      self.send(Increment)}, 1000));
    ReasonReact.NoUpdate
  },
  willUnmount: (self) => {
    switch (self.state.timerId^) {
    | Some(id) => Js.Global.clearInterval(id);
    | None => ()
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
        newSelf.send(Increment)}, 1000));
    };
  },
  initialState: () => { running: true, timerId: ref(None), count: 1 },
  reducer: (action, state) =>
    switch (action) {
    | Increment => ReasonReact.Update({...state, count: state.count + 1})
    | Decrement => ReasonReact.Update({...state, count: state.count - 1})
    | StartTimer => ReasonReact.Update({...state, running: true })
    | StopTimer => ReasonReact.Update({...state, running: false })
    },
  render: ({state, send}) => {
    <div>
      <h2> { ReasonReact.stringToElement("Count: " ++
                                         string_of_int(state.count)) } </h2>
      <button onClick={_event => send(Increment)}>{
        ReasonReact.stringToElement("Inc") }</button>
      <button onClick={_event => send(Decrement)}>{
        ReasonReact.stringToElement("Dec") }</button>
      <br />
      <button onClick={_event => send(StartTimer)}>{
        ReasonReact.stringToElement("Start") }</button>
      <button onClick={_event => send(StopTimer)}>{
        ReasonReact.stringToElement("Stop") }</button>
    </div>
  }
};
