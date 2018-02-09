[%bs.raw {|require('./app.css')|}];

[@bs.module] external logo : string = "./logo.svg";

let link = (name, href) => {
  <a href=(href)>(ReasonReact.stringToElement(name))</a>
};

let component = ReasonReact.statelessComponent("App");

let make = (~message, _children) => {
  ...component,
  render: (_self) =>
    <div className="App">
      <div className="App-header">
        <img src=logo className="App-logo" alt="logo" />
        <h2> (ReasonReact.stringToElement(message)) </h2>
      </div>
      <p className="App-intro">
        (ReasonReact.stringToElement("An implementation of "))
        (link("Conway's Game of Life",
              "https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life"))
        (ReasonReact.stringToElement(" written with "))
        (link("ReasonML", "https://reasonml.github.io/"))
        (ReasonReact.stringToElement(" and "))
        (link("React", "https://reactjs.org/"))
      </p>
      <Life />
    </div>
};
