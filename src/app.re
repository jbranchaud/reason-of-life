[%bs.raw {|require('./app.css')|}];

[@bs.module] external logo : string = "./logo.svg";

let component = ReasonReact.statelessComponent("App");

type name = { firstName: string, lastName: string };

let myName: name = { firstName: "Josh", lastName: "Branchaud" };

let formatName = (name) =>
  name.firstName ++ " " ++ name.lastName;

let nameComponent = (aName: name) => {
  <p>
    (ReasonReact.stringToElement("My name is " ++ formatName(aName)))
  </p>
};

let make = (~message, _children) => {
  ...component,
  render: (_self) =>
    <div className="App">
      <div className="App-header">
        <img src=logo className="App-logo" alt="logo" />
        <h2> (ReasonReact.stringToElement(message)) </h2>
      </div>
      <p className="App-intro">
        (ReasonReact.stringToElement("To get started, edit"))
        <code> (ReasonReact.stringToElement(" src/app.re ")) </code>
        (ReasonReact.stringToElement("and save to reload."))
      </p>
      (nameComponent(myName))
      <Counter />
      <Life />
    </div>
};
