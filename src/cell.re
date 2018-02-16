let component = ReasonReact.statelessComponent("Cell");

let make = (~cellData: CellData.data, _children) => {
  ...component,
  render: (_self) =>
    if (cellData.value == 1) {
      <span className="cell alive"
        style=(ReactDOMRe.Style.make(~backgroundColor=cellData.color, ())) />
    } else {
      <span className="cell dead"
        style=(ReactDOMRe.Style.make(~backgroundColor=cellData.color, ())) />
    }
};
