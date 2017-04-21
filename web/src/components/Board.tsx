import * as React from "react";

import { Grid, CellState } from "../core/grid";

interface BoardProps { compiler: string; framework: string; }
interface BoardState { grid: Grid; }

export class Board extends React.Component<BoardProps, BoardState> {
  private width: number;
  private height: number;
  private socket: WebSocket;

  constructor(props: BoardProps) {
    super(props);

    this.width = 5;
    this.height = 5;

    this.state = { grid: new Grid(this.width, this.height) };

    this.socket = new WebSocket("ws://0.0.0.0:9160");

    this.socket.onopen = () => {
      console.debug("Соединение установлено.");
      this.socket.send("N"); // N for New.
    };

    this.socket.onclose = function(event) {
      if (event.wasClean) {
        console.info("Clean close");
      } else {
        console.error("Connection was cut");
      }
      console.info("Code: " + event.code + " reason: " + event.reason);
    };

    this.socket.onmessage = (event) => {
      console.info(event.data);

      this.setState({ grid: this.state.grid.refresh(event.data) });
    };

    this.socket.onerror = function(error) {
      console.error("Error " + error);
    };

  }

  render() {
    let grid = [];

    let header = [];
    for (let j = 0; j < this.width; j++) {
      header.push(<th key={j}>{this.columnName(j)}</th>);
    }
    grid.push(<tr key="-1"><th key="-1"></th>{header}</tr>);

    for (let i = 0; i < this.height; i++) {
      let row = [];

      for (let j = 0; j < this.width; j++) {
        const state: CellState = this.state.grid.getCellState(i, j);

        let style: string = "";
        let content: string = "";

        switch (state) {
          case CellState.HasShip:
            style = "ship";

            if (this.notTaken(i, j + 1)) { style += " ship-border-right"; }
            if (this.notTaken(i, j - 1)) { style += " ship-border-left"; }
            if (this.notTaken(i + 1, j)) { style += " ship-border-bottom"; }
            if (this.notTaken(i - 1, j)) { style += " ship-border-top"; }
            break;

          case CellState.Hit:
            style = "ship-hit";

            if (this.notTaken(i, j + 1)) { style += " ship-border-right"; }
            if (this.notTaken(i, j - 1)) { style += " ship-border-left"; }
            if (this.notTaken(i + 1, j)) { style += " ship-border-bottom"; }
            if (this.notTaken(i - 1, j)) { style += " ship-border-top"; }
            break;
          case CellState.Kill:
            style = "ship-kill";

            if (this.notTaken(i, j + 1)) { style += " ship-border-right"; }
            if (this.notTaken(i, j - 1)) { style += " ship-border-left"; }
            if (this.notTaken(i + 1, j)) { style += " ship-border-bottom"; }
            if (this.notTaken(i - 1, j)) { style += " ship-border-top"; }
            break;

          case CellState.Miss:
            style = "miss";
            content = "•";
            break;

          case CellState.Empty:
            style = "unknown";
            break;
        }

        row.push(
          <td
            className={style} key={i * j + j}
            onClick={ () => this.cellClicked(i, j) }
            >{content}</td>
        );
      }

      grid.push(<tr key={i}><th key={i}>{this.rowName(i)}</th>{row}</tr>);
    }

    return <div>
      <h1>
        from {this.props.compiler} and {this.props.framework}!
      </h1>
      <table className="battlefield table table-sm">
        <tbody>
          {grid}
        </tbody>
      </table>
    </div>;
  }

  private cellClicked(i: number, j: number): void {
    console.log("(" + i + "," + j + ")");
    this.socket.send("(" + i + "," + j + ")");
  }

  private notTaken(i: number, j: number): boolean {
    const state: CellState = this.state.grid.getCellState(i, j);
    return state === CellState.Miss || state === CellState.Empty;
  }

  private rowName(id: number): string {
    return String(id + 1);
  }

  private columnName(id: number): string {
    return "абвгдежзиклмнопрстуфхцчшщъыьэюя"[id % 31];
  }
}
