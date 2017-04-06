import * as React from "react";

import { Grid, CellState } from "../core/grid";

export interface HelloProps { compiler: string; framework: string; }

// 'HelloProps' describes the shape of props.
// State is never set so we use the 'undefined' type.
export class Hello extends React.Component<HelloProps, undefined> {
  render() {
    const width: number = 10;
    const height: number = 10;

    let g = new Grid(width, height);

    let grid = [];

    let header = [];
    for (let j = 0; j < width; j++) {
      header.push(<th key={j}>{this.columnName(j)}</th>);
    }
    grid.push(<tr key="-1"><th key="-1"></th>{header}</tr>);

    for (let i = 0; i < height; i++) {
      let row = [];

      for (let j = 0; j < width; j++) {
        const state: CellState = g.getCellState(i, j);

        let style: string = "";
        let content: string = "";

        switch (state) {
          case CellState.HasShip:
            style = "ship";

            if (this.notTaken(g, i, j + 1)) { style += " ship-border-right"; }
            if (this.notTaken(g, i, j - 1)) { style += " ship-border-left"; }
            if (this.notTaken(g, i + 1, j)) { style += " ship-border-bottom"; }
            if (this.notTaken(g, i - 1, j)) { style += " ship-border-top"; }
            break;

          case CellState.Hit:
            style = "ship-hit";

            if (this.notTaken(g, i, j + 1)) { style += " ship-border-right"; }
            if (this.notTaken(g, i, j - 1)) { style += " ship-border-left"; }
            if (this.notTaken(g, i + 1, j)) { style += " ship-border-bottom"; }
            if (this.notTaken(g, i - 1, j)) { style += " ship-border-top"; }
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
          <td className={style} key={i * j + j}>{content}</td>
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

  private notTaken(grid: Grid, i: number, j: number): boolean {
    const state: CellState = grid.getCellState(i, j);
    return state === CellState.Miss || state === CellState.Empty;
  }

  private rowName(id: number): string {
    return String(id + 1);
  }

  private columnName(id: number): string {
    return "абвгдежзиклмнопрстуфхцчшщъыьэюя"[id % 31];
  }
}
