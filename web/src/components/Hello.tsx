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
      header.push(<th>{this.columnName(j)}</th>);
    }
    grid.push(<tr><th></th>{header}</tr>);

    for (let i = 0; i < height; i++) {
      let row = [];

      for (let j = 0; j < width; j++) {
        const state: CellState = g.getCellState(i, j);

        let style: string;

        switch (state) {
          case CellState.HasShip:
            style = "ship";
            break;
          default:
            style = "";
            break;
        }

        row.push(
          <td className={style} key={i * j + j}></td>
        );
      }

      grid.push(<tr key={i}><th>{this.rowName(i)}</th>{row}</tr>);
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

  private rowName(id: number): string {
    return String(id + 1);
  }

  private columnName(id: number): string {
    return "абвгдежзиклмнопрстуфхцчшщъыьэюя"[id % 31];
  }
}
