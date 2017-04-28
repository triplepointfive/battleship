export enum CellState { Empty = 0, HasShip = 1, Miss = 2, Hit = 3, Kill = 4 }

export class Grid {
  private grid: Array<Array<CellState>> = [];

  constructor(public width: number, public height: number) {
    for (let i: number = 0; i < height; i++) {
      let row = [];

      for (let j: number = 0; j < width; j++) {
        row.push(CellState.Empty);
      }

      this.grid.push(row);
    }
  }

  public refresh(grid: string): Grid {
    for (let k: number = 0; k < grid.length; k++) {
      let cell = CellState.Empty;

      switch (grid[k]) {
        case "E":
          cell = CellState.Empty;
          break;
        case "M":
          cell = CellState.Miss;
          break;
        case "H":
          cell = CellState.HasShip;
          break;
        case "I":
          cell = CellState.Hit;
          break;
        case "K":
          cell = CellState.Kill;
          break;
        default:
          console.error("Unknown cell type: " + grid[k]);
      }

      let i = Math.trunc(k / this.height);
      let j = k % this.width;
      this.grid[i][j] = cell;
    }


    for (let i: number = 0; i < this.height; i++) {
      let row = [];

      for (let j: number = 0; j < this.width; j++) {
        row.push(CellState.Empty);
      }

      this.grid.push(row);
    }

    return this;
  }

  public getCellState(i: number, j: number): CellState {
    if (i < 0 || j < 0 || i >= this.width || j >= this.height) {
      return CellState.Empty;
    } else {
      return this.grid[i][j];
    }
  }
}
