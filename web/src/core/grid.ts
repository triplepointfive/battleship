export enum CellState { Empty = 0, HasShip = 1, Miss = 2, Hit = 3 }

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

    this.grid[2][3] = CellState.HasShip;
    this.grid[2][4] = CellState.HasShip;

    this.grid[5][4] = CellState.HasShip;
    this.grid[7][4] = CellState.HasShip;
    this.grid[8][4] = CellState.HasShip;

    this.grid[6][6] = CellState.HasShip;

    this.grid[8][2] = CellState.Hit;
    this.grid[8][3] = CellState.Miss;
  }

  public actOn(i: number, j: number): void {
    switch (this.getCellState(i, j)) {
      case CellState.Empty:
        this.grid[i][j] = CellState.Miss;
        break;
      case CellState.HasShip:
        this.grid[i][j] = CellState.Hit;
        break;
    }
  }

  public getCellState(i: number, j: number): CellState {
    if (i < 0 || j < 0 || i >= this.width || j >= this.height) {
      return CellState.Empty;
    } else {
      return this.grid[i][j];
    }
  }
}
