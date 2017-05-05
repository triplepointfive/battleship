import * as React from "react";

import { Server } from "../core/server";
import { Board } from "./Board";
import { Grid } from "../core/grid";

import { JoinGameDialog } from "./JoinGameDialog";

enum Screen { JoinGame, EnemyField }

interface MainMenuState {
  screen: Screen;
  gameID?: string;
  ownGrid: Grid;
  enemyGrid: Grid;
}

export class MainMenu extends React.Component<null, MainMenuState> {
  private server: Server;

  constructor() {
    super();

    this.server = new Server(
      e => { this.setGameID(e); },
      e => { this.setOwnField(e); },
      e => { this.setEnemyField(e); }
    );

    this.state = {
      screen: Screen.JoinGame,
      ownGrid: new Grid(10, 10),
      enemyGrid: new Grid(10, 10)
    };
  }

  render() {
    return <div className="row mt-3 row-eq-height">
      <div className="col-md-6">
        <div className="card">
          <div className="card-header">
            Your field
            <div className="float-right">
              &nbsp;{this.state.gameID}
            </div>
          </div>
          <div className="card-block">
            <Board grid={this.state.ownGrid}/>
          </div>
        </div>
      </div>
      <div className="col-md-6">
        <div className="card">
          <div className="card-header">
            Enemy's field
          </div>
          <div className="card-block">
            {this.currentScreen()}
          </div>
        </div>
      </div>
    </div>;
  }

  private currentScreen() {
    switch (this.state.screen) {
      case Screen.JoinGame:
        return this.joinGameScreen();
      case Screen.EnemyField:
        return this.enemysFieldScreen();
    }
  }

  private joinGameScreen() {
    return <div className="row">
        <div className="col-md-6">
          <a href="#" className="btn btn-primary">Start new game</a>
        </div>
        <div className="col-md-6">
          <JoinGameDialog server={this.server}/>
        </div>
      </div>;
  }

  private enemysFieldScreen() {
    return <Board grid={this.state.enemyGrid}/>;
  }

  private setGameID(gameID: string): void {
    this.setState({ gameID: gameID });
  }

  private setOwnField(grid: string): void {
    this.setState({ ownGrid: this.state.ownGrid.refresh(grid) });
  }

  private setEnemyField(grid: string): void {
    this.setState({
      screen: Screen.EnemyField,
      ownGrid: this.state.enemyGrid.refresh(grid)
    });
  }
}
