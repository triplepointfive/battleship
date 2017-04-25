import * as React from "react";

interface MainMenuState { screen: string; }

export class MainMenu extends React.Component<null, MainMenuState> {
  constructor() {
    super();

    this.state = { screen: "main" };
  }

  render() {
    return <div className="card">
        <div className="card-header">
          Battleship
        </div>
        <div className="card-block">
          {this.currentScreen()}
        </div>
      </div>;
  }

  private currentScreen() {
    if ("main" === this.state.screen) {
      return this.mainScreen();
    }
  }

  private mainScreen() {
    return <div className="row">
        <div className="col-md-6">
          <a href="#" className="btn btn-primary">Start new game</a>
        </div>
        <div className="col-md-6">
          <div className="input-group">
            <input type="text" className="form-control" placeholder="Game ID"/>
            <span className="input-group-btn">
              <button className="btn btn-secondary" type="button">Join!</button>
            </span>
          </div>
        </div>
      </div>;
  }
}
