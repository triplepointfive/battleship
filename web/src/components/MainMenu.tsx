import * as React from "react";

interface JoinGameDialogState { gameID: string; error: string; }

class JoinGameDialog extends React.Component<null, JoinGameDialogState> {
  constructor() {
    super();

    this.state = { gameID: "", error: "" };
  }

  render() {
    let errorMessage = null;

    if (this.state.error.length) {
      errorMessage = <div className="form-control-feedback">
          {this.state.error}
          </div>;
    }

    return <form onSubmit={e => this.onSubmit(e)}>
        <div className="form-group has-danger">
          <div className="input-group">
            <input type="text" value={this.state.gameID} className="form-control" placeholder="Game ID" onChange={e => this.handleChange(e)}/>
            <span className="input-group-btn">
              <button disabled={!this.state.gameID.length} className="btn btn-secondary btn-outline-danger" type="button">
                Join!
              </button>
            </span>
          </div>
          {errorMessage}
        </div>
      </form>;
  }

  private handleChange(event: React.FormEvent<HTMLInputElement>): void {
    this.setState({ gameID: event.currentTarget.value });
  }

  private onSubmit(event: React.FormEvent<HTMLFormElement>) {
    event.preventDefault();
    this.sendRequest();
  }

  private sendRequest(): void {
    if (!this.state.gameID.length) {
      return;
    }
    console.log(this.state.gameID);
    this.setState({ error: this.state.gameID });
  }
}

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
          <JoinGameDialog />
        </div>
      </div>;
  }
}
