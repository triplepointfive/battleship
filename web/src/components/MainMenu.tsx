import * as React from "react";

import { Server } from "../core/server";
import { Board } from "./Board";
import { Grid } from "../core/grid";

interface JoinGameDialogState { gameID: string; error: string; }

class JoinGameDialog extends React.Component<null, JoinGameDialogState> {
  constructor() {
    super();

    this.state = { gameID: "", error: "" };
  }

  render() {
    let errorMessage = null;

    let formClass = "form-group" ;
    let buttonClass = "btn btn-secondary";

    if (this.hasError()) {
      errorMessage = <div className="form-control-feedback">
          {this.state.error}
          </div>;
      formClass += " has-danger";
      buttonClass += " btn-outline-danger";
    }

    return <form onSubmit={e => this.onSubmit(e)}>
        <div className={formClass}>
          <div className="input-group">
            <input type="text" value={this.state.gameID} className="form-control" placeholder="Game ID" onChange={e => this.handleChange(e)}/>
            <span className="input-group-btn">
              <button disabled={!this.state.gameID.length} className={buttonClass} type="button" onClick={e => this.onButtonClick(e) }>
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

  private onButtonClick(event: React.FormEvent<HTMLButtonElement>) {
    event.preventDefault();
    this.sendRequest();
  }

  private onSubmit(event: React.FormEvent<HTMLFormElement>) {
    event.preventDefault();
    this.sendRequest();
  }

  private hasError(): boolean {
    return !!this.state.error.length;
  }

  private sendRequest(): void {
    if (!this.state.gameID.length) {
      return;
    }
    console.log(this.state.gameID);
    this.setState({ error: this.state.gameID });
  }
}

interface MainMenuState { screen: string; gameID?: string; field: Grid; }

export class MainMenu extends React.Component<null, MainMenuState> {
  private server: Server;

  constructor() {
    super();

    this.server = new Server(
      e => { this.setGameID(e); },
      e => { this.setOwnField(e); }
    );

    this.state = { screen: "main", field: new Grid(10, 10) };
  }

  render() {
    return <div className="row mt-3 row-eq-height">
      <div className="col-md-6">
        <div className="card">
          <div className="card-header">
            Your field
            <div className="float-right">
              {this.state.gameID}
            </div>
          </div>
          <div className="card-block">
            <Board grid={this.state.field}/>
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

  private setGameID(gameID: string): void {
    this.setState({ gameID: gameID });
  }

  private setOwnField(field: string): void {
    this.setState({ field: this.state.field.refresh(field) });
  }
}
