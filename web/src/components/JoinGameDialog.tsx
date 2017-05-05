import * as React from "react";

import { Server } from "../core/server";

interface JoinGameDialogState { gameID: string; error: string; }
export interface JoinGameDialogProps { server: Server; }

export class JoinGameDialog extends React.Component<JoinGameDialogProps, JoinGameDialogState> {
  constructor(props: JoinGameDialogProps) {
    super(props);

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
    if (this.state.gameID.length) {
      this.props.server.joinGame(
        this.state.gameID,
        (error) => this.setErrorMessage(error)
      );
    }
  }

  private setErrorMessage(msg: string): void {
    console.log(msg);
    switch (msg) {
      case "NotFoundGameID":
        this.setState({ error: "No players with specified id!" });
        break;
    }
  }
}
