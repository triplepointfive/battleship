interface OutMessage {
  build(): string;
}

class NewGameOut implements OutMessage {
  build(): string { return "NewGame"; }
}

class NewGameIDIn { constructor(public id: number) {} }

export class Server {
  private socket: WebSocket;

  private newGameIDInCallback: (msg: NewGameIDIn) => void;

  constructor() {
    this.socket = new WebSocket("ws://0.0.0.0:9160");

    this.socket.onopen = () => {
      this.socket.send("");
    };

    this.socket.onclose = function(event) {
      // TODO: Reopen?
      if (event.wasClean) {
        console.error("Clean close");
      } else {
        console.error("Connection was cut");
      }
      console.error("Code: " + event.code + " reason: " + event.reason);
    };

    this.socket.onmessage = (event) => this.parseEvent(event);
    this.socket.onerror = (error) => console.error("Error " + error);
  }

  public newGame(callback: (msg: NewGameIDIn) => void): void {
    this.newGameIDInCallback = callback;
    this.socket.send((new NewGameOut).build());
  }

  private parseEvent(event: Event): void {
    if (this.newGameIDInCallback) {
      this.newGameIDInCallback(new NewGameIDIn(123));
    }
  }
}
