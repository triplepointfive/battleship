interface OutMessage {
  build(): string;
}

type Callback = (msg: string) => void;

export class Server {
  private socket: WebSocket;

  constructor(
    private playerIDCallback: Callback,
    private ownCallback: Callback
  ) {
    this.socket = new WebSocket("wss://powerful-headland-52719.herokuapp.com");

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

  private parseEvent(event: MessageEvent): void {
    const data = event.data.split(" ");

    switch (data[0]) {
      case "PID":
        this.playerIDCallback(data[1]);
        break;
      case "OWN":
        this.ownCallback(data[1]);
        break;
      default:
        console.error(`Invalid message ${event.data}`);
    }
  }
}
