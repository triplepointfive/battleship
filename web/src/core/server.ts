type Callback = (msg: string) => void;

// const url = "wss://powerful-headland-52719.herokuapp.com";
const url = "ws://0.0.0.0:3636";

export class Server {
  private socket: WebSocket;

  private gameJoinErrorCallback: Callback;

  constructor(
    private playerIDCallback: Callback,
    private ownCallback: Callback,
    private enemyCallback: Callback
  ) {
    this.socket = new WebSocket(url);

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

  public joinGame(gameID: string, onError: Callback): void {
    this.gameJoinErrorCallback = onError;
    this.socket.send(`JoinGame ${gameID}`);
  }

  private parseEvent(event: MessageEvent): void {
    const data = event.data.split(" ");

    switch (data[0]) {
      case "PlayerID":
        this.playerIDCallback(data[1]);
        break;
      case "Own":
        this.ownCallback(data[1]);
        break;
      case "Enemy":
        this.enemyCallback(data[1]);
        break;
      case "NotFoundGameID":
        this.gameJoinErrorCallback(data[0]);
        break;
      default:
        console.error(`Invalid message ${event.data}`);
    }
  }
}
