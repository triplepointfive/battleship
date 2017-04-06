import * as React from "react";
import * as ReactDOM from "react-dom";

import { Board } from "./components/Board";

ReactDOM.render(
    <Board compiler="a" framework="b" />,
    document.getElementById("example")
);
