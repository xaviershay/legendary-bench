import React, { Component } from 'react';
import logo from './logo.svg';
import './App.css';

class App extends Component {
  constructor() {
    super();
    this.state = {
      "gameData": {"b": 2}
    }
  }

  componentDidMount() {
    fetch('http://localhost:8080/games/1')
      .then(results => results.json())
      .then(data => {
        console.log(data)
        this.setState({"gameData": data})
      })
  }

  render() {
    return (
      <div className="App">
        <pre style={{textAlign: "left"}}>
          {JSON.stringify(this.state.gameData, null, 2)}
        </pre>
      </div>
    );
  }
}

export default App;
