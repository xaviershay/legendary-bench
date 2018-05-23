import React, { Component } from 'react';
import './App.css';

class App extends Component {
  constructor() {
    super();
    this.state = {
      "gameData": {"b": 2}
    }
  }

  componentDidMount() {
    let version = 0;
    let f = () => {
      fetch('http://localhost:8080/games/1?version=' + version)
        .then(results => results.json())
        .then(data => {
          this.setState({"gameData": data})
          version = data.board.version;
          f();
          // TODO: Set up a long poll here
        })
    }

    f()
  }

  render() {
    return (
      <div className="App">
        <Board board={this.state.gameData.board} />
      </div>
    );
      /*
          <pre style={{textAlign: "left"}}>
            {JSON.stringify(this.state.gameData, null, 2)}
          </pre>
        */
  }
}
class Board extends Component {
  render() {
    const board = this.props.board;

    if (!board)
      return null;

    return (
      <div>
        <Location cards={board.cards["hq"]} title="HQ" />
        {board.players.map((p) => <Player board={board} id={p.id} key={p.id} />)}
      </div>
    )
  }
}

function playerLocation(id, key) {
  return "player-" + id + "-" + key;
}

function playCard(playerId, i) {
  return () => {
    fetch('http://localhost:8080/games/1/players/' + playerId + '/act', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({action: "PlayCard", index: i})
    })
  }
}

function playCardActions(playerId) {
  return (c, i) => <a href="#play" onClick={playCard(playerId, i)}>Play</a>
}

class Player extends Component {
  render() {
    const {id, board} = this.props;
    const resources = board.players[id].resources;
    const cardsAt = (key) => board.cards[playerLocation(id, key)]

    return (
      <div>
        <h2>Player {id}</h2>
        <p>{resources.attack} Attack, {resources.money} Money</p>
        <Location cards={cardsAt("hand")} title="Hand" actions={playCardActions(id)} />
        <Location cards={cardsAt("played")} title="Played" />
        <Location cards={cardsAt("discard")} title="Discard" layout="stacked" />
        <Location cards={cardsAt("playerdeck")} title="Deck" layout="stacked" />
      </div>
    )
  }
}

class Location extends Component {
  render() {
    let {cards, title, layout, actions} = this.props;

    if (!cards)
      cards = [];

    if (!actions)
      actions = (c, i) => null;

    let cardRender = null;

    if (layout === "stacked") {
      if (cards.length > 0) {
        cardRender = (
          <div>
            <Card card={cards[0]} />
            <span>({cards.length} cards)</span>
          </div>
        )

      } else {
        cardRender = <div>Empty</div>
      }
    } else {
      cardRender = (
        <ul>
          {cards.map((c, i) => <div key={i}><Card card={c} /> {actions(c, i)}</div>)}
        </ul>
      )
    }

    return (
      <div>
        <h3>{title}</h3>
        {cardRender}
      </div>
    )
  }
}

class Card extends Component {
  render() {
    const card = this.props.card;

    if (card.visible) {
      return <div>{card.name}</div>
    } else {
      return <div>Hidden</div>
    }
  }
}

export default App;
