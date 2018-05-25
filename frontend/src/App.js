import React, { Component } from 'react';
import './App.css';

const CardsContext = React.createContext({});

class App extends Component {
  constructor() {
    super();
    this.state = {
      "gameData": {},
      "cards": {}
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
        })

      if (Object.keys(this.state.cards).length === 0) {
        fetch('http://localhost:8080/games/1/cards')
          .then(results => results.json())
          .then(data => {
            this.setState({"cards": data})
          })
      }
    }

    f()
  }

  render() {
    const board = this.state.gameData.board;
    let log = [];

    if (board) {
      log = board.log
    }
    return (
      <CardsContext.Provider value={this.state.cards}>
        <div className="App">
          <Board board={board} />
          <Log log={log} />
        </div>
      </CardsContext.Provider>
    );
      /*
          <pre style={{textAlign: "left"}}>
            {JSON.stringify(this.state.gameData, null, 2)}
          </pre>
        */
  }
}

function formatLocation(target) {
  return target[0] + "/" + target[1]
}

function formatDestination(to, order) {
  return to + "/" + order
}

function logMove(entry, key) {
  const {target, to, order} = entry;
  return <li key={key}>Move {formatLocation(target)} to {formatDestination(to, order)} </li>
}

function logReveal(entry, key) {
  const {visibility, target} = entry;

  switch (visibility) {
    case "All": return <li key={key}>Reveal {formatLocation(target)}</li>;
    case "Hidden": return <li key={key}>"Hide " {formatLocation(target)}</li>;
    default: return <li key={key}>Unknown vis: {visibility}</li>
  }
}

function logResources(entry, key) {
  const {player, amount} = entry;

  let amountDesc = Object.keys(amount)
    .filter((x) => amount[x] !== 0)
    .map((x) => x + " " + (amount[x] > 0 ? "+" : "") + amount[x])
    .join(", ")

  return <li key={key}>Change player {player} resources: {amountDesc}</li>;
}

function logShuffle(entry, key) {
  return <li key={key}>Shuffle {entry.target}</li>;
}

class LogEntryTagged extends Component {
  constructor() {
    super()
    this.state = {showDetail: false}

    this.handleClick = this.handleClick.bind(this)
  }

  handleClick() {
    this.setState({showDetail: !this.state.showDetail})
  }

  render() {
    const {tag, action} = this.props.entry;
    const showDetail = this.state.showDetail;

    let actions = [action]
    if (action.type === "sequence") {
      actions = action.actions
    }
    return (
      <li>
        {tag}
        &nbsp;
        <a href="#toggle" onClick={this.handleClick}>({showDetail ? "-" : "+"})</a>
      {showDetail && <ul>
          {actions.map((l, i) => lookupLogComponent(l.type)(l, i))}
        </ul>}
      </li>
    )
  }
}

function lookupLogComponent(type) {
  let logComponents = {
    "move": logMove,
    "reveal": logReveal,
    "tagged": (entry, key) => <LogEntryTagged key={key} entry={entry} />,
    "resources": logResources,
    "shuffle": logShuffle,
    "none": () => ""
  }

  let x = logComponents[type];

  if (!x) {
    return (e, k) => <li key={k}>{JSON.stringify(e)}</li>
  }
  return x
}



class Log extends Component {
  render() {
    let log = this.props.log;

    if (!log)
      log = [];

    return (
      <div className='logContainer'>
        <h2>Log</h2>
        <ul>
          {log.reverse().map((l, i) => lookupLogComponent(l.type)(l, i))}
        </ul>
      </div>
    )
  }
}

function statusMessage(board) {
  if (board.state.tag === "lost") {
    return <p className="lost">Game Lost: {board.state.status}</p>
  } else if (board.state.tag === "waiting") {
    return <p className="waiting">{board.state.description}</p>
  } else {
    return null;
  }
}

class Board extends Component {
  render() {
    const board = this.props.board;
    const currentPlayer = 0;

    if (!board)
      return null;

    return (
      <div className='board'>
        {statusMessage(board)}
        <a href='#end' onClick={endTurn(currentPlayer)}>End Turn</a>
        <div className='boardRow'>
          <Location cards={board.cards["villian-deck"]} title="Villian Deck" layout="stacked" />
          <div className='city'>
            <Location cards={board.cards["city-0"]} title="Sewers" actions={attackActions(currentPlayer, 0)} />
            <Location cards={board.cards["city-1"]} title="Bank" actions={attackActions(currentPlayer, 1)}  />
            <Location cards={board.cards["city-2"]} title="Rooftops" actions={attackActions(currentPlayer, 2)} />
            <Location cards={board.cards["city-3"]} title="Streets" actions={attackActions(currentPlayer, 3)}  />
            <Location cards={board.cards["city-4"]} title="Bridge" actions={attackActions(currentPlayer, 4)} />
          </div>
          <Location cards={board.cards["escaped"]} title="Escaped" layout="stacked" />
          <Location cards={board.cards["ko"]} title="KO" layout="stacked" />
        </div>
        <div className='boardRow'>
          <Location cards={board.cards["hero-deck"]} title="Hero Deck" layout="stacked" />
          <Location cards={board.cards["hq"]} title="HQ" actions={purchaseCardActions(0)} />
        </div>
        {board.players.map((p) => <Player board={board} id={p.id} key={p.id} />)}
      </div>
    )
  }
}

function playerLocation(id, key) {
  return "player-" + id + "-" + key;
}

function endTurn(playerId) {
  return () => {
    fetch('http://localhost:8080/games/1/players/' + playerId + '/choose', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({type: "ChooseEndTurn"})
    })
  }
}

function chooseCard(playerId, specificCard) {
  return () => {
    fetch('http://localhost:8080/games/1/players/' + playerId + '/choose', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({type: "ChooseCard", card: specificCard})
    })
  }
}


function playCardActions(playerId) {
  return (c, i) => chooseCard(playerId, ["player-" + playerId + "-hand", i])
}

function purchaseCardActions(playerId) {
  return (c, i) => chooseCard(playerId, ["hq", i])
}

function attackActions(playerId, city) {
  return (c, i) => chooseCard(playerId, ["city-" + city, i])
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
        <div className="playerRow">
          <Location cards={cardsAt("playerdeck")} title="Deck" layout="stacked" />
          <div className="workingArea">
            <Location cards={cardsAt("played")} title="Played" />
            <Location cards={cardsAt("hand")} title="Hand" actions={playCardActions(id)} />
          </div>
          <Location cards={cardsAt("discard")} title="Discard" layout="stacked" />
          <Location cards={cardsAt("victory")} title="Victory" layout="stacked" />
        </div>
      </div>
    )
  }
}

class Location extends Component {
  render() {
    return <CardsContext.Consumer>{cardDb => {
      let {cards, title, layout, actions} = this.props;

      if (!layout)
        layout = "horizontal";
      if (!cards)
        cards = [];

      if (!actions)
        actions = (c, i) => null;

      let cardRender = null;

      if (layout === "stacked") {
        if (cards.length > 0) {
          let cardDetail = cards[0].visible ? cardDb[cards[0].name] : null
          if (cardDetail) {
            cardRender = (
              <div>
                <Card card={cardDetail} />
                <span>({cards.length} cards)</span>
              </div>
            )
          } else {
            cardRender = (
              <div>
                <CardBasic card={cards[0]} />
                <span>({cards.length} cards)</span>
              </div>
            )
          }
        } else {
          cardRender = <div>Empty</div>
        }
      } else {
        cardRender = (
          <div className={"location-" + layout}>
            {cards.map((c, i) => {
              let cardDetail = c.visible ? cardDb[c.name] : null

              return <a className="cardLink" href='#x' onClick={actions(c, i)} key={i}>
                {cardDetail ? <Card card={cardDetail} /> : <CardBasic card={c} />}
              </a>
            })}
          </div>
        )
      }

      return (
        <div className='cardLocation'>
          <h3>{title}</h3>
          {cardRender}
        </div>
      )
    }}</CardsContext.Consumer>
  }
}

class Card extends Component {
  render() {
    const card = this.props.card;

    if (card.type === "hero") {
      return (
        <div className="card">
          <span className="cardName">{card.name}</span>
          <span className="cardDescription">{card.description}</span>
          <div className="footer">
            <span>{card.baseMoney > 0 ? ("★" + card.baseMoney) : ("⚔" + card.baseAttack)}</span>
            <span>{card.cost > 0 ? "$" + card.cost : ""}</span>
          </div>

        </div>
      )
    } else if (card.type === "enemy") {
      return (
        <div>
          <div className="card">
            <span className="cardName">{card.name}</span>
            <span className="cardDescription">{card.description}</span>
            <div className="footer">
              <span></span>
              <span>{"⚔" + card.health}</span>
            </div>

          </div>
        </div>
      )
    }
  }
}

class CardBasic extends Component {
  render() {
    const card = this.props.card;

    if (card.visible) {
      return (
        <div>
          {card.name}
        </div>
      )
    } else {
      return <div>Hidden</div>
    }
  }
}

export default App;
