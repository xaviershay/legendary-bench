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
          <Log log={log} />
          <Board board={board} />
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
    return <p className="status lost">Game Lost: {formatDescription(board.state.status)}</p>
  } else if (board.state.tag === "waiting") {
    return <p className="status waiting">{board.state.description}</p>
  } else {
    return null;
  }
}

class Board extends Component {
  constructor() {
    super()

    this.state = {
      actingAs: 0,
      tracking: true
    }

    this.handleActingChange = this.handleActingChange.bind(this)
    this.handleTrackingChange = this.handleTrackingChange.bind(this)
  }

  handleActingChange(e) {
    this.setState({
      actingAs: e.target.value * 1
    })
  }

  handleTrackingChange(e) {
    const board = this.props.board;
    const currentPlayer = board.players[0].id

    this.setState({
      tracking: !this.state.tracking,
      actingAs: currentPlayer
    })
  }

  render() {
    const board = this.props.board;
    let currentPlayer;

    if (board && this.state.tracking) {
      currentPlayer = board.players[0].id
    } else {
      currentPlayer = this.state.actingAs
    }

    if (!board)
      return null;

    return (
      <div className='board'>
        <div className='boardHeader'>
          <div>
          <a href='#end' onClick={endTurn(currentPlayer)}>End Turn</a>
          <br />
          <a href='#end' onClick={pass(currentPlayer)}>Pass</a>
          <br />
          <a href='#end' onClick={chooseBool(currentPlayer, false)}>No</a> | <a href='#end' onClick={chooseBool(currentPlayer, true)}>Yes</a>
          <form>
            {board.players.map((p) => <div key={p.id}>
              <label>
              <input
                name='acting'
                type='radio'
                checked={this.state.actingAs === p.id}
                onChange={this.handleActingChange}
                value={p.id}
                disabled={this.state.tracking}
              /> Player {p.id}
              </label>
            </div>)}
            <label>
              <input
                name='tracking'
                type='checkbox'
                checked={this.state.tracking}
                onChange={this.handleTrackingChange}
              /> Track current player
            </label>
          </form>
          </div>
          {statusMessage(board)}
        </div>
        <div className='boardRow'>
          <Location cards={board.cards["villian-deck"]} title="Villian Deck"
            layout="stacked" />
          <div className='city'>
            <Location cards={board.cards["city-0"]} title="Sewers"
              layout="stacked"
              actions={attackActions(currentPlayer, 0)} />
            <Location cards={board.cards["city-1"]} title="Bank"
              layout="stacked"
              actions={attackActions(currentPlayer, 1)}  />
            <Location cards={board.cards["city-2"]} title="Rooftops"
              layout="stacked"
              actions={attackActions(currentPlayer, 2)}
            />
            <Location cards={board.cards["city-3"]} title="Streets"
              layout="stacked"
              actions={attackActions(currentPlayer, 3)}  />
            <Location cards={board.cards["city-4"]} title="Bridge"
              layout="stacked"
              actions={attackActions(currentPlayer, 4)} />
          </div>
          <Location cards={board.cards["escaped"]} title="Escaped"
            layout="stacked" />
          <Location cards={board.cards["ko"]} title="KO"
            layout="stacked" />
          <Location cards={board.cards["bystander"]} title="Bystander"
            layout="stacked" />
          <Location cards={board.cards["wound"]} title="Wound"
            layout="stacked" />
        </div>
        <div className='boardRow'>
          <Location cards={board.cards["hero-deck"]} title="Hero Deck"
            layout="stacked" />
          <Location cards={board.cards["hq"]} title="HQ"
            actions={purchaseCardActions(currentPlayer)} />
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

function pass(playerId) {
  return () => {
    fetch('http://localhost:8080/games/1/players/' + playerId + '/choose', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({type: "ChoosePass"})
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
      body: JSON.stringify({type: "ChooseCard", card: {
        type: "ByIndex",
        location: specificCard[0],
        index: specificCard[1]
      }})
    })
  }
}

function chooseBool(playerId, choice) {
  return () => {
    fetch('http://localhost:8080/games/1/players/' + playerId + '/choose', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({type: "ChooseBool", choice: choice})
    })
  }
}

function chooseCardActions(loc, playerId) {
  return (c, i) => chooseCard(playerId, [loc, i])
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
        <div className="playerRow">
          <div>
            <h4>Player {id}</h4>
            <p>{resources.attack} Attack<br/>{resources.money} Recruit</p>
          </div>
          <Location cards={cardsAt("playerdeck")} title="Deck" layout="stacked" />
          <div className="workingArea">
            <Location cards={cardsAt("hand")} title="Hand" actions={chooseCardActions(playerLocation(id, "hand"), id)} />
            <Location cards={cardsAt("played")} title="Played" actions={chooseCardActions(playerLocation(id, "played"), id)} />
            <Location cards={cardsAt("working")} title="Working" actions={chooseCardActions(playerLocation(id, "working"), id)} hideIfEmpty={true} />
          </div>
          <Location cards={cardsAt("discard")} title="Discard" layout="stacked" actions={chooseCardActions(playerLocation(id, "discard"), id)} />
          <Location cards={cardsAt("victory")} title="Victory" layout="stacked" />
        </div>
      </div>
    )
  }
}

function lookupCard(cardDb, templateId) {
  let card = cardDb[templateId];

  return card;
}

function pluralize(n, base) {
  if (n === 1) {
    return n + " " + base
  } else {
    return n + " " + base + "s"
  }
}

class Location extends Component {
  constructor(props) {
    super(props)
    this.state = {}
  }
  render() {
    var self = this;
    return <CardsContext.Consumer>{cardDb => {
      let {cards, title, layout, actions, hideIfEmpty} = this.props;

      if (!layout)
        layout = "horizontal";
      if (!cards)
        cards = [];

      if (!actions)
        actions = (c, i) => null;

      let cardRender = null;

      if (layout === "stacked" && !self.state.expand) {
        if (cards.length > 0) {
          let cardDetail = cards[0].visible ? lookupCard(cardDb, cards[0].templateId) : null
          if (cardDetail) {
            cardRender = (
              <div>
                <a className="cardLink" href='#x' onClick={actions(cards[0], 0)}>
                  <Card card={cardDetail} />
                </a>
                <div className="stackSummary">(<a href='#expand' onClick={() => this.setState({expand: true})}>{pluralize(cards.length, "card")}</a>)</div>
              </div>
            )
          } else {
            cardRender = (
              <div>
                <CardBasic card={cards[0]} />
                <div className="stackSummary">(<a href='#expand' onClick={() => this.setState({expand: true})}>{pluralize(cards.length, "card")}</a>)</div>
              </div>
            )
          }
        } else {
          cardRender = <div>Empty</div>
        }
      } else {
        cardRender = (
          <div>
          {layout === "stacked" ? <a href='#collapse' onClick={() => this.setState({expand: false})}>Collapse</a> : null}
          <div className={"location-horizontal"}>
            {cards.map((c, i) => {
              let cardDetail = c.visible ? lookupCard(cardDb, c.templateId) : null

              return <a className="cardLink" href='#x' onClick={actions(c, i)} key={i}>
                {cardDetail ? <Card card={cardDetail} /> : <CardBasic card={c} />}
              </a>
            })}
          </div>
          </div>
        )
      }
      var locationClass = "";
      if (hideIfEmpty && cards.length === 0)
        locationClass = "hidden";

      return (
        <div className={'cardLocation ' + locationClass}>
          <h4>{title}</h4>
          {cardRender}
        </div>
      )
    }}</CardsContext.Consumer>
  }
}

function formatDescription(x) {
  return x.split('\n').map((item, key) => {
    return <span key={key}>{item}<br/></span>
  })
}

function cardPip(card) {
  if (card.baseMoney !== null) {
    return <span className="cardMoney">★{card.baseMoney}</span>
  } else if (card.baseAttack !== null) {
    return <span className="cardAttack">⚔{card.baseAttack}</span>
  } else {
    return <span></span>
  }

}
class Card extends Component {
  render() {
    const card = this.props.card;

    if (card.type === "hero") {
      /* Some weird name/ablity logic here to deal with S.H.E.I.L.D cards */
      return (
        <div className={"card ability-type-" + card.heroAbilityType}>
          <span className="cardName">{card.ability !== "" ? card.ability : card.name}</span>
          {card.ability !== "" && <span className="heroName">{card.name}</span>}
          <span className="cardDescription">{formatDescription(card.description)}</span>
          <div className="footer">
            {cardPip(card)}
            <span className="cardCost">{card.cost > 0 ? "$" + card.cost : ""}</span>
          </div>

        </div>
      )
    } else if (card.type === "enemy") {
      return (
        <div>
          <div className="card">
            <span className="cardName">{card.name}</span>
            <span className="cardDescription">
            {card.fight && <span><strong>Fight:</strong> {card.fight}</span>}
            </span>
            <div className="footer">
              <span className="card-vp">{"♕" + card.vp}</span>
              <span className="cardAttack">{"⚔" + card.attack}</span>
            </div>

          </div>
        </div>
      )
    } else if (card.type === "bystander") {
      return (
        <div>
          <div className="card">
            <span className="cardName">Bystander</span>
            <span className="cardDescription"></span>
            <div className="footer">
            </div>
          </div>
        </div>
      )
    } else if (card.type === "wound") {
      return (
        <div>
          <div className="card">
            <span className="cardName">Wound</span>
            <span className="cardDescription"></span>
            <div className="footer">
            </div>
          </div>
        </div>
      )
    } else if (card.type === "twist") {
      return (
        <div>
          <div className="card">
            <span className="cardName">Scheme Twist</span>
            <span className="cardDescription"></span>
            <div className="footer">
            </div>
          </div>
        </div>
      )
    } else if (card.type === "masterstrike") {
      return (
        <div>
          <div className="card">
            <span className="cardName">Master Strike</span>
            <span className="cardDescription"></span>
            <div className="footer">
            </div>
          </div>
        </div>
      )
    } else {
      throw new Error("Unknown card type: " + card.type)
    }
  }
}

class CardBasic extends Component {
  render() {
    const card = this.props.card;

    if (card.visible !== "Hidden") {
      return (
        <div>
          {card.name}
        </div>
      )
    } else {
      return <div className="card back"></div>
    }
  }
}

export default App;
