import React, { Component } from 'react';

import NavBar from './components/NavBar'

import logo from './logo.svg';
import './App.scss';

class App extends Component {
  render() {
    return (
      <div className="App">
          <NavBar/>
      </div>
    );
  }
}

export default App;
