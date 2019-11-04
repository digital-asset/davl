import React from 'react';
import ReactDOM from 'react-dom';
import { Provider } from 'react-redux';
import { SemanticToastContainer } from 'react-semantic-toasts';
import 'semantic-ui-css/semantic.min.css';
import 'react-semantic-toasts/styles/react-semantic-alert.css';
import './index.css';

import store from './app/store';


const render = () => {
  const App = require('./app/App').default

  ReactDOM.render(
    <Provider store={store}>
      <App />
      <SemanticToastContainer animation='fly up' />
    </Provider>,
    document.getElementById('root')
  )
}

render()

// if (process.env.NODE_ENV === 'development' && module.hot) {
//   module.hot.accept('./app/App', render)
// }
