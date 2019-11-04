import { configureStore, Action, getDefaultMiddleware } from 'redux-starter-kit'
import { ThunkAction } from 'redux-thunk';
import createThunkErrorHandlerMiddleware from 'redux-thunk-error-handler';
import thunkMiddleware from 'redux-thunk-recursion-detect';

import rootReducer, { RootState } from './rootReducer'
import Ledger from '../ledger/Ledger';

const defaultErrorHandler = (error: unknown) => {
  console.log(error);
  alert(`Unhandled error:\n${error}`);
}

const store = configureStore({
  reducer: rootReducer,
  middleware: [
    createThunkErrorHandlerMiddleware({ onError: defaultErrorHandler }),
    thunkMiddleware,
    ...getDefaultMiddleware().slice(1),
  ]
});

if (process.env.NODE_ENV === 'development' && module.hot) {
  module.hot.accept('./rootReducer', () => {
    const newRootReducer = require('./rootReducer').default
    store.replaceReducer(newRootReducer)
  })
}

export type AppDispatch = typeof store.dispatch;

export type AppThunk<R = void> = ThunkAction<Promise<R>, RootState, null, Action<string>>;

export default store;

export const getLedger = (getState: () => RootState): Ledger => {
  const credentials = getState().auth.credentials;
  if (credentials) {
    return new Ledger(credentials);
  } else {
    throw Error('credentials not initialized')
  }
}
