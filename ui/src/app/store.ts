import { configureStore, Action, getDefaultMiddleware } from 'redux-starter-kit'
import { ThunkAction } from 'redux-thunk';
import createThunkErrorHandlerMiddleware from 'redux-thunk-error-handler';
import thunkMiddleware from 'redux-thunk-recursion-detect';

import rootReducer, { RootState } from './rootReducer'
import { toast } from 'react-semantic-toasts';

const defaultErrorHandler = (error: unknown) => {
  console.log(error);
  const description: string =
    typeof error === 'string'
    ? error
    : error instanceof Error
    ? error.toString()
    : JSON.stringify(error);
  toast({
    title: 'Unhandled error',
    type: 'error',
    description,
    time: 0,
  });
}

const store = configureStore({
  reducer: rootReducer,
  middleware: [
    createThunkErrorHandlerMiddleware({ onError: defaultErrorHandler }),
    thunkMiddleware,
    ...getDefaultMiddleware({
      serializableCheck: false,
    }).slice(1),
  ],
});

// if (process.env.NODE_ENV === 'development' && module.hot) {
//   module.hot.accept('./rootReducer', () => {
//     const newRootReducer = require('./rootReducer').default
//     store.replaceReducer(newRootReducer)
//   })
// }

export type AppDispatch = typeof store.dispatch;

export type AppThunk<R = void> = ThunkAction<Promise<R>, RootState, null, Action<string>>;

export default store;
