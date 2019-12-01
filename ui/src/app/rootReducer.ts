import { combineReducers } from 'redux';

import * as auth from './authReducer';

const rootReducer = combineReducers({
  auth: auth.reducer,
});

export type RootState = ReturnType<typeof rootReducer>;

export default rootReducer;
