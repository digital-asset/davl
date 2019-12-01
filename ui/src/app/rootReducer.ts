import { combineReducers } from 'redux';

import * as auth from './authReducer';
import * as daml from './damlReducer';

const rootReducer = combineReducers({
  auth: auth.reducer,
  daml: daml.reducer,
});

export type RootState = ReturnType<typeof rootReducer>;

export default rootReducer;
