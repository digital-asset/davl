import { combineReducers } from 'redux';

import * as employeeView from '../features/employeeView/employeeViewReducer';
import * as auth from './authReducer';
import * as daml from './damlReducer';

const rootReducer = combineReducers({
  employeeView: employeeView.reducer,
  auth: auth.reducer,
  daml: daml.reducer,
});

export type RootState = ReturnType<typeof rootReducer>;

export default rootReducer;
