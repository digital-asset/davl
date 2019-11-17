import { combineReducers } from 'redux';
import { Action } from 'redux-starter-kit';
import { ThunkAction } from 'redux-thunk';

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

export const reload = (): ThunkAction<Promise<void>, RootState, null, Action<string>> => async (dispatch) => {
  await Promise.all([
    dispatch(employeeView.loadSummary()),
  ]);
}
