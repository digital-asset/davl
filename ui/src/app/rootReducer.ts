import { combineReducers, Action } from 'redux-starter-kit'
import { ThunkAction } from 'redux-thunk';

import * as bossView from '../features/bossView/bossViewReducer';
import * as employeeView from '../features/employeeView/employeeViewReducer';
import * as auth from './authReducer';
import Ledger from '../ledger/Ledger';

const rootReducer = combineReducers({
  employeeView: employeeView.reducer,
  bossView: bossView.reducer,
  auth: auth.reducer,
})

export type RootState = ReturnType<typeof rootReducer>;

export default rootReducer;

export const reload = (ledger: Ledger): ThunkAction<Promise<void>, RootState, null, Action<string>> => async (dispatch) => {
  await Promise.all([
    dispatch(employeeView.loadAll(ledger)),
    dispatch(bossView.loadAll(ledger)),
  ]);
}
