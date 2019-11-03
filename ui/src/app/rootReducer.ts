import { combineReducers, Action } from 'redux-starter-kit'
import { ThunkAction } from 'redux-thunk';

import * as bossView from '../features/bossView/reducer';
import * as employeeView from '../features/employeeView/reducer';
import Ledger from '../ledger/Ledger';

const rootReducer = combineReducers({
  employeeView: employeeView.reducer,
  bossView: bossView.reducer,
})

export type RootState = ReturnType<typeof rootReducer>

export default rootReducer

export const reload = (ledger: Ledger): ThunkAction<void, RootState, null, Action<string>> => async (dispatch) => {
  await Promise.all([
    dispatch(employeeView.loadAll(ledger)),
    dispatch(bossView.loadRequests(ledger)),
  ]);
}
