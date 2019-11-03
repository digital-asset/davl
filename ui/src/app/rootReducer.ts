import { combineReducers, Action } from 'redux-starter-kit'

import * as bossView from '../features/bossView/reducer';
import * as employeeView from '../features/employeeView/reducer';
import { ThunkAction } from 'redux-thunk';
import Ledger from '../ledger/Ledger';

const rootReducer = combineReducers({
  employeeView: employeeView.reducer,
  pendingApprovals: bossView.reducer,
})

export type RootState = ReturnType<typeof rootReducer>

export default rootReducer

export const reload = (ledger: Ledger): ThunkAction<void, RootState, null, Action<string>> => async (dispatch) => {
  await Promise.all([
    dispatch(employeeView.loadAll(ledger)),
    dispatch(bossView.load(ledger)),
  ]);
}
