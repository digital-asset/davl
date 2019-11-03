import { combineReducers, Action } from 'redux-starter-kit'

import * as employeeInfo from '../features/employeeInfo/reducer';
import * as myApprovedVacations from '../features/myApprovedVacations/reducer';
import * as pendingApprovals from '../features/pendingApprovals/reducer';
import * as pendingRequests from '../features/pendingRequests/reducer'
import { ThunkAction } from 'redux-thunk';
import Ledger from '../ledger/Ledger';

const rootReducer = combineReducers({
  employeeInfo: employeeInfo.reducer,
  approvedVacations: myApprovedVacations.reducer,
  pendingApprovals: pendingApprovals.reducer,
  pendingRquests: pendingRequests.reducer,
})

export type RootState = ReturnType<typeof rootReducer>

export default rootReducer

export const reload = (ledger: Ledger): ThunkAction<void, RootState, null, Action<string>> => async (dispatch) => {
    dispatch(employeeInfo.load(ledger));
    dispatch(myApprovedVacations.load(ledger));
    dispatch(pendingApprovals.load(ledger));
    dispatch(pendingRequests.load(ledger));
}
