import { combineReducers, Action } from 'redux-starter-kit'

import * as employeeInfo from '../features/employeeInfo/reducer';
import * as myApprovedVacations from '../features/myApprovedVacations/reducer';
import { ThunkAction } from 'redux-thunk';
import Ledger from '../ledger/Ledger';

const rootReducer = combineReducers({
  employeeInfo: employeeInfo.reducer,
  approvedVacations: myApprovedVacations.reducer,
})

export type RootState = ReturnType<typeof rootReducer>

export default rootReducer

export const reload = (ledger: Ledger): ThunkAction<void, RootState, null, Action<string>> => async (dispatch) => {
  return Promise.all([
    dispatch(employeeInfo.load(ledger)),
    dispatch(myApprovedVacations.load(ledger)),
  ])
}
