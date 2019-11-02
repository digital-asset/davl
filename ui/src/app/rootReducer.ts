import { combineReducers } from 'redux-starter-kit'

import employeeInfoReducer from '../features/employeeInfo/reducer';
import myApprovedVacationsReducer from '../features/myApprovedVacations/reducer';

const rootReducer = combineReducers({
  employeeInfo: employeeInfoReducer,
  approvedVacations: myApprovedVacationsReducer,
})

export type RootState = ReturnType<typeof rootReducer>

export default rootReducer
