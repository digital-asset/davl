import { createSlice, PayloadAction } from 'redux-starter-kit';
import Ledger from '../../ledger/Ledger';
import { AppThunk } from '../../app/store';
import * as davl from '../../daml/DAVL';
import { Vacation, makeVacation } from '../../utils/vacation';
import { Summary } from './types';

export type State = {
  summary: Summary | null;
  pending: Vacation[];
  approved: Vacation[];
}

const initialState: State = {
  summary: null,
  approved: [],
  pending: [],
}

const slice = createSlice({
  name: 'employeeView',
  initialState,
  reducers: {
    setSummary: (state: State, action: PayloadAction<Summary>) => ({...state, summary: action.payload}),
    setApproved: (state: State, action: PayloadAction<Vacation[]>) => ({...state, approved: action.payload}),
    setPending: (state: State, action: PayloadAction<Vacation[]>) => ({...state, pending: action.payload}),
  },
});

export const { setSummary, setApproved, setPending } = slice.actions;

export const reducer = slice.reducer;

export const loadSummary = (ledger: Ledger): AppThunk<Promise<void>> => async (dispatch) => {
  try {
    const key = {employeeRole: {employee: ledger.party()}};
    const {data: {employeeRole: {employee, boss}, remainingDays}} =
      await ledger.pseudoFetchByKey(davl.EmployeeVacationAllocation, key);
    const summary: Summary = {
      employee,
      boss,
      remainingVacationDays: remainingDays,
    }
    dispatch(setSummary(summary));
  } catch (error) {
    alert(`Unknown error:\n${error}`);
  }
}

export const loadApproved = (ledger: Ledger): AppThunk<Promise<void>> => async (dispatch) => {
  try {
    const vacationContracts =
      await ledger.query(davl.Vacation, {employeeRole: {employee: ledger.party()}});
    const vacations: Vacation[] =
      vacationContracts.map((vacation) => makeVacation(vacation.contractId, vacation.data));
    dispatch(setApproved(vacations));
  } catch (error) {
    alert(`Unknown error:\n${error}`);
  }
}

export const loadPending = (ledger: Ledger): AppThunk<Promise<void>> => async (dispatch) => {
  try {
    const requests =
      await ledger.query(davl.VacationRequest, {vacation: {employeeRole: {employee: ledger.party()}}});
    const vacations: Vacation[] =
      requests.map((request) => makeVacation(request.contractId, request.data.vacation));
    dispatch(setPending(vacations));
  } catch (error) {
    alert(`Unknown error:\n${error}`);
  }
}

export const loadAll = (ledger: Ledger): AppThunk<Promise<void>> => async (dispatch) => {
  await Promise.all([
    dispatch(loadSummary(ledger)),
    dispatch(loadApproved(ledger)),
    dispatch(loadPending(ledger)),
  ]);
}
